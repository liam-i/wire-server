-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Spar needs scim 'externalId's and saml 'UserRef's to identify a user: depending on the
-- context, one, the other, or both.  'externalId' is scoped inside a 'TeamId' (same
-- 'externalId' in different teams identifies different users).  'AuthId' is the type that
-- captures all allowed combinations of these, while trying to make illegal combinations
-- unrepresentable.
--
-- Since 'AuthId' needs to be stored with the user, and spar stores users in brig, 'AuthId' is
-- defined here.
module Brig.Types.SparAuthId
  ( AuthId (..),
    ScimDetails (..),
    ExternalId (..),
    EmailWithSource (..),
    EmailSource (..),
    LegacyAuthId (..),
    runAuthId,
    -- TODO: export whatever else whenever needed; remove what turns out to be not needed.
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Id
import Data.String.Conversions (cs)
import Imports
import qualified SAML2.WebSSO as SAML
import qualified Text.Email.Parser as Email
import Wire.API.User.Identity

data AuthId
  = AuthSAML SAML.UserRef
  | AuthSCIM ScimDetails
  | AuthBoth TeamId SAML.UserRef (Maybe EmailWithSource)
  deriving (Eq, Show)

data ScimDetails = ScimDetails ExternalId EmailWithSource
  deriving (Eq, Show)

data ExternalId
  = ExternalId TeamId Text
  deriving (Eq, Show)

data EmailWithSource = EmailWithSource
  { ewsEmail :: Email,
    ewsEmailSource :: EmailSource
  }
  deriving (Eq, Show)

data EmailSource
  = EmailFromExternalIdField
  | EmailFromEmailField
  deriving (Eq, Show, Generic)

-- | Before 'AuthId', data was stored in brig's cassandra in a type called 'UserSSOId' that
-- didn't correctly scope 'externalId's in their resp. 'TeamId's.  'LegacyAuthId' is used to
-- support reading these old values.
--
-- This can be done safely: the correct 'TeamId' is stored in the user (just not in the same
-- column), and this type only needs to be used in "Brig.Data.*".
--
-- 'LegacyAuthId' only has a 'FromJSON' instance; it is never written to the database.
--
-- FUTUREWORK: once UserSSOId has been migrated away from all cassandra instances, remove this
-- data type and everyhing that's using it.
newtype LegacyAuthId = LegacyAuthId {fromLegacyAuthId :: TeamId -> AuthId}

-- | 'SAML.UserRef' and 'externalId' are isomorphic.  (Both DB indices in spar are kept in
-- sync: if both exist and identify the same user, they do so via those `spar.user` and
-- `spar.scim_external`.)
--
-- The 'Nothing' case can occur if the 'NameID' has unsupported qualifiers.
--
-- (The 'TeamId' could be inferred from the saml issuer, but that would be a database lookup,
-- which would make the function even less total.)
urefToExternalId :: TeamId -> SAML.UserRef -> Maybe ExternalId
urefToExternalId tid uref = ExternalId tid <$> SAML.shortShowNameID (uref ^. SAML.uidSubject)

-- | See 'userRefToExternalId'.  'NameID' format is either email (if parseable) or unspecified
-- (if not).
externalIdToUref :: SAML.Issuer -> ExternalId -> SAML.UserRef
externalIdToUref = undefined

-- | Internal; only needed for aeson instances.
data AuthIdTyp = AuthIdTypSAML | AuthIdTypSCIM | AuthIdTypBoth

instance FromJSON AuthIdTyp where
  parseJSON (Aeson.String "saml") = pure AuthIdTypSAML
  parseJSON (Aeson.String "scim") = pure AuthIdTypSCIM
  parseJSON (Aeson.String "both") = pure AuthIdTypBoth
  parseJSON bad = fail $ "unknown AuthIdTyp: " <> show bad

instance ToJSON AuthIdTyp where
  toJSON AuthIdTypSAML = "saml"
  toJSON AuthIdTypSCIM = "scim"
  toJSON AuthIdTypBoth = "both"

instance ToJSON AuthId where
  toJSON =
    object . \case
      AuthSAML (SAML.UserRef tenant subject) ->
        [ "type" .= AuthIdTypSAML,
          "tenant" .= tenant,
          "subject" .= subject
        ]
      AuthSCIM (ScimDetails (ExternalId team extStr) ews) ->
        [ "type" .= AuthIdTypSCIM,
          "team" .= team,
          "external_id" .= extStr,
          "email" .= ewsEmail ews,
          "email_source" .= ewsEmailSource ews
        ]
      AuthBoth team (SAML.UserRef tenant subject) mbews ->
        [ "type" .= AuthIdTypBoth,
          "team" .= team,
          "tenant" .= tenant,
          "subject" .= subject,
          "email" .= (ewsEmail <$> mbews),
          "email_source" .= (ewsEmailSource <$> mbews)
        ]

instance FromJSON AuthId where
  parseJSON = withObject "AuthId" $ \ob -> do
    typ <- ob .: "type"
    case typ of
      AuthIdTypSAML -> do
        tenant <- ob .: "tenant"
        subject <- ob .: "subject"
        pure $ AuthSAML (SAML.UserRef tenant subject)
      AuthIdTypSCIM -> do
        team <- ob .: "team"
        external_id <- ob .: "external_id"
        email <- ob .: "email"
        email_source <- ob .: "email_source"
        pure $ AuthSCIM (ScimDetails (ExternalId team external_id) (EmailWithSource email email_source))
      AuthIdTypBoth -> do
        team <- ob .: "team"
        tenant <- ob .: "tenant"
        subject <- ob .: "subject"
        mbews <- do
          mbemail <- ob .:? "email"
          mbemail_source <- ob .:? "email_source"
          case (mbemail, mbemail_source) of
            (Just email, Just email_source) -> pure . Just $ EmailWithSource email email_source
            (Nothing, Nothing) -> pure Nothing
            bad -> fail $ "AuthId: need either email *and* source or neither: " <> show bad
        pure $ AuthBoth team (SAML.UserRef tenant subject) mbews

instance FromJSON LegacyAuthId where
  parseJSON val = withObject "LegacyAuthId" switch val
    where
      switch :: Aeson.Object -> Aeson.Parser LegacyAuthId
      switch = (.:? "type") >=> maybe (current val) (\(Aeson.String _) -> legacy val)

      current :: Aeson.Value -> Aeson.Parser LegacyAuthId
      current = fmap (LegacyAuthId . const) . parseJSON

      legacy :: Aeson.Value -> Aeson.Parser LegacyAuthId
      legacy = withObject "LegacyAuthId" $ \obj -> do
        mtenant <- obj .:? "tenant"
        msubject <- obj .:? "subject"
        meid <- obj .:? "scim_external_id"
        case (mtenant, msubject, meid) of
          (Just tenant, Just subject, Nothing) -> do
            pure . LegacyAuthId $
              (\_ -> AuthSAML (SAML.UserRef tenant subject))
          (Nothing, Nothing, Just eid) -> do
            email <- error "parseEmail" eid
            pure . LegacyAuthId $
              (\tid -> AuthSCIM (ScimDetails (ExternalId tid eid) (EmailWithSource email EmailFromExternalIdField)))
          _ -> do
            fail "either need tenant and subject, or scim_external_id, but not both"

instance ToJSON EmailSource

instance FromJSON EmailSource

-- | Take apart a 'AuthId', using 'SAML.UserRef' if available, otherwise 'Email'.
runAuthId :: (SAML.UserRef -> a) -> (Email -> a) -> AuthId -> a
runAuthId doUref doEmail = \case
  AuthSAML uref -> doUref uref
  AuthSCIM (ScimDetails _ (EmailWithSource email _)) -> doEmail email
  AuthBoth _ uref _ -> doUref uref

authIdUref :: AuthId -> Maybe SAML.UserRef
authIdUref = runAuthId (Just . id) (const Nothing)

authIdExternalId :: AuthId -> Maybe ExternalId
authIdExternalId = \case
  AuthSAML _ -> Nothing
  AuthSCIM (ScimDetails extId _) -> Just extId
  AuthBoth tid uref _ -> urefToExternalId tid uref

authIdEmail :: AuthId -> Maybe Email
authIdEmail = \case
  AuthSAML uref -> urefToEmail uref
  other -> authIdScimEmail other
  where
    urefToEmail :: SAML.UserRef -> Maybe Email
    urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
      SAML.UNameIDEmail email -> Just $ emailFromSAML email
      _ -> Nothing

    emailFromSAML :: HasCallStack => SAML.Email -> Email
    emailFromSAML = fromJust . parseEmail . cs . Email.toByteString . SAML.fromEmail

authIdScimEmail :: AuthId -> Maybe Email
authIdScimEmail = fmap ewsEmail . authIdScimEmailWithSource

authIdScimEmailWithSource :: AuthId -> Maybe EmailWithSource
authIdScimEmailWithSource =
  \case
    AuthSAML _ -> Nothing
    AuthSCIM (ScimDetails _ ews) -> Just ews
    AuthBoth _ _ mbEws -> mbEws
