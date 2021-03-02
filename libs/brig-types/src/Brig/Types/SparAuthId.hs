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

module Brig.Types.SparAuthId
  ( AuthId (..),
    EmailSource (..),
    EmailWithSource (..),
    ScimDetails (..),
    ExternalId (..),
    runAuthId,
    authIdUref,
    authIdSCIMEmail,
  )
where

import Control.Lens ((^.))
-- import Control.Lens
-- import Data.String.Conversions (cs)
-- import qualified Text.Email.Parser
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair)
import Data.Id
import Imports
import qualified SAML2.WebSSO as SAML
import Wire.API.User.Identity

data AuthId
  = AuthSAML SAML.UserRef
  | AuthSCIM ScimDetails
  | AuthBoth TeamId SAML.UserRef (Maybe EmailWithSource) -- userref and externalid are iso. Both DB indices (SAMLUserRef index and ExternalId index) are kept in sync
  deriving (Eq, Show)

instance ToJSON AuthId where
  toJSON authid =
    object . (("is_auth_id" .= True) :) $
      case authid of
        AuthBoth tid uref mbEWS ->
          [ "team" .= tid,
            "email" .= (ewsEmail <$> mbEWS),
            "email_source" .= (ewsEmailSource <$> mbEWS)
          ]
            <> urefPairs uref
        AuthSCIM (ScimDetails (ExternalId tid extStr) (EmailWithSource email emailSource)) ->
          [ "team" .= tid,
            "external_id" .= extStr,
            "email" .= email,
            "email_source" .= emailSource
          ]
        AuthSAML uref -> urefPairs uref
    where
      urefPairs :: SAML.UserRef -> [Pair]
      urefPairs uref =
        [ "uref_tenant" .= (uref ^. SAML.uidTenant),
          "uref_subject" .= (uref ^. SAML.uidSubject)
        ]

instance FromJSON AuthId where
  parseJSON v =
    assertIsAuthId v
      >> ( parseBoth v
             <|> parseSCIM v
             <|> parseSAML v
         )
    where
      parseBoth = withObject "AuthId object" $ \ob -> do
        tid :: TeamId <- ob .: "team"
        email :: Maybe Email <- ob .:? "email"
        emailSource :: Maybe EmailSource <- ob .:? "email_source"
        uref <- parseUref ob
        pure $ AuthBoth tid uref (EmailWithSource <$> email <*> emailSource)

      parseSCIM = withObject "AuthId object" $ \ob -> do
        tid :: TeamId <- ob .: "team"
        extStr :: Text <- ob .: "external_id"
        email :: Email <- ob .: "email"
        emailSource :: EmailSource <- ob .: "email_source"
        pure $ AuthSCIM (ScimDetails (ExternalId tid extStr) (EmailWithSource email emailSource))

      parseSAML =
        withObject "AuthId object" $
          fmap AuthSAML . parseUref

      parseUref ob = do
        SAML.UserRef <$> (ob .: "uref_tenant") <*> (ob .: "uref_subject")

      assertIsAuthId = withObject "ob" $ \ob -> do
        isAuthId <- ob .: "is_auth_id"
        if isAuthId
          then pure ()
          else fail "is_auth_id key is not set to \"true\""

data EmailWithSource = EmailWithSource {ewsEmail :: Email, ewsEmailSource :: EmailSource}
  deriving (Eq, Show)

data EmailSource
  = EmailFromExternalIdField
  | EmailFromEmailField
  deriving (Eq, Show, Generic)

instance ToJSON EmailSource

instance FromJSON EmailSource

data ExternalId
  = ExternalId TeamId Text
  deriving (Eq, Show)

data ScimDetails = ScimDetails ExternalId EmailWithSource
  deriving (Eq, Show)

-- | Take apart a 'AuthId', using 'SAML.UserRef' if available, otherwise 'Email'.
runAuthId :: (SAML.UserRef -> a) -> (Email -> a) -> AuthId -> a
runAuthId doUref doEmail = \case
  AuthSAML uref -> doUref uref
  AuthSCIM (ScimDetails _ (EmailWithSource email _)) -> doEmail email
  AuthBoth _tid uref _ -> doUref uref

authIdUref :: AuthId -> Maybe SAML.UserRef
authIdUref =
  \case
    AuthSAML uref -> Just uref
    AuthSCIM _ -> Nothing
    AuthBoth _ uref _ -> Just uref

-- TODO: remove if this is not needed
-- authIdExternalId :: AuthId -> Maybe ExternalId
-- authIdExternalId =
--   \case
--     AuthSAML _ -> Nothing
--     AuthSCIM (ScimDetails extId _) -> Just extId
--     AuthBoth tid uref _ ->
--       -- TODO : also try EmailWithSource?
--       ExternalId tid <$> urefToExternalId uref
--   where
--     urefToExternalId :: SAML.UserRef -> Maybe Text
--     urefToExternalId = SAML.shortShowNameID . view SAML.uidSubject

authIdSCIMEmail :: AuthId -> Maybe Email
authIdSCIMEmail = fmap ewsEmail . authIdSCIMEmailWithSource

authIdSCIMEmailWithSource :: AuthId -> Maybe EmailWithSource
authIdSCIMEmailWithSource =
  \case
    AuthSAML _ -> Nothing
    AuthSCIM (ScimDetails _ ews) -> Just ews
    AuthBoth _ _ mbEws -> mbEws

-- TODO: remove this if not needed
-- authIdEmail :: AuthId -> Maybe Email
-- authIdEmail =
--   \case
--     AuthSAML _ uref -> urefToEmail uref
--     AuthSCIM (ScimDetails _ (EmailWithSource email _)) -> Just email
--     AuthBoth _ uref mbEws ->
--       urefToEmail uref <|> (ewsEmail <$> mbEws)
--   where
--     urefToEmail :: SAML.UserRef -> Maybe Email
--     urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
--       SAML.UNameIDEmail email -> Just $ emailFromSAML email
--       _ -> Nothing

--     emailFromSAML :: HasCallStack => SAML.Email -> Email
--     emailFromSAML =
--       fromJust . parseEmail . cs
--         . Text.Email.Parser.toByteString
--         . SAML.fromEmail
