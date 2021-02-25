module Brig.Types.SparAuthId
  ( AuthId (..),
    EmailSource (..),
    EmailWithSource (..),
    ScimDetails (..),
    ExternalId (..),
    runAuthId,
    authIdUref,
    authIdExternalId,
    authIdSCIMEmail,
  )
where

import Control.Lens
import Data.Id
-- import Data.String.Conversions (cs)
import Imports
import qualified SAML2.WebSSO as SAML
-- import qualified Text.Email.Parser
import Wire.API.User.Identity

data AuthId
  = AuthSAML SAML.UserRef
  | AuthSCIM ScimDetails
  | AuthBoth TeamId SAML.UserRef (Maybe EmailWithSource) -- userref and externalid are iso. Both DB indices (SAMLUserRef index and ExternalId index) are kept in sync
  deriving (Eq, Show)

data EmailWithSource = EmailWithSource {ewsEmail :: Email, ewsEmailSource :: EmailSource}
  deriving (Eq, Show)

data EmailSource
  = EmailFromExternalIdField
  | EmailFromEmailField
  deriving (Eq, Show)

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

authIdExternalId :: AuthId -> Maybe ExternalId
authIdExternalId =
  \case
    AuthSAML _ -> Nothing
    AuthSCIM (ScimDetails extId _) -> Just extId
    AuthBoth tid uref _ -> ExternalId tid <$> urefToExternalId uref
  where
    urefToExternalId :: SAML.UserRef -> Maybe Text
    urefToExternalId = SAML.shortShowNameID . view SAML.uidSubject

authIdSCIMEmail :: AuthId -> Maybe Email
authIdSCIMEmail = fmap ewsEmail . authIdSCIMEmailWithSource

authIdSCIMEmailWithSource :: AuthId -> Maybe EmailWithSource
authIdSCIMEmailWithSource =
  \case
    AuthSAML _ -> Nothing
    AuthSCIM (ScimDetails _ ews) -> Just ews
    AuthBoth _ _ mbEws -> mbEws

-- TODO: Do we need this?
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
