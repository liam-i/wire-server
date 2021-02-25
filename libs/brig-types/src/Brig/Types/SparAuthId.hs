module Brig.Types.SparAuthId
  ( AuthId (..),
    EmailSource (..),
    EmailWithSource (..),
    ScimDetails (..),
    ExternalId (..),
    runAuthId,
    authIdUref,
  )
where

import Control.Lens
import Data.Id
import Imports
import qualified SAML2.WebSSO as SAML
import Wire.API.User.Identity

data AuthId
  = AuthSAML SAML.UserRef
  | AuthSCIM ScimDetails
  | AuthBoth TeamId SAML.UserRef (Maybe EmailWithSource) -- userref and externalid are iso. Both DB indices (SAMLUserRef index and ExternalId index) are kept in sync
  deriving (Eq, Show)

data EmailWithSource
  = EmailWithSource Email EmailSource
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

-- samlToExtId :: TeamId -> SAML.UserRef -> ExternalId
-- samlToExtId = undefined

-- getScimDetails :: AuthId -> Maybe ScimDetails
-- getScimDetails = error "Brig.Types.SparAuthId.getScimDetails"

-- getScimEmail :: AuthId -> Maybe Email
-- getScimEmail (AuthSAML _) = Nothing
-- getScimEmail (AuthSCIM _) = Nothing

-- getScimDetails (AuthSAML _uref) = Nothing
-- getScimDetails (AuthSCIM d) = Just d
-- getScimDetails (AuthBoth _tid _uref Nothing) = Nothing
-- getScimDetails (AuthBoth tid uref (Just ews)) = Just $ ScimDetails (samlToExtId tid uref) ews

-- setScimDetails :: ScimDetails -> AuthId -> AuthId
-- setScimDetails d (AuthSAML uref) = AuthSAML uref
-- setScimDetails d (AuthSCIM _) = AuthSCIM d
-- setScimDetails (ScimDetails extId (Just ews)) (AuthBoth tid uref mbEmailSource) = error "TODO"
-- setScimDetails (ScimDetails extId Nothing) (AuthBoth tid uref mbEmailSource) = error "TODO"

authIdUref :: Prism' AuthId SAML.UserRef
authIdUref = prism' AuthSAML $
  \case
    AuthSAML uref -> Just uref
    AuthSCIM _ -> Nothing
    AuthBoth _ uref _ -> Just uref

-- authIdEmail :: AuthId -> Maybe Email
-- authIdEmail =
--   \case
--     AuthSAML _ -> Nothing
--     AuthSCIM (ScimDetails _ mbes) -> emailFromSource mbes
--     AuthBoth _ uref mbes) -> Just email
--   where
--     emailFromSource :: Maybe EmailWithSource -> Maybe Email
--     emailFromSource (Just (EmailWithSource email _)) = Just email
--     emailFromSource Nothing = Nothing
