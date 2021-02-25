module Brig.Types.SparAuthId
  ( AuthId (..),
    AuthPassDetails (..),
    EmailSource (..),
    ExternalId (..),
    runAuthId,
    authIdUref,
    authIdEmail,
  )
where

import Control.Lens
import Data.Id
import Imports
import qualified SAML2.WebSSO as SAML
import Wire.API.User.Identity

data AuthId
  = AuthSAML SAML.UserRef
  | AuthPass AuthPassDetails
  | AuthBoth SAML.UserRef AuthPassDetails
  deriving (Eq, Show)

data AuthPassDetails = AuthPassDetails ExternalId Email EmailSource
  deriving (Eq, Show)

data EmailSource
  = EmailFromExternalIdField
  | EmailFromEmailField
  deriving (Eq, Show)

data ExternalId
  = ExternalId TeamId Text
  deriving (Eq, Show)

-- | Take apart a 'Auth', using 'SAML.UserRef' if available, otehrwise 'Email'.
runAuthId :: (SAML.UserRef -> a) -> (Email -> a) -> AuthId -> a
runAuthId doUref doEmail = \case
  AuthSAML uref -> doUref uref
  AuthPass (AuthPassDetails _ email _) -> doEmail email
  AuthBoth uref _ -> doUref uref

authIdUref :: Prism' AuthId SAML.UserRef
authIdUref = prism' AuthSAML $
  \case
    AuthSAML uref -> Just uref
    AuthPass (AuthPassDetails _ _ _) -> Nothing
    AuthBoth uref _ -> Just uref

authIdEmail :: AuthId -> Maybe Email
authIdEmail =
  \case
    AuthSAML _ -> Nothing
    AuthPass (AuthPassDetails _ email _) -> Just email
    AuthBoth _ (AuthPassDetails _ email _) -> Just email

makeLenses ''AuthId
