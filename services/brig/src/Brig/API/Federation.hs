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

module Brig.API.Federation where

import Brig.API.Error (handleNotFound, throwStd)
import Brig.API.Handler (Handler)
import Brig.App (viewFederationDomain)
import Brig.Types (UserHandleInfo (UserHandleInfo))
import qualified Brig.User.Handle as API
import Data.Handle (Handle)
import Data.Qualified (Qualified (Qualified))
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import qualified Wire.API.Federation.API.Brig as FederationAPIBrig

federationSitemap :: ServerT (ToServantApi FederationAPIBrig.Api) Handler
federationSitemap = genericServerT (FederationAPIBrig.Api getUserByHandle)

getUserByHandle :: Handle -> Handler UserHandleInfo
getUserByHandle handle = do
  maybeOwnerId <- lift $ API.lookupHandle handle
  case maybeOwnerId of
    Nothing -> throwStd handleNotFound
    Just ownerId -> UserHandleInfo . Qualified ownerId <$> viewFederationDomain