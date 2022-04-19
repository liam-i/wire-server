-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Version where

import Brig.API.Handler
import Brig.App
import Control.Lens
import Imports
import Servant (ServerT)
import Wire.API.Routes.Named
import Wire.API.Routes.Version

versionAPI :: ServerT VersionAPI (Handler r)
versionAPI = Named $ do
  fed <- view federator
  dom <- viewFederationDomain
  pure $
    VersionInfo
      { vinfoSupported = supportedVersions,
        vinfoFederation = isJust fed,
        vinfoDomain = dom
      }