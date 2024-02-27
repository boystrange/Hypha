-- This file is part of Hypha.
--
-- Hypha is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Hypha is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Hypha.  If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2013-2014 Luca Padovani

module DeadLock.Polarity where

import qualified Data.Set as S

data Capability = CInput | COutput
                deriving (Eq, Ord)

instance Show Capability where
  show CInput = "?"
  show COutput = "!"
  
type Polarity = S.Set Capability

showPolarity :: Polarity -> String
showPolarity cset = case S.toList cset of
                      []     -> "∅"
                      [cap]  -> show cap
                      _      -> "#"

  
