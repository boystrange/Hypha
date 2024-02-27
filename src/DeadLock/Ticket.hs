--
-- WARNING!
-- This Module is not used!
--

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

module DeadLock.Ticket where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Data.Set.Unicode

type Ticket = Int
type TicketV = Int

data TicketE = Ticket Ticket
             | TiVar TicketV
             | TicketE :⊞: TicketE
               deriving (Eq, Ord)

type TicketNF = (Ticket, MS.MultiSet TicketV)

normalizeT :: TicketE -> TicketNF
normalizeT (Ticket n) = (n, MS.empty)
normalizeT (TiVar tvar) = (0, MS.singleton tvar)
normalizeT (tic1 :⊞: tic2) = let (n1, tvars1) = normalizeT tic1 in
                             let (n2, tvars2) = normalizeT tic2 in
                             (n1 + n2, MS.union tvars1 tvars2)

simplifyTic :: TicketE -> TicketE
simplifyTic tic = case normalizeT tic of
                    (n, ms) | MS.null ms -> Ticket n
                    (0, ms) | (tvar : tvars) <- MS.toList ms -> foldl (:⊞:) (TiVar tvar) (map TiVar tvars)
                    (n, ms) | tvars <- MS.toList ms -> foldl (:⊞:) (Ticket n) (map TiVar tvars)

findTic :: M.Map TicketV TicketV -> TicketV -> TicketV
findTic m ticvar | Just ticvar' <- M.lookup ticvar m = findTic m ticvar'
                 | otherwise = ticvar

substTic :: M.Map TicketV TicketV -> TicketE -> TicketE
substTic m (TiVar ticvar) = TiVar $ findTic m ticvar
substTic m (tic1 :⊞: tic2) = substTic m tic1 :⊞: substTic m tic2
substTic _ tic = tic

varsT :: TicketE -> S.Set TicketV
varsT (Ticket _) = S.empty
varsT (TiVar tivar) = S.singleton tivar
varsT (tic1 :⊞: tic2) = varsT tic1 ∪ varsT tic2

instance Show TicketE where
    show (Ticket ticket) = show ticket
    show (TiVar tivar) = "$" ++ show (-tivar) -- ticket variables are always negative
    show (ticket1 :⊞: ticket2) = show ticket1 ++ " + " ++ show ticket2
