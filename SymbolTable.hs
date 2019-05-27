module SymbolTable where

import qualified Data.Map as Map
import GoatAST

type LaberCounter = Int
data SynTables = SynTables ProcTable LocalTable LaberCounter

-- proc id, 
data ProcTable = ProcTable (Map.Map Ident Param)


data LocalTable = LocalTable (Map.Map Ident VarInfo)
type Slotnum = Int
data VarInfo = VarInfo BaseType Slotnum Bool (Int,Int) Indic

initTables :: SynTables
initTables = SynTables initProcTable initLocalTable

initProcTable :: ProcTable
initProcTable = ProcTable Map.empty

initLocalTable :: LocalTable
initLocalTable = LocalTable Map.empty

selIncLabel :: SynTables -> SynTables
selIncLabel (SynTables a b i) = (SynTables a b i+1)