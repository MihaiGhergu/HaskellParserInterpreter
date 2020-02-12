module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

-- TODO - Trebuie definit ClassState
data ClassState = ClassState (Map (InstrType, Int) [String]) deriving (Show, Eq, Ord)

initEmptyClass :: ClassState
initEmptyClass = ClassState Map.empty

--functie ce calculeaza dimensiunea unui map
f1 :: ClassState -> Int
f1 (ClassState map) = Map.size map

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState map) instrtype lista = ClassState (map' lista map)
    where map' = Map.insert (instrtype, (f1 (ClassState map)) +1 )

--Functie care returneaza din map Func/Var
f2 :: ClassState -> InstrType -> ClassState
f2 (ClassState map) instrtype = ClassState (Map.filterWithKey (\(k, _) _ -> k == instrtype) map)

--functie ce returneaza map ul din ClassState ul meu
f3 (ClassState myMap) = myMap

getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState map) instrtype =  Map.elems f4
    where f4 = f3 $ f2 (ClassState map) instrtype
