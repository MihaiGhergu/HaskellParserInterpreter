module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.List as List

-- Definire Program
data SProgram = SProgram {
            nume :: String,
            pnume :: String,
            info :: ClassState
} deriving (Show, Eq, Ord)

data Program = Program [SProgram] deriving (Show, Eq, Ord)
type Instruction = String

initEmptyProgram :: Program
initEmptyProgram = Program [SProgram "Global" "Global" initEmptyClass]

getVars :: Program -> [[String]]
getVars (Program []) = [] 
getVars (Program p) = if (nume (head p) == "Global") then (getValues (info (head p)) Var) 
                        else getVars (Program (tail p))

getClasses :: Program -> [String]
getClasses (Program []) = []
getClasses (Program p) = (nume (head p)) : (getClasses (Program (tail p)))

getParentClass :: String -> Program -> String
getParentClass _ (Program []) = ""
getParentClass nume_clasa (Program p) = if (nume(head p) == nume_clasa) then (pnume(head p)) 
                                        else (getParentClass nume_clasa (Program (tail p)))
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass _ (Program []) = []
getFuncsForClass nume_clasa (Program p) = if(nume(head p) == nume_clasa) 
                                            then(getValues (info (head p) ) Func)
                                        else (getFuncsForClass nume_clasa (Program (tail p)))
-- Instruction poate fi ce considerați voi
parse :: String -> [Instruction]
parse str = filter(\k -> k /= "") (List.lines str)

--functia de parsare dupa spatiu = : ( )
mySplit' :: String -> [String]
mySplit' [] = [""]
mySplit' (x:xs) 
    | x ==' ' = "" : rest
    | x =='=' = "" : rest
    | x ==':' = "" : rest
    | x ==')' = "" : rest
    | x =='(' = "" : rest
    | x ==',' = "" : rest
    |otherwise = (x : head rest) : tail rest 
    where rest = mySplit' xs
{-
functie care imi elimina stringurile goale din lista de 
stringurile returnata de functia de parsare
-}
mySplit :: String -> [String]
mySplit xs = filter(\k -> k /= "") (mySplit' xs)

--functie cu care voi verifica daca o functie are toti param valizi
paramValizi :: [String] -> Program -> Int
paramValizi [] (Program p) = 0
paramValizi (x:xs) (Program p) = if (x`elem`(getClasses (Program p)))
                                    then 1 + (paramValizi xs (Program p)) 
                                else (paramValizi xs (Program p))

interpret :: Instruction -> Program -> Program
interpret instr (Program p) = if( cheie=="class" && lenn==2 ) then --pentru clase
    if( nume_clasa `elem` getClasses (Program p)) then (Program p) else Program(newClsGLBL:p)
                            else if( cheie=="class" && lenn==4 ) then 
    if(nume_clasa`elem`getClasses (Program p)) then (Program p) else
        if(nume_parinte`elem`getClasses (Program p)) then Program(newClsP:p) 
            else Program(newClsGLBL:p)
        -- pentru variabile
    else if(cheie=="newvar" && lenn==3) then if(numeVarCls`elem` getClasses (Program p)) then 
        Program(map (\k -> if (nume k) == "Global" then 
                    (SProgram "Global" "Global" (insertIntoClass (info k) Var (nume_clasa:numeVarCls:[])))
                            else k) p)
                                            else (Program p)
        --pentru functii
    else if(tip_returnat`elem`getClasses (Program p) && (nume_clasaF`elem`getClasses (Program p)) 
                            && (paramValizi listaParam (Program p)) == lenListaParam) then
        Program( map (\k -> if((nume k) == nume_clasaF) then 
                (SProgram nume_clasaF (getParentClass nume_clasaF (Program p)) 
                    (insertIntoClass (info k) Func (nume_functie:tip_returnat:listaParam)) ) 
                            else k  ) p )
    else (Program p)
    where
    cheie = head(mySplit instr) -- class/newvar
    lenn = length(mySplit instr) --lungime lista returnata de functia de parse
    nume_clasa = head(tail(mySplit instr)) -- nume clasa
    nume_parinte = last(mySplit instr) -- nume parinte
    newClsGLBL = SProgram nume_clasa "Global" initEmptyClass -- noua clasa cu parintele global
    newClsP = SProgram nume_clasa nume_parinte initEmptyClass -- noua clasa cu parintele care exista deja
    numeVarCls = last(mySplit instr) -- nume variabila
    tip_returnat = head (mySplit instr) --param intors
    nume_clasaF = head(tail(mySplit instr))--clasa din care face parte functia
    nume_functie = head(tail(tail(mySplit instr))) --numele functiei
    listaParam = tail(tail(tail(mySplit instr))) --lista de param a functiei
    lenListaParam = length listaParam--lungime lista parametrii

{-
  Functie care intoarce toate functiile din lantul de mosteniri 
  ale unei clase plecand de la o clasa data
-}
takeAllFuncsAvailable:: String -> Program -> [[String]]
takeAllFuncsAvailable "Global" p = []
takeAllFuncsAvailable numeClasa p = (getFuncsForClass numeClasa p) ++ rest
    where
        rest = (takeAllFuncsAvailable (getParentClass numeClasa p) p)

infer :: Expr -> Program -> Maybe String
infer (Va a) p = varExist a (getVars p)
infer (FCall var func expr) p = funcExist clsvar func p (map' infer expr p) allFunc
    where 
        clsvar = varExist var (getVars p)
        allFunc = takeAllFuncsAvailable (takeJust clsvar) p 

map' f [] p = []
map' f expr p = ((f (head expr) p):(map' f (tail expr) p))

--functia care verifica daca variabila este in lista de variabile a programului
varExist :: String -> [[String]] -> Maybe String
varExist var globVar
    | globVar == [] = Nothing
    | var == head h = Just (head (tail h))
    | otherwise = varExist var t
    where 
        h = head globVar
        t = tail globVar

funcExist :: Maybe String -> String -> Program -> [Maybe String] -> [[String]] -> Maybe String
funcExist Nothing _ _ _ _ = Nothing
funcExist (Just cls) nfunc p lexpr lfunc
    | lfunc == [] = Nothing
    | lfunc == [] && pcls /= "Global" = funcExist (Just pcls) nfunc p lexpr funcs
    | (nfunc:l) == ((head h ):(tail (tail h))) = Just (head(tail h))
    | otherwise = funcExist (Just cls) nfunc p lexpr (tail lfunc)
    where 
        h = head lfunc
        l = map takeJust lexpr
        pcls = getParentClass cls p
        funcs  = (getFuncsForClass pcls p)

--functie care intoarce a din Just a
takeJust :: Maybe String -> String
takeJust (Just a) = a
takeJust Nothing = "nimic"

