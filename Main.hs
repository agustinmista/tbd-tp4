module Main where

import Data.List (subsequences, (\\), intersperse)

type AttrSet a = [a]                    -- Conjuntos de atributos
type FuncDep a = (AttrSet a, AttrSet a) -- Dependencias funcionales

------- Funciones auxiliares ---------------------------------------------------

-- Elimina elementos repetidos de una lista (la abstrae a un conjunto)
toSet :: Eq a => [a] -> [a]
toSet = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

-- Checkea si dos listas tienen los mismos elementos (igualdad de conjuntos)
(=~=) :: Eq a => [a] -> [a] -> Bool
(=~=) a b = (null (a \\ b)) && (null (b \\ a))

-- Devuelve las subsecuencias de tamaño n de una lista
subSeqOfSize :: Int -> [a] -> [[a]]
subSeqOfSize n xs = subSeqBySize xs !! ((length xs)-n)
            where subSeqBySize []     = [[[]]]
                  subSeqBySize (x:xs) =
                      let next = subSeqBySize xs
                      in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
--------------------------------------------------------------------------------

-- Checkea si un alpha satisface una dependencia
sat :: Eq a => AttrSet a -> FuncDep a ->  Bool
sat attr (xs, ys) = foldr (\x -> ((x `elem` attr) &&)) True xs


-- Si un alpha satisface una dependencia devuelve un nuevo alpha
-- agregando los atributos determinados por la dependencia
(>=>) :: Eq a => FuncDep a -> AttrSet a -> AttrSet a
(>=>) dep@(_, ys) attr = if attr `sat` dep
                             then (attr++ys)
                             else attr

-- Realiza un paso del algoritmo de cierre
step :: Eq a => AttrSet a -> [FuncDep a] -> AttrSet a
step attr deps = toSet (foldr (\dep attr' -> (dep >=> attr')) attr  deps)

-- Dado una alpha y una lista de dependencias devuelve alpha^+
closure :: Eq a => [FuncDep a] -> AttrSet a -> AttrSet a
closure deps attr = let attr' = step attr deps in
                    if attr == attr'
                        then attr
                        else closure deps attr'

--Devuelve el conjunto de todas las claves candidatas
keys :: Eq a => AttrSet a -> [FuncDep a] -> [AttrSet a]
keys a deps = keys' a deps 1

-- Busca las claves candidatas de una lista de dependencias funcionales y una
-- lista de atributos. Si con subsecuenciass de n elementos de los atributos
-- encontramos que algunas de ellas son clave entonces el algoritmo se detiene,
-- y podemos ver que además son claves candidatas, ya que ningun subconjunto
-- de ellas es clave (de lo contrario el algoritmo habría parado en ese paso).
keys' :: Eq a => AttrSet a -> [FuncDep a] -> Int -> [AttrSet a]
keys' attr deps n = let attr_n = subSeqOfSize n attr
                        tup_ac = map (\a -> (a, closure deps a)) attr_n
                        tup_ck = filter (\(_, clo) -> clo =~= attr) tup_ac
                        keys_n = map fst tup_ck
                    in if null keys_n
                       then keys' attr deps (n+1)
                       else keys_n


----------------------------------------------------------------------------
-- Mostramos un esquema, sus dependencias y las claves encontradas
showKeys :: AttrSet Char -> [FuncDep Char] -> IO ()
showKeys r f = do putStrLn $ "Esquema:            (" ++ intersperse ' ' r ++ ")"
                  putStrLn $ "Dependencias:       "  ++ pretifyDeps f
                  putStrLn $ "Claves candidatas:  "  ++ pretifyKeys (keys r f) ++ "\n"

pretifyDeps = foldl (++) "" . map (\(x, y) -> "(" ++ x ++ "->" ++ y ++ ") ")
pretifyKeys = foldl (++) "" . intersperse ", "

main :: IO ()
main = do showKeys r1 f1
          showKeys r1 f2
          showKeys r3 f3

----------------------------------------------------------------------------
-- datasets
r1 = "ABCDEFGHIJ"
f1 = [("AB", "C"), ("BD", "EF"), ("AD", "GH"), ("A", "I"), ("H", "J")]
a1 = "BD"

r2 = "ABCDEFGH"
f2 = [("AB","C"), ("C", "D"), ("D", "G"), ("H", "E"), ("E", "A"), ("E", "H")]
a2 = "AC"

r3 = "ABCDEFG"
f3 = [("A", "G"), ("A", "F"), ("B", "E"), ("C", "D"), ("E", "A"), ("D", "B"), ("GF", "C")]
a3 = "FG"
