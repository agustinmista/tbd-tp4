module Main where

    import Data.List (subsequences, (\\), intersperse)

    type FuncDep a = ([a], [a])

    -- Checkea si x está en ys
    isIn :: Eq a => a -> [a] -> Bool
    isIn x = foldr (\y -> ((y == x) ||) ) False

    -- Checkea si un alpha satisface una dependencia
    sat :: Eq a => [a] -> FuncDep a ->  Bool
    sat attr (xs, ys) = foldr (\x -> ((x `isIn` attr) &&)) True xs

    -- Elimina elementos repetidos
    remDup :: Eq a => [a] -> [a]
    remDup = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

    -- Si un alpha satisface una dependencia devuelve un nuevo alpha
    -- agregando los atributos determinados por la dependencia
    (<--) :: Eq a => [a] -> FuncDep a -> [a]
    (<--) a dep@(_, ys) = if a `sat` dep
                                 then (a++ys)
                                 else a

    -- Realiza un paso del el algoritmo de cierre
    step :: Eq a => [a] -> [FuncDep a] -> [a]
    step a deps = remDup (foldr (\dep a' -> (a' <-- dep)) a  deps)

    -- Dado una alpha y una lista de dependencias devuelve alpha^+
    closure :: Eq a => [FuncDep a] -> [a] -> [a]
    closure deps a = let a' = step a deps in
                        if a == a'
                        then a
                        else closure deps a'

    -- Checkea si dos listas tienen los mismos elementos (igualdad de conjuntos)
    (=*=) :: Eq a => [a] -> [a] -> Bool
    (=*=) a b = (null (a \\ b)) && (null (b \\ a))

    --Devuelve el conjunto de todas las claves candidatas
    keys :: Eq a => [a] -> [FuncDep a] -> [[a]]
    keys a deps = keys' a deps 1

    -- Busca las claves candidatas de una lista de dependencias funcionales y una
    -- lista de atributos. Si con subsecuenciass de n elementos de los atributos
    -- encontramos que algunas de ellas son clave entonces el algoritmo se detiene,
    -- y podemos ver que además son claves candidatas, ya que ningun subconjunto
    -- de ellas es clave (de lo contrario el algoritmo habría parado en ese paso).
    keys' :: Eq a => [a] -> [FuncDep a] -> Int -> [[a]]
    keys' a deps n = let attr_n  = filter (\x -> length x == n) (subsequences a)
                         tup_ac  = map (\la -> (la, closure deps la)) attr_n
                         tup_kc  = filter (\(la, cl) -> cl =*= a) tup_ac
                         keys_n  = map fst tup_kc
                      in if null keys_n
                         then keys' a deps (n+1)
                         else keys_n



    ----------------------------------------------------------------------------
    -- Mostramos un esquema, sus dependencias y las claves encontradas
    showKeys :: [Char] -> [FuncDep Char] -> IO ()
    showKeys r f = do putStrLn $ "Esquema:            " ++ intersperse ',' r
                      putStrLn $ "Dependencias:       " ++ pretifyDeps f
                      putStrLn $ "Claves candidatas:  " ++ pretifyKeys (keys r f) ++ "\n"

    pretifyDeps = foldl (++) "" . map (\(x, y) -> "(" ++ x ++ "->" ++ y ++ ") ")
    pretifyKeys = foldl (++) "" . intersperse ","

    main :: IO ()
    main = do showKeys r1 f1
              showKeys r1 f2
              showKeys r3 f3


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
