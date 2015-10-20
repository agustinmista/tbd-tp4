module Main where

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
    cierre :: Eq a => [a] -> [FuncDep a] -> [a]
    cierre a deps = let a' = step a deps in
                        if a == a'
                        then a
                        else cierre a' deps

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
