import Data.List (sortOn)

main = print (aStar vecinos heuristica inicio meta)


type Estado = (Int, Int)
type Nodo = Estado
type Costo = Int
type Grafo = [(Nodo, [(Nodo, Costo)])]

capA = 5
capB = 3

vecinos (a,b) =
  [ ((capA, b), 1)            -- Llenar jarra A
  , ((a, capB), 1)            -- Llenar jarra B
  , ((0, b), 1)               -- Vaciar jarra A
  , ((a, 0), 1)               -- Vaciar jarra B
  , (transferirAB (a,b), 1)  -- Verter A→B
  , (transferirBA (a,b), 1)  -- Verter B→A
  ]
  
transferirAB (a,b) =
  let espacioB = capB - b
      cantidad = min a espacioB
  in (a - cantidad, b + cantidad)
  
transferirBA (a,b) =
  let espacioA = capA - a
      cantidad = min b espacioA
  in (a + cantidad, b - cantidad)
  
heuristica (a, _) = abs (4 - a)

inicio = (0,0)
meta = (4,0)

aStar sucesores heuristica inicio meta = buscar [(inicio, 0)] []
  where
    buscar [] _ = []
    buscar ((nodo, costo):cola) visitados
      | nodo == meta = [(nodo, costo)]
      | nodo `elem` visitados = buscar cola visitados
      | otherwise =
          let nuevos = [ (v, costo + c) | (v, c) <- sucesores nodo, v `notElem` visitados ]
              ordenados = sortOn (\(v, c) -> c + heuristica v) (cola ++ nuevos)
          in (nodo, costo) : buscar ordenados (nodo : visitados)


--Output: [((0,0),0),((5,0),1),((5,3),2),((2,3),2),((0,3),1),((3,0),2),((3,3),3),((2,0),3),((5,1),4),((0,2),4),((5,2),5),((4,3),6),((4,0),7)]
