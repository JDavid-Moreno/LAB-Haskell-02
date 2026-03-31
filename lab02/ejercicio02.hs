--Punto 1

--a) Obtener los cuadrados de los primeros 10 números.
main = print([x^2 | x <- [1..10]])

--Output: [1,2,3,4,5,6,7,8,9,10]

-- b) Filtrar los divisibles por 3 entre 1 y 30.
main = print([x | x <- [1..30], mod x 3 == 0])

--Output: [3,6,9,12,15,18,21,24,27,30]

-- c) Generar una lista de pares (x, y) donde x < y y ambos estén en [1..5].

main = print([(x,y) | x <- [1..5], y <- [1..5], x < y])

--Output: [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]

--Punto 2

main = print(divisores 24)

divisores a = [x | x <- [1..a], mod a x == 0]

--Output: [1,2,3,4,6,8,12,24]

--Punto 3

main = print (divisores 28)

divisores a = sum (divisoresPropios a) == a
divisoresPropios a = [x | x <- [1..a-1],mod a x == 0]

--Output: True
                    
