--Punto 1

main = print (procesarLecturas celsiusAFaranheit [0,12,20])

procesarLecturas f xs = map f xs
celsiusAFaranheit c = c * 1.8 + 32

--Output: [32.0,53.6,68.0]

--Punto 2

main = print (crearFiltro soloPares [1..10])

crearFiltro f xs = filter f xs
soloPares c = even 

--Output: [2,4,6,8,10]

--Punto 3

main = print (enRango 5 10 7)

enRango a b = \x -> x >= a && x <= b

--Output: True

--Punto 4

main = print (potenciador 5 7)

potenciador a b = b^a

--Output: 16807
