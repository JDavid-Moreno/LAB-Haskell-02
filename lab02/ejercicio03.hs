import Data.Char (isUpper)

--Punto 1

-- a) Suma de todos los elementos

main = print(suma [1,2,3,4,5])

suma a = foldr (+) 0 a

--Output: 15

--b) Contar cuántos elementos son mayores que 10

main = print(mayoresA10 [1,2,3,10,11,12,13])

mayoresA10 a = dropWhile (<10) a

--Output: [10,11,12,13]

--c) Calcular el producto de una lista de números

main = print(producto [1,2,3,4,5])

producto a = foldr (*) 1 a

--Output: 120

--d) Convierte una lista de grados Celsius a Fahrenheit

main = print(gradosAFahrenheit [1,2,3,4,5])

gradosAFahrenheit a = map(\x -> x*(9/5)+32) a

-- Output:[33.8,35.6,37.4,39.2,41.0]

-- e) Suma 5 a cada número impar de una lista y deja igual los pares.

main = print (suma5impares [1,2,3,4,5])

suma5impares a = map f a
where
    f x
      | odd x     = x + 5
      | otherwise = x
      
--Punto 2

--a. Devuelve solo los nombres con más de 5 letras.

main = print (nombresLargos ["juan","camilo","nicole","daniel","samuel"])

nombresLargos xs = filter (\x -> length x > 5) xs

--Output: ["camilo","nicole","daniel","samuel"]

--b. Filtra los números negativos de una lista.

main = print (soloNegativos [1,-2,3,-4,5])

soloNegativos xs = filter (\x -> x < 0) xs

--Output: [-2,-4]

--c. De una lista de edades, obtiene las que estén entre 18 y 25 años.

main = print (edadesUniversitarios [1,5,9,19,18,24,23,46,28,25])

edadesUniversitarios xs = filter (\x -> x > 17 && x < 26) xs

--Output: [19,18,24,23,25]

--Punto 3

--a. Dada una lista de precios, sumar IVA (19%) solo a los valores mayores o iguales a 1000.
main = print (agregarIVA [1000,4000,700,200,10000,13000])

agregarIVA xs = map (\x -> if x >= 1000 then x * 1.19 else x) xs

--b. De una lista de palabras, obtener solo las que comienzan con vocal y devolver su longitud.
main = print (longitudesVocales ["ayuda","auxilio","jesus","iapp"])

longitudesVocales xs = map length (filter empiezaConVocal xs)
  
empiezaConVocal [] = False
empiezaConVocal (x:_) = x `elem` "aeiou"

--Output: [5,7,4]
                                     

--c. Dada una lista de números, devuelve los cuadrados de los pares.
main = print (cuadradosPares [1,2,3,4,5,6])

cuadradosPares xs = map (^2) (filter even xs)

--Output: [4,16,36]

--Punto 4

--a. Verifica si algún número en la lista es negativo.

main = print (hayNegativos [1,-2,-3,4,5])

hayNegativos xs = any (<0) xs

--Output: True

--b. Verifica si todos los números son pares.

main = print (todosPares [1,2,3,4,5])

todosPares xs = all even xs

--Output: False

--c. Comprueba si todos los nombres empiezan con mayúscula.
main = print (nombresCorrectos ["Juan","camilo","Nicole","Daniel","samuel"])

nombresCorrectos (x:_) = all isUpper x

--Output: False

--Punto 5

--a.De una lista ordenada, toma los elementos menores que 100.

main = print (menoresQue100 [1,2,3,5,9,20,50,70,100,110,150,170,200,220,300])

menoresQue100 xs = dropWhile (<100) xs

--b. Quita los ceros iniciales de una lista.

main = print (sinCerosInicales [0,0,0,0,1,2,3,4,5])

sinCerosInicales xs = dropWhile (== 0) xs

--Output: [1,2,3,4,5]
