import System.Random
import Data.Char

--Sacar N-esimo elemento de una LISTA			
sacarNesimo :: [Int]->Int->Int->Int
sacarNesimo [] _ _ = 0
sacarNesimo (x:xs) n r = if (r + length [x]) == n
                           then   x
                           else   (sacarNesimo(xs) n (r+length[x]))  
						   
--Obtener Entero del Random
intRandonR :: (Int,StdGen) -> Int
intRandonR (x,y) = x

--Funcion Aleatoria
miAleatorio :: StdGen -> Int-> Int->Int			
miAleatorio rng min max=let
						x = randomR (min, max) rng 
						in intRandonR x

--Cambiar Aleatorio diferente de M		
existElem :: [Int]->Int->Int->IO Int
existElem lista min max= do
					rng <- newStdGen
					let a = miAleatorio rng min max
					
					if a `elem` lista
							then existElem lista min max
							else return a


						
--Cambiar Aleatorio diferente de M		
mientras :: Int->Int->Int->IO Int
mientras n min max = do
					rng <- newStdGen
					let
						a = miAleatorio rng min max
					if a == n
							then mientras n min max
							else return a
--Lista de POSICION correcta 							
lC :: [Int]->[Int]->Int->[Int] -> IO[Int]
lC lista1 lista2 n m = do
										aleatorio<- existElem m 1 4
										let
											posAleatorio = aleatorio
											enesimo = sacarNesimo lista1 posAleatorio 0
											lista =(insertar lista2 posAleatorio enesimo 0)
										if n > 0
												then lC lista1 lista (n-1) ([posAleatorio]++m)
												else return lista2							

 
fun ::[Int]->[Int]->[Int]->[Int]->Int->IO[Int]
fun lista1 listaP1 listaP2 l p= do
						aleatorio1<- existElem listaP1 1 4
						aleatorio2<- existElem listaP2 1 4
						let		pA1 = aleatorio1
								pA2 = aleatorio2
								num = sacarNesimo lista1 pA1 0
								li = insertar l pA2 num 0
								lis1 =listaP1++[pA1]
								lis2 =listaP2++[pA2]
								rt = p-1
						if rt > 0
									then fun lista1 lis1 lis2 li (p-1)
									else return li
						
						
lNC :: [Int]->[Int]->Int->[Int]->IO[Int]
lNC lista1 lista2 pt m= do
                                let     bitLista = listaIncorrectos lista1 lista2
                                        size = length bitLista
                                aleatorio2 <- existElem m 1 (size)
                                f <- fun lista2 [0] [0] [0,0,0,0] pt
                                let     posAleatorio = aleatorio2
                                        num = sacarNesimo bitLista posAleatorio 0
                                        listaFinal = insertaIncorrectos lista1 lista2 num
                                        funcion = f
                                        p = pt-1
                                if p == 1 || p==2 || p==3
                                        then lNC lista1 listaFinal (p-1) ([posAleatorio]++m)
                                        else if p==4
                                                                then return funcion
                                                                else return listaFinal

												
--Lista de POSICION correcta 							
listaPosicionC :: [Int]->[Int]->Int->Int -> IO[Int]
listaPosicionC lista1 lista2 n m = do
										aleatorio<- mientras m 0 3
										let
											posAleatorio = aleatorio
											enesimo = sacarNesimo lista1 posAleatorio 0
											lista =(insertar lista2 posAleatorio enesimo 0)
										if n > 0
												then listaPosicionC lista1 lista (n-1) posAleatorio
												else return lista2
--Lista de NUMERO correcto                                          
listaNumeroC :: [Int]->[Int]->Int->Int->IO[Int]
listaNumeroC lista1 lista2 p m= do
									let
										bitLista = listaIncorrectos lista1 lista2 
										size = length bitLista
									
									aleatorio2 <- mientras m 0 (size-1)
									let	
										posAleatorio = aleatorio2
										num = sacarNesimo bitLista posAleatorio 0
										listaFinal = insertaIncorrectos lista1 lista2 num
									if p > 0 
											then listaNumeroC lista1 listaFinal (p-1) posAleatorio
											else return lista2

listaIncorrectos :: [Int]->[Int]->[Int]
listaIncorrectos [] [] = []
listaIncorrectos (x:xs) (y:ys) = if x /= y
									then [x]++listaIncorrectos xs ys
									else listaIncorrectos xs ys

		
insertaIncorrectos :: [Int]->[Int]->Int->[Int] 		
insertaIncorrectos [] [] _ = []
insertaIncorrectos (x:xs) (y:ys) num = do
							if y == 0
									then if x == num
											then [y]++insertaIncorrectos xs ys num
											else [num]++insertaIncorrectos xs ys 0
									else [y]++insertaIncorrectos xs ys num

listaP :: [Int]->[Int]->[Int]	
listaP [] [] = []								
listaP (x:xs) (y:ys) = do
						if x == y
								then [y]++listaP xs ys
								else [0]++listaP xs ys
--Jugar MasterMind                                                       
jugarMM :: [Int]->[Int]->Int->IO()
jugarMM lista1 lista2 i = do
								aleatorio1<- existElem lista2 1 6
								aleatorio2<- existElem lista2 1 6
								aleatorio3<- existElem lista2 1 6
								let		pAleatorio1 = aleatorio1
										pAleatorio2 = aleatorio2
										pAleatorio3 = aleatorio3
								
								let	n = posicionCorrecta lista1 lista2  
									l1= sacaiguales lista2
									l2= sacaiguales l1
									l3= sacaiguales l2
									l4= sacaiguales l3
									l = cero l4
									m = numeroCorrecto lista1 l 0
									p = m-n
									par = [n,p]
									listaComb = [0,0,0,0]
								putStrLn $ " "++show(i)++")  Clave: "++show(lista2)++"  "++show(par)	
								listanueva1 <- lC lista2 listaComb n [0]
								let listaN1 = listanueva1
								
								listanueva2 <- lNC lista2 listaN1 p [9]
								let	listaN2 = listanueva2
								
								--l1 <- llenaCero listaN2 a1 0
								let	list1 = llenaCero listaN2 pAleatorio1 0
									list2 = llenaCero list1 pAleatorio2 0
									lista2 = llenaCero list2 pAleatorio3 0
								if i > 2000
										then putStrLn "NO adivino la CLAVE (MAS DE 15 CONJETURAS)"
										else if	lista1 == lista2
													then putStrLn $ "El COMPUTADOR adivino la clave :"++show(lista1)
													else do	jugarMM lista1 lista2 (i+1)
								
llenaCero:: [Int]->Int->Int->[Int]											
llenaCero [] _ _ = []
llenaCero (x:xs) num cont= do
							if x == 0
								then if cont < 1
										then [num]++llenaCero xs num 1
										else [x]++llenaCero xs num cont
								else [x]++ llenaCero xs num cont


--Numeros en correcta posicion
posicionCorrecta::[Int]->[Int] ->Int
posicionCorrecta [] [] = 0
posicionCorrecta (xs) (ys) = if [head xs] == [head ys] then 1 + posicionCorrecta (tail xs) (tail ys)
                                        else posicionCorrecta (tail xs) (tail ys)
								
						
--Numeros existente en la clave
numeroCorrecto :: [Int]->[Int]->Int->Int
numeroCorrecto [] [] _ = 0
numeroCorrecto x (y:ys) cont = do
                                        if length[n| n<-x, y==n] >= 1
                                                        then if ys == []
                                                                then cont+1
                                                                else numeroCorrecto x ys (cont+1)
                                                        else if ys == []
                                                                then cont
                                                                else numeroCorrecto x ys (cont)   										
sacaiguales:: [Int]->[Int]
sacaiguales [] = []
sacaiguales (x:xs) = do
						if x == head xs
								then if tail xs == []
											then [0]++[x]
											else [0]++sacaiguales (x:(tail xs))
								else if tail xs == []
											then [head xs]++[x]
											else [head xs]++sacaiguales(x:(tail xs))
									
cero :: [Int]->[Int]
cero []=[]
cero (x:xs) = do
					if x == 0
							then cero xs
							else [x]++cero xs
insertar :: [Int]->Int->Int->Int->[Int]
insertar [] _ _ _ = []
insertar (x:xs) posicion num cont = if ((cont + length [x]) == posicion)
                           then   [num]++(insertar(xs) posicion num (cont+length[x]))
                           else   [x]++(insertar(xs) posicion num (cont+length[x]))
menuMM ::IO ()
menuMM = do	
			putStrLn ""
			putStrLn "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"
			putStrLn "\t\t\t\tMASTER MIND"
			putStrLn "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"
			putStrLn ""
			putStr "Ingrese el nombre del archivo : " 
			fentrada <- getLine
			file <- readFile fentrada
			lista1 <- fileList file
			--clave <- readLn
			rng <- newStdGen -- generador de aleatorio
			let		a = miAleatorio rng 0 1295
					colors = [1..6]
					combination = [[w,x,y,z]| w<-colors, x<-colors, y<-colors, z<-colors]
					--lista1 = descomponerLista clave
					verdad =  numero lista1 combination
					lista2 = combination !! a
					--lista2 = [1,2,3,4]
			putStr "Clave del Computador: ";print lista2
			if verdad == 1
							then if lista1 == lista2
										then	putStrLn "CLAVE INGRESADA CORRECTA"
										else	do 
													jugarMM lista1 lista2 1
							else do 
									putStrLn "Clave del Archivo Incorrecta"
									menuMM  

	
--Descomponer un numero a lista
descomponerLista::Int -> [Int]
descomponerLista n 
                |n<10=n:[]
                |otherwise = descomponerLista (div n 10) ++ [mod n 10]

--Si la clave es Correcta retorna 1
numero :: [Int]->[[Int]]->Int
numero [] [] = 0
numero (lista1) (y:ys) = if lista1 == y 
                                then 1
                                else if (tail ys) == []
												then	0
												else numero lista1 ys
												
fileList :: String -> IO [Int]
fileList = readIO