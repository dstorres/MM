import System.Random
import Data.Char
insertar :: [Int]->Int->Int->Int->[Int]
insertar [] _ _ _ = []
insertar (x:xs) nc y r = if ((r + length [x]) == nc)
                           then   [y]++(insertar(xs) nc y (r+length[x]))
                           else   [x]++(insertar(xs) nc y (r+length[x]))

 --Inserta un numero a una lista en la posicion n-esima                 
insertarNumero :: [Int]->Int->[Int]
insertarNumero [] _ = []
insertarNumero (x:xs)  y = if (x == 0)
                           then   [y]++(insertarNumero(xs) y)
                           else   [x]++(insertarNumero(xs) y)

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
mientras :: Int->Int->Int->IO Int
mientras n min max = do
					rng <- newStdGen
					let
						a = miAleatorio rng min max
					if a == n
							then mientras n min max
							else return a
							
--Lista de POSICION correcta 							
listaPosicionC :: [Int]->[Int]->Int->Int -> IO[Int]
listaPosicionC lista1 lista2 n m = do
										aleatorio<- mientras m 1 4
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
									aleatorio2 <- mientras m 1 size
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

								
insertaIncorrectos :: [Int]->[Int]->Int-> [Int]
insertaIncorrectos [] [] _ = []
insertaIncorrectos (x:xs) (y:ys) num = do
								if x == y
										then [y]++insertaIncorrectos xs ys num
										else if num == x
													then	[y]++insertaIncorrectos xs ys num
													else	[num]++insertaIncorrectos xs ys num

pasa :: [Int] -> [Int]->Int ->IO [Int]
pasa lista1 lista2 n = do
							let listaComb = [0,0,0,0]
							if n == 0
								then return lista1
								else listaPosicionC lista1 listaComb n 0
								
listaI :: [Int]->[Int]
listaI [] = []
listaI (x:xs) = if x == 0
						then listaI xs
						else [x]++listaI xs

aleat :: [Int]->[Int]->Int->IO Int
aleat lista1 lista2 n = do
					rng <- newStdGen
					let	a = miAleatorio rng 1 6
					
					if n == head lista1
							then	aleat lista2 lista2 a 
							else 	if lista1 == [0]
												then return n 
												else  if (tail lista1) == []
															then aleat [0] lista1 n		
															else aleat (tail lista1) lista1 n	
															
--Jugar MasterMind                                                       
jugarMM :: [Int]->[Int]->IO()
jugarMM (lista1) (lista2) = do
								let	n = posicionCorrecta lista1 lista2  
									m = numeroCorrecto lista1 lista2
									p = m-n
									par = [n,p]
									listaComb = [0,0,0,0]
										
								listanueva1 <- pasa lista1 lista2 n
								let listaN1 = listanueva1
								
								listanueva2 <- listaNumeroC lista2 listaN1 p 0
								let	listaN2 = listanueva2
								putStrLn $ ""++show(lista2)++"  "++show(par)	
								if	lista1 == lista2
											then putStrLn $ "El COMPUTADOR adivino la clave :"++show(lista1)
											else do	jugarMM lista1 listaN2

												 
--Numeros en correcta posicion 
posicionCorrecta::[Int]->[Int] ->Int
posicionCorrecta [] [] = 0
posicionCorrecta (xs) (ys) = if [head xs] == [head ys] then 1 + posicionCorrecta (tail xs) (tail ys)
                                        else posicionCorrecta (tail xs) (tail ys)
								
--Numeros existente en la clave
numeroCorrecto::[Int]->[Int]->Int
numeroCorrecto [] [] = 0
numeroCorrecto (xs) (ys) = length [ (l,m) | l <- xs, m <- ys , l==m]

menuMM ::IO ()
menuMM = do	
			putStrLn "MASTER MIND"
			putStr "Ingrese el nombre del archivo : " 
			fentrada <- getLine
			file <- readFile fentrada
			lista1 <- fileList file
			--clave <- readLn
			rng <- newStdGen
			let		a = miAleatorio rng 1 6
					colors = [1..6]
					combination = [[w,x,y,z]| w<-colors, x<-colors, y<-colors, z<-colors]
					--lista1 = descomponerLista clave
					verdad =  numero lista1 combination
					lista2 = combination !! a
			putStr "Lista2 : ";print lista2
			if verdad == 1
							then do 
									putStrLn "Clave correcta"
							else do 
									putStrLn "Clave Incorrecta"
									menuMM  


main::IO()
main = do
	putStr "Ingrese nombre de archivo de la clave (ej: archivo.txt):"
	fentrada <- getLine
	cad <- readFile fentrada
	y<- fileList cad
	--cfg<- CrearCFG y
	putStr "Clave: "; print y
	print y		
	if head y == 1
		then putStrLn "numero1"
		else putStrLn "numero2"

	
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