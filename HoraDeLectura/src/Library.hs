module Library where
import PdePreludat
import Data.Int
import Data.Char (toLower)

-- Tipos
type Titulo = String
type CantidadDePaginas = Number
type Autor = String
type Saga = String
type Biblioteca = [Libro]
type Libro = (Titulo, CantidadDePaginas, Autor, Saga)

-- Creo los libros
elVisitante :: Libro
elVisitante = ("El visitante", 592, "Stephen King", "")
shingekiNoKyojin :: Libro
shingekiNoKyojin = ("Shingeki no Kyojin", 120, "Hajime Isayama", "")
fundacion :: Libro
fundacion = ("Fundacion", 230, "Isaac Asimov", "")
sandman :: Libro
sandman = ("Sandman", 105, "Neil Gaiman", "")
eragon :: Libro
eragon = ("Eragon", 544, "Christopher Paolini", "Eragon")
eldest :: Libro
eldest = ("Eldest", 704, "Christopher Paolini", "Eragon")
brisignr :: Libro
brisignr = ("Brisignr", 700, "Christopher Paolini", "Eragon")
legado :: Libro
legado = ("Legado", 811, "Christopher Paolini", "Eragon")

-- Cargo los libros
biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin, fundacion, sandman, eragon, eldest, brisignr, legado]

-- Accesors
cantidadDePaginas :: Libro -> Number
cantidadDePaginas (_, paginas, _, _) = paginas
autor :: Libro -> Autor
autor (_,_,autor,_) = autor
titulo :: Libro -> Titulo
titulo (titulo,_,_,_) = titulo

-- Punto A
-- El promedioDeHojas de los libros de nuestra biblioteca. 
promedioDeHojas :: Biblioteca -> Number
promedioDeHojas biblioteca = sum (map cantidadDePaginas biblioteca) / length biblioteca

-- Punto B
{- Qué lectura es una lecturaObligatoria, esto es así cuando es de Stephen 
King o de la saga de Eragon o es el ejemplar de Fundación de 230 páginas 
de Isaac Asimov (¡ningún otro!). -}

-- Chequeos de atributos
esDeAutor :: Autor -> Libro -> Bool
esDeAutor autor (_,_,autorLibro,_) = autor == autorLibro
esDeSaga :: Saga -> Libro -> Bool
esDeSaga saga (_,_,_,sagaLibro) = saga == sagaLibro
esTitular :: Titulo -> Libro -> Bool
esTitular titulo (tituloLibro,_,_,_) = titulo == tituloLibro
tieneXPaginas :: CantidadDePaginas -> Libro -> Bool
tieneXPaginas cantidadDePaginas (_,cantidadDePaginasLibro,_,_) = cantidadDePaginas == cantidadDePaginasLibro
tieneMaximoXPaginas :: CantidadDePaginas -> Libro -> Bool
tieneMaximoXPaginas cantidadDePaginas (_,cantidadDePaginasLibro,_,_) = cantidadDePaginas >= cantidadDePaginasLibro

lecturaObligatoria :: Libro -> Bool
lecturaObligatoria libro = esDeAutor "Stephen King" libro || esDeSaga "Eragon" libro || ((esTitular "Fundacion" libro) && (tieneXPaginas 230 libro) && (esDeAutor "Isaac Asimov" libro))
-- convendría armar otra función para el último chequeo?

-- Punto C
{- Si la biblioteca es fantasiosa, es decir, si tiene algún libro de Christopher 
Paolini o de Neil Gaiman. -}

autores :: Biblioteca -> [Autor]
autores biblioteca = map autor biblioteca

esLibroFantasioso :: Libro -> Bool
esLibroFantasioso libro = esDeAutor "Christopher Paolini" libro || esDeAutor "Neil Gaiman" libro
esBibliotecaFantasiosa :: Biblioteca -> Bool
esBibliotecaFantasiosa biblioteca = any esLibroFantasioso biblioteca

-- Punto D
{- El  nombreDeLaBiblioteca,  que  es  el nombre de todos los títulos juntos, 
sacándole las vocales. -}
noEsVocal :: Char -> Bool
noEsVocal letra = letra /= 'a' && letra /= 'e' && letra /= 'i' && letra /= 'o' && letra /= 'u' 

titulos :: Biblioteca -> [Titulo]
titulos biblioteca = map titulo biblioteca

juntarTitulos :: Biblioteca -> String
juntarTitulos biblioteca = concat(titulos biblioteca)

pasarAMinusculas :: String -> String
pasarAMinusculas palabra = map toLower palabra

noEsEspacio :: Char -> Bool
noEsEspacio caracter = caracter /= ' '

quitarEspacios :: Biblioteca -> String
quitarEspacios biblioteca = filter noEsEspacio (pasarAMinusculas (juntarTitulos biblioteca))

cortarVocales :: Biblioteca -> String
cortarVocales biblioteca = filter noEsVocal (quitarEspacios biblioteca)

nombreDeLaBiblioteca :: Biblioteca -> [Char]
nombreDeLaBiblioteca biblioteca = cortarVocales biblioteca

-- Punto E
{- Si  tenemos  una  bibliotecaLigera,  o  sea, si todas sus lecturas tienen 40 
páginas o menos. -}

cantidadesDePaginas :: Biblioteca -> [CantidadDePaginas]
cantidadesDePaginas biblioteca = map cantidadDePaginas biblioteca

esLibroLigero :: Libro -> Bool
esLibroLigero libro = tieneMaximoXPaginas 40 libro
esBibliotecaLigera :: Biblioteca -> Bool
esBibliotecaLigera biblioteca = all esLibroLigero biblioteca
