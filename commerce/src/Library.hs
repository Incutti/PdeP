module Library where
import PdePreludat 
import GHC.Num (Num)
import Data.Char (toLower)
import Control.Exception (BlockedIndefinitelyOnMVar)
import Data.Int

-- Tipados generales
type Nombre = String
type PrecioUnitario = Number
type Cantidad = Number
type PrecioEnvio = Number
type FechaEnvio = String
type Envio = (PrecioEnvio, FechaEnvio)
type Descuento = Number
type Producto = (Nombre, PrecioUnitario)
type ProductoAComprar = (Producto, Cantidad)
type ListaDeProductos = [ProductoAComprar]
type Compra = (ListaDeProductos, Envio)

-- Tipados de funciones utiles
{- 
take :: Number -> String -> String - (usada)
drop :: Number -> String -> String
head :: String -> Char ------------- (usada)
elem :: Char -> String -> Bool ----- (usada)
reverse :: String -> String -------- (usada)
-}
esLetra :: Char -> Char -> Bool
esLetra letraMia letraAComparar = toLower letraMia == toLower letraAComparar
esVocal :: Char -> Bool
esVocal letra = esLetra letra 'a' || esLetra letra 'b' || esLetra letra 'c' || esLetra letra 'd' || esLetra letra 'e'

{- PUNTO A
precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de 
envío calcular el precio total. Para eso, hay que calcular el precio unitario con 
descuento y multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del 
envío! -}
precioTotal :: PrecioUnitario -> Cantidad -> Descuento -> PrecioEnvio -> Number
precioTotal precioUnitario cantidad descuento precioEnvio = aplicarCostoDeEnvio (cantidad * (aplicarDescuento precioUnitario descuento)) precioEnvio

{- PUNTO B
esProductoDeElite: Un producto es de elite si es de lujo, codiciado y no es un 
producto corriente. -}
esProductoDeElite :: Producto -> Bool
esProductoDeElite (nombreProducto,_) = (esProductoDeLujo nombreProducto) && (esProductoCodiciado nombreProducto) && (not (esProductoCorriente nombreProducto))

{- PUNTO C
aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el 
descuento aplicado. -}
aplicarDescuento :: PrecioUnitario -> Descuento -> Number
aplicarDescuento precioUnitario descuento = precioUnitario - descuento

{- PUNTO D
entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días 
sencillos son lo que tienen una cantidad de letras par en el nombre. Ejemplo de un 
día: “20 de Abril de 2020”. -}
entregaSencilla :: FechaEnvio -> Bool
entregaSencilla fechaEnvio = even (length fechaEnvio)

{- PUNTO E
descodiciarProducto: Dado el nombre de un producto, generar uno que no sea 
codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de 
letras en el nombre quede igual a 10 (ó menor a 10 en productos con nombres 
cortos) -}
descodiciarProducto :: Nombre -> Nombre
descodiciarProducto nombreProducto = take 10 nombreProducto

{- PUNTO F
esProductoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un 
producto es de lujo cuando contiene una “x” o “z” en su nombre. -}
esProductoDeLujo :: Nombre -> Bool
esProductoDeLujo nombreProducto = elem 'x' nombreProducto || elem 'z' nombreProducto
{- PUNTO G
aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final 
una vez sumado el costo de envío. -}
aplicarCostoDeEnvio :: PrecioUnitario -> PrecioEnvio -> Number
aplicarCostoDeEnvio precioUnitario precioEnvio = precioUnitario + precioEnvio 

{- PUNTO H
esProductoCodiciado: Dado el nombre de un producto, saber si es un producto 
codiciado. Un producto es codiciado cuando la cantidad de letras en su nombre es 
mayor a 10 -}
esProductoCodiciado :: Nombre -> Bool
esProductoCodiciado nombreProducto = 10 < length nombreProducto

{- PUNTO I
esProductoCorriente: Dado el nombre de un producto, saber si es un producto 
corriente. Un producto es corriente si la primera letra de su nombre es una vocal. -}
esProductoCorriente :: Nombre -> Bool
esProductoCorriente nombreProducto = esVocal (head nombreProducto)

{- PUNTO J
productoXL: Dado un producto, conseguir su versión XL. Esta se consigue 
agregando ‘XL’ al final del nombre. -}
productoXL :: Producto -> Producto
productoXL (nombreProducto,precio) = (nombreProducto ++ "XL", precio) 

{- PUNTO K
versionBarata: Dado el nombre de un producto conseguir su versión barata. La 
misma es el producto descodiciado y con su nombre dado vuelta. -}
versionBarata :: Nombre -> Nombre
versionBarata nombreProducto = reverse nombreProducto