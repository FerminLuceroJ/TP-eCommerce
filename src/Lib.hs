module Lib
    ( someFunc,
    nombre,
    precio,
    productoXL,
    productoCorriente,
    productoDeLujo,
    aplicarCostoDeEnvio,
    productoCodiciado,
    descodiciarProducto,
    versionBarata,
    aplicarDescuento,
    entregaSencilla,
    productoDeElite,
    precioTotal
    
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Producto = (String, Float)

nombre :: Producto -> String
nombre (unNombre, _) = unNombre

precio :: Producto -> Float
precio (_, unPrecio) = unPrecio

productoXL :: Producto -> Producto
productoXL producto = ( nombre producto ++ " " ++ "XL", precio producto)

productoCorriente :: Producto ->  Bool
productoCorriente producto = elem (head.nombre $ producto)"aeiouAEIOU"

productoDeLujo :: Producto -> Bool
productoDeLujo producto = elem 'x' (nombre producto) || elem 'z' (nombre producto) || elem 'X' ( nombre producto) || elem 'Z' ( nombre producto)

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado producto = (length.nombre $ producto) > 10

descodiciarProducto :: Producto -> String
descodiciarProducto producto = ((take 10).nombre) producto

versionBarata :: Producto -> String
versionBarata producto = reverse.descodiciarProducto $ producto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento producto descuentoProducto = precio producto - ((* descuentoProducto). precio $ producto)/100

entregaSencilla :: String -> Bool
entregaSencilla diaDeEntrega = even.length $ diaDeEntrega

productoDeElite :: Producto -> Bool
productoDeElite producto = (productoDeLujo producto) && (productoCodiciado producto) && not(productoCorriente producto)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal producto cantidad descuentoProducto costoEnvio = aplicarCostoDeEnvio ((* cantidad). aplicarDescuento producto $ descuentoProducto) costoEnvio

-- head :: [a] -> a
-- take :: Int -> [a] -> [a]
-- elem :: (Eq a) => a -> [a] -> Bool
-- reverse :: [a] -> [a]
-- drop :: Int -> [a] -> [a]


