module Lib
    ( someFunc,
    darNombreProducto,
    darPrecioProducto,
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

darNombreProducto :: Producto -> String
darNombreProducto (nombre, _) = nombre

darPrecioProducto :: Producto -> Float
darPrecioProducto  (_, precio) = precio

productoXL :: Producto -> Producto
productoXL producto = (darNombreProducto producto ++ " " ++ "XL", darPrecioProducto producto)

productoCorriente :: Producto ->  Bool
productoCorriente producto = elem (head (darNombreProducto producto))"aeiouAEIOU"

productoDeLujo :: Producto -> Bool
productoDeLujo producto = elem 'x' (darNombreProducto producto) || elem 'z' (darNombreProducto producto) || elem 'X' (darNombreProducto producto) || elem 'Z' (darNombreProducto producto)

aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio producto costoEnvio = darPrecioProducto producto + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado producto = length (darNombreProducto producto) > 10

descodiciarProducto :: Producto -> String
descodiciarProducto producto = ((take 10).darNombreProducto) producto

versionBarata :: Producto -> String
versionBarata producto = reverse.descodiciarProducto $ producto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento producto descuentoProducto = darPrecioProducto producto - ((darPrecioProducto producto)*descuentoProducto/100)

entregaSencilla :: String -> Bool
entregaSencilla diaDeEntrega = even.length $ diaDeEntrega

productoDeElite :: Producto -> Bool
productoDeElite producto = (productoDeLujo producto) && (productoCodiciado producto) && not(productoCorriente producto)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal producto cantidad descuentoProducto costoEnvio = (cantidad*(aplicarDescuento producto descuentoProducto)) + costoEnvio

-- head :: [a] -> a
-- take :: Int -> [a] -> [a]
-- elem :: (Eq a) => a -> [a] -> Bool
-- reverse :: [a] -> [a]
-- drop :: Int -> [a] -> [a]