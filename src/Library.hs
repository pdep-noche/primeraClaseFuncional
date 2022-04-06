module Library where
import PdePreludat

doble :: Number -> Number
doble numero = 2 * numero

siguiente :: Number -> Number
siguiente nro = nro + 1

calcular :: Number -> Number
calcular nro | even nro = siguiente nro
             | otherwise = doble nro


mes :: (Number, Number, Number) -> Number
mes (_, unMes, _ ) = unMes

calcular' :: (Number, Number) -> (Number, Number)
calcular' (numero1, numero2) = (primerElemento numero1, segundoElemento numero2)

primerElemento :: Number -> Number
primerElemento nro | even nro = doble nro
                   | otherwise = nro

segundoElemento :: Number -> Number
segundoElemento nro | odd nro = siguiente nro
                    | otherwise = nro


and' :: Bool -> Bool -> Bool
and' unBool otroBool | not unBool = False 
                     | not otroBool = False
                     | otherwise = True

and'' :: Bool -> Bool -> Bool
and'' unBool otroBool | unBool = otroBool
                      | otherwise = False

and''' :: Bool -> Bool -> Bool
and''' True unBool = unBool
and''' _ _  = False
    
or' :: Bool -> Bool -> Bool
or' True _ = True
or' _  unBool = unBool

or'' :: Bool -> Bool -> Bool
or'' False unBool = unBool
or'' _ _ = True