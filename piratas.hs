data Tesoro = UnTesoro{ nombreTesoro::String
                        , valor::Int
                        } deriving Eq
instance Show Tesoro where
    show tesoro = "Tesoro: " ++ nombreTesoro tesoro ++ ". Valor: " ++ show (valor tesoro)
instance Ord Tesoro where
    compare t1 t2 = compare (valor t1) (valor t2)

data Pirata = UnPirata{ nombrePirata::String
                        , botin::[Tesoro]
                        } deriving Eq
instance Show Pirata where
    show pirata = "Pirata: " ++ nombrePirata pirata ++ ". Tesoros: " ++ show (botin pirata)

tesoroJack1 = (UnTesoro "Brujula que apunta lo que mas deseas" 10000)
tesoroJack2 = (UnTesoro "Frasco de arena" 3)
pirataJack = (UnPirata "Jack Sparrow" [tesoroJack1, tesoroJack2])

tesoroDavid1 = (UnTesoro "Cajita musical" 1)
pirataDavid = (UnPirata "David Jones" [tesoroDavid1])

tesoroAnne1 = (UnTesoro "Doblones" 101)
tesoroAnne2 = (UnTesoro "Frasco de arena" 1)
pirataAnne = (UnPirata "Anne Bonny" [tesoroAnne1, tesoroAnne2])

cantTesorosPorPirata :: Pirata -> Int
cantTesorosPorPirata pirata = length (botin pirata)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = (sum (map (valor) (botin pirata))) > 10000

igualNombreDiferenteValor :: Tesoro -> Tesoro -> Bool
igualNombreDiferenteValor tesoro1 tesoro2 = ((nombreTesoro tesoro1) == (nombreTesoro tesoro2)) && ((valor tesoro1) /= (valor tesoro2))
loTiene :: [Tesoro] -> Tesoro -> Bool
loTiene botin1 tesoro2 = any (igualNombreDiferenteValor tesoro2) botin1
tesoroRepetidoPeroDiferenteValor :: Pirata -> Pirata -> Bool
tesoroRepetidoPeroDiferenteValor pirata1 pirata2 = any (loTiene (botin pirata1)) (botin pirata2)

tesoroMasValioso :: Pirata -> Tesoro
tesoroMasValioso pirata = maximum (botin pirata)

adquirirNuevoTesoro :: Pirata -> Tesoro -> Pirata
adquirirNuevoTesoro pirata tesoroNuevo = cambioEnElBotin (tesoroNuevo : (botin pirata)) pirata

tesoroValioso :: Tesoro -> Bool
tesoroValioso tesoro = valor tesoro > 100

perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos pirata = cambioEnElBotin (filter (not.tesoroValioso) (botin pirata)) pirata

tesoroDeNombreEspecifico :: String -> Tesoro -> Bool
tesoroDeNombreEspecifico nombre tesoro = nombreTesoro tesoro == nombre

perderTesorosDeNombreEspecifico :: Pirata -> String -> Pirata
perderTesorosDeNombreEspecifico pirata nombre = cambioEnElBotin (filter (not.tesoroDeNombreEspecifico nombre) (botin pirata)) pirata

cambioEnElBotin :: [Tesoro] -> Pirata -> Pirata
cambioEnElBotin botinNuevo pirata = (UnPirata (nombrePirata pirata)  botinNuevo)

saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata formaDeSaqueo tesoro 
    | formaDeSaqueo tesoro = cambioEnElBotin (tesoro : (botin pirata)) pirata
    | otherwise = pirata

--tesoroValioso ya existe en la linea 39

--tesoroDeNombreEspecifico ya existe en linea 50

conCorazon :: Tesoro -> Bool
conCorazon tesoro = False

complejo :: [(Tesoro -> Bool)] -> Tesoro -> Bool
complejo formasDeSaqueo tesoro = any (aplicaSaqueo tesoro) formasDeSaqueo

aplicaSaqueo :: Tesoro -> (Tesoro -> Bool) -> Bool
aplicaSaqueo tesoro formaDeSaqueo = formaDeSaqueo tesoro

data Barco = UnBarco{ nombreBarco::String
                        , tripulacion::[Pirata]
                        , saqueoPreferido :: (Tesoro -> Bool)
                        } 
instance Show Barco where
    show barco = "Barco: " ++ nombreBarco barco ++ ". Tripulacion: " ++ show (tripulacion barco)

perlaNegra = (UnBarco "Perla Negra" [pirataJack, pirataAnne] conCorazon)

incorporacionDePirata :: Pirata -> Barco -> Barco
incorporacionDePirata pirata barco 
        | not(elem pirata (tripulacion barco)) = cambioEnLaTripulacion (pirata : (tripulacion barco)) barco
        | otherwise = barco
       
abandonaBarco :: Pirata -> Barco -> Barco
abandonaBarco pirata barco 
        | elem pirata (tripulacion barco) = cambioEnLaTripulacion (filter (diferentePirata pirata) (tripulacion barco)) barco
        | otherwise = barco

diferentePirata :: Pirata -> Pirata -> Bool        
diferentePirata p1 p2 = (nombrePirata p1) /= (nombrePirata p2)

cambioEnLaTripulacion :: [Pirata] -> Barco -> Barco
cambioEnLaTripulacion tripulacionNueva barco = (UnBarco (nombreBarco barco) tripulacionNueva (saqueoPreferido barco))