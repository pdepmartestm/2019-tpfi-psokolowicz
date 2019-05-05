data Tesoro = UnTesoro{ nombreTesoro::[Char]
                        , valor::Int
                        }
instance Show Tesoro where
    show tesoro = "Tesoro: " ++ nombreTesoro tesoro ++ ". Valor: " ++ show (valor tesoro)

data Pirata = UnPirata{ nombrePirata::[Char]
                        , botin::[Tesoro]
                        }
instance Show Pirata where
    show pirata = "Pirata: " ++ nombrePirata pirata ++ ". Tesoros: " ++ show (botin pirata)

tesoroJack1 = (UnTesoro "Brujula que apunta lo que mas deseas" 10000)
tesoroJack2 = (UnTesoro "Frasco de arena" 3)
pirataJack = (UnPirata "Jack Sparrow" [tesoroJack1, tesoroJack2])

tesoroDavid1 = (UnTesoro "Cajita musical" 1)
pirataDavid = (UnPirata "David Jones" [tesoroDavid1])

tesoroAnne1 = (UnTesoro "Doblones" 100)
tesoroAnne2 = (UnTesoro "Frasco de arena" 1)
pirataAnne = (UnPirata "Anne Bonny" [tesoroAnne1, tesoroAnne2])

cantTesorosPorPirata :: Pirata -> Int
cantTesorosPorPirata pirata = length (botin pirata)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = (sum (map (valor) (botin pirata))) > 10000

igualNombreDiferenteValor tesoro1 tesoro2 = ((nombreTesoro tesoro1) == (nombreTesoro tesoro2)) && ((valor tesoro1) /= (valor tesoro2))
loTiene botin1 tesoro2 = any (igualNombreDiferenteValor tesoro2) botin1
tesoroRepetidoPeroDiferenteValor pirata1 pirata2 = any (loTiene (botin pirata1)) (botin pirata2)

adquirirNuevoTesoroYMostrar pirata tesoroNuevo = (UnPirata(nombrePirata pirata) (tesoroNuevo : (botin pirata)))