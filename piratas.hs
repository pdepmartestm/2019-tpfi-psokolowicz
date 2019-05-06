data Tesoro = UnTesoro{ nombreTesoro::[Char]
                        , valor::Int
                        } deriving Eq
instance Show Tesoro where
    show tesoro = "Tesoro: " ++ nombreTesoro tesoro ++ ". Valor: " ++ show (valor tesoro)
instance Ord Tesoro where
    compare t1 t2 = compare (valor t1) (valor t2)

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

tesoroAnne1 = (UnTesoro "Doblones" 101)
tesoroAnne2 = (UnTesoro "Frasco de arena" 1)
pirataAnne = (UnPirata "Anne Bonny" [tesoroAnne1, tesoroAnne2])

cantTesorosPorPirata :: Pirata -> Int
cantTesorosPorPirata pirata = length (botin pirata)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = (sum (map (valor) (botin pirata))) > 10000

igualNombreDiferenteValor tesoro1 tesoro2 = ((nombreTesoro tesoro1) == (nombreTesoro tesoro2)) && ((valor tesoro1) /= (valor tesoro2))
loTiene botin1 tesoro2 = any (igualNombreDiferenteValor tesoro2) botin1
tesoroRepetidoPeroDiferenteValor pirata1 pirata2 = any (loTiene (botin pirata1)) (botin pirata2)

tesoroMasValioso pirata = maximum (botin pirata)

adquirirNuevoTesoroYMostrar pirata tesoroNuevo = (UnPirata (nombrePirata pirata) (tesoroNuevo : (botin pirata)))

tesoroNoValioso tesoro = valor tesoro < 100
perderTesorosValiososYMostrar pirata = (UnPirata (nombrePirata pirata) (filter (tesoroNoValioso) (botin pirata)))

tesoroDeNombreEspecifico nombre tesoro = nombreTesoro tesoro /= nombre
perderTesorosDeNombreEspecifico pirata nombre = (UnPirata (nombrePirata pirata) (filter (tesoroDeNombreEspecifico nombre) (botin pirata)))