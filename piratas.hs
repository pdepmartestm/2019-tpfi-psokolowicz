data Tesoro = UnTesoro{ nombreTesoro::[Char]
                        , valor::Int
                        } deriving Show

data Pirata = UnPirata{ nombrePirata::[Char]
                        , botin::[Tesoro]
                        } deriving Show

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

--igualTesoroDiferenteValor :: Botin -> Botin -> Bool
--igualTesoroDiferenteValor botin1 botin2 = (fst botin1 == fst botin2) && (snd botin1 /= snd botin2)

--loTiene :: [Botin] -> [Botin] -> Bool
--loTiene botin1 botin2 = any (igualTesoroDiferenteValor botin1 botin2) botin2

--tesoroRepetidoPeroDiferenteValor :: Pirata -> Pirata -> Bool
--tesoroRepetidoPeroDiferenteValor pirata1 pirata2 = any (loTiene (snd pirata1) (snd pirata2)) (snd pirata2)
