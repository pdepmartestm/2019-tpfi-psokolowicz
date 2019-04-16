type Botin = ([Char], Integer)
type Pirata = ([Char], [Botin])
type Piratas = [Pirata]

listaPiratas = [("Jack Sparrow", [("Brujula que apunta lo que mas deseas", 10000), ("Frasco de arena", 0)]), ("David Jones", [("Cajita musical", 1)]), ("Anne Bonny", [("Doblones", 100), ("Frasco de arena", 1)])]
pirataJack = ("Jack Sparrow", [("Brujula que apunta lo que mas deseas", 10000), ("Frasco de arena", 0)])
pirataDavid = ("David Jones", [("Cajita musical", 1)])
pirataAnne = ("Anne Bonny", [("Doblones", 100), ("Frasco de arena", 1)])

cantTesorosPorPirata :: Pirata -> Int
cantTesorosPorPirata pirata = length (snd pirata)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = (sum (map (snd) (snd pirata))) > 10000

--tesoroRepetidoPeroDiferenteValor :: Pirata -> Pirata -> Bool
--tesoroRepetidoPeroDiferenteValor pirata1 pirata2 =  map (any (snd (fst pirata1)==) (snd (fst pirata2))) pirata1