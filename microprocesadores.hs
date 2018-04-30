import Text.Show.Functions

--3.1 punto 1--

--1.--
data microProcesador = unMicroprocesador { memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter = Int, etiqueta :: String} deriving (Show)

--1.a.--
xt80800 = unMicroprocesador { memoria = [] , acumuladorA = 0 , acumuladorB = 0 , programCounter = 0, etiqueta = [] }

--3.2 Punto 2--

--1.--
NOP :: microProcesador -> microProcesador
NOP unMicroprocesador = unMicroprocesador { programCounter = programCounter unMicroprocesador + 1}

--2.--

programaQueIncrementeElPC3 :: microProcesador -> microProcesador
programaQueIncrementeElPC3 = NOP.NOP.NOP

--En este punto interviene la composicion--

--3.3 punto 3--

--1.--

LODV :: Int -> microProcesador -> microProcesador
LODV val unMicroprocesador = unMicroprocesador { acumuladorA = val + acumuladorA unMicroprocesador}

SWAP :: microProcesador -> microProcesador
SWAP unMicroprocesador = unMicroprocesador { acumuladorB = acumuladorA}

ADD :: microProcesador -> microProcesador
ADD unMicroprocesador = unMicroprocesador { acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador , acumuladorB = 0}

--2.--


--3.4 punto 4--

--1.--

DIVIDE unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorA unMicroprocesador / acumuladorB unMicroprocesador , acumuladorB = 0}

STR addr val unMicroprocesador = unMicroprocesador { memoria = (take (addr-1) (memoria unMicroprocesador)) ++ [val] ++ (drop (addr-1) (memoria unMicroprocesador))}

LOD addr unMicroprocesador =
