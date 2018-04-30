import Text.Show.Functions

--3.1 punto 1--

--1.--

data MicroProcesador = UnMicroprocesador { memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, etiqueta :: String} deriving (Show)

--1.a.--
xt80800 = UnMicroprocesador { memoria = [] , acumuladorA = 0 , acumuladorB = 0 , programCounter = 0, etiqueta = [] }

--3.2 Punto 2--

--1.--
nop :: MicroProcesador -> MicroProcesador
nop unMicroprocesador = unMicroprocesador { programCounter = programCounter unMicroprocesador + 1}

--2.--

programaQueIncrementeElPC3 :: MicroProcesador -> MicroProcesador
programaQueIncrementeElPC3 = nop.nop.nop

--En este punto interviene la composicion--

--3.3 punto 3--

--1.--

lodv :: Int -> MicroProcesador -> MicroProcesador
lodv val unMicroprocesador = unMicroprocesador { acumuladorA = val + acumuladorA unMicroprocesador, programCounter = programCounter unMicroprocesador +1}

swap :: MicroProcesador -> MicroProcesador
swap unMicroprocesador = unMicroprocesador { acumuladorB = acumuladorA unMicroprocesador,acumuladorA= acumuladorB unMicroprocesador, programCounter =  programCounter unMicroprocesador +1}

add :: MicroProcesador -> MicroProcesador
add unMicroprocesador = unMicroprocesador { acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador , acumuladorB = 0, programCounter = programCounter unMicroprocesador +1}

--2.--


--3.4 punto 4--

--1.--

divide unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorA unMicroprocesador / acumuladorB unMicroprocesador , acumuladorB = 0, programCounter = programCounter unMicroprocesador +1}

str addr val unMicroprocesador = unMicroprocesador { memoria = (take (addr-1) (memoria unMicroprocesador)) ++ [val] ++ (drop (addr-1) (memoria unMicroprocesador)), programCounter = programCounter unMicroprocesador +1}

lod addr unMicroprocesador = True

--2.--
