module MicroEntrega1 where
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
lodv val unMicroprocesador = aumentarPC unMicroprocesador { acumuladorA = val}

swap :: MicroProcesador -> MicroProcesador
swap unMicroprocesador = aumentarPC unMicroprocesador { acumuladorB = acumuladorA unMicroprocesador, acumuladorA= acumuladorB unMicroprocesador}

add :: MicroProcesador -> MicroProcesador
add unMicroprocesador = aumentarPC unMicroprocesador { acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador , acumuladorB = 0}

aumentarPC = nop
--2.--

programaQueSume10Con22 :: Int->Int->MicroProcesador->MicroProcesador
programaQueSume10Con22 valor1 valor2 unMicroprocesador = (add.(lodv valor2).swap.(lodv valor1)) unMicroprocesador

--3.4 punto 4--

--1.--

divide :: MicroProcesador->MicroProcesador
divide unMicroprocesador
      |(acumuladorB unMicroprocesador) /= 0 =  aumentarPC unMicroprocesador {acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador) , acumuladorB = 0}
      |otherwise = aumentarPC unMicroprocesador {etiqueta = "division by zero"}

str :: Int->Int->MicroProcesador->MicroProcesador
str addr val unMicroprocesador = aumentarPC unMicroprocesador { memoria = (take (addr-1) (memoria unMicroprocesador)) ++ [val] ++ (drop (addr-1) (memoria unMicroprocesador))}

lod :: Int->MicroProcesador->MicroProcesador
lod addr unMicroprocesador = aumentarPC unMicroprocesador { acumuladorA = (!!) (memoria unMicroprocesador) (addr-1)}

--2.--

programaQueDivide2Por0 :: Int->Int->Int->Int->MicroProcesador->MicroProcesador
programaQueDivide2Por0 valor1 valor2 addr1 addr2 unMicroprocesador  = (divide.(lod addr1).swap.(lod addr2).(str addr2 valor1) .(str addr1 valor2)) unMicroprocesador
