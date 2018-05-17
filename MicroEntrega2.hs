module MicroEntrega1 where
import Test.Hspec
import Text.Show.Functions


data MicroProcesador = UnMicroprocesador { memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String, programas :: [Programa]} deriving (Show)

--PARTE 2--

--3.1 Punto 1--

type Programa = [(MicroProcesador -> MicroProcesador)]
cargarPrograma :: Programa -> MicroProcesador -> MicroProcesador
cargarPrograma programa unMicroprocesador = unMicroprocesador { programas = programas unMicroprocesador ++ [programa] }

programaQueDivide12Por4 = [(str 1 12), (str 2 4), (lod 2), swap, (lod 1), divide]

programaQueDivide2Por0 = [(str 1 2), (str 2 0), (lod 2), swap, (lod 1), divide]


--3.2 Punto 2--
ejecutarPrograma :: Programa->MicroProcesador->MicroProcesador
ejecutarPrograma [ ] unMicroProcesador = unMicroProcesador
ejecutarPrograma (x : xs) (UnMicroprocesador memoria acumuladorA acumuladorB programCounter [] programas) = ( ejecutarPrograma xs . x ) (UnMicroprocesador memoria acumuladorA acumuladorB programCounter [] programas)
ejecutarPrograma _ unMicroProcesador = unMicroProcesador

--3.3 Punto 3--

ifnz :: Programa->MicroProcesador->MicroProcesador
ifnz [ ] unMicroProcesador = unMicroProcesador
ifnz _ (UnMicroprocesador memoria 0 acumuladorB programCounter mensajeError programas) = (UnMicroprocesador memoria 0 acumuladorB programCounter mensajeError programas)
ifnz programa unMicroProcesador = ejecutarPrograma programa unMicroProcesador

  --3.4 Punto 4--
prueba = [swap, nop, lodv 133, lodv 0, str 1 3, str 2 0]
depurarPrograma :: Programa -> MicroProcesador -> Programa
depurarPrograma programa unMicro = filter (condicion unMicro) programa

--condicion :: MicroProcesador->(MicroProcesador->MicroProcesador)->MicroProcesador
condicion unMicro instruccion = ( controlar. instruccion) unMicro

--controlar :: MicroProcesador->Bool
controlar unMicro = acumuladorA unMicro /= 0 || acumuladorB unMicro /= 0 || memoria unMicro /= []
