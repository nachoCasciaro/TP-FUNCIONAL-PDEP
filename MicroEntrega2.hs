module MicroEntrega2 where
import Test.Hspec
import Text.Show.Functions


--3.1 punto 1--

--1.--

data MicroProcesador = UnMicroprocesador { memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String, programas :: [Programa]} deriving (Show)

--1.a.--

xt80800 :: MicroProcesador
xt80800 = UnMicroprocesador { memoria = replicate 1024 0 , acumuladorA = 0 , acumuladorB = 0 , programCounter = 0, mensajeError = [], programas = []}

fp20 :: MicroProcesador
fp20 = UnMicroprocesador { memoria = replicate 1024 0 , acumuladorA = 7 , acumuladorB = 24 , programCounter = 0, mensajeError = [], programas = []}

at8086 :: MicroProcesador
at8086 = UnMicroprocesador {memoria=[1..20] , acumuladorA=0 , acumuladorB=0 , programCounter=0 , mensajeError=[], programas = []}

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

aumentarPC :: MicroProcesador->MicroProcesador
aumentarPC = nop


--3.4 punto 4--

--1.--

divide :: MicroProcesador->MicroProcesador
divide unMicroprocesador
      |(acumuladorB unMicroprocesador) /= 0 =  aumentarPC unMicroprocesador {acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador) , acumuladorB = 0}
      |otherwise = aumentarPC unMicroprocesador {mensajeError = "division by zero"}

str :: Int->Int->MicroProcesador->MicroProcesador
str addr val unMicroprocesador = aumentarPC unMicroprocesador { memoria = (take (addr-1) (memoria unMicroprocesador)) ++ [val] ++ (drop (addr) (memoria unMicroprocesador))}

lod :: Int->MicroProcesador->MicroProcesador
lod addr unMicroprocesador = aumentarPC unMicroprocesador { acumuladorA = (!!) (memoria unMicroprocesador) (addr-1)}


--PARTE 2--

--3.1 Punto 1--

type Programa = [(MicroProcesador -> MicroProcesador)]

cargarPrograma :: Programa -> MicroProcesador -> MicroProcesador
cargarPrograma programa unMicroprocesador = unMicroprocesador { programas = programas unMicroprocesador ++ [programa] }

programaQueSume10Con22 = [(str 1 12), (str 2 4), (lod 2), swap, (lod 1), divide]

programaQueDivide2Por0 = [(str 1 2), (str 2 0), (lod 2), swap, (lod 1), divide]


--3.2 Punto 2--
ejecutarPrograma :: Programa->MicroProcesador->MicroProcesador
ejecutarPrograma [] unMicroProcesador = unMicroProcesador
ejecutarPrograma (x : xs) (UnMicroprocesador memoria acumuladorA acumuladorB programCounter [] programas) = ( ejecutarPrograma xs . x ) (UnMicroprocesador memoria acumuladorA acumuladorB programCounter [] programas)
ejecutarPrograma _ unMicroProcesador = unMicroProcesador

--3.3 Punto 3--

ifnz :: Programa->MicroProcesador->MicroProcesador
ifnz [] unMicroProcesador = unMicroProcesador
ifnz _ (UnMicroprocesador memoria 0 acumuladorB programCounter mensajeError programas) = (UnMicroprocesador memoria 0 acumuladorB programCounter mensajeError programas)
ifnz programa unMicroProcesador = ejecutarPrograma programa unMicroProcesador

--3.4 Punto 4--
type Instruccion = (MicroProcesador->MicroProcesador)
prueba = [swap,nop,lodv 133,lodv 0,str 1 3,str 2 0]

depurarPrograma :: Programa -> MicroProcesador -> Programa
depurarPrograma programa unMicro = filter (condicion unMicro) programa

condicion :: MicroProcesador->Instruccion->Bool
condicion unMicro instruccion = ( controlar. instruccion) unMicro

controlar :: MicroProcesador->Bool
controlar unMicro = acumuladorA unMicro /= 0 || acumuladorB unMicro /= 0 || memoria unMicro /= replicate 1024 0


--3.5 Punto 5

tieneMemoriaOrdenada :: MicroProcesador->Bool
tieneMemoriaOrdenada unMicro = (chequearBooleanos.operarLista.memoria) unMicro  

chequearBooleanos :: [Bool]->Bool
chequearBooleanos lista = foldl1 (&&) lista

operarLista :: [Int]->[Bool]
operarLista (x:y:xs) = zipWith (<=) (x:y:xs) (y:xs)

--3.6 Punto 6

microInfinito :: MicroProcesador
microInfinito = UnMicroprocesador { memoria = [0..] , acumuladorA = 0 , acumuladorB = 0 , programCounter = 0, mensajeError = [], programas = []}

--a) Carga y ejecuta el programa correctamente, pero al querer mostrar el resultado por pantalla, este nunca termina de mostrar la memoria de microprocesador ya que es infinita.
--b) El programa nunca termina de ejecutar ya que la memoria es infinita y no puede encontrar alguna posicion de memoria que no este ordenada.
--c) En el caso a, ya que haskell utiliza lazy evaluation el programa funciona correctamente ya que en las instrucciones en las que opera con la memoria solamente utiliza una posicion especifica. Por esta razon, haskell evalua solamente hasta la posicion de memoria que necesita y el resto de la memoria infinita es ignorada. Por otra parte, en el punto b, haskell evaluara la memoria hasta encontrar alguna posicion que no este ordenada y en dicho caso devolvera false pero como en este caso, la memoria infinita esta ordena, nunca termina la ejecucion del programa.

--
