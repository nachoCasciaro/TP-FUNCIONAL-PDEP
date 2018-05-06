module MicroEntrega1 where
import Test.Hspec
--3.1 punto 1--

--1.--

data MicroProcesador = UnMicroprocesador { memoria :: [Int], acumuladorA :: Int, acumuladorB :: Int, programCounter :: Int, mensajeError :: String} deriving (Show)

--usamos una lista de enteros para la memoria donde se guardaran los datos en cada posicion del mismo, los acumuladores son enteros para contener los valores, lo mismo para el programCounter ya que se incrementa cada vez que se ejecuta una instruccion y por ultimo la etiqueta que almacena un string con el ultimo mensaje de error producido

--1.a.--

xt80800 :: MicroProcesador
xt80800 = UnMicroprocesador { memoria = replicate 1024 0 , acumuladorA = 0 , acumuladorB = 0 , programCounter = 0, mensajeError = [] }

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
aumentarPC unMicroprocesador = unMicroprocesador { programCounter = programCounter unMicroprocesador + 1}
--2.--

programaQueSume10Con22 :: MicroProcesador->MicroProcesador
programaQueSume10Con22 unMicroprocesador = (add.(lodv 22).swap.(lodv 10)) unMicroprocesador

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

--2.--

programaQueDivide2Por0 :: MicroProcesador->MicroProcesador
programaQueDivide2Por0 unMicroprocesador  = (divide.(lod 1).swap.(lod 2).(str 2 0) .(str 1 2)) unMicroprocesador

--4. CASOS DE PRUEBA--

fp20 :: MicroProcesador
fp20 = UnMicroprocesador { memoria = replicate 1024 0 , acumuladorA = 7 , acumuladorB = 24 , programCounter = 0, mensajeError = [] }

at8086 :: MicroProcesador
at8086 = UnMicroprocesador {memoria=[1..20] , acumuladorA=0 , acumuladorB=0 , programCounter=0 , mensajeError=[]}

programaQueDivide12Por4 :: MicroProcesador->MicroProcesador
programaQueDivide12Por4 unMicroprocesador  = (divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)) unMicroprocesador

main = hspec $ do
    describe "Test 4.1 Punto 2 - Tests de NOP" $ do
        it "NOP no modifica el acumulador A" $ do
            ((acumuladorA.nop) xt80800) `shouldBe` (0::Int)

        it "NOP no modifica el acumulador B" $ do
            ((acumuladorB.nop) xt80800) `shouldBe` (0::Int)

        it "NOP no modifica la memoria" $ do
            ((memoria.nop) xt80800) `shouldBe` (replicate 1024 0::[Int])

        it "NOP no modifica el mensaje de error" $ do
            ((mensajeError.nop) xt80800) `shouldBe` ([]::String)

        it "Programa que incremente 3 veces el program counter" $ do
            ((programCounter.programaQueIncrementeElPC3) xt80800) `shouldBe` (3::Int)

    describe "Test 4.2 Punto 3 - Tests de Sumas" $ do
        it "LODV 5 lo carga en acumulador A" $ do
            ((acumuladorA.(lodv 5)) xt80800) `shouldBe` (5::Int)

        it "LODV 5 deja el acumuladorB en 0" $ do
            ((acumuladorB.(lodv 5)) xt80800) `shouldBe` (0::Int)

        it "SWAP intercambia el valor del acumulador A (7) por el del acumulador B (24)" $ do
            ((acumuladorA.swap) fp20) `shouldBe` (24::Int)

        it "SWAP intercambia el valor del acumulador B (24) por el del acumulador A (7)" $ do
            ((acumuladorB.swap) fp20) `shouldBe` (7::Int)

        it "El programa que suma 10 con 22, que da como resultado 32, y lo guarda en Acumulador A" $ do
            ((acumuladorA.programaQueSume10Con22) xt80800) `shouldBe` (32::Int)

        it "El programa que suma 10 con 22 deja en 0 el Acumulador B" $ do
            ((acumuladorB.programaQueSume10Con22) xt80800) `shouldBe` (0::Int)

        it "El programa que suma 10 con 22 aumenta en 4 el Program Counter" $ do
            ((programCounter.programaQueSume10Con22) xt80800) `shouldBe` (4::Int)

    describe "Test 4.3 Punto 4 - Tests de División" $ do
        it "STR 2 5 en la memoria pone un 5 en la posicion 2" $ do
            ((memoria.(str 2 5)) at8086) `shouldBe` ([1, 5, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]::[Int])

        it "LOD 2 de una memoria vacía debe dejar con 0 el acumuladorA" $ do
            ((acumuladorA.(lod 2))  xt80800) `shouldBe` (0::Int)

        it "Division por cero da error" $ do
            ((mensajeError.programaQueDivide2Por0) xt80800) `shouldBe` ("division by zero"::String)

        it "Division por cero aumenta en 6 el program counter" $ do
            ((programCounter.programaQueDivide2Por0) xt80800) `shouldBe` (6::Int)

        it "Division de 12 por 4 debe guardar un 3 en el acumulador A " $ do
            ((acumuladorA.programaQueDivide12Por4) xt80800) `shouldBe` (3::Int)

        it "Division de 12 por 4 deja en 0 el Acumulador B" $ do
            ((acumuladorB.programaQueDivide12Por4) xt80800) `shouldBe` (0::Int)

        it "Division de 12 por 4 no deja el mensaje de error porque funciona bien" $ do
            ((mensajeError.programaQueDivide12Por4) xt80800) `shouldBe` ([]::String)
