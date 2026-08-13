area_triangulo :: Float -> Float -> Float
area_triangulo base altura = (base * altura)/2


omenor :: Int -> Int -> Int -> Int
omenor a b c
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | otherwise = c

sao_iguais :: Int -> Int -> Int -> Int -> Bool
sao_iguais a b c d 
    | a == b && b == c && c == d = True
    | otherwise = False


diferentes :: Int -> Int -> Int -> Bool
diferentes a b c = a /= b && b /= c && c /= a

media_ponderada :: Float -> Float -> Float -> Float -> Float
media_ponderada a b c d = (a*1 + b*2 + c*3 + d*4)/(1 + 2 + 3 + 4)


desconto :: Float -> Float
desconto preco = preco * 0.8


xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

prog :: Int -> Int -> Int
prog razao primeiro_termo = primeiro_termo  + razao*9 

sinal :: Int -> String
sinal numero 
    | numero > 0 = "Positivo"
    | numero < 0 = "Negativo"
    | otherwise = "Zero"


nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

edigito :: Char -> Bool
edigito digito = digito >= '0' && digito <= '9'

maiuscula_2_minuscula :: Char -> Char
maiuscula_2_minuscula letra
    | letra >= 'A' && letra <= 'Z' = toEnum (fromEnum letra + 32)
    | otherwise = letra

minuscula_2_maiuscula :: Char -> Char
minuscula_2_maiuscula letra
    | letra >= 'a' && letra <= 'z' = toEnum (fromEnum letra - 32)
    | otherwise = letra

soma_digitos :: Int -> Int
soma_digitos numero =
    (numero `div` 100) +
    ((numero `div` 10) `mod` 10) +
    (numero `mod` 10)


media_aritmetica :: Float -> Float -> Float -> Float
media_aritmetica a b c = (a + b + c) / 3

lados_triangulo :: Float -> Float -> Float -> Bool
lados_triangulo a b c = a + b > c && a + c > b && b + c > a

tipo_triangulo :: Float -> Float -> Float -> String
tipo_triangulo a b c
    | c^2 == a^2 + b^2 = "Retangulo"
    | c^2 < a^2 + b^2 = "Acutangulo"
    | otherwise = "Obtusangulo"


soma_pa :: Int -> Int -> Int -> Int
soma_pa primeiro_termo numero_termos n_esimo_termo = ((primeiro_termo + n_esimo_termo) * numero_termos) `div` 2

localizacao :: Float -> Float -> String
localizacao x y
    | x == 0 && y == 0 = "Origem"
    | x == 0 && y > 0  = "Eixo Y positivo"
    | x == 0 && y < 0  = "Eixo Y negativo"
    | y == 0 && x > 0  = "Eixo X positivo"
    | y == 0 && x < 0  = "Eixo X negativo"
    | x > 0 && y > 0   = "1º quadrante"
    | x < 0 && y > 0   = "2º quadrante"
    | x < 0 && y < 0   = "3º quadrante"
    | otherwise        = "4º quadrante"


mes :: Int -> String
mes n
    | n == 1  = "Janeiro"
    | n == 2  = "Fevereiro"
    | n == 3  = "Marco"
    | n == 4  = "Abril"
    | n == 5  = "Maio"
    | n == 6  = "Junho"
    | n == 7  = "Julho"
    | n == 8  = "Agosto"
    | n == 9  = "Setembro"
    | n == 10 = "Outubro"
    | n == 11 = "Novembro"
    | n == 12 = "Dezembro"
    | otherwise = "Mes invalido"

e_par :: Int -> Bool
e_par numero = numero `mod` 2 == 0

num_raizes :: Float -> Float -> Float -> Int
num_raizes a b c
    | delta > 0 = 2
    | delta < 0 = 0
    | otherwise = 1
    where
        delta = b^2 - 4*a*c 

fah_2_cel :: Float -> Float
fah_2_cel temperatura = (temperatura - 32)/ 1.8

classificar_idade :: Int -> String
classificar_ idade idade
    | idade < 0  = "Idade inválida"
    | idade <= 12 = "Criança"
    | idade <= 17 = "Adolescente"
    | idade <= 59 = "Adulto"
    | otherwise   = "Idoso"


classificar_nota :: Int -> String
classificar_nota nota
    | nota < 0 || nota > 100 = "Nota inválida"
    | nota <= 49             = "F"
    | nota <= 59             = "D"
    | nota <= 69             = "C"
    | nota <= 79             = "B"
    | otherwise              = "A"


prox_caractere :: Char -> Char
prox_caractere caractere 
    | caractere == 'z' = 'a'
    | caractere == 'Z' = 'A'
    | otherwise = toEnum (fromEnum caractere + 1)
