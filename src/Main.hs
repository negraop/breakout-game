module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Control.Monad.State


-- Novos tipos + State
data Posicao = Posicao Float Float deriving Show
data Velocidade = Velocidade Float Float deriving Show
data Tamanho = Tamanho Float Float deriving Show

data Bola = Bola Posicao Velocidade Picture deriving Show
data Paddle = Paddle Posicao Tamanho Picture deriving Show

-- Posicao, Tamanho, Picture, Destruido e Ponto
data Bloco = Bloco Posicao Tamanho Picture Bool Int
-- Armazena o estado das setas para conseguir realizar o movimento
-- continuo do paddle ao segurar a seta esquerda ou direita
data EstadoSetas = EstadoSetas Bool Bool

data GameState = GameState
  {
  bola            :: Bola,
  paddle          :: Paddle,
  blocos          :: [Bloco],
  estadoSetas     :: EstadoSetas,
  vidas           :: Int,
  pontuacao       :: Int,
  acabouJogo      :: Bool,
  start           :: Bool,
  colidiuInferior :: Bool
  }

-- GameState -> (a, GameState)
type World = State GameState


-- Funções auxiliares da Bola
desenhaBola :: Bola -> Picture
desenhaBola (Bola (Posicao x y) _ p) = translate x y p

{- Na grande maioria das funções é realizado a etapa de:
-- 1) Pegar o estado atual
-- 2) Realizar o cálculo necessário
-- 3) Atualizar o estado
-}
atualizaPosicaoBola :: Float -> World ()
atualizaPosicaoBola dt = do
  s <- get -- Pega o estado atual
  if start s && not (colidiuInferior s) && not (acabouJogo s)
    then do
      let (Bola (Posicao x y) (Velocidade vx vy) p) = bola s
          novaBola = Bola (Posicao (x + vx * dt) (y + vy * dt)) (Velocidade vx vy) p
      put $ s { bola = novaBola } -- Atualiza o estado
    else do
      return ()


-- Funções auxiliares do Paddle
desenhaPaddle :: Paddle -> Picture
desenhaPaddle (Paddle (Posicao x y) _ p) = translate x y p

atualizaPosicaoPaddle :: Float -> World ()
atualizaPosicaoPaddle dt = do
  s <- get
  if start s && not (colidiuInferior s) && not (acabouJogo s)
    then do
      let (Paddle (Posicao x y) t p) = paddle s
          (EstadoSetas esq dir) = estadoSetas s
          velocidade = 500
          movX | esq && dir = 0            -- Verifica qual estadoSeta está ativo no momento
              | esq        = -velocidade  -- ||
              | dir        = velocidade   -- ||
              | otherwise  = 0            -- ||
          novoX = x + movX * dt
          lx = limiteX - (largura / 2)     -- Adiciona um limite para o paddle não ultrapassar
          novoX' | novoX > lx  = lx        -- a borda do jogo
                | novoX < -lx = -lx
                | otherwise   = novoX
          novoPaddle = Paddle (Posicao novoX' y) t p
      put $ s { paddle = novoPaddle }
    else do
      return ()


-- Funções auxiliares do Bloco
desenhaBloco :: Bloco -> Picture
desenhaBloco (Bloco _ _ _ True _) = Blank  -- Se o bloco foi destruido, entao substitui a Picture dele para Blank
desenhaBloco (Bloco (Posicao x y) _ p False _) = translate x y p

-- Bloco não muda de posição
-- atualizaPosicaoBloco :: Float -> World ()
-- atualizaPosicaoBloco _ = return ()





---------- Funções de Desenhos (Render) ----------
-- Renderiza o separador Branco no topo da janela do jogo
desenhaSeparacao :: Picture
desenhaSeparacao = translate 0 285 $ color white $ rectangleSolid (limiteX * 2) 10

-- Renderiza a pontuação no lado superior direito
desenhaPontuacao :: Int -> Picture
desenhaPontuacao ponto = translate 110 310 $ scale 0.2 0.2 $ color white $ Text ("Pontuacao: " ++ show ponto)

-- Renderiza as vidas restantes no lado superior esquerdo
desenhaVidas :: Int -> Picture
desenhaVidas vidasAtual =
    pictures [translate (-300) 310 textoVidas, translate (-200) 320 translateCoracoes]
  where
    textoVidas = scale 0.2 0.2 $ color white $ text "Vidas: "
    {- Nessa etapa abaixo eu utilizei o chatGPT para me ajudar a renderizar os 3 corações de forma automática.
    Aqui ele replica a quantidade de vidas atuais para desenhar o Coração, e para dar o espaçamento entre eles
    estou usando a função zipWith(), onde ele vai aplicar a função translate infixa somente no primeiro parametro,
    que na verdade é a posição X do elemento na tela. Então ele vai espaçando cada coração de 35 a 35 pixels. -}
    translateCoracoes = pictures $ zipWith (`translate` 0) [0, 35, 70] (replicate vidasAtual desenhaCoracao)

desenhaCoracao :: Picture
desenhaCoracao = color red $ pictures [
                                       scale 0.7 0.7 $ translate (-10) 10 (arcSolid 0 180 10),
                                       scale 0.7 0.7 $ translate 10 10 (arcSolid 0 180 10),
                                       scale 0.7 0.7 $ polygon [(-20, 10), (0, -20), (20, 10)]
                                     ]

desenhaTelaInicial :: Picture
desenhaTelaInicial = pictures [translate (-230) 70     $ scale 0.7 0.7 $ color white $ Text "BREAKOUT",
                               translate (-180) (-30)  $ scale 0.2 0.2 $ color white $ Text "Utilize as setas <- e ->",
                               translate (-150) (-80)  $ scale 0.2 0.2 $ color white $ Text "para mover o paddle!",
                               translate (-200) (-180) $ scale 0.2 0.2 $ color white $ Text "Pressione Espaco para iniciar!",
                               translate (-170) (-250) $ scale 0.2 0.2 $ color white $ Text "Pressione ESC para sair"]


desenhaColidiuTexto :: Picture
desenhaColidiuTexto = translate (-180) (-150) $ scale 0.2 0.2 $ color white $ Text "Pressione 'M' para continuar"

desenhaFimDeJogo :: Int -> Picture
desenhaFimDeJogo ponto = pictures [translate (-185) 30 $ scale 0.4 0.4 $ color white $ Text "FIM DE JOGO!",
                                   translate (-90) (-80) $ scale 0.2 0.2 $ color white $ Text ("Pontuacao: " ++ show ponto),
                                   translate (-180) (-180) $ scale 0.2 0.2 $ color white $ Text "Pressione 'R' para reiniciar!"]






--------------- Funções de Colisão ---------------
verificaColisaoLimite :: World ()
verificaColisaoLimite = do
  s <- get
  let b@(Bola (Posicao x y) (Velocidade vx vy) p) = bola s
      vidasRestantes = vidas s
      lx = limiteX - raio
      ly = limiteY - raio

  if y < -ly
    then do
      let vidaFinal = vidasRestantes - 1
      if vidaFinal <= 0
        then do
          put $ s { acabouJogo = True, vidas = vidaFinal }
        else do
          put $ s { colidiuInferior = True, vidas = vidaFinal, bola = Bola (Posicao 0 30) (Velocidade 100 (-400)) (color white $ circleSolid raio), paddle = Paddle (Posicao 0 (-(limiteY / 1.2))) (Tamanho largura 10) (color blue $ rectangleSolid largura 10) }

    else do
      let novaBola
            | x > lx = Bola (Posicao lx y) (Velocidade (-vx) vy) p
            | x < -lx = Bola (Posicao (-lx) y) (Velocidade (-vx) vy) p
            | y > (ly - 70) = Bola (Posicao x (ly - 70)) (Velocidade vx (-vy)) p
            | otherwise = b
      put $ s { bola = novaBola }


verificaColisaoPaddle :: World ()
verificaColisaoPaddle = do
  s <- get
  let b@(Bola (Posicao xBola yBola) (Velocidade _ vy) p) = bola s
      (Paddle (Posicao xPaddle yPaddle) (Tamanho larguraPaddle alturaPaddle) _) = paddle s
      topoPaddle = yPaddle + (alturaPaddle / 2)
      fundoBola  = yBola - raio
      dentroLargura = xBola >= (xPaddle - larguraPaddle / 2) &&
                      xBola <= (xPaddle + larguraPaddle / 2)
      colisao = dentroLargura &&
                fundoBola <= topoPaddle &&
                yBola >= topoPaddle
      yB' = topoPaddle + raio
      larguraSetor = larguraPaddle / 7
      setor = (xBola - (xPaddle - larguraPaddle / 2)) / larguraSetor
      novaBola
        | colisao = Bola (Posicao xBola yB') (Velocidade (ajustaVelocidadeX setor) (-vy)) p
        | otherwise = b
  put $ s { bola = novaBola }

-- Função auxiliar para determinar qual o ângulo da bola após colisão com o paddle
ajustaVelocidadeX :: Float -> Float
ajustaVelocidadeX setor
  | setor < 1 = - 500
  | setor < 2 = - 350
  | setor < 3 = - 200
  | setor < 4 = 0
  | setor < 5 = 200
  | setor < 6 = 350
  | otherwise = 500


verificaColisaoBlocos :: World ()
verificaColisaoBlocos = do
  s <- get
  let (Bola (Posicao xB yB) (Velocidade vx vy) p) = bola s
      blocosAtuais = blocos s

  (blocosAtualizados, novaVelocidadeBola, pontosGanhados) <- processaColisao blocosAtuais (Velocidade vx vy)

  put $ s { blocos = blocosAtualizados, bola = Bola (Posicao xB yB) novaVelocidadeBola p, pontuacao = pontuacao s + pontosGanhados }

  s2 <- get

  let todosBlocosDestruidos = all (\(Bloco _ _ _ destruido _) -> destruido) (blocos s2)
  if todosBlocosDestruidos
    then do
      put $ s2 { acabouJogo = True }
    else do
      return ()

processaColisao :: [Bloco] -> Velocidade -> World ([Bloco], Velocidade, Int)
processaColisao [] velAtual = return ([], velAtual, 0)
processaColisao (b:bs) velAtual = do
  (blocoAtualizado, novaVelocidade, pontosBloco) <- verificaColisaoIndividual b velAtual
  (blocosAtualizados, velFinal, pontosRestantes) <- processaColisao bs novaVelocidade
  return (blocoAtualizado : blocosAtualizados, velFinal, pontosBloco + pontosRestantes)

verificaColisaoIndividual :: Bloco -> Velocidade -> World (Bloco, Velocidade, Int)
verificaColisaoIndividual b@(Bloco (Posicao xBloco yBloco) (Tamanho larguraBloco alturaBloco) p destruido ponto) v@(Velocidade vx vy)
  | destruido = return (b, v, 0)
  | otherwise = do
      s <- get
      let (Bola (Posicao xBola yBola) _ _) = bola s
          superiorBloco = yBloco + alturaBloco  / 2
          inferiorBloco = yBloco - alturaBloco  / 2
          esquerdaBloco = xBloco - larguraBloco / 2
          direitaBloco  = xBloco + larguraBloco / 2

          houveColisao = xBola + raio > esquerdaBloco &&
                         xBola - raio < direitaBloco &&
                         yBola + raio > inferiorBloco &&
                         yBola - raio < superiorBloco

          novaVelocidade
            | yBola - raio < superiorBloco = Velocidade vx (-vy)
            | yBola + raio > inferiorBloco = Velocidade vx (-vy)
            | xBola - raio < direitaBloco  = Velocidade (-vx) vy
            | xBola + raio > esquerdaBloco = Velocidade (-vx) vy
            | otherwise = v

      if houveColisao
        then return (Bloco (Posicao xBloco yBloco) (Tamanho larguraBloco alturaBloco) p True ponto, novaVelocidade, ponto)
        else return (b, v, 0)




---------- Funções Principais utilizadas no play() ----------
-- Função principal que desenha todos os elementos na tela
desenhaMundo :: World Picture
desenhaMundo = do
  s <- get
  if acabouJogo s && start s -- Verifica os estados atuais
    then return $ pictures
      [desenhaSeparacao, desenhaPontuacao (pontuacao s), desenhaVidas (vidas s), desenhaFimDeJogo (pontuacao s)]
    else if start s && colidiuInferior s -- Verifica os estados atuais
      then return $ pictures
        $ desenhaSeparacao
        : desenhaPontuacao (pontuacao s)
        : desenhaVidas (vidas s)
        : desenhaBola (bola s)
        : desenhaPaddle (paddle s)
        : desenhaColidiuTexto
        : map desenhaBloco (blocos s)
      else if start s  -- Verifica os estados atuais
        then return $ pictures
            $ desenhaSeparacao
            : desenhaPontuacao (pontuacao s)
            : desenhaVidas (vidas s)
            : desenhaBola (bola s)
            : desenhaPaddle (paddle s)
            : map desenhaBloco (blocos s)
        else return desenhaTelaInicial


-- Função principal que realiza as validações a cada ciclo no jogo
atualizaMundo :: Float -> World ()
atualizaMundo dt = do
  atualizaPosicaoBola dt
  atualizaPosicaoPaddle dt

  verificaColisaoPaddle
  verificaColisaoLimite
  verificaColisaoBlocos
  s <- get
  let novaBola = bola s
      novoPaddle = paddle s
  put $ s { bola = novaBola, paddle = novoPaddle }



-- Função principal para controle dos inputs do jogo
trataInput :: Event -> World ()
trataInput (EventKey (SpecialKey KeyLeft) Down _ _) = do
  s <- get
  let (EstadoSetas _ dir) = estadoSetas s
  put $ s { estadoSetas = EstadoSetas True dir }
trataInput (EventKey (SpecialKey KeyLeft) Up _ _) = do
  s <- get
  let (EstadoSetas _ dir) = estadoSetas s
  put $ s { estadoSetas = EstadoSetas False dir }
trataInput (EventKey (SpecialKey KeyRight) Down _ _) = do
  s <- get
  let (EstadoSetas esq _) = estadoSetas s
  put $ s { estadoSetas = EstadoSetas esq True }
trataInput (EventKey (SpecialKey KeyRight) Up _ _) = do
  s <- get
  let (EstadoSetas esq _) = estadoSetas s
  put $ s { estadoSetas = EstadoSetas esq False }
trataInput (EventKey (SpecialKey KeySpace) Up _ _) = do
  s <- get
  put $ s { start = True }
trataInput (EventKey (Char 'm') Up _ _) = do
  s <- get
  if colidiuInferior s
    then do
      put $ s { colidiuInferior = False }
    else do return ()
trataInput (EventKey (Char 'r') Up _ _) = do
  s <- get
  if acabouJogo s
    then do
      put estadoInicial
    else do return ()
trataInput _ = return ()




-- Constantes
limiteX, limiteY, raio, largura, altura :: Num a => a
limiteX = 320
limiteY = 350
raio = 10
largura = 100
altura = 20


-- Funções auxiliares para criação de blocos
replicarBloco :: Int -> Bloco -> Float -> [Bloco]
replicarBloco 0 _ _ = []
replicarBloco n bloco dx =
  let Bloco (Posicao x y) t p d ponto = bloco
  in Bloco (Posicao (x - fromIntegral (n - 1) * dx) y) t p d ponto : replicarBloco (n - 1) bloco dx

criarBloco :: Color -> Float -> Float -> Int -> Bloco
criarBloco cor x y = Bloco (Posicao x y) (Tamanho 60 altura) (color cor $ rectangleSolid 60 altura) False


-- Função que gera o estado inicial
estadoInicial :: GameState
estadoInicial = GameState
  {
    bola   = Bola   (Posicao 0 30) (Velocidade 100 (-400)) (color white $ circleSolid raio),
    paddle = Paddle (Posicao 0 (-(limiteY / 1.2))) (Tamanho largura 10) (color blue $ rectangleSolid largura 10),
    blocos = concat [ replicarBloco 8 (criarBloco yellow (-250) 70 5  ) (-70),
                      replicarBloco 8 (criarBloco green  (-250) 100 10) (-70),
                      replicarBloco 8 (criarBloco orange (-250) 130 15) (-70),
                      replicarBloco 8 (criarBloco red    (-250) 160 20) (-70) ],
    estadoSetas = EstadoSetas False False,
    vidas = 3,
    pontuacao = 0,
    acabouJogo = False,
    start = False,
    colidiuInferior = False
  }



main :: IO ()
main = do
  play
    janela
    black
    60
    estadoInicial
    (evalState desenhaMundo)
    (execState . trataInput)
    (execState . atualizaMundo)
  where
    janela = InWindow "Breakout" (2 * limiteX, 2 * limiteY) (50, 100)
