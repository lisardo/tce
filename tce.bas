REM   Autor deste programa: Tarso B. Ledur Kist
REM   Porto Alegre, 8/dez/2018.
REM   Para interromper digitar "Fim".
REM   Eletroforese em Toroide com L=80 cm e integracao em intervalos de tempo dt.
CLEAR : CLS
REM   Definicao das Variaveis.
REM   x=posicao, v=velocidade, s2=variance, D=coef. de difusao, c=concentracao do analito
REM   na=num de analitos,
DEFSTR A: DEFINT I-N: DIM x(100): DIM v(100): DIM c(100): DIM s2(100): DIM D(100)
dt = .01: t = 0: na = 10
REM  Definicao do arquivo de saida dos resultados intermediarios e finais
PRINT "Nome do arquivo para salvar os resultados="; : INPUT Arq$
IF Arq$ = "End" THEN GOTO FinishProgram
IF Arq$ = "end" THEN GOTO FinishProgram
OPEN "O", #1, Arq$
REM  Posicao inicial e variancca inicial dos dez analitos (injecao da amostra)
FOR i = 1 TO na
x(i) = 0
s2(i) = 1
NEX
REM  Velocidades, cof de difusao e concentracao de cada um dos dez analitos
REM  As velocidades devem estar em ordem decrescente de v(1) a v(10)
v(1) = 1.1: v(2) = 1.095: v(3) = 1.091: v(4) = 1.083: v(5) = 1.081: v(6) = 1.08: v(7) = 1.079: v(8) = 1.077: v(9) = 1.07: v(10) = 1.065
D(1) = .02: D(2) = .02: D(3) = .03: D(4) = .03: D(5) = .022: D(6) = .02: D(7) = .022: D(8) = .028: D(9) = .025: D(10) = .02
c(1) = 10: c(2) = 5: c(3) = 5: c(4) = 4: c(5) = 1: c(6) = 5: c(7) = 5: c(8) = 8: c(9) = 8: c(10) = 7
tau = 80 / v(1)
t = 0
LABEL VariableInput
REM  Sobre estas variaveis o agente (Machine Learning Agent) deve atuar:
PRINT "Dar mais uma volta (End=nao)"; " "; : INPUT a$
IF a$ = "End" THEN GOTO FinishProgram
IF a$ = "end" THEN GOTO FinishProgram
PRINT "Instante de tempo - tau/m1 - para ligar zona termica pela 1a vez="; : INPUT m1: t1 = tau / m1
PRINT "Instante de tempo - tau/m2 - para desligar zona termica="; : INPUT m2: t2 = tau / m2
PRINT "Instante de tempo - tau/m3 - para ligar zona termica pela 2a vez="; : INPUT m3: t1 = tau / m3
PRINT "Instante de tempo - tau/m4 - para desligar zona termica="; : INPUT m4: t2 = tau / m4
REM  Obs: m1>m2>m3>m4>1
REM  As variaveis que podem ser uteis para o agente "aprende": x(i) e v(i). As variaveis D(i) e C(i) tambem podem ser uteis.
REM  Agora eles vao correr (eletromigrar) ate que o "front running peak" complete uma volta.
tt = 0
210
REM
t = t + dt: tt = tt + dt: z = 0
FOR i = 1 TO na
y = x(1)
x(i) = x(i) + v(i) * dt: s2(i) = s2(i) + 2 * D(i) * dt
IF x(i) > 35 OR x(i) < 25 THEN GOTO LoopVerification
IF tt > t1 AND tt < t2 THEN GOTO BandCompression
IF tt > t3 AND tt < t4 THEN GOTO BandCompression
GOTO LoopVerification
LABEL BandCompression
REM A compressao termica da banda "i" por um fator de 2 ocorre quando esta condicao e´ satisfeita
IF y < 25 AND x(i) > 25 THEN s2(i) = s2(i) / 2
REM  Se a banda estiver dentro da seccao com T diferente entao o deslocamento sera menor (sera' a metade)
x(i) = x(i) - (v(i) * dt / 2)
REM  A difusao tambem sera afetada: menor (resfriameno) ou maior (aquecimento). Normalmente ocorre aquecimento na zona termica. Entao:
s2(i) = s2(i) + (2 * D(i) * dt / 10)
LABEL LoopVerification
REM  Verificando se ja completou uma volta
IF x(1) > 80 THEN z = 1
IF x(i) > 80 THEN x(i) = x(i) - 80
NEXT i
IF z = 1 GOTO PerfomanceMeter
GOTO VariableRu
LABEL PerfomanceMeter
REM  Performance meters: R1 deve ser maior do que um (1) e R2 deve ser menor que cinco (5). Na situacao ideal R1 > 1.5 e R2 < 2.5 !!
REM  Se o Agente conseguir operar a maquina e chegar a este resultado entao sera algo revolucionario para muitas areas !!!
REM  Na pratica significa que os picos estarao lado-a-lado, bem resolvidos (R1 > 1) mas nao espacados demais (R2 < 2.5), assim aumenta a capacidade de picos !!!!
REM  Ou seja, dara para diagnosticar e prognosticar, por exemplo, centenas de doencas (de forma pressintomatica) numa única amostra e numa única corrida !!!!!
R1 = 0: R2 = 0: x(1) = x(1) + 80
PRINT x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10)
PRINT s2(1), s2(2), s2(3), s2(4), s2(5), s2(6), s2(7), s2(8), s2(9), s2(10)
INPUT aa$
FOR i = 1 TO (na - 1)
R1 = R1 + (2 * (SQR(s2(i)) + SQR(s2(i + 1))) / (x(i) - x(i + 1) + .001))
R2 = R2 + ((x(i) - x(i + 1)) / (2 * SQR(x(i)) + 2 * SQR(x(i + 1)))) ^ 4
NEXT i
R1 = 9 / R1
R2 = (R2 / 9) ^ (1 / 4)
x(1) = x(1) - 80
REM  Salvando os dados apos uma volta completa
PRINT #1, t, R1, R2
REM   "t = "; t; "Previous R1 = "; PrevR1; "Current R1 = "; R1; "Previous R2 = "; PrevR2; "Current R2 = "; R2
REM  Agora salvando os dados para poder visualizar o eletroferograma
FOR i = 1 TO na
PRINT #1, x(i); s2(i); c(i);
NEXT i
PRINT #1, "  "
PrevR1 = R1: PrevR2 = R2
GOTO VariableInput
REM
CLOSE #1
LABEL FinishProgram
END
