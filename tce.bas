10   REM   Autor deste programa: Tarso B. Ledur Kist
15   REM   Porto Alegre, 8/dez/2018.
20   REM   Para interromper digitar "Fim".
25   REM   Eletroforese em Toroide com L=80 cm e integração em intervalos de tempo dt.

30   CLEAR : CLS

35   REM   Definição das Variáveis.
40   REM   x=posição, v=velocidade, s2=variance, D=coef. de difusao, c=concentracao do analito
41   REM   na=num de analitos,
45   DEFSTR A: DEFINT I-N: DIM x(100): DIM v(100): DIM c(100): DIM s2(100): DIM D(100)
50   dt = .01: t = 0: na = 10

80   REM  Definição do arquivo de saida dos resultados intermediarios e finais
85   PRINT "Nome do arquivo para salvar os resultados="; : INPUT Arq$
90   IF Arq$ = "End" THEN GOTO FinishProgram
95   IF Arq$ = "end" THEN GOTO FinishProgram
96   OPEN "O", #1, Arq$

100  REM  Posição inicial e variancça inicial dos dez analitos (injecao da amostra)
105  FOR i = 1 TO na
110  x(i) = 0
115  s2(i) = 1
120  NEXT i

130  REM  Velocidades, cof de difusao e concentracao de cada um dos dez analitos
135  REM  As velocidades devem estar em ordem decrescente de v(1) a v(10)
140  v(1) = 1.1: v(2) = 1.095: v(3) = 1.091: v(4) = 1.083: v(5) = 1.081: v(6) = 1.08: v(7) = 1.079: v(8) = 1.077: v(9) = 1.07: v(10) = 1.065
145  D(1) = .02: D(2) = .02: D(3) = .03: D(4) = .03: D(5) = .022: D(6) = .02: D(7) = .022: D(8) = .028: D(9) = .025: D(10) = .02
150  c(1) = 10: c(2) = 5: c(3) = 5: c(4) = 4: c(5) = 1: c(6) = 5: c(7) = 5: c(8) = 8: c(9) = 8: c(10) = 7
155  tau = 80 / v(1)
160  t = 0

169  LABEL VariableInput
170  REM  Sobre estas variaveis o agente (Machine Learning Agent) deve atuar:
175  PRINT "Dar mais uma volta (End=nao)"; " "; : INPUT a$
176  IF a$ = "End" THEN GOTO FinishProgram
177  IF a$ = "end" THEN GOTO FinishProgram
180  PRINT "Instante de tempo - tau/m1 - para ligar zona termica pela 1a vez="; : INPUT m1: t1 = tau / m1
185  PRINT "Instante de tempo - tau/m2 - para desligar zona termica="; : INPUT m2: t2 = tau / m2
190  PRINT "Instante de tempo - tau/m3 - para ligar zona termica pela 2a vez="; : INPUT m3: t1 = tau / m3
195  PRINT "Instante de tempo - tau/m4 - para desligar zona termica="; : INPUT m4: t2 = tau / m4
196  REM  Obs: m1>m2>m3>m4>1
197  REM  As variaveis que podem ser uteis para o agente "aprende": x(i) e v(i). As váriáveis D(i) e C(i) também podem ser uteis.


200  REM  Agora eles vao correr (eletromigrar) ate que o "front running peak" complete uma volta.
205  tt = 0
210
210  REM
215  t = t + dt: tt = tt + dt: z = 0

255  FOR i = 1 TO na
258  y = x(1)
260  x(i) = x(i) + v(i) * dt: s2(i) = s2(i) + 2 * D(i) * dt
265  IF x(i) > 35 OR x(i) < 25 THEN GOTO LoopVerification
280  IF tt > t1 AND tt < t2 THEN GOTO BandCompression
290  IF tt > t3 AND tt < t4 THEN GOTO BandCompression
300  GOTO LoopVerification

320  LABEL BandCompression
320  REM A compressão termica da banda "i" por um fator de 2 ocorre quando esta condicao e´ satisfeita
330  IF y < 25 AND x(i) > 25 THEN s2(i) = s2(i) / 2

332  REM  Se a banda estiver dentro da seccao com T diferente então o deslocamento será menor (sera' a metade)
335  x(i) = x(i) - (v(i) * dt / 2)
340  REM  A difusão também será afetada: menor (resfriameno) ou maior (aquecimento). Normalmente ocorre aquecimento na zona termica. Então:
345  s2(i) = s2(i) + (2 * D(i) * dt / 10)


470  LABEL LoopVerification
470  REM  Verificando se já completou uma volta
475  IF x(1) > 80 THEN z = 1
480  IF x(i) > 80 THEN x(i) = x(i) - 80
490  NEXT i
495  IF z = 1 GOTO PerfomanceMeter

500  GOTO VariableRun

600  LABEL PerfomanceMeter
600  REM  Performance meters: R1 deve ser maior do que um (1) e R2 deve ser menor que cinco (5). Na situação ideal R1 > 1.5 e R2 < 2.5 !!
601  REM  Se o Agente conseguir operar a máquina e chegar a este resultado então será algo revolucionário para muitas áreas !!!
602  REM  Na prática significa que os picos estarão lado-a-lado, bem resolvidos (R1 > 1) mas não espaçados demais (R2 < 2.5), assim aumenta a capacidade de picos !!!!
603  REM  Ou seja, dará para diagnosticar e prognosticar, por exemplo, centenas de doenças (de forma pressintomática) numa única amostra e numa única corrida !!!!!
605  R1 = 0: R2 = 0: x(1) = x(1) + 80
610  PRINT x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10)
612  PRINT s2(1), s2(2), s2(3), s2(4), s2(5), s2(6), s2(7), s2(8), s2(9), s2(10)
613  INPUT aa$
614  FOR i = 1 TO (na - 1)
615  R1 = R1 + (2 * (SQR(s2(i)) + SQR(s2(i + 1))) / (x(i) - x(i + 1) + .001))
617  R2 = R2 + ((x(i) - x(i + 1)) / (2 * SQR(x(i)) + 2 * SQR(x(i + 1)))) ^ 4
620  NEXT i
625  R1 = 9 / R1
630  R2 = (R2 / 9) ^ (1 / 4)
635  x(1) = x(1) - 80

700  REM  Salvando os dados apos uma volta completa

810  PRINT #1, t, R1, R2
811  REM   "t = "; t; "Previous R1 = "; PrevR1; "Current R1 = "; R1; "Previous R2 = "; PrevR2; "Current R2 = "; R2
815  REM  Agora salvando os dados para poder visualizar o eletroferograma
820  FOR i = 1 TO na
830  PRINT #1, x(i); s2(i); c(i);
840  NEXT i
850  PRINT #1, "  "
860  PrevR1 = R1: PrevR2 = R2

890  GOTO VariableInput

900  REM

950  CLOSE #1

998  LABEL FinishProgram
999  END
