# --------------------------------- 1 -------------------------------
# Uma pesquisa envolvendo 1000 pacientes selecionados aleatoriamente observou que 823 deles, 
# durante um per�odo de 10 anos, faleceram por causa de c�ncer de pulm�o. 
# Um intervalo de confian�a retornou os valores de 0,799344 a 0,846656. 
# O n�vel de confian�a utilizado pelo pesquisador foi de:

tamanho_amostra <- 1000
proporcao_amostral <- 823/1000
z_95 <- 1.96
erro_amostral <- z_95 * sqrt((proporcao_amostral * (1 - proporcao_amostral))/
                               tamanho_amostra)
# Intervalo de confian�a
c(proporcao_amostral - erro_amostral, proporcao_amostral + erro_amostral)
# 0.7993439 0.8466561

# Testando com diferentes op��es de Z, o de 95% deu o intervalo de confian�a da quest�o.
# Logo, a resposta � 95%.


# --------------------------------- 2 -------------------------------
# Para a compara��o de dois algoritmos de otimiza��o, foi realizado um experimento com seis ensaios. 
# Em cada ensaio, foram usados separadamente os dois algoritmos em estudo, 
# mas sob as mesmas condi��es (dados pareados). Os tempos de resposta ao usu�rio foram:
#  Ensaio         1    2      3     4     5      6
#  Algoritmo 1   8.1   8.9   9.3   9.6   8.1   11.2
#  Algoritmo 2   9.2   9.8   9.9   10.3  8.9   13.1
# Os tempos de resposta dos dois algoritmos s�o, em m�dia, diferentes? Utilize um n�vel de signific�ncia de 5%.

a1 = c(8.1, 8.9, 9.3, 9.6, 8.1, 11.2)
a2 = c(9.2, 9.8, 9.9, 10.3, 8.9, 13.1)
# hip�tese nula: m�dias iguais. Teste de hip�tese bilateral
teste_h = t.test(a1, a2, alternative = "t", conf.level = 0.05, paired=TRUE)
teste_h$p.value
# 0.003538609
teste_h$p.value <= 0.05
# TRUE, valor p menor que 0.05, hip�tese nula deve ser rejeitada, logo, s�o diferentes
mean(a1) < mean(a2)
# TRUE, Tempo medio do algoritmo 1 � menor do que do algoritmo 2


# --------------------------------- 3 -------------------------------
# Na compara��o de duas topologias de rede de computadores, C1 e C2, 
# avaliou-se o tempo de transmiss�o de pacotes de dados entre duas m�quinas. 
# Foram realizados 22 ensaios em C1 indicando uma m�dia de 10,625 d�cimos de segundo 
# e uma vari�ncia de 6.371 (d�cimos de segundo). 
# Foram realizados 14 ensaios em C2 apontando uma m�dia de 13,458 d�cimos de segundo 
# e uma vari�ncia de 4,781 (d�cimos de segundo). Existe diferen�a significativa entre 
# o tempo m�dio de transmiss�o nas duas topologias? Utilize um n�vel de 5% de signific�ncia.

#hip�tese nula: tempos m�dios iguais
teste_h = tsum.test(mean.x = 10.625, mean.y = 13.458, s.x = sqrt(6.371), s.y =
                      sqrt(4.781), n.x = 22, n.y = 14)
teste_h$p.value
#0.001210049
teste_h$p.value <= 0.05
#TRUE, ent�o a hip�tese nula deve ser rejeitada, logo, os tempos m�dios s�o diferentes


# --------------------------------- 4 -------------------------------
# Um sensor tem vida m�dia de 2000 dias com desvio-padr�o de 80 dias. O sensor tem distribui��o aproximadamente normal.
# A partir dessa informa��o s�o feitas as afirma��es:
# I. O n�mero m�ximo de dias necess�rios para que se tenha que repor no m�ximo 5% dos produtos � 1750,412 dias.
# II. O n�mero m�ximo de dias necess�rios para que se tenha que repor no m�ximo 95% dos produtos � 2131,588 dias.
# III. A probabilidade de este sensor durar entre 2100 e 2200 dias � 0,09944.
# Est�(�o) correta(s) a(s) afirma��o(�es):

qnorm(mean = 2000, sd = 80, p = 0.05)
# 1868.412
qnorm(mean = 2000, sd = 80, p = 0.95)
# 2131.588
pnorm(mean = 2000, sd = 80, q = 2200) - pnorm(mean = 2000, sd = 80, q = 2100)
# 0.09944011
# Resposta: II e III est�o corretas


# --------------------------------- 5 -------------------------------
# A M�dia � uma das medidas de tend�ncia central, e seus valores se posicionam dentro de um conjunto num�rico e 
# visam fornecer ao pesquisador informa��es representativas do n�cleo das observa��es 
# de um fen�meno relativo a qualquer campo da atividade humana. E, de modo geral, 
# o Desvio Padr�o representa a mais cl�ssica medida de dispers�o da Estat�stica.
# Foi coletada uma amostra das notas de 10 alunos da disciplina de An�lise e Interpreta��o de Dados 
# dos cursos de tecn�logos da Faculdade Ensina para Vida Real.
#    21   25   23   20   18   21   18   19   22   24
# Com base nessa amostra, calcule o valor da nota m�dia e o desvio padr�o da nota dessa disciplina:

dados = c(21, 25, 23, 20, 18, 21, 18, 19, 22, 24)
mean(dados)
# 21.1
sd(dados)
# 2.424413


# --------------------------------- 6 -------------------------------
# Suponha que o tempo de resposta na execu��o de um algoritmo � uma vari�vel aleat�ria 
# com distribui��o Normal de m�dia 23 segundos e desvio padr�o de 4 segundos. 
# A probabilidade do tempo de resposta ser menor do que 26 segundos �:

pnorm(mean = 23, sd = 4, q = 26, lower.tail = T)
# 0.7733726


# --------------------------------- 7 -------------------------------
# Antes mesmo de entrar na parte da estat�stica descritiva, ap�s a coleta dos dados, 
# � importante saber qual o tipo de dados que ir� trabalhar. 
# Em uma pesquisa, foram coletadas as seguintes vari�veis de um total de 200 pessoas.
# - Idade
# - Renda
# - Estado Civil
# - Escolaridade
# - N�mero de Aparelhos de TV na Resid�ncia
# - Grau de Satisfa��o sobre a TV a Cabo (1 - P�ssimo a 5 - �timo)
# A classifica��o correta dessas seis vari�veis s�o:

# Resposta: Quantitativa, quantitativa, qualitativa, qualitativa, quantitativa e qualitativa. 

#--------------------------------- 8 -------------------------------
# Uma amostra aleat�ria de 50 capacetes de corredores de motos e de autom�veis foi sujeita
# a um teste de impacto, sendo observado algum dano em 10 desses capacetes. 
# Se o fabricante garante que at� 10% de seus capacetes suportam os danos em um acidente 
# (similar ao teste realizado), voc� diria que esses capacetes est�o aprovados para a venda ao consumidor final?
# O valor p que sinaliza sobre a rejei��o ou n�o da hip�tese nula � igual a:

teste_hipotese = prop.test(x = 10, n = 50, p = 0.10, alternative = 'greater', correct = F)
teste_hipotese$p.value
# 0.009211063

#--------------------------------- 9 -------------------------------
# Sabe-se que a vida, em horas, de uma bateria � aproximadamente normalmente distribu�da, 
# com desvio-padr�o ?? = 1,87 hora. Uma amostra aleat�ria de 17 baterias tem uma vida m�dia de 36,3 horas. 
# O valor p que avalie que a vida da bateria exceda 35 horas � igual a:

pnorm(mean = 36.3, sd = 1.87, q = 35, lower.tail = T)
# 0.243469


#--------------------------------- 10 ------------------------------
# O tempo de vida (em horas) de um transistor � uma vari�vel aleat�ria T com distribui��o exponencial. 
# O tempo m�dio de vida do transistor � de 500 horas. 
# A probabilidade do transistor durar entre 300 e 1000 horas � de:

pexp(rate = 1/500, q = 1000) - pexp(rate = 1/500, q = 300)
# 0.4134764
