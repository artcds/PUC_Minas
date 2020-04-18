# --------------------------------- 1 -------------------------------
# Uma pesquisa envolvendo 1000 pacientes selecionados aleatoriamente observou que 823 deles, 
# durante um período de 10 anos, faleceram por causa de câncer de pulmão. 
# Um intervalo de confiança retornou os valores de 0,799344 a 0,846656. 
# O nível de confiança utilizado pelo pesquisador foi de:

tamanho_amostra <- 1000
proporcao_amostral <- 823/1000
z_95 <- 1.96
erro_amostral <- z_95 * sqrt((proporcao_amostral * (1 - proporcao_amostral))/
                               tamanho_amostra)
# Intervalo de confiança
c(proporcao_amostral - erro_amostral, proporcao_amostral + erro_amostral)
# 0.7993439 0.8466561

# Testando com diferentes opções de Z, o de 95% deu o intervalo de confiança da questão.
# Logo, a resposta é 95%.


# --------------------------------- 2 -------------------------------
# Para a comparação de dois algoritmos de otimização, foi realizado um experimento com seis ensaios. 
# Em cada ensaio, foram usados separadamente os dois algoritmos em estudo, 
# mas sob as mesmas condições (dados pareados). Os tempos de resposta ao usuário foram:
#  Ensaio         1    2      3     4     5      6
#  Algoritmo 1   8.1   8.9   9.3   9.6   8.1   11.2
#  Algoritmo 2   9.2   9.8   9.9   10.3  8.9   13.1
# Os tempos de resposta dos dois algoritmos são, em média, diferentes? Utilize um nível de significância de 5%.

a1 = c(8.1, 8.9, 9.3, 9.6, 8.1, 11.2)
a2 = c(9.2, 9.8, 9.9, 10.3, 8.9, 13.1)
# hipótese nula: médias iguais. Teste de hipótese bilateral
teste_h = t.test(a1, a2, alternative = "t", conf.level = 0.05, paired=TRUE)
teste_h$p.value
# 0.003538609
teste_h$p.value <= 0.05
# TRUE, valor p menor que 0.05, hipótese nula deve ser rejeitada, logo, são diferentes
mean(a1) < mean(a2)
# TRUE, Tempo medio do algoritmo 1 é menor do que do algoritmo 2


# --------------------------------- 3 -------------------------------
# Na comparação de duas topologias de rede de computadores, C1 e C2, 
# avaliou-se o tempo de transmissão de pacotes de dados entre duas máquinas. 
# Foram realizados 22 ensaios em C1 indicando uma média de 10,625 décimos de segundo 
# e uma variância de 6.371 (décimos de segundo). 
# Foram realizados 14 ensaios em C2 apontando uma média de 13,458 décimos de segundo 
# e uma variância de 4,781 (décimos de segundo). Existe diferença significativa entre 
# o tempo médio de transmissão nas duas topologias? Utilize um nível de 5% de significância.

#hipótese nula: tempos médios iguais
teste_h = tsum.test(mean.x = 10.625, mean.y = 13.458, s.x = sqrt(6.371), s.y =
                      sqrt(4.781), n.x = 22, n.y = 14)
teste_h$p.value
#0.001210049
teste_h$p.value <= 0.05
#TRUE, então a hipótese nula deve ser rejeitada, logo, os tempos médios são diferentes


# --------------------------------- 4 -------------------------------
# Um sensor tem vida média de 2000 dias com desvio-padrão de 80 dias. O sensor tem distribuição aproximadamente normal.
# A partir dessa informação são feitas as afirmações:
# I. O número máximo de dias necessários para que se tenha que repor no máximo 5% dos produtos é 1750,412 dias.
# II. O número máximo de dias necessários para que se tenha que repor no máximo 95% dos produtos é 2131,588 dias.
# III. A probabilidade de este sensor durar entre 2100 e 2200 dias é 0,09944.
# Está(ão) correta(s) a(s) afirmação(ões):

qnorm(mean = 2000, sd = 80, p = 0.05)
# 1868.412
qnorm(mean = 2000, sd = 80, p = 0.95)
# 2131.588
pnorm(mean = 2000, sd = 80, q = 2200) - pnorm(mean = 2000, sd = 80, q = 2100)
# 0.09944011
# Resposta: II e III estão corretas


# --------------------------------- 5 -------------------------------
# A Média é uma das medidas de tendência central, e seus valores se posicionam dentro de um conjunto numérico e 
# visam fornecer ao pesquisador informações representativas do núcleo das observações 
# de um fenômeno relativo a qualquer campo da atividade humana. E, de modo geral, 
# o Desvio Padrão representa a mais clássica medida de dispersão da Estatística.
# Foi coletada uma amostra das notas de 10 alunos da disciplina de Análise e Interpretação de Dados 
# dos cursos de tecnólogos da Faculdade Ensina para Vida Real.
#    21   25   23   20   18   21   18   19   22   24
# Com base nessa amostra, calcule o valor da nota média e o desvio padrão da nota dessa disciplina:

dados = c(21, 25, 23, 20, 18, 21, 18, 19, 22, 24)
mean(dados)
# 21.1
sd(dados)
# 2.424413


# --------------------------------- 6 -------------------------------
# Suponha que o tempo de resposta na execução de um algoritmo é uma variável aleatória 
# com distribuição Normal de média 23 segundos e desvio padrão de 4 segundos. 
# A probabilidade do tempo de resposta ser menor do que 26 segundos é:

pnorm(mean = 23, sd = 4, q = 26, lower.tail = T)
# 0.7733726


# --------------------------------- 7 -------------------------------
# Antes mesmo de entrar na parte da estatística descritiva, após a coleta dos dados, 
# é importante saber qual o tipo de dados que irá trabalhar. 
# Em uma pesquisa, foram coletadas as seguintes variáveis de um total de 200 pessoas.
# - Idade
# - Renda
# - Estado Civil
# - Escolaridade
# - Número de Aparelhos de TV na Residência
# - Grau de Satisfação sobre a TV a Cabo (1 - Péssimo a 5 - Ótimo)
# A classificação correta dessas seis variáveis são:

# Resposta: Quantitativa, quantitativa, qualitativa, qualitativa, quantitativa e qualitativa. 

#--------------------------------- 8 -------------------------------
# Uma amostra aleatória de 50 capacetes de corredores de motos e de automóveis foi sujeita
# a um teste de impacto, sendo observado algum dano em 10 desses capacetes. 
# Se o fabricante garante que até 10% de seus capacetes suportam os danos em um acidente 
# (similar ao teste realizado), você diria que esses capacetes estão aprovados para a venda ao consumidor final?
# O valor p que sinaliza sobre a rejeição ou não da hipótese nula é igual a:

teste_hipotese = prop.test(x = 10, n = 50, p = 0.10, alternative = 'greater', correct = F)
teste_hipotese$p.value
# 0.009211063

#--------------------------------- 9 -------------------------------
# Sabe-se que a vida, em horas, de uma bateria é aproximadamente normalmente distribuída, 
# com desvio-padrão ?? = 1,87 hora. Uma amostra aleatória de 17 baterias tem uma vida média de 36,3 horas. 
# O valor p que avalie que a vida da bateria exceda 35 horas é igual a:

pnorm(mean = 36.3, sd = 1.87, q = 35, lower.tail = T)
# 0.243469


#--------------------------------- 10 ------------------------------
# O tempo de vida (em horas) de um transistor é uma variável aleatória T com distribuição exponencial. 
# O tempo médio de vida do transistor é de 500 horas. 
# A probabilidade do transistor durar entre 300 e 1000 horas é de:

pexp(rate = 1/500, q = 1000) - pexp(rate = 1/500, q = 300)
# 0.4134764
