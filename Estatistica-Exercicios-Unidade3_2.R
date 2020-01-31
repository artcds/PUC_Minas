library(BSDA)

#-------------------- 1 --------------------
#Sabe-se que a vida em horas de um bulbo de uma lâmpada de 75 W é distribuída normalmente com σ = 25 horas. 
#Uma amostra aleatória de 20 bulbos tem uma vida média de x  = 1014 horas. 
#O intervalo de confiança de 95% para a vida média é igual a:

teste_h = zsum.test(mean.x = 1014, sigma.x = 25, n.x = 20, conf.level = 0.95)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#1003.043 1024.957


#-------------------- 2 --------------------
#Um engenheiro do setor de pesquisa de um fabricante de pneu está investigando a vida do pneu em relação a um novo componente da borracha. 
#Ele fabricou 16 pneus e testou-os até o final da vida em teste na estrada. 
#A média e o desvio-padrão da amostra são, respectivamente, 60.139,7 e 3.645,94 km. 
#O intervalo de confiança de 95% para a vida média do pneu é igual a:

teste_h = tsum.test(mean.x = 60139.7, s.x = 3645.94, n.x = 16, conf.level = 0.95)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#58196.92 62082.48


#-------------------- 3 --------------------
#Um artigo no Journal of Composite Materials (Dezembro de 1989, Vol. 23, p. 1200) 
#descreve o efeito da delaminação na frequência natural de vigas feitas de laminados compósitos. 
#Cinco dessas vigas delaminadas foram submetidas a cargas e as frequências (em Hz) 
#resultantes foram obtidas pelos seguintes valores:
#  230,33       233,05      232,58      229,48      232,58
#Supondo que a variável em estudo segue a distribuição Normal, 
#o intervalo de 99% de confiança sobre a frequência média é igual a:

amostra <- c(230.33, 233.05, 232.58, 229.48, 232.58)
teste_h = t.test(x = amostra, alternative = "two.sided", conf.level = 0.99)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#228.3272 234.8808


#-------------------- 4 --------------------
#Um estudo realizado para avaliar a fração de circuitos integrados defeituosos em um processo de fotolitografia, 
#obteve como resultado um total de 13 defeitos em 300 circuitos investigados. 
#Um intervalo de 90% de confiança para proporção de circuitos defeituosos é igual a:

tamanho_amostra <- 300
proporcao_amostral <- 13/tamanho_amostra
z_90 <- 1.645

erro_amostral <- z_90 * sqrt((proporcao_amostral * (1 - proporcao_amostral))/tamanho_amostra)

#Intervalo de confiança
c(proporcao_amostral - erro_amostral, proporcao_amostral + erro_amostral)
#0.02399600 0.06267067

#-------------------- 5 --------------------
#Sabe-se que a vida em horas de um bulbo de uma lâmpada de 75 W é distribuída normalmente com σ = 25 horas. 
#Uma amostra aleatória de 20 bulbos tem uma vida média de x = 1014 horas. 
#O intervalo de confiança de 95% para a vida média é igual a:

teste_h = zsum.test(mean.x = 1014, sigma.x = 25, n.x = 20)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#1003.043 1024.957


#-------------------- 6 --------------------
#Um engenheiro do setor de pesquisa de um fabricante de pneu está investigando a vida do pneu em relação 
#a um novo componente da borracha. Ele fabricou 16 pneus e testou-os até o final da vida em teste na estrada. 
#A média e o desvio-padrão da amostra são, respectivamente, 60.139,7 e 3.645,94 km. 
#O intervalo de confiança de 95% para a vida média do pneu é igual a:

teste_h = tsum.test(mean.x = 60139.7, s.x = 3645.94, n.x = 16, conf.level = 0.95)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#58196.92 62082.48


#-------------------- 7 --------------------
#Um artigo no Journal of Composite Materials (Dezembro de 1989, Vol. 23, p. 1200) descreve
#o efeito da delaminação na frequência natural de vigas feitas de laminados compósitos. 
#Cinco dessas vigas delaminadas foram submetidas a cargas e as frequências (em Hz) 
#resultantes foram obtidas pelos seguintes valores:
#  230,33       233,05      232,58      229,48      232,58
#Supondo que a variável em estudo segue a distribuição Normal, 
#o intervalo de 99% de confiança sobre a frequência média é igual a:

amostra <- c(230.33, 233.05, 232.58, 229.48, 232.58)
teste_h = t.test(x = amostra, conf.level = 0.99)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#228.3272 234.8808


#-------------------- 8 --------------------
#Suponha que o número de eleitores que chegam a uma seção de uma Zona Eleitoral no dia de uma determinada eleição, 
#siga a uma distribuição de Poisson com uma média de chegada de 30 eleitores por meia hora. 
#A probabilidade de que cheguem menos de 3 eleitores em 5 minutos é:

ppois(lambda = 5, q = 2)
#0.124652


#-------------------- 9 --------------------
#Um fabricante de certas peças de automóveis garante que uma caixa de suas peças conterá no máximo 2 itens defeituosos. 
#Se a caixa contem 20 peças e a experiência tem demonstrado que esse processo de fabricação produz 2% de itens defeituosos, 
#qual a probabilidade de que uma caixa de suas peças não vá satisfazer a garantia?

pbinom(prob = 0.02, q = 2, size = 20, lower.tail = F)
#0.007068693


#-------------------- 10 --------------------
#O salário dos Cientistas de Dados recém formados em Belo Horizonte distribuem-se normalmente com média de R$ 8.000,00
#e desvio padrão de R$ 500,00. Qual a percentagem de cientistas de dados que recebem menos de R$ 6.470,00?

pnorm(mean = 8000, sd = 500, q = 6470)
#0.001106685