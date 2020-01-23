#-------------------- 1 --------------------
#Sabe-se que a vida em horas de um bulbo de uma lâmpada de 75 W é distribuída normalmente com σ = 25 horas. 
#Uma amostra aleatória de 20 bulbos tem uma vida média de x  = 1014 horas. 
#O intervalo de confiança de 95% para a vida média é igual a:

dp_populacional = 25
tamanho_amostra = 20
media_amostral = 1014
nivel_confianca = 0.95
z_95 = 1.96 #Z de 0.475 (0.95/2)

erro_amostral = z_95 * (dp_populacional / sqrt(tamanho_amostra))

#Intervalo de confiança
c(media_amostral-erro_amostral, media_amostral+erro_amostral)
#1003.043 1024.957

#-------------------- 2 --------------------
#Um engenheiro do setor de pesquisa de um fabricante de pneu está investigando a vida do pneu em relação a um novo componente da borracha. 
#Ele fabricou 16 pneus e testou-os até o final da vida em teste na estrada. 
#A média e o desvio-padrão da amostra são, respectivamente, 60.139,7 e 3.645,94 km. 
#O intervalo de confiança de 95% para a vida média do pneu é igual a:

tamanho_amostra = 16
media_amostral = 60139.7
dp_amostral = 3645.94
nivel_confianca = 0.95
t_95 = 2.131 #grau de liberdade 15, nivel de confianca 95

erro_amostral = t_95 * (dp_amostral / sqrt(tamanho_amostra))

#Intervalo de confiança
c(media_amostral-erro_amostral, media_amostral+erro_amostral)
#58197.33 62082.07

#-------------------- 3 --------------------
#Um artigo no Journal of Composite Materials (Dezembro de 1989, Vol. 23, p. 1200) 
#descreve o efeito da delaminação na frequência natural de vigas feitas de laminados compósitos. 
#Cinco dessas vigas delaminadas foram submetidas a cargas e as frequências (em Hz) 
#resultantes foram obtidas pelos seguintes valores:
#  230,33       233,05      232,58      229,48      232,58
#Supondo que a variável em estudo segue a distribuição Normal, 
#o intervalo de 99% de confiança sobre a frequência média é igual a:

amostra <- c(230.33, 233.05, 232.58, 229.48, 232.58)
t_99 = 4.604 #grau de liberdade 4, nivel de confianca 99
media_amostral = mean(amostra)

erro_amostral = t_99 * (sd(amostra) / sqrt(length(amostra)))

#Intervalo de confiança
c(media_amostral-erro_amostral, media_amostral+erro_amostral)
#228.3273 234.8807
