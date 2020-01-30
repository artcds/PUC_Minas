#---------------- 1 -----------------
#(Magalhães) A Tabela a seguir apresenta informações de alunos de uma universidade 
#quanto às variáveis: Período, sexo e opinião sobre a reforma agrária. 
#                           Reforma agrária
#Período     Sexo         Contra    A favor   Sem opinião
#Diurno     Feminino        2         8           2
#Diurno     Masculino       8         9           8
#Noturno    Feminino        4         8           2
#Noturno    Masculino      12         10          1
#
#Determine a probabilidade de escolhermos, aleatoriamente, 
#uma pessoa do sexo masculino e sem opinião sobre a reforma agrária?

(8+1)/(2+8+2+8+9+8+4+8+2+12+10+1)
#0.1216216


#---------------- 2 -----------------
#O São Paulo Futebol Clube ganha com probabilidade 0,7 se chove e com 0,8 se não chove. 
#Em setembro a probabilidade de chuva é de 0,3. 
#O São Paulo ganhou uma partida em Setembro, qual a probabilidade de ter chovido nesse dia?

(0.3 * 0.7) / ((0.3 * 0.7) + (0.7 * 0.8))
#0.2727273


#---------------- 3 -----------------
#Uma indústria de tintas recebe pedidos de seus vendedores através de telefone e internet. 
#O número médio de pedidos, que chegam por qualquer meio, é de 5 por hora. 
#Em um dia de trabalho (8 horas), qual seria a probabilidade de haver 50 pedidos? 

dpois(lambda = 40, 50)
#0.01770702


#---------------- 4 -----------------
#(Freund, 2006) A experiência mostra que 30% dos lançamentos de foguete de uma base da NASA 
#foram adiados em virtude do mau tempo. Determine a probabilidade de que, 
#em dez lançamentos de foguete daquela base, de três a cinco sejam adiados em virtude do mau tempo.

pbinom(q = 5, size = 10, prob = 0.3) - pbinom(q = 2, size = 10, prob = 0.3)
#0.5698682


#---------------- 5 -----------------
#O tempo de utilização de um caixa eletrônico por clientes de um certo banco, em minutos,  
#foi modelado por uma variável T com distribuição exponencial com parâmetro igual a 3. 
#Determine a probabilidade de que um cliente demore menos de um minuto utilizando o caixa eletrônico.

pexp(q = 1, rate = 3)
#0.9502129


#---------------- 6 -----------------
#(Stevenson, 2001) A vida  útil de lavadoras de pratos automáticas pode ser modelada 
#pela distribuição normal com uma média de 1,5 ano e com desvio padrão de 0,3 ano. 
#Que percentagem das lavadoras vendidas necessitará de conserto antes de expirar o tempo de garantia de 12 meses? 

pnorm(mean = 1.5, sd = 0.3, q = 1)
#0.04779035


#---------------- 7 -----------------
#(Magalhães, 2002) Uma clínica de emagrecimento recebe pacientes adultos com peso 
#seguindo uma distribuição normal com média 130 kg e desvio padrão 20 kg. 
#Para efeito de determinar o tratamento mais adequado, os 25% pacientes de menor peso 
#são classificados de “magros”, enquanto dos 25% de maior peso de “obesos”. 
#Determine os valores que delimitam cada uma dessas classificações.

c(qnorm(mean = 130, sd = 20, p = 0.25), qnorm(mean = 130, sd = 20, p = 0.75))
#116.5102 143.4898


#---------------- 8 -----------------
#Uma companhia fabrica motores. As especificações requerem que o comprimento de uma 
#certa haste deste motor esteja entre 7,48 cm e 7,52 cm. 
#Os comprimentos destas hastes, fabricadas por um fornecedor, têm uma distribuição normal 
#com média 7,505 cm e desvio padrão 0,01 cm. 
#Qual a probabilidade de uma haste escolhida ao acaso estar dentro das especificações?

pnorm(mean = 7.505, sd = 0.01, q = 7.52) - pnorm(mean = 7.505, sd = 0.01, q = 7.48)
#0.9269831


#---------------- 9 -----------------
#(Adaptado de Freund, 2006) Um estudo mostra que em 60% dos casos de divórcio 
#requeridos num certo município, a incompatibilidade é apontada como causa. 
#Encontre a probabilidade de que entre 14 casos de divórcio requeridos naquele município 
#mais de 12 apontem a incompatibilidade como causa.

pbinom(prob = 0.6, size = 14, q = 12, lower.tail = FALSE)
#0.00809763


#---------------- 10 ----------------
#A distribuição da altura de 500 estudantes do sexo masculino de uma escola é 
#aproximadamente Normal com média igual a 1,70 metro e desvio padrão igual a 2,5 centímetros. 
#Aproximadamente quantos têm altura superior a 1,65m?

pnorm(mean = 170, sd = 2.5, q = 165, lower.tail = FALSE) * 500
#488.6249