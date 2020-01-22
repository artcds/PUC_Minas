#-------------- 1 -------------- 
#O período de estiagem liga um sinal de alerta quanto ao abastecimento de água das grandes cidades. 
#Certa cidade brasileira possui várias adutoras que abastecem de água seus bairros, 
#então verifica-se que a probabilidade de uma dessas adutoras, nessas condições, 
#não ter problemas na distribuição de água seja de 0,2. 
#Dez dessas adutoras são observadas e assim deseja-se estimar a probabilidade de que pelo menos 
#uma dessas adutoras funcione perfeitamente, isto é, 
#não traga problemas para o abastecimento de água da população?

pbinom(q = 0, size = 10, prob = 0.2, lower.tail = F)
#0.8926258

#-------------- 2 -------------- 
#Suponha que o número de eleitores que chegam a uma seção de uma Zona Eleitoral no dia de uma determinada eleição, 
#siga a uma distribuição de Poisson com uma média de chegada de 30 eleitores por meia hora. 
#A probabilidade de que cheguem menos de 3 eleitores em 5 minutos é:

#lambda, por unidade de 5 minutos = 5 eleitores
#q = 2, pois trata da probabilidade de serem menos de 3
ppois(lambda = 5, q = 2)
#0.124652

#-------------- 3 -------------- 
#O tempo de vida de um arranjo mecânico em um teste vibracional é distribuído exponencialmente, 
#com uma média de 400 horas (λ = 1/400). 
#A probabilidade de que um arranjo em teste falhe em menos de 100 horas é:

pexp(q = 100, rate = 1/400)
#0.2211992

#-------------- 4 -------------- 
#Um sensor tem vida média de 1500 dias com desvio padrão de 70 dias que tem distribuição aproximadamente normal. 
#A partir desta informação são feitas as afirmações:
#  I. O número máximo de dias necessários para que se tenha que repor no máximo 5% dos produtos é 1615 dias.
#II. O número máximo de dias necessários para que se tenha que repor no máximo 5% dos produtos é 1385 dias.
#III. A probabilidade de este sensor durar entre 1500 e 1570 dias é 0,3413.
#Está(ão) correta(s) a(s) afirmação(ões):

#I
pnorm(q = 1615, mean = 1500, sd = 70)
#0.9497938

#II
pnorm(q = 1385, mean = 1500, sd = 70)
#0.05020625

#III
pnorm(q = 1570, mean = 1500, sd = 70) - pnorm(q = 1500, mean = 1500, sd = 70)
#0.3413447

#-------------- 5 -------------- 
#Antes de lançar um novo carro no mercado as montadoras fazem testes com alguns protótipos, 
#cujo objetivo é alcançar a melhor performance possível, dentro da sua categoria. 
#Em testes com seu novo carro RBX em uma superfície seca, 
#a distância de frenagem média foi 145 pés e o desvio padrão 6,53 pés. 
#As distâncias de frenagem do carro são normalmente distribuídas. 
#Qual é a maior distância de frenagem em uma superfície seca que um desses RBX poderia ter e ainda estar no 1,5% do topo?

qnorm(p = 0.015, sd = 6.53, mean = 145, lower.tail = F)
#159.1707

#-------------- 6 -------------- 
#Uma companhia de seguros acredita que as pessoas possam ser divididas em duas classes: 
#aquelas que são propícias a sofrerem acidentes e as que não são. 
#Suas estatísticas mostram que uma pessoa propícia a acidentes terá um acidente 
#em algum momento dentro do período de um ano com probabilidade 0,4; 
#enquanto esta probabilidade diminui para 0,2 para pessoas não propícias a acidentes. 
#Supondo que 30% da população é propícia a sofrer acidentes, qual é a probabilidade de que 
#um novo segurado sofra um acidente durante um ano em que comprou uma apólice?

(0.3 * 0.4) + (0.7 * 0.2)
#0.26

#-------------- 7 -------------- 
#A probabilidade de um indivíduo da classe A comprar um carro é de 3/4, da B é de 1/5 e da C é de 1/20. 
#As probabilidades de os indivíduos comprarem um carro da marca X são 1/10, 3/5, e 3/10, 
#dado que sejam de A, B e C, respectivamente. Certa loja vendeu um carro da marca X. 
#Qual a probabilidade de que o indivíduo que o comprou seja da classe B? 
(3/5 * 1/5) / ((1/10 * 3/4) + (3/5 * 1/5) + (3/10 * 1/20))
#0.5714286

#-------------- 8 -------------- 
#Um certo programa pode ser usado com uma entre duas sub-rotinas A e B, dependendo do problema. 
#A experiência tem mostrado que a sub-rotina A é usada 40% das vezes e a B é usada 60% das vezes. 
#Se A é usada, existe  75% de chance de que o programa chegue a um resultado dentro do limite de tempo. 
#Se B é usada, a chance é de 50%. Se o programa foi realizado dentro do limite de tempo, 
#qual a probabilidade de que a sub-rotina A tenha sido escolhida? 
(0.4 * 0.75) / ((0.4 * 0.75) + (0.6 * 0.5))
#0.5

#-------------- 9 -------------- 
#Uma caixa tem 3 moedas: uma não viciada, outra com duas caras e uma terceira viciada, 
#de modo que a probabilidade de ocorrer cara para essa moeda é de 1/5. 
#Uma moeda é selecionada ao acaso desta caixa. Saiu cara. 
#Qual a probabilidade de que a terceira moeda tenha sido a selecionada?  
(1/5 * 1/3) / ((0.5 * 1/3) + (1 * 1/3) + (1/5 * 1/3))
#0.1176471

#-------------- 10 -------------- 
#A urna X contem 2 bolas azuis, 2 brancas e 1 cinza; e a urna Y contém 2 bolas azuis, 1 branca e 1 cinza. 
#Retira-se uma bola de cada urna. 
#Calcule a probabilidade de saírem 2 bolas brancas sabendo que são bolas de mesma cor.
(2/5 * 1/4) / ((2/5 * 2/4) + (2/5 * 1/4) + (1/5 * 1/4))
#0.2857143