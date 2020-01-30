library(BSDA)

#---------------- 1 -------------------
#Uma revista de viagens de negócios quer classificar os aeroportos internacionais de acordo com 
#a avaliação média da população de pessoas que viajam a negócios. 
#Será usada uma escala de classificação, sendo 0 uma avaliação baixa e 10 uma avaliação elevada, 
#e os aeroportos que receberem uma avaliação média populacional maior que 7 
#serão designados como aeroportos com um atendimento de alto nível. 
#A equipe da revista pesquisou uma amostra de 27 viajantes de negócios em cada aeroporto 
#para obter os dados da avaliação. A amostra do aeroporto de Londres produziu 
#uma avaliação média igual a 7,25 com desvio padrão igual a 1,052. 
#Os dados indicam que o aeroporto de Londres deveria ser 
#designado como um aeroporto de alto nível? Utilize um nível de 5% de significância.

teste_h = tsum.test(mean.x = 7.25, s.x = 1.052, n.x = 27, mu=7, alternative = "greater")
teste_h$p.value
#0.1139692
teste_h$p.value <= 0.05
#FALSE, então a hipótese nula (aeroporto com nota média <= 7) não deve ser rejeitada

#---------------- 2 -------------------
#As companhias de seguro de automóvel estão cogitando elevar os prêmios para 
#aqueles que falam ao telefone enquanto dirigem. Um grupo de defesa dos consumidores alega que 
#este problema não é tão sério, porque menos de 10% dos motoristas usam o telefone. 
#Uma companhia de seguro faz uma pesquisa e constata que, 
#entre 500 motoristas selecionados aleatoriamente, 72 usam o telefone. 
#Teste a afirmação do grupo de consumidores ao nível de 2% de significância.

teste_h = prop.test(x = 72, n = 500, p = 0.1, alternative = "less")
teste_h$p.value
#0.9993248
teste_h$p.value <= 0.02
#FALSE, então a hipótese nula (motoristas que usam telefone >= 10%) não deve ser rejeitada

#---------------- 3 -------------------
#Um banco realiza um estudo idealizado para identificar as diferenças na utilização 
#das contas correntes pelos clientes em duas de suas filiais. 
#Uma amostra aleatória de 28 contas correntes é selecionada da filial situada em CG e 
#uma amostra aleatória independente de 22 contas correntes é selecionada da sua filial em BM. 
#O saldo atual da conta corrente é registrado para cada uma das contas. 
#A seguir temos um resumo dos saldos bancários:
#             Média amostral    Desvio padrão amostral
#Filial CG        $1025                 $150
#Filial BM        $910                  $125
#O saldo médio das contas correntes mantidas pela população de clientes difere entre as duas filiais? 
#Utilize um nível de 5% de significância.

teste_h = tsum.test(mean.x = 1025, mean.y = 910, s.x = 150, s.y = 125, n.x = 28, n.y = 22)
teste_h$p.value
#0.004831796
teste_h$p.value <= 0.05
#TRUE, então a hipótese nula (os saldos médios são iguais) deve ser rejeitada, logo, existe diferença

#---------------- 4 -------------------
#Um experimento (hipotético) sobre o efeito do álcool na habilidade perceptual motora é conduzido. 
#10 indivíduos são testado duas vezes, uma depois de ter tomado dois drinks 
#e uma depois de tomado dois copos de água. 
#Os dois testes foram realizados em dois dias diferentes para evitar influência do efeito do álcool. 
#Metade dos indivíduos tomou a bebida alcoólica primeiro e a outra metade água. 
#Os escores dos 10 indivíduos são mostrados abaixo. Escores mais altos refletem uma melhor performance. 
#
#Indivíduo	1 	2	  3 	4	  5	  6	  7	  8	  9	  10
#Água	      16	15	11	20	19	14	13	15	14	16
#Álcool	    13	13	12	16	16	11	10	15	9	  16
#
#Deseja-se testar se houve alteração na habilidade perceptual motora mediante as duas bebidas testadas. 
#Utilize um nível de significância de 1%.

ds_agua = c(16, 15, 11, 20, 19, 14, 13, 15, 14, 16)
ds_alcool = c(13, 13, 12, 16, 16, 11, 10, 15, 9, 16)

teste_h = t.test(ds_alcool, ds_agua, alternative = "t", conf.level = 0.01, paired=TRUE)
teste_h$p.value
#0.005742973
teste_h$p.value <= 0.01
#TRUE, então a hipótese nula (não há alteração da habilidade) deve ser rejeitada, logo, existe diferença

#---------------- 5 -------------------
#Em um concurso público promovido por uma empresa estatal, os candidatos às vagas de 
#Engenharia constituem a nossa população de interesse. 
#Entre eles, os que se submeteram a uma preparação específica para 
#o concurso constituem a subpopulação A e os que não fizeram essa preparação 
#constituem a sub-população B. Foram coletadas amostras aleatórias em ambas as 
#sub-populações e os resultados obtidos foram os seguintes:
#
#Sub-população       Tamanho amostral     Aprovados
#Prepararam-se (A)        140               34
#Não se prepararam (B)    230               53
#
#Pode-se dizer que houve diferença na proporção de aprovados entre as duas sub-populações estudadas? 
#Utilize um nível de 10% de significância.

teste_h = prop.test(x=c(34, 53), n=c(140, 230), alternative = "two.sided", correct=FALSE)
teste_h$p.value <= 0.10
#FALSE, então a hipótese nula (as proporções são iguais) não deve ser rejeitada

