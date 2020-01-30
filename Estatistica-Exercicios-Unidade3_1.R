library(BSDA)

#---------------- 1 -----------------
#Numa tentativa de melhorar o esquema de atendimento, um médico procurou estimar 
#o tempo médio que gasta com cada paciente. Uma amostra aleatória de 29 pacientes, 
#colhida num período de três semanas, acusou uma média de 30 minutos, 
#com desvio padrão de 7 minutos. Construa um intervalo de 95% de confiança 
#para o verdadeiro tempo médio de consulta.

teste_h = tsum.test(mean.x = 30, s.x = 7, n.x = 29, conf.level = 0.95)
c(teste_h$conf.int[1], teste_h$conf.int[2])
#27.33734 32.66266


#---------------- 2 -----------------
#O diretor de um comitê de admissão de uma universidade deseja estimar 
#a idade média de todos os estudantes aprovados no momento. 
#Em uma amostra aleatória de 20 estudantes, a idade média encontrada foi de 22,9 anos. 
#A partir de estudos passados, sabe-se que o desvio padrão é de 1,5 anos e 
#que a população está normalmente distribuída. Qual é o intervalo de 90% de confiança da idade média da população?

teste_h = tsum.test(mean.x = 22.9, s.x = 1.5, n.x = 20, conf.level = 0.90)
c(teste_h$conf.int[1], teste_h$conf.int[2])


#---------------- 3 -----------------
#Um escritório de contabilidade analisou uma amostra aleatória formada por 
#180 documentos de uma empresa cliente. Detectou que 18 documentos apresentavam falhas de algum tipo. 
#Empregando um nível de confiança igual a 95%, encontre a estimativa intervalar do percentual 
#de documentos da empresa que mostravam alguma falha.
teste_h = prop.test(x = 18, n = 180, conf.level = 0.95, alternative = "two.sided")
c(teste_h$conf.int[1], teste_h$conf.int[2])


#---------------- 4 -----------------
#Uma companhia americana está cogitando fazer uma concorrência para o serviço telefônico interurbano. 
#Deseja-se fazer uma pesquisa para estimar a percentagem de assinantes que estão satisfeitos 
#com o atual serviço de interurbanos. Queremos ter 90% de confiança em que a 
#percentagem amostral esteja a menos de 2,5 pontos percentuais do verdadeiro valor populacional. 
#Qual deve ser o tamanho da amostra  quando não há qualquer pesquisa anterior sobre o 
#assunto que possa fornecer uma estimativa do percentual de satisfação dos assinantes?


#---------------- 5 -----------------
#Um pesquisador deseja estimar a atual taxa média de juros cobrada por casas hipotecárias. 
#Estudos anteriores indicam que a variância da taxa de juros é de 0,1764%. 
#Qual deveria ser o tamanho amostral a ser estudado para obtermos uma margem de erro de 0,05% 
#e um nível de confiança de 99%?