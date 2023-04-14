##Ex1 

##a Acrescente aos dados importados uma coluna com o tempo no sistema POSIXct no formato
##"yy/mm/dd HH:MM:SS GMT". Deve usar as opções origin = "1970-01-01" e tz = "GMT" para
##transformar o tempo (em segundos) para o sistema pedido.


#get current directory
getwd()
#set current working directory to a Desktop directory
setwd("C:/Users/biach/OneDrive/Desktop")
#ler data de "DADOS1.CSV" para o dataframe dados1 dando skip as 2 primerias linhas 
dados1 <- read.csv("DADOS1.csv",skip=2)
#Renomiar a 1 coluna para "Time(s)"
colnames(dados1)[1] <- "Time(s)"

#conveter coluna "Time" em um objeto de POSIXct  
dados1$TEMPO_POSIX <- as.POSIXct(dados1$Time ,origin = "1970-01-01", tz= "GMT")
View(dados1)


##b Efetue um gráfico que permita comparar a temperatura do motor nas bombas 1,2 e 3, no dia 4 de
##agosto de 2013.


library(ggplot2)

# Criar subset para o dia 04/08 
dados1b <- subset(dados1, as.Date(TEMPO_POSIX) == as.Date("2013-08-04"))
View(dados1b)

# Criar vetores de temperatura para cada bomba
temp_b1 <- dados1b$K.1
temp_b2 <- dados1b$K.3
temp_b3 <- dados1b$K.5

# Criar vetor de tempo para o eixo x
tempo <- dados1b$TEMPO_POSIX

# Criar gráfico de linhas
plot(tempo, temp_b1, type = "l", col = "red",main="Temperatura do motor nas bombas no dia 2013-08-04", xlab = "Tempo", ylab = "Temperatura", ylim = c(365, 390))
lines(tempo, temp_b2, col = "blue")
lines(tempo, temp_b3, col = "green")
legend("topright", c("Bomba 1", "Bomba 2", "Bomba 3"), col = c("red", "blue", "green"), lty = 1)


##c Efetue um boxplot com os dados da alínea anterior. Comente os resultados obtidos


#combinar os 3 vetores para um dataframe
dados1c=cbind(temp_b1,temp_b2,temp_b3)

#criar boxplot
boxplot(dados1c,main="Temperatura do motor nas bombas no dia 2013-08-04",xlab="Bombas",ylab="Temperatura",col=c(3,5,7),names = c("Bomba 1", "Bomba 2", "Bomba 3"))
#boxplot(dados[-c(9,10)],outline=FALSE,main="Dias sem chuva",ylab="Dias sem chuva")


##d Uma forma de avaliar a quantidade de barris produzida num dia é calcular a média das medições do
##“oil rate” efetuadas no dia em questão

##i Efetue um gráfico de barras que compare os barris de petróleo produzidos diariamente pelas
##bombas 1 e 2 no mês de março de 2014


#criar um subset com os dados do mes 03 de 2014
dados1di <- subset(dados1, format(TEMPO_POSIX, "%Y-%m") == "2014-03")
View(dados1di)

#numero de linhas do dataframe
nlinha <- nrow(dados1di)
# Definindo o tamanho do grupo
n <- round(nlinha/31)

# Calculando a média em cada grupo de n linhas para bomba1
mediasb1 <- list()
for (i in seq(1, nrow(dados1di), by=n)) {
  grupo <- dados1di[i:(i+n-1),]
  media_grupo <- mean(grupo$bbl.d.2)
  mediasb1 <- append(mediasb1, media_grupo)
}

# Calculando a média em cada grupo de n linhas para bomba2
mediasb2 <- list()
for (i in seq(1, nrow(dados1di), by=n)) {
  grupo <- dados1di[i:(i+n-1),]
  media_grupo <- mean(grupo$bbl.d.5)
  mediasb2 <- append(mediasb2, media_grupo)
}

bomba1 <- unlist(mediasb1)
bomba2 <- unlist(mediasb2)
counts <- rbind(bomba1,bomba2)

#boxplot
barplot(counts, main="Numero de Barris produzidos no mes de marco em 2014",
        xlab="Numero de Barris", col=c("darkblue","red"),beside=TRUE)


##ii Em que mês a bomba 1 extraiu mais barris de petróleo?
##N.B. Considere os meses compreendidos entre os dias 1-6-2013 e 31-5-2014


#criar subset com os dados entre dias 2013/06/01 a 2014/05/31
bomba1 <- subset(dados1, as.Date(TEMPO_POSIX) >= as.Date("2013-06-01") & as.Date(TEMPO_POSIX) <= as.Date("2014-05-31") )
View(bomba1)

#agrega dados da bomba1 fazendo soma do oil rate para cada mes(%m)
total_barris <- aggregate(bomba1$bbl.d.2, list(format(bomba1$TEMPO_POSIX, "%m")), sum)

#procura o mes com maior producao, apresentatando no final o mes(1-12 onde 1 representa mes 06/13 e 12 o mes 05/14)
mes_maior <- total_barris[which.max(total_barris$x), 1]
#mes_maior=3 significa que mes 08/13 foi o mes com maior producao


##iii Extraiu-se uma amostra aleatória de dias entre os dias 1-6-2013 e 31-5-2014 usando as
##seguintes instruções
 

## [1] 78 362 66 156 277 41 339 169 272 268
#Sendo que os dias foram numerados por ordem crescente, ou seja, 1
#representa o dia 1 de junho de 2013, 2 representa o dia 2 de junho
#de 2013,... etc.


set.seed(300)
dias_amostra <- sample(1:365,10)

date <- as.Date(1, origin = as.Date("2013-06-01"))
#criar subset com os dados entre datas 2013-06-01 a 2014-05-31
dadosD4 <- subset(dados1,as.Date(TEMPO_POSIX) >= as.Date("2013-06-01") & as.Date(TEMPO_POSIX) <= as.Date("2014-05-31") ,origin= as.Date("2013-06-01"))

#dadosD5 <- subset(dadosD4,as.Date(TEMPO_POSIX) = as.Date(1:364, origin = as.Date("2013-06-01")))
View(dadosD4)

#criar subset convertendo as datas do dadsoD4 no formato 1,2,3,4 e tirando aquelas que nao estao no dias_amostras
dados_amostra <- subset(dadosD4,format(TEMPO_POSIX, "%j" ) %in% dias_amostra)
View(dados_amostra)

#calcula total de barris para cada bomba para cada dia
total_barris1 <- aggregate(dados_amostra$bbl.d.2, list(format(dados_amostra$TEMPO_POSIX, "%j")), sum)
total_barris2 <- aggregate(dados_amostra$bbl.d.5, list(format(dados_amostra$TEMPO_POSIX, "%j")), sum)

# Boxplot para a produção da Bomba 1
boxplot(total_barris1$x,
        main = "Produção diária da Bomba 1 (amostra aleatória)",
        ylab = "Barris produzidos",
        ylim = c(0, max(total_barris1$x)),
        col = "lightblue")

# Boxplot para a produção da Bomba 2
boxplot(total_barris2$x,
        main = "Produção diária da Bomba 2 (amostra aleatória)",
        ylab = "Barris produzidos",
        ylim = c(0, max(total_barris2$x)),
        col = "lightgreen")



##iv Utilize as amostras aleatórias da alínea anterior para efetuar um teste de hipóteses que
##permita verificar se a média da produção diária de petróleo da bomba 1 foi superior à da
##bomba 2 no período de 1-6-2013 e 31-5-2014


install.packages("nortest")
library(nortest)
library(moments)
install.packages("BSDA")
library(BSDA)


#shapiro verifica se tem dist normal para n<30
#H0:segue normalidade
#H1:nao segue normalidade
Shap_t <- shapiro.test(total_barris2$x)
Shap_t$p.value
#sendo p=0.0197 < alfa de 0,05 entao rejeita-se H0 e logo nao tem dist normal


diff_bomba <- total_barris1$x - total_barris2$x

#skewness verifica se tem simetria
skewness(diff_bomba)

# value= 1.524 > 1 logo nao e simetrico


# Hipoteses:  h0 dif_bomba =0
#             h1 dif_bomba >0
#teste de sinal uma vez que as amostras nao tem dist normal nem sao simetricos
signt <- SIGN.test(diff_bomba,md=0,alternative = "greater") 
signt$p.value
#p value= 0.0078125< alfa de 0.05, rejeitamos assim H0.Ha evidencia de que bomba1 teve media de producao diaria superior a bomba 2 de ordem significativa


##v Confirme se a decisão obtida no teste da alínea anterior corresponde à “realidade”.

#confirma-se


##Ex2 Pretende-se comparar a precisão de 6 algoritmos de Machine Learning: SVM, DT, KN, RF, ML e GB. Para
##o efeito calculou-se a precisão de cada algoritmo sobre 10 conjuntos de dados: D1, D2, ..., D10. Os dados
##encontram-se guardados no ficheiro DADOS2.csv

##a

library(readr)
library(Hmisc)

#ler dados de "DADOS2.CSV" para dataframe dados2
dados2 <- read.csv("DADOS2.csv")
View(dados2)

#usa-se "pearson" uma vez que as variaveis sao continuas
rcorr(as.matrix(dados2[3:8]),type="pearson") 

#sabendo que se tiver proximo de 1 as variaveis estao positivamente fortemente corrrelacionadas
#proximos de -1 estao negativamente fortemente correlacionadas 
#proximo de 0 estao fracamente correlacionadas

     #SVM   DT   KN   RF   ML   GB
#SVM 1.00 0.26 0.64 0.47 0.71 0.86
#DT  0.26 1.00 0.43 0.88 0.62 0.21
#KN  0.64 0.43 1.00 0.48 0.85 0.75
#RF  0.47 0.88 0.48 1.00 0.57 0.32
#ML  0.71 0.62 0.85 0.57 1.00 0.72
#GB  0.86 0.21 0.75 0.32 0.72 1.00

# Positivamente fortemente correlacionadas: SVM-KN SVM-ML SVM-GB DT-RF DT-ML KN-ML KN-GB RF-ML ML-GB
# Negativamente fortemente correlacionadas: N/A
# Fracamente correlacionadas: SVM-DT SVM-RF DT-KN DT-GB KN-RF RF-GB 

##b

#test shapiro uma vez que todos tem n<30. verifica dist normal
#H0:segue normalidade
#H1:nao segue normalidade

Shap_t <- shapiro.test(dados2$SVM)
Shap_t$p.value
#sendo p=0.26 < alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal 

Shap_t <- shapiro.test(dados2$DT)
Shap_t$p.value
#sendo p=0.067 < alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal

Shap_t <- shapiro.test(dados2$KN)
Shap_t$p.value
#sendo p=0.69 < alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal

Shap_t <- shapiro.test(dados2$RF)
Shap_t$p.value
#sendo p=0.31 < alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal

Shap_t <- shapiro.test(dados2$ML)
Shap_t$p.value
#sendo p=0.021 < alfa de 0,05 entao rejeita se H0 e logo nao tem dist normal

Shap_t <- shapiro.test(dados2$GB)
Shap_t$p.value
#sendo p=0.051 < alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal
#basta um nao ser normal que vamos ter de fazer teste nao parametrico

#H0 medianaSVM = medianaDT = medianaRF =medianaML =medianaML =medianaGB
#H1 pelo menos uma diferente

dados_numericos <-c(dados2$SVM,dados2$DT,dados2$KN,dados2$RF,dados2$ML,dados2$GB)
grupos <- factor (c(rep("SVM",10),rep("DT",10),rep ("KN",10),rep ("RF",10),rep ("ML",10),rep ("GB",10)))

#teste kruskal uma vez uqe tem mais que 2 amostras independentes
kruskal.test(dados_numericos,grupos)
#como pvalue = 0,3335 > 0.05 nao rejeita-se H0 nao ha diferencas significativas entre as precisoes de algoritmos

##c como pvalue = 0,3335 > 0.05 nao rejeita-se H0 nao ha diferencas significativas entre as precisoes de algoritmos
