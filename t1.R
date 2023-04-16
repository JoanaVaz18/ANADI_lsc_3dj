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

total_barris1 <- aggregate(dados1di$bbl.d.2, list(format(dados1di$TEMPO_POSIX, "%j")), mean)
total_barris2 <- aggregate(dados1di$bbl.d.5, list(format(dados1di$TEMPO_POSIX, "%j")), mean)

counts <- rbind(total_barris1$x,total_barris2$x)

#boxplot
barplot(counts, main="Numero de Barris produzidos no mes de marco em 2014",ylab= "Numero de barris",
        xlab="Dias", col=c("darkblue","red"),beside=TRUE)


##ii Em que mês a bomba 1 extraiu mais barris de petróleo?
##N.B. Considere os meses compreendidos entre os dias 1-6-2013 e 31-5-2014


#criar subset com os dados entre dias 2013/06/01 a 2014/05/31
bomba1 <- subset(dados1, as.Date(TEMPO_POSIX) >= as.Date("2013-06-01") & as.Date(TEMPO_POSIX) <= as.Date("2014-05-31") )
View(bomba1)

#agrega dados da bomba1 fazendo soma do oil rate para cada mes(%m)
total_barris <- aggregate(bomba1$bbl.d.2, list(format(bomba1$TEMPO_POSIX, "%m")), mean)
total_barris <- total_barris$x*30

#procura o mes com maior producao, apresentatando no final o mes(1-12 onde 1 representa mes 1/14 e 12 o mes 12/13)
mes_maior <-which.max(total_barris)
#mes_maior=8 significa que mes 08/13 foi o mes com maior producao

##iii Extraiu-se uma amostra aleatória de dias entre os dias 1-6-2013 e 31-5-2014 usando as
##seguintes instruções


## [1] 78 362 66 156 277 41 339 169 272 268
#Sendo que os dias foram numerados por ordem crescente, ou seja, 1
#representa o dia 1 de junho de 2013, 2 representa o dia 2 de junho
#de 2013,... etc.


set.seed(300)
dias_amostra <- sample(1:365, 10)

data_inicial <- as.Date("2013-06-01")

datas_amostra <- data_inicial + dias_amostra  - 1
print(datas_amostra)
dt <- subset(dados1, as.numeric(as.Date(TEMPO_POSIX)) %in% datas_amostra)

#criar subset com os dados entre datas 2013-06-01 a 2014-05-31
dadosD4 <- subset(dados1,as.Date(TEMPO_POSIX) == as.Date(datas_amostra) ,origin= as.Date("2013-06-01"))

#dadosD5 <- subset(dadosD4,as.Date(TEMPO_POSIX) = as.Date(1:364, origin = as.Date("2013-06-01")))
View(dadosD4)

#calcula total de barris para cada bomba para cada dia
total_barris1 <- aggregate(dadosD4$bbl.d.2, list(format(dadosD4$TEMPO_POSIX, "%j")), mean)
total_barris2 <- aggregate(dadosD4$bbl.d.5, list(format(dadosD4$TEMPO_POSIX, "%j")), mean)

classes=c("total_barris1","total_barris2")
# Boxplot para a produção da Bomba 1 e 2
boxplot(total_barris1$x,total_barris2$x,
        main = "Produção diária da Bomba 1 e bomba2 (amostra aleatória)",
        ylab = "Barris produzidos",
        ylim = c(300, 700),
        names=classes,
        col=c(4,2))


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
#sendo p=0.394946 > alfa de 0,05 entao nao se rejeita H0 logo tem dist normal
Shap_t <- shapiro.test(total_barris1$x)
Shap_t$p.value
#sendo p=0.979781 > alfa de 0,05 entao nao se rejeita H0 logo tem dist normal

##t.test(total_barris1$x, total_barris2$x, paired=TRUE, var=TRUE,alternative="greater")

diff_bomba <- total_barris1$x - total_barris2$x

library(car)
dados <- as.data.frame(cbind(total_barris1$x,total_barris2$x))

library(reshape2) #para usar função melt
Mdados <- melt(dados , variable.name = "Bomba", value.name = "Producao")
View(Mdados)

#levene verifica se as variancias sao iguais
#H0:iguais
#H1:diferentes
result <- leveneTest(Producao~Bomba,Mdados,center=mean)
#como pvalue=0.01919 < alfa=0.05, logo rejeita-se H0, ou seja consideramos as variancas diferentes

# Hipoteses:  h0 meanBomba1 = meanBomba2
#             h1 meanBomba1 > meanBomba2
t.test(total_barris1$x, total_barris2$x,mu=0, paired=TRUE, var.equal=FALSE,alternative="greater")
#p value= 9.841e-05< alfa de 0.05, rejeitamos assim H0.Ha evidencia de que bomba1 teve media de producao diaria superior a bomba 2 de ordem significativa




#hist(diff_bomba)

#skewness verifica se tem simetria
#skewness(diff_bomba)

# value= 1.664474 > 1 logo nao e simetrico


# Hipoteses:  h0 dif_bomba =0
#             h1 dif_bomba >0
#teste de sinal uma vez que as amostras nao tem dist normal nem sao simetricos
#signt <- SIGN.test(diff_bomba,md=0,alternative = "greater") 
#signt$p.value
#p value= 0.00195< alfa de 0.05, rejeitamos assim H0.Ha evidencia de que bomba1 teve media de producao diaria superior a bomba 2 de ordem significativa


##v Confirme se a decisão obtida no teste da alínea anterior corresponde à “realidade”.

total_barris1 <- aggregate(dados1$bbl.d.2, list(format(dados1$TEMPO_POSIX, "%m")), mean)
total_barris2 <- aggregate(dados1$bbl.d.5, list(format(dados1$TEMPO_POSIX, "%m")), mean)

counts <- rbind(total_barris1$x,total_barris2$x)

#boxplot
barplot(counts, main="ddl diario de cada mes(2013-6 2014-5)",ylab= "Numero de barris",
        xlab="mes", col=c("darkblue","red"),beside=TRUE)


#confirma-se


##2 Pretende-se comparar a precisão de 6 algoritmos de Machine Learning: SVM, DT, KN, RF, ML e GB. Para
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

#C
     #SVM   DT   KN   RF   ML   GB
#SVM 1.00 0.26 0.64 0.47 0.71 0.86
#DT  0.26 1.00 0.43 0.88 0.62 0.21
#KN  0.64 0.43 1.00 0.48 0.85 0.75
#RF  0.47 0.88 0.48 1.00 0.57 0.32
#ML  0.71 0.62 0.85 0.57 1.00 0.72
#GB  0.86 0.21 0.75 0.32 0.72 1.00

# Positivamente fortemente correlacionadas: (C>0.7)     
# Negativamente fortemente correlacionadas: (C<-0.7)
# Fracamente correlacionadas: (-0.3<C<0.3) 
# Positivamente moderadamente correlacionadas:(0.3<C<0.7)   
# Negativamente moderadamente correlacionadas:(-0.3>C>-0.7)

#P
      #SVM    DT     KN     RF     ML     GB    
#SVM        0.4646 0.0474 0.1747 0.0211 0.0013
#DT  0.4646        0.2102 0.0008 0.0535 0.5552
#KN  0.0474 0.2102        0.1569 0.0017 0.0124
#RF  0.1747 0.0008 0.1569        0.0841 0.3610
#ML  0.0211 0.0535 0.0017 0.0841        0.0186
#GB  0.0013 0.5552 0.0124 0.3610 0.0186    


#cor SVM/ML é forte(0.71) (positiva e proxima de 1 e significativa p(0.0211)<alfa(0.05))
#cor SVM/GB é forte(0.86) (positiva e proxima de 1 e significativa p(0.0013)<alfa(0.05))
#cor DT/RF é forte(0.88) (positiva e proxima de 1 e significativa p(0.0008)<alfa(0.05))
#cor KN/ML é forte(0.85) (positiva e proxima de 1 e significativa p(0.0017)<alfa(0.05))
#cor KN/GB é forte(0.75) (positiva e proxima de 1 e significativa p(0.0124)<alfa(0.05))
#cor ML/GB é forte(0.72) (positiva e proxima de 1 e significativa p(0.0186)<alfa(0.05))

#cor SVM/DT é fraca(0.26) (positiva e proxima de 0 e nao significativa p(0.4646)>alfa(0.05))
#cor GB,/DT é fraca(0.21) (positiva e proxima de 0 e nao significativa p(0.5552)>alfa(0.05))

#cor SVM/KN é moderada(0.64) (positiva entre 0.3 e 0.7 e significativa p(0.0474)<alfa(0.05))
#cor DT/ML é moderada(0.62) (positiva entre 0.3 e 0.7 e nao significativa p(0.0535)>alfa(0.05))
#cor RF/ML é moderada(0.57) (positiva entre 0.3 e 0.7 e nao significativa p(0.0841)>alfa(0.05))
#cor SVM/RF é moderada(0.47) (positiva entre 0.3 e 0.7 e nao significativa p(0.1747)>alfa(0.05))
#cor DT/KN é moderada(0.43) (positiva entre 0.3 e 0.7 e nao significativa p(0.2102)>alfa(0.05))
#cor KN/RD é moderada(0.48) (positiva entre 0.3 e 0.7 e nao significativa p(0.1569)>alfa(0.05))
#cor RF/GB é moderada(0.32) (positiva entre 0.3 e 0.7 e nao significativa p(0.3610)>alfa(0.05))



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



##3 O ficheiro DADOS3.csv contém dados de 4 variáveis (aceleração, número de cilindros, peso e
##potência) de 99 viaturas escolhidas aleatoriamente:


##a Divida as 99 viaturas em três grupos: viaturas com 4 cilindros, com 6 e com 8. Existirá diferenças
##significativas na aceleração entre os três grupos?


#ler dados de "DADOS3.CSV" para dataframe dados3


dados3 <- read.csv("DADOS3.csv")
View(dados3)

aceleracao <- dados3$Acceleration
cilindros <- dados3$Cylinders

aceleracao_4c <- aceleracao[cilindros == 4]
length(aceleracao_4c)  #lilith pk n>30
aceleracao_6c <- aceleracao[cilindros == 6]
length(aceleracao_6c)  #shapiro test pk n<30
aceleracao_8c <- aceleracao[cilindros == 8]
length(aceleracao_8c)  #lilith pk n>30

#H0:segue normalidade
#H1:nao segue normalidade

#verifica dist normal
Shap_t <- shapiro.test(aceleracao_6c)
Shap_t$p.value
#sendo p=0.036 < alfa de 0,05 entao rejeita se H0 e logo nao tem dist normal

Lill_t <- lillie.test(aceleracao_4c)
Lill_t$p.value
#sendo p=0.289 >alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal

Lill_t <- lillie.test(aceleracao_8c)
Lill_t$p.value
#sendo p=0.780 >alfa de 0,05 entao nao se rejeita H0 e logo tem dist normal

dados_numericos <-c(aceleracao_4c,aceleracao_6c,aceleracao_8c)
grupos <- factor (c(rep("4",51),rep("6",17),rep ("8",31)))

#teste kruskal uma vez uqe tem mais que 2 amostras independentes
kruskal.test(dados_numericos,grupos)
#como pvalue =2.79e-11 < 0.05 rejeita-se H0  ha diferencas significativas na aceleração entre os três grupos


##bSupondo que a aceleração é a variável dependente e as restantes variáveis são independentes:
##i Encontre o modelo de regressão linear. N.B. considere a variável número de cilindros uma
##variável “Dummy”.


#y= acele x1= weight x2= horse  x3=factor x4=cylin

colnames(dados3) <- c("Acceleration","Cylinders","Weight","Horsepower")
dados3$Cylinders <- factor(dados3$Cylinders)

reg3 <- lm(dados3$Acceleration ~  dados3$Weight + dados3$Horsepower + dados3$Cylinders )
reg3

summary(reg3)


# para cilindro 4 -> x1=0 entao y(chapeu)= 13.749056 + 0.003151x2 - 0.057381x3
# para cilindro 6 -> x1=1 entao y(chapeu)= 13.749056 -1.515998 + 0.003151x2 - 0.057381x3
# para cilindro 8 -> x1=2 entao y(chapeu)= 13.749056 -4.867451 * 2 + 0.003151x2 - 0.057381x3


#ii Use o modelo encontrado na alínea anterior para estimar a aceleração de uma viatura com:
#um peso de 2950 kg, potência de 100 Hp e 4 cilindros.

#y(chapeu) = 13.749056 + 0.003151*2950 - 0.057381*100

y <- 13.749056 + 0.003151*2950 - 0.057381*100
y
#y estimado e de 17.30641 para aceleracao

# Criar um novo data frame com os valores das variáveis independentes
nova_viatura <- data.frame(Cylinders = 4, Weight = 2950 , Hosepower = 100)

estimativa_aceleracao <- predict(reg3, nova_viatura )

print(estimativa_aceleracao)


plot (fitted(reg3), residuals(reg3), xlab="Val. Ajustados", ylab="Residuos")
abline(h=0)




