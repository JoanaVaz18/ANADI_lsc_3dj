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
