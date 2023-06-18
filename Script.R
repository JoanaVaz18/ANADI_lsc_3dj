library(dplyr)
library(ggplot2)
library(rpart)
library(corrplot)
library(rpart.plot)
library(neuralnet)
library(nortest)
library(moments)
library(car)
library(FNN)
library(class)
library(plyr)


RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

MAE<-function(ytest,ypred){
  mean(abs(ytest-ypred))
}

minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

minmaxdesnorm <- function(x, goal.attrib) {
  return (x * (max(goal.attrib) - min(goal.attrib)) + min(goal.attrib))
}

parse_results <- function(m.conf) {
  accuracy <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  recall = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  specificity <- m.conf[2, 2] / (m.conf[2, 2] + m.conf[2, 1])
  f1 <- (2 * recall * specificity) / (recall + specificity)
  
  message("accuracy: ", accuracy, "%")
  message("sensitivity: ", recall)
  message("specificity: ", specificity)
  message("F1: ", f1)
  
  my_list <-
    list(
      "F1" = f1,
      "specificity" = specificity,
      "recall/sensitivity" = recall,
      "accuracy" = accuracy
    )
  return(my_list)
}

tree_accuracy_funct <- function(column_name, train_data, test_data) {
  decision_tree <- rpart(paste(column_name, "~ ."), data = train_data, method = "class")
  tree_predict <- predict(decision_tree, newdata = test_data, type = "class")
  cfmatrix <- table(tree_predict, test_data[[column_name]])
  return(sum(diag(cfmatrix)) / sum(cfmatrix) * 100) 
}

knn_info_func <- function(dataset, column_name, data.train, data.test, train_labels, test_labels) {
  k_values <- seq(1, 50, 2)
  rmse <- numeric()
  accuracy <- numeric()
  
  for (i in k_values) {
    knnreg.pred <- knn.reg(data.train, data.test, train_labels, k = i)
    knnreg.pred$pred <- minmaxdesnorm(knnreg.pred$pred, dataset[[column_name]])
    rmse <- c(rmse, RMSE(knnreg.pred$pred, minmaxdesnorm(test_labels, dataset[[column_name]])))
    
    knn.pred <- knn(train = data.train, test = data.test, cl = train_labels, k = i)
    cfmatrix <- table(test_labels, knn.pred)
    accuracy <- c(accuracy, sum(diag(cfmatrix)) / sum(cfmatrix))
  }
  
  res_rmse <- data.frame(k = k_values, rmse)
  min_rmse <- res_rmse[res_rmse$rmse == min(res_rmse$rmse), ]
  
  res_accuracy <- data.frame(k = k_values, accuracy)
  max_accuracy <- res_accuracy[res_accuracy$accuracy == max(res_accuracy$accuracy), ]
  
  return(list(accuracy = max_accuracy, rmse = min_rmse))
}


nn_info_func <- function(column, train_data, test_data, nodes) {
  formula <- as.formula(paste(column, "~ ."))
  nn.model <- neuralnet(formula, data = train_data, hidden = nodes, stepmax = 1e6)
  
  nn.pred <- compute(nn.model, test_data[, -which(names(test_data) == column)])
  
  predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)
  
  accuracy <- sum(predicted_labels == test_data[[column]]) / length(predicted_labels) * 100
  
  denormalized_pred <- minmaxdesnorm(nn.pred$net.result, train_data[, column])
  denormalized_actual <- minmaxdesnorm(test_data[, column], train_data[, column])
  
  rmse <- RMSE(denormalized_pred, denormalized_actual)
  
  return(list(accuracy = accuracy, rmse = rmse))
}

normalize <- function(data) {
  centered <- scale(data, center = TRUE, scale = FALSE)
  normalized <- scale(centered, center = FALSE, scale = TRUE)
  return(normalized)
}

# Cálculo da accuracy
calculate_accuracy <- function(predictions, actual_values) {
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Assuming binary classification with threshold 0.5
  accuracy <- sum(predicted_classes == actual_values) / length(actual_values) * 100
  return(accuracy)
}


set.seed(42)

#EX1

#get current directory
getwd()
#set current working directory to a Desktop directory
setwd("C:/Users/biach/OneDrive/Desktop")

# Carregar o arquivo "ciclismo.csv"
dados <- read.csv("ciclismo.csv",stringsAsFactors=TRUE)

#EX2

dados$dob <- as.Date(dados$dob)

# Calcular a idade com base na data de nascimento
dados <- dados %>%
  mutate(Age = as.integer(Sys.Date() - dob) %/% 365)
# Verificar a dimensão dos dados
dim(dados)

# Obter um sumário dos dados
summary(dados)

View(dados)

#EX3

ggplot(dados, aes(x = hr_results)) +
  geom_histogram(stat = "bin", color = "black",bins = 15) +
  facet_grid(Pro.level ~ gender ) +
  xlab("Resultado de testes frequencia cardiaca hr") +
  ylab("Numero de pessoas") +
  labs(title = "Distribuição de hr_results por Nível de competição e Gênero")

ggplot(dados, aes(x = vo2_results)) +
  geom_histogram(stat = "bin", color = "black",bins = 15) +
  facet_grid(Pro.level ~ gender) +
  xlab("Resultado do test volume de oxigênio máximo") +
  ylab("Contagem") +
  labs(title = "Distribuição de vo2_results por Nível de competição e Gênero")

ggplot(dados, aes(x = altitude_results)) +
  geom_histogram(stat = "bin", color = "black",bins = 15) +
  facet_grid(Pro.level ~ gender) +
  xlab("Resultado de testes de altitude") +
  ylab("Contagem") +
  labs(title = "Distribuição de altitude_results por Nível de competição e Gênero")

#EX4

#A e C
missing_values <- sum(is.na(dados))
dataset <- na.omit(dados)

dados <- dados %>%
  mutate(Age = as.integer(Sys.Date() - dob) %/% 365)

dataset <- dataset[,-1]

dataset <- dataset[,-9] 

# colunas em binario
columns <- c("Background", "Continent", "Team")

matriz_binaria <- list()

for (col in columns) {
  data_matrix <- model.matrix(~ . - 1, data = dataset[col])
  matriz_binaria[[col]] <- data_matrix
}

data_bin <- do.call(cbind, matriz_binaria)

columns_to_bind <- c("vo2_results", "hr_results", "altitude_results", "Age", "Pro.level", "gender", "Winter.Training.Camp")

additional_data <- dataset[columns_to_bind]

dataset <- cbind(data_bin, additional_data)

# Transforma colunas Dummy em 0 e 1
dataset$Pro.level <- as.numeric(as.factor(dataset$Pro.level)) -1
dataset$gender <- as.numeric(as.factor(dataset$gender)) -1
dataset$Winter.Training.Camp <- as.numeric(as.factor(dataset$Winter.Training.Camp)) -1

View(dataset)

#B

boxplot(dataset[,c("vo2_results","hr_results","altitude_results")])

# Get outliers for vo2_results
outliers_vo2 <- boxplot(dataset$vo2_results)$out

# Get outliers for hr_results
outliers_hr <- boxplot(dataset$hr_results)$out

# Get outliers for altitude_results
outliers_altitude <- boxplot(dataset$altitude_results)$out

#D

normalized_dataset <- as.data.frame(lapply(dataset, minmaxnorm))
data.norm <- as.data.frame(lapply(dataset, minmaxnorm))
#EX5
corrplot(cor(dataset))

#EX6

sample <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
dataset2.train  <- dataset[sample, ]
dataset2.test  <- dataset[!sample, ]

#A

modelo <- lm(altitude_results ~ hr_results, data = dataset2.train)
intercept <- coef(modelo)[1]
slope <- coef(modelo)[2]

# Print the linear regression equation
cat("Altitude_results = ", intercept, " + ", slope, " * hr_results")

#B

ggplot(modelo, aes(dataset2.train$hr_results, dataset2.train$altitude_results)) +
  geom_point() +  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = dataset2.train$hr_results, yend = .fitted), color = "red")

#mais op
plot(dataset2.train$hr_results, dataset2.train$altitude_results, pch=20)
abline(modelo$coefficients[1],modelo$coefficients[2], col='red')

#C

prediction <- predict(modelo,  dataset2.test)

actual <- dataset2.test$altitude_results

MAE(actual,prediction)
#6.876
RMSE(actual, prediction)
#8.274

#D

model <- lm(altitude_results ~ ., data=dataset2.train)

# Make predictions on the test data
predictions1 <- predict(model, newdata = dataset2.test)

MAE(dataset2.test$altitude_results,predictions1)
#4.644
RMSE(dataset2.test$altitude_results,predictions1)
#5.721


#7

#A

# Criar o modelo de regressão linear múltipla
model <- lm(vo2_results ~ ., data=dataset2.train)
# Imprimir o sumário do modelo
summary(model)$coef

#B

tree.model <- rpart(vo2_results ~ ., data=dataset2.train)

rpart.plot(tree.model)
rpart.plot(tree.model, digits = 3, fallen.leaves = TRUE, type = 3)
#rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

#C
#normalized
sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
dataset3.train  <- normalized_dataset[sample, ]
dataset3.test  <- normalized_dataset[!sample, ]


nn.model <- neuralnet(vo2_results ~ ., data = dataset3.train, hidden = numnodes <- 2)

plot(nn.model)


#EX8

#RL  mae=44.84 rmse=44.84
predictions2 <- predict(model, newdata = dataset2.test)

rl.mae <- MAE(dataset2.test$altitude_results,predictions2)
#7.01
rl.rmse <- RMSE(dataset2.test$altitude_results,predictions2)
#8.41

#tree
predictions3 <- predict(tree.model, newdata = dataset2.test)

av.mae <- MAE(dataset2.test$altitude_results,predictions3)
#7.98
av.rmse <- RMSE(dataset2.test$altitude_results,predictions3)
#9.68
 
#nn 
temp_test <- dataset3.test
temp_test$vo2_results <- NULL

nn.pred <- compute(nn.model,temp_test)
# Denormalize the predicted values

denormalized_pred <- minmaxdesnorm(nn.pred$net.result,dataset$vo2_results)

# Denormalizar
denormalized_actual <- minmaxdesnorm(dataset3.test$vo2_results,dataset$vo2_results)

# Calculate evaluation metrics
nn.mae <- MAE(denormalized_actual,denormalized_pred)
#3.60
nn.rmse <- RMSE(denormalized_actual,denormalized_pred)
#4.34

#EX9

# H0: Os resultados obtidos para os dois melhores modelos são estatisticamente significativos.
# H1: Os resultados obtidos para os dois melhores modelos não são estatisticamente significativos.
# a = 0.05

rl.sample<-c(rl.mae, rl.rmse) # amostra arvore de regressao
nn.sample<-c(nn.mae, nn.rmse) # amostra rede neuronal

res<-t.test(rl.sample, nn.sample); res 
# p-value = 0.06928 (> 0.05, logo nao se rejeita H0)



######################################################
#4.2
#EX1
set.seed(42)

column_name <- "Pro.level"

sample <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))

norm_train_data_knn  <- normalized_dataset[sample, -which(names(normalized_dataset) == column_name)]
norm_test_data_knn  <- normalized_dataset[-sample, -which(names(normalized_dataset) == column_name)]

norm_train_labels <- normalized_dataset[sample,column_name]
norm_test_labels <- normalized_dataset[-sample,column_name]

knn_info_norm <- knn_info_func(dataset, column_name, norm_train_data_knn, norm_test_data_knn, norm_train_labels, norm_test_labels)
knn_info_norm 
# k=35 max accuracy: 68.0 % 
# k=47 min rmse: 0.4674
k <- knn_info_norm$accuracy$k

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
norm_train_data  <- normalized_dataset[sample, ]
norm_test_data  <- normalized_dataset[!sample, ]

tree_accuracy_norm <- tree_accuracy_funct(column_name, norm_train_data, norm_test_data)
tree_accuracy_norm 
# accuracy: 70.73955 %


nn_results <- c()
internal_nodes <- list(1,2,c(6,2))
for (n in internal_nodes) {
  nn_info <- nn_info_func(column_name, norm_train_data, norm_test_data, n)
  nn_results <- c(nn_results, list(nodes=n,nn_info))
}
nn_results

# nodes = 1
# accuracy: 71.06109
# rmse: 0.4506036

# nodes = 2
# accuracy: 67.52412
# rmse: 0.4854399

# levels = 2 (nodes = 6,2)
# accuracy: 60.45016
# rmse: 0.5623287


#A

# Melhores modelos
# Arvore Decisão accuracy: 70.74 %
# Rede Neuronal (nodes=1) accuracy: 71.06 %
numnodes <- 1
cvf <- 10
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)
metrics <- matrix(nrow = cvf, ncol = 8)

for (i in 1:cvf) {
  # Divisão da amostra
  train.cv <- normalized_dataset[folds != i, ]
  test.cv <- normalized_dataset[folds == i, ]
  
  # Árvore de decisão
  rpart.model <- rpart(Pro.level ~ ., method="class" , data = train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  cfmattree <- table(test.cv$Pro.level,rpart.pred)
  
  # Rede neuronal
  nn.model <- neuralnet(Pro.level ~ ., data = train.cv, hidden = numnodes ,stepmax = 1e6 )
  nn.pred <- compute(nn.model, test.cv[, -which(names(normalized_dataset) == column_name)])
  predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)  
  cfmatnn <- table(predicted_labels, test.cv$Pro.level)
  
  accuracy[i, ] <- c( sum(diag(cfmattree))/sum(cfmattree)*100,
                      sum(diag(cfmatnn))/sum(cfmatnn)*100) 
  
  tree_results <-  parse_results(cfmattree)
  nn_results <- parse_results(cfmatnn)
  
  metrics[i, ] <- c(tree_results$accuracy,tree_results$`recall/sensitivity`,tree_results$specificity,tree_results$F1,nn_results$accuracy,nn_results$`recall/sensitivity`,nn_results$specificity,nn_results$F1)
  
}



# Média Accuracy
apply(accuracy, 2, mean) 
# Árvore de decisão = 68.72911    
# Rede neuronal = 69.43564

# Desvio padrão
apply(accuracy, 2, sd)
# Árvore de decisão = 3.153826  
# Rede neuronal = 6.000584

# O modelo Rede Neuronal tem uma precisão ligeiramente
# superior ao modelo Árvore Decisão
# Os dados da Árvore decisão são mais consistentes e
# variam menos que os da Rede neuronal



#C
# H0: os modelos tem desempenho iguais 
# H1: os modelos tem desempenho diferentes
model_test <- t.test(accuracy[, 1], accuracy[, 2])
p_value <- model_test$p.value
alpha <- 0.05

p_value



#D)

# resultados resultantes do ex do k fold
medidas <- matrix(nrow = 2, ncol = 4)

medidas[1,1] <- mean(metrics[,1])
medidas[1,2] <- mean(metrics[,2])
medidas[1,3] <- mean(metrics[,3])
medidas[1,4] <- mean(metrics[,4])
medidas[2,1] <- mean(metrics[,5])
medidas[2,2] <- mean(metrics[,6])
medidas[2,3] <- mean(metrics[,7])
medidas[2,4] <- mean(metrics[,8])

rownames(medidas) <- c("TREE", "NN")
colnames(medidas) <- c("Accuracy", "Sensitivity", "Precision", "F1")

View(medidas)
#------------------------------------------------------------------------------------------------------

#EX2



# Árvore de decisão

# Verificação dos dados antes da normalização
summary(dataset$Winter.Training.Camp)

# Após normalização - valores iguais -> não se normaliza 
summary(normalized_dataset$Winter.Training.Camp)

# Separação do dataset em dois subconjuntos (70% treino/30% teste)
sample = sample(1:nrow(dataset), 0.7 * nrow(dataset))
winterTrainingCamp.train = dataset[sample,]
winterTrainingCamp.test = dataset[-sample,]

# Criação do modelo da árvore de decisão
tree.model = rpart(Winter.Training.Camp ~ . , data = winterTrainingCamp.train, method = "class")

# Visualização da árvore de decisão
library(rattle)
fancyRpartPlot(tree.model)

# Previsões no conjunto de teste
tree.pred = predict(tree.model, winterTrainingCamp.test, type = 'class')

# Matriz de confusão - Avaliação do desempenho do modelo
m.conf <- table(winterTrainingCamp.test$Winter.Training.Camp, tree.pred)
acc <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
cat("Accuracy: ", acc, "%") # 69.67 %

#plot(tree.pred)

# Rede neuronal

# Separação do dataset em dois subconjuntos (70% treino/30% teste)
index <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))

data.train <- normalized_dataset[index,]
data.test <- normalized_dataset[-index,]

print('treino: ')
summary(data.train$Winter.Training.Camp)
print('teste: ')
summary(data.test$Winter.Training.Camp)

#-------------------------------
# 2 internal nodes
numnodes <- 2
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
#nn.pred <- compute(nn.model, data.test)
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 71.33 %

#-------------------------------
# 2 internal levels: 6,2 nodes
numnodes <- c(6, 2)
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24]) # 24: número da coluna Winter.Training.Camp 
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 63.67 %

#-------------------------------
# 1 internal node
numnodes <- 1

nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )
plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 74.33 %

# Desnormalização
nn.pred.winterTrainingCamp <- minmaxdesnorm(nn.pred$net.result, dataset$Winter.Training.Camp)
test.winterTrainingCamp <- minmaxdesnorm(data.test$Winter.Training.Camp, dataset$Winter.Training.Camp)

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

# Cálculo do RMSE 
RMSE(nn.pred.winterTrainingCamp, test.winterTrainingCamp) # 0.4142704

#A)
# K-folds

# Número de folds 
k <- 10 

# Gera array com o número de folds para cada amostra
folds <- sample(1:k, nrow(normalized_dataset), replace = TRUE)

# Tamanho de cada fold
table(folds)

# Modelo apresenta melhor accuracy com 1 nó
numnodes = 1

# Matrizes para armazenar os resultados das matrizes de confusão
conf_matrix_tree_cumulative <- matrix(0, nrow = 2, ncol = 2)
conf_matrix_nn_cumulative <- matrix(0, nrow = 2, ncol = 2)

# Listas para armazenar as taxas de acerto
tree_accuracy_list <- c() 
nn_accuracy_list <- c() 

for (i in 1:k) {
  # Divisão da amostra
  tree_train.cv <- dataset[folds != i, ]
  tree_test.cv <- dataset[folds == i, ]
  
  train.cv <- normalized_dataset[folds != i, ]
  test.cv <- normalized_dataset[folds == i, ]
  
  # Árvore de decisão
  rpart.model <- rpart(Winter.Training.Camp ~ . , data = tree_train.cv, method = "class")
  rpart.pred <- predict(rpart.model, tree_test.cv , type = 'class')
  
  # Matriz de confusão
  conf_matrix_tree <- table(tree_test.cv$Winter.Training.Camp, rpart.pred)
  conf_matrix_tree_cumulative <- conf_matrix_tree_cumulative + conf_matrix_tree
  
  # Cálculo da taxa de acerto
  tree_accuracy <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
  tree_accuracy_list <- c(tree_accuracy_list, tree_accuracy) # Armazenar a taxa de acerto numa lista
  
  ###########################################
  
  # Rede neuronal
  nn.model <- neuralnet(Winter.Training.Camp ~ ., data = train.cv, hidden = numnodes, stepmax = 1e6)
  nnet.pred <- compute(nn.model, test.cv[, -24])
  
  # Matriz de confusão
  predicted_classes <- ifelse(nnet.pred$net.result > 0.5, 1, 0) 
  conf_matrix_nn <- table(predicted_classes, test.cv$Winter.Training.Camp)
  conf_matrix_nn_cumulative <- conf_matrix_nn_cumulative + conf_matrix_nn
  
  # Cálculo da taxa de acerto
  nn_accuracy <- sum(diag(conf_matrix_nn)) / sum(conf_matrix_nn)
  nn_accuracy_list <- c(nn_accuracy_list, nn_accuracy)
  
  # Desnormalização
  nnet.pred <- minmaxdesnorm(nnet.pred$net.result, dataset$Winter.Training.Camp)
  test.cv$Winter.Training.Camp <- minmaxdesnorm(test.cv$Winter.Training.Camp, dataset$Winter.Training.Camp)
}

# Cálculo da média e do desvio padrão da árvore de decisão
tree_mean_accuracy <- mean(tree_accuracy_list) # 0.6974917
tree_sd_accuracy <- sd(tree_accuracy_list) #  0.03334006

# Cálculo da média e do desvio padrão da rede neuronal
nn_mean_accuracy <- mean(nn_accuracy_list) # 0.7203828
nn_sd_accuracy <- sd(nn_accuracy_list) # 0.03801747

#B)

# H0: os modelos são significativamente iguais 
# H1: os modelos são significativamente diferentes

# Teste Paramétrico
t.test(tree_accuracy_list, nn_accuracy_list) # p-value = 0.1697
# Como p-value > alfa (0.05) não se rejeita a hipótese nula

#C)

# Função para as medidas de avaliação: taxa de acerto (accuracy), 
# recall/sensitivity, specificity e F1 
parse_results <- function(m.conf) {
  accuracy <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  recall = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  specificity <- m.conf[2, 2] / (m.conf[2, 2] + m.conf[2, 1])
  f1 <- (2 * recall * specificity) / (recall + specificity)
  
  message("accuracy: ", accuracy, "%")
  message("sensitivity: ", recall)
  message("specificity: ", specificity)
  message("F1: ", f1)
  
  my_list <-
    list(
      "F1" = f1,
      "specificity" = specificity,
      "recall/sensitivity" = recall,
      "accuracy" = accuracy
    )
  return(my_list)
}       

# Desempenho da árvore de decisão
tree_results <- parse_results(conf_matrix_tree_cumulative)
message("Decision Tree Perfomance:")
print(tree_results)
# F1 = 0.4960386
# specificity = 0.8834356
# recall/sensitivity = 0.3448276
# accuracy = 69.6 %

# Desempenho da rede neuronal
nn_results <- parse_results(conf_matrix_nn_cumulative)
cat("Neural Network Perfomance:")
print(nn_results)
# F1 = 0.6893705
# specificity = 0.7447368
# recall/sensitivity` = 0.6416667
# accuracy = 72 %


#-----------------------------------------------------------------------------


#EX3
set.seed(42)
index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))

data.train <- data.norm[index, -which(names(data.norm) == "gender")]
data.test <- data.norm[-index,-which(names(data.norm) == "gender")]

train_labels <- data.norm[index,"gender"]
test_labels <- data.norm[-index,"gender"]

# procurar o k para o min rmse 
k <- c()
rmse <- c()
for (i in seq(1, 50, 2)) {
  knnreg.pred <- knn.reg(data.train, data.test, train_labels, k = i)
  
  knnreg.pred$pred <- minmaxdesnorm(knnreg.pred$pred, dataset$gender)
  
  rmse <-
    c(rmse, RMSE(knnreg.pred$pred,
                 minmaxdesnorm(test_labels, dataset$gender)))
  
  k <- c(k, i)
}

#Min RMSE
resNeigh <- data.frame(k, rmse)
resNeigh[resNeigh$rmse == min(resNeigh$rmse), ]

#Color in red the lowest RMSE
plot(resNeigh$k,
     resNeigh$rmse,
     col = ifelse(
       resNeigh$rmse == min(resNeigh$rmse),
       'orangered1',
       'steelblue4'
     ))
#k=21 rmse=0.5008157
#accuracy

#procurar k para max accuracy
k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){   
  
  knn.pred <- knn(train=data.train, test=data.test, cl=train_labels, k=i) 
  
  cfmatrix <- table(test_labels,knn.pred)
  
  
  accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
  
  
  k <- c(k,i)
}
resNeigh<-data.frame(k,accuracy)

#Max accuracy
resNeigh[resNeigh$accuracy==max(resNeigh$accuracy), ]   
plot(resNeigh$k,resNeigh$accuracy)


#Plot Max accuracy
resNeigh[resNeigh$accuracy == max(resNeigh$accuracy), ]
k[which.max(accuracy)]
plot(
  resNeigh$k,
  resNeigh$accuracy,
  col = ifelse(
    resNeigh$accuracy == max(resNeigh$accuracy),
    'orangered1',
    'steelblue4'
  )
)

#k=17 accuracy=0.58

#NN
# numero de hidden nodes igual a 2, foi testada outros valores n igual a 3/5 e tamebm para c(6,2) e davam todos menor que 2
index <- sample(1:nrow(data.norm), 0.7 * nrow(data.norm))
data.train <- data.norm[index,  ]
data.test <- data.norm[-index,  ]

numnodes <- 2

nn.model <- neuralnet(gender ~ ., data = data.train, hidden = numnodes ,stepmax = 1e6 )

plot(nn.model)
nn.pred <- compute(nn.model, data.test[,  -which(names(data.norm) == "gender")])

#nn.pred.gender <- minmaxdesnorm(nn.pred$net.result, dataset$gender)
#test.gender  <- minmaxdesnorm(data.test$gender, dataset$gender)
#RMSE(nn.pred.gender, test.gender)

predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)  # Convert probabilities to class labels

cfmatrix2 <- table(predicted_labels,  data.test$gender)
nn_accuracy <- sum(diag(cfmatrix2) / sum(cfmatrix2)) * 100
#90%

#A
#fold com k igual a 17 e n igual a 2
cvf <- 10
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)
k <- 17
other <- matrix(nrow = cvf, ncol = 8)

for (i in 1:cvf){
  
  train.cv <- data.norm[folds != i, -which(names(data.norm) == "gender")]
  test.cv <- data.norm[folds == i, -which(names(data.norm) == "gender")]
  
  train1.cv <- data.norm[folds != i, ]
  test1.cv <- data.norm[folds == i, ]
  
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  knn.pred <- knn(train=train.cv, test=test.cv, cl=train_labels, k) 
  cfmatknn <- table(tst_labels,knn.pred)
  
  nn.model <- neuralnet(gender ~ ., data = train1.cv, hidden = 2 ,stepmax = 1e6 )
  
  #plot(nn.model)
  nn.pred <- compute(nn.model, test1.cv[, -which(names(data.norm) == "gender")])
  
  predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)  # Convert probabilities to class labels
  
  cfmatnn <- table(predicted_labels, test1.cv$gender)
    
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn)*100,
                      sum(diag(cfmatnn))/sum(cfmatnn)*100) 
  
  knn_results <-  parse_results(cfmatknn)
  nn_results <- parse_results(cfmatnn)
  
  other[i, ] <- c(knn_results$accuracy,knn_results$`recall/sensitivity`,knn_results$specificity,knn_results$F1,nn_results$accuracy,nn_results$`recall/sensitivity`,nn_results$specificity,nn_results$F1)
  
}

#accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)


# Media Knn = 54.861%
# Desvio Knn = 4.259%
# Media nn = 86.980%
# Desvio nn = 2.564%
# ambos modelos tem desvio pardao parecido a rondar os 2.6%-4.3% mas na accuracy o NN e melhor que KNN 


#B
# H0: os modelos sÃ£o significativamente iguais 
# H1: os modelos sÃ£o significativamente diferentes

t.test(accuracy[, 1], accuracy[, 2])  #p-value = 3.421e-11
# Como o p-value Ã© inferior a alfa (0.05) rejeitamos a hipÃ³tese nula. 
# Ou seja, os modelos os modelos sÃ£o significativamente diferentes

#C

# resultados resultantes do ex do k fold
medidas3 <- matrix(nrow = 2, ncol = 4)

medidas3[1,1] <- mean(other[,1])
medidas3[1,2] <- mean(other[,2])
medidas3[1,3] <- mean(other[,3])
medidas3[1,4] <- mean(other[,4])
medidas3[2,1] <- mean(other[,5])
medidas3[2,2] <- mean(other[,6])
medidas3[2,3] <- mean(other[,7])
medidas3[2,4] <- mean(other[,8])

rownames(medidas3) <- c("KNN", "NN")
colnames(medidas3) <- c("Accuracy em %", "Sensitivity", "Sepecificity", "F1")

View(medidas3)


