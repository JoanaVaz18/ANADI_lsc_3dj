# EX 4.2.2

# Cálculo da accuracy
calculate_accuracy <- function(predictions, actual_values) {
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Assuming binary classification with threshold 0.5
  accuracy <- sum(predicted_classes == actual_values) / length(actual_values) * 100
  return(accuracy)
}

# Árvore de decisão
sample = sample(1:nrow(dataset), 0.7 * nrow(dataset))
winterTrainingCamp.train = dataset[sample,]
winterTrainingCamp.test = dataset[-sample,]

# Criação do modelo da árvore de decisão
tree.model = rpart(Winter.Training.Camp ~ . , data = winterTrainingCamp.train, method = "class")

# Visualização da árvore de decisão
library(rattle)
fancyRpartPlot(tree.model)

# Fazendo previsões no conjunto de teste
tree.pred = predict(tree.model, winterTrainingCamp.test, type = 'class')

# Matriz de confusão - Avaliação do desempenho do modelo
m.conf <- table(winterTrainingCamp.test$Winter.Training.Camp, tree.pred)
acc <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
cat("Accuracy: ", acc, "%") # 69.33 %
parse_results(m.conf);

plot(tree.pred)


# Rede neuronal

index <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))

data.train <- normalized_dataset[index,]
data.test <- normalized_dataset[-index,]

print('treino: ')
summary(data.train$Winter.Training.Camp)
print('teste: ')
summary(data.test$Winter.Training.Camp)

#-------------------------------
# 1 internal node
numnodes <- 1

#nn.model <-
#  neuralnet(
#    Winter.Training.Camp ~ BackgroundCobblestones + BackgroundHill + BackgroundMountain + BackgroundNone + BackgroundSprinter + BackgroundTime.Trial + ContinentAfrica + 
#      ContinentAsia + ContinentAustralia + ContinentEurope + ContinentNorth.America + ContinentSouth.America + Teamgroup.A + Teamgroup.B + Teamgroup.C + Teamgroup.D +
#      Teamgroup.E + vo2_results + hr_results + altitude_results + Age + Pro.level + gender,
#    data = data.train,
#    hidden = numnodes, 
#    stepmax = 1e7
#  )

nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )
plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 74.67 %

#-------------------------------
# 2 internal nodes
numnodes <- 2
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
#nn.pred <- compute(nn.model, data.test)
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 72 %

#-------------------------------
# 2 internal levels: 6,2 nodes
numnodes <- c(6, 2)
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 63.33 %

#############################################
# Cálculo da accuracy

# Extração dos valores previstos
predictions <- nn.pred$net.result
predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Assuming binary classification with threshold 0.5

# Extração dos actual values
actual_values <- data.test$Winter.Training.Camp

# Matriz de confusão
cfmatrix <- table(predicted_classes, actual_values)
nn_accuracy <- sum(diag(cfmatrix)) / sum(cfmatrix) * 100

# Avaliação do desempenho do modelo
parse_results(cfmatrix)

# Cálculo da accuracy
accuracy <- sum(predicted_classes == actual_values) / length(actual_values)
accuracy

############################################

# Desnormalização
nn.pred.winterTrainingCamp <- minmaxdesnorm(nn.pred$net.result, dataset$Winter.Training.Camp)
test.winterTrainingCamp <- minmaxdesnorm(data.test$Winter.Training.Camp, dataset$Winter.Training.Camp)

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

RMSE(nn.pred.winterTrainingCamp, test.winterTrainingCamp) # 0.4147293

# Matriz de confusão - Avaliação do desempenho do modelo
#cfmatrix <- table(nn.pred.winterTrainingCamp, test.winterTrainingCamp)
#nn_accuracy <- sum(diag(cfmatrix)) / sum(cfmatrix) * 100 # 33.3%
#parse_results(cfmatrix);

#A)
# K-folds

# Número de folds 
k <- 10 

# Gera array com o número de folds para cada amostra
folds <- sample(1:k, nrow(normalized_dataset), replace = TRUE)

# Modelo apresenta melhor accuracy com 1 nó
numnodes = 1

# Tamanho de cada fold
table(folds)

# Matriz com nrFolds linhas e 2 colunas (2 modelos)
cv.error <- matrix(nrow = k, ncol = 2)

for (i in 1:k) {
  # Divisão da amostra
  tree_train.cv <- dataset[folds != i, ]
  tree_test.cv <- dataset[folds == i, ]
  
  train.cv <- normalized_dataset[folds != i, ]
  test.cv <- normalized_dataset[folds == i, ]
  
  # Árvore de decisão
  rpart.model <- rpart(Winter.Training.Camp ~ . , data = tree_train.cv)
  rpart.pred <- predict(rpart.model, newdata = tree_test.cv)
  #rpart.pred <- minmaxdesnorm(rpart.pred, dataset$Winter.Training.Camp)
  
  # Rede neuronal
  nn.model <- neuralnet(Winter.Training.Camp ~ ., data = train.cv, hidden = numnodes, stepmax = 1e6)
  
  nnet.pred <- compute(nn.model, test.cv[, -24])
  
  # Desnormalização
  nnet.pred <- minmaxdesnorm(nnet.pred$net.result, dataset$Winter.Training.Camp)
  test.cv$Winter.Training.Camp <- minmaxdesnorm(test.cv$Winter.Training.Camp, dataset$Winter.Training.Camp)
  
  cv.error[i,] <- c(
    RMSE(rpart.pred, test.cv$Winter.Training.Camp),
    RMSE(nnet.pred, test.cv$Winter.Training.Camp)
  )
}

cv.error

# Média 
apply(cv.error, 2, mean) 
# Árvore de decisão = 0.4615854 
# Rede neuronal = 0.4352531

# Desvio padrão
apply(cv.error, 2, sd)
# Árvore de decisão = 0.01346129
# Rede neuronal = 0.02536660


#B)

# H0: os modelos são significativamente iguais 
# H1: os modelos são significativamente diferentes

# Teste Paramétrico
t.test(cv.error[, 1], cv.error[, 2]) # p-value = 1.016e-07


#C)

# Função para as medidas de avaliação: taxa de acerto (accuracy), 
# recall/sensitivity, precision e F1 
parse_results <- function(m.conf) {
  accuracy <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  recall = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  precision = m.conf[1, 1] / (m.conf[1, 1] + m.conf[2, 1])
  f1 = (2 * precision * recall) / (precision + recall)
  
  message("accuracy: ", accuracy, "%")
  message("Recall: ", recall)
  message("precision: ", precision)
  message("F1: ", f1)
  
  my_list <-
    list(
      "F1" = f1,
      "precision" = precision,
      "recall/sensitivity" = recall,
      "accuracy" = accuracy
    )
  return(my_list)
}
