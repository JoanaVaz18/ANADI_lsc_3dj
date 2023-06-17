# EX 4.2.2

# Cálculo da accuracy
calculate_accuracy <- function(predictions, actual_values) {
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)  # Assuming binary classification with threshold 0.5
  accuracy <- sum(predicted_classes == actual_values) / length(actual_values) * 100
  return(accuracy)
}

# Árvore de decisão

# Verificação dos dados antes da normalização
summary(dataset$Winter.Training.Camp)

# Após normalização - valores iguais -> não se normaliza 
summary(normalized_dataset$Winter.Training.Camp)

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
cat("Accuracy: ", acc, "%") # 69.33 %

plot(tree.pred)

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

# Accuracy = 70.33 %

#-------------------------------
# 2 internal levels: 6,2 nodes
numnodes <- c(6, 2)
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24]) # 24: número da coluna Winter.Training.Camp 
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 65 %

#-------------------------------
# 1 internal node
numnodes <- 1

nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )
plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[,-24])
calculate_accuracy(nn.pred$net.result, data.test$Winter.Training.Camp)

# Accuracy = 72.33 %

# Desnormalização
nn.pred.winterTrainingCamp <- minmaxdesnorm(nn.pred$net.result, dataset$Winter.Training.Camp)
test.winterTrainingCamp <- minmaxdesnorm(data.test$Winter.Training.Camp, dataset$Winter.Training.Camp)

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

# Cálculo do RMSE 
RMSE(nn.pred.winterTrainingCamp, test.winterTrainingCamp) # 0.4147293

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
tree_mean_accuracy <- mean(tree_accuracy_list) # 0.6890901
tree_sd_accuracy <- sd(tree_accuracy_list) # 0.04483996

# Cálculo da média e do desvio padrão da rede neuronal
nn_mean_accuracy <- mean(nn_accuracy_list) # 0.7114371
nn_sd_accuracy <- sd(nn_accuracy_list) # 0.04545551

#B)

# H0: os modelos são significativamente iguais 
# H1: os modelos são significativamente diferentes

# Teste Paramétrico
t.test(tree_accuracy_list, nn_accuracy_list) # p-value = 0.2051
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
# F1 = 0.479943
# specificity = 0.8972393
# recall/sensitivity = 0.3275862
# accuracy = 69.9 %

# Desempenho da rede neuronal
nn_results <- parse_results(conf_matrix_nn_cumulative)
cat("Neural Network Perfomance:")
print(nn_results)
# F1 = 0.6940992
# specificity = 0.7434896
# recall/sensitivity` = 0.6508621
# accuracy = 72.2 %
