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
set.seed(42)
#EX1

#get current directory
getwd()
#set current working directory to a Desktop directory
setwd("C:/Users/rocha/Downloads/ANADI/tp2_2022/")

# Carregar o arquivo "ciclismo.csv"
dados <- read.csv("ciclismo.csv",stringsAsFactors=TRUE)

#EX2

dados$dob <- as.Date(dados$dob)

# Calcular a idade com base na data de nascimento
dados <- dados %>%
  mutate(Age = as.integer(Sys.Date() - dob) %/% 365)
# EX4

#A
missing_values <- sum(is.na(dados))
dataset <- na.omit(dados)

dados <- dados %>%
  mutate(Age = as.integer(Sys.Date() - dob) %/% 365)

missing_values <- sum(is.na(dados))
dataset <- na.omit(dados)


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

columns_to_bind <- c("vo2_results", "hr_results", "altitude_results", "Age", "gender", "Winter.Training.Camp", "Pro.level")

additional_data <- dataset[columns_to_bind]

dataset <- cbind(data_bin, additional_data)

# Transforma colunas Dummy em 1 e 2
dataset$Pro.level <- as.numeric(as.factor(dataset$Pro.level))
dataset$gender <- as.numeric(as.factor(dataset$gender))
dataset$Winter.Training.Camp <- as.numeric(as.factor(dataset$Winter.Training.Camp))

#D
# Denormalize the predicted values
minmaxdesnorm <- function(x,goal.attrib) {
  return (x*(max(goal.attrib)-min(goal.attrib))+min(goal.attrib))
}
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}


######################################################

#4.2

# Função para as medidas de avaliação: taxa de acerto (accuracy), 
# recall/sensitivity, precision e F1 
parse_results <- function(m.conf) {
  accuracy <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  recall = m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  precision = m.conf[1, 1] / (m.conf[1, 1] + m.conf[2, 1])
  f1 = (2 * precision * recall) / (precision + recall)

  
  my_list <-
    list(
      "F1" = f1,
      "precision" = precision,
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

set.seed(42)

normalized_dataset <- as.data.frame(lapply(dataset, min_max_normalize))

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
  
  metrics[i, ] <- c(tree_results$accuracy,tree_results$`recall/sensitivity`,tree_results$precision,tree_results$F1,nn_results$accuracy,nn_results$`recall/sensitivity`,nn_results$precision,nn_results$F1)
  
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
# H0: os modelos são significativamente iguais 
# H1: os modelos são significativamente diferentes
model_test <- t.test(accuracy[, 1], accuracy[, 2])
p_value <- model_test$p.value
alpha <- 0.05


if (p_value <= alpha) {
  hypothesis_result <- glue("Como o p_value < alpha, rejeitamos a hipótese nula. Ou seja, os modelos são significativamente diferentes")
} else {
  hypothesis_result <- glue("Como o p_value > alpha, não rejeitamos a hipótese nula. Ou seja, os modelos são significativamente iguais")
}

p_value
hypothesis_result


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


