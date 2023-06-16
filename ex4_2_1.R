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


tree_accuracy_funct <- function(column_name, train_data, test_data) {
  decision_tree <- rpart(paste(column_name, "~ ."), data = train_data, method = "class")
  tree_predict <- predict(decision_tree, newdata = test_data, type = "class")
  cfmatrix <- table(tree_predict, test_data[[column_name]])
  return(sum(diag(cfmatrix)) / sum(cfmatrix) * 100) 
}

knn_func <- function(dataset, column_name, data.train, data.test, train_labels, test_labels) {
  k <- c()
  rmse <- c()
  for (i in seq(1, 50, 2)) {
    knnreg.pred <- knn.reg(data.train, data.test, train_labels, k = i)
    
    knnreg.pred$pred <- minmaxdesnorm(knnreg.pred$pred, dataset[[column_name]])
    
    rmse <-
      c(rmse, RMSE(knnreg.pred$pred,
                   minmaxdesnorm(test_labels, dataset[[column_name]])))
    
    k <- c(k, i)
  }
  resNeigh <- data.frame(k, rmse)
  min_rmse <- resNeigh[resNeigh$rmse == min(resNeigh$rmse), ]
  
  k <- c()
  accuracy <- c()
  for (i in seq(1, 50, 2)){   
    
    knn.pred <- knn(train=data.train, test=data.test, cl=train_labels, k=i) 
    
    cfmatrix <- table(test_labels,knn.pred)
    
    accuracy <- c(accuracy, sum(diag(cfmatrix))/sum(cfmatrix))
    
    
    k <- c(k,i)
  }
  
  resNeigh<-data.frame(k,accuracy)
  max_acc <- resNeigh[resNeigh$accuracy==max(resNeigh$accuracy), ] 
  k_max <- k[which.max(accuracy)]
  
  return(list(accuracy = max_acc, rmse = min_rmse))
}

nn_accuracy_func <- function(column, train_data, test_data, nodes) {
  formula <- as.formula(paste(column, "~ ."))
  nn.model <- neuralnet(formula, data = train_data, hidden = nodes, stepmax = 1e6)
  
  nn.pred <- compute(nn.model, test_data[, -which(names(test_data) == column)])
  
  predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)  # Convert probabilities to class labels
  
  accuracy <- sum(predicted_labels == test_data[[column]]) / length(predicted_labels) * 100
  
  denormalized_pred <- minmaxdesnorm(nn.pred$net.result, train_data[, column])
  denormalized_actual <- minmaxdesnorm(test_data[, column], train_data[, column])
  
  rmse <- RMSE(denormalized_pred, denormalized_actual)

  return(list(accuracy = accuracy, rmse = rmse))
}

normalize <- function(data) {
  centered <- scale(data, center = TRUE, scale = FALSE)  # Center the data
  normalized <- scale(centered, center = FALSE, scale = TRUE)  # Scale the data
  return(normalized)
}

set.seed(42)

column_name <- "Pro.level"

sample <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))

norm_train_data_knn  <- normalized_dataset[sample, -which(names(normalized_dataset) == column_name)]
norm_test_data_knn  <- normalized_dataset[-sample, -which(names(normalized_dataset) == column_name)]

norm_train_labels <- normalized_dataset[sample,column_name]
norm_test_labels <- normalized_dataset[-sample,column_name]

knn_info_norm <- knn_func(dataset, column_name, norm_train_data_knn, norm_test_data_knn, norm_train_labels, norm_test_labels)


normalized_dataset <- as.data.frame(lapply(dataset, min_max_normalize))

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
norm_train_data  <- normalized_dataset[sample, ]
norm_test_data  <- normalized_dataset[!sample, ]


nodes <- 5
nn_accuracy_norm <- nn_accuracy_func(column_name, norm_train_data, norm_test_data, nodes)

normalized_dataset <- as.data.frame(lapply(dataset, min_max_normalize))

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
norm_train_data  <- normalized_dataset[sample, ]
norm_test_data  <- normalized_dataset[!sample, ]

tree_accuracy_norm <- tree_accuracy_funct(column_name, norm_train_data, norm_test_data)
tree_accuracy_norm
nn_accuracy_norm
knn_info_norm
