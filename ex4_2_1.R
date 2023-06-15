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
dataset <- dataset[,-2] 
dataset <- dataset[,-8] 

# Specify the column names for which you want to create a model matrix
columns <- c("Background", "Continent")

# Initialize an empty list to store the model matrices
model_matrices <- list()

# Iterate through each column in the dataset
for (col in columns) {
  # Create the model matrix for the current column
  data_matrix <- model.matrix(~ . - 1, data = dataset[col])
  
  # Add the model matrix to the list
  model_matrices[[col]] <- data_matrix
}

# Combine all the model matrices into a single dataframe
data_bin <- do.call(cbind, model_matrices)

# Specify the columns to bind with data_bin
columns_to_bind <- c("vo2_results", "hr_results", "altitude_results", "Age", "Pro.level", "gender", "Winter.Training.Camp")

# Select the columns from the dataset
additional_data <- dataset[columns_to_bind]

# Bind the additional columns with data_bin
dataset <- cbind(data_bin, additional_data)

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




######################################################

#4.2

set.seed(42)
train_indices <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
normalized_dataset <- as.data.frame(lapply(dataset, min_max_normalize))

train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
norm_train_data  <- normalized_dataset[sample, ]
norm_test_data  <- normalized_dataset[!sample, ]

tree_accuracy_funct <- function(train_data, test_data, column_name) {
  decision_tree <- rpart(paste(column_name, "~ ."), data = train_data, method = "class")
  tree_predict <- predict(decision_tree, newdata = test_data, type = "class")
  cfmatrix <- table(tree_predict, test_data[[column_name]])
  return(sum(diag(cfmatrix)) / sum(cfmatrix) * 100) 
}

knn_accuracy_func <- function(train_data, test_data, column_name, neighbors) {
  knn_model <- knn(train = train_data[, -ncol(train_data)],
                   test = test_data[, -ncol(test_data)],
                   cl = train_data[[column_name]],
                   k = neighbors)
  knn_predict <- as.factor(knn_model)
  cfmatrix <- table(knn_predict, test_data[[column_name]])
  return(sum(diag(cfmatrix)) / sum(cfmatrix) * 100)
}

nn_accuracy_func <- function(train_data, test_data, column, nodes) {
  nn.data <- train_data
  nn.data$target <- train_data[, column]  # Create a new column "target" for the target variable
  
  nn.model <- neuralnet(target ~ ., data = nn.data, hidden = numnodes <- nodes)
  
  nn.pred <- compute(nn.model, test_data)
  
  denormalized_pred <- minmaxdesnorm(nn.pred$net.result, train_data[, column])
  denormalized_actual <- minmaxdesnorm(test_data[, column], train_data[, column])
  
  cfmatrix <- table(denormalized_pred, denormalized_actual)
  return(sum(diag(cfmatrix)) / sum(cfmatrix) * 100)
}

normalize <- function(data) {
  centered <- scale(data, center = TRUE, scale = FALSE)  # Center the data
  normalized <- scale(centered, center = FALSE, scale = TRUE)  # Scale the data
  return(normalized)
}

column_name <- "Pro.level"

tree_accuracy <- tree_accuracy_funct(train_data, test_data, column_name)

k_neighbors <- 5
knn_accuracy <- knn_accuracy_func(train_data, test_data, column_name, k_neighbors)


tree_accuracy_norm <- tree_accuracy_funct(norm_train_data, norm_test_data, column_name)

k_neighbors <- 5
knn_accuracy_norm <- knn_accuracy_func(norm_train_data, norm_test_data, column_name, k_neighbors)

nodes <- 5
nn_accuracy_norm <- nn_accuracy_func(norm_train_data, norm_test_data, column_name, nodes)



tree_accuracy
tree_accuracy_norm
knn_accuracy
nn_accuracy_norm
knn_accuracy_norm
