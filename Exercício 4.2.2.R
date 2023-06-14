#EX2

# Árvore de decisão
set.seed(42)
sample = sample(1:nrow(dataset), 0.7 * nrow(dataset))
winterTrainingCamp.train = dataset[sample,]
winterTrainingCamp.test = dataset[-sample,]

dim(winterTrainingCamp.train)
dim(winterTrainingCamp.test)

# Criação do modelo da árvore de decisão
library(rpart)
library(rpart.plot)

tree.model = rpart(Winter.Training.Camp ~ . , data = winterTrainingCamp.train, method = "class")

library(rattle)
fancyRpartPlot(tree.model)

# Previsões no conjunto de teste
tree.pred = predict(tree.model, winterTrainingCamp.test, type = 'class')

# Matriz de confusão - Avaliando o desempenho do modelo
m.conf <- table(winterTrainingCamp.test$Winter.Training.Camp, tree.pred)
acc <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
cat("Accuracy: ", acc, "%") # 70.67 %

plot(tree.pred)


# Rede neuronal

# Verificação de dados antes da normalização
summary(dataset$Winter.Training.Camp)

# Normalização
data.norm <- as.data.frame(lapply(dataset, min_max_normalize ))

# Após normalização (agora entre 0 e 1)
summary(data.norm$Winter.Training.Camp)
summary(data.norm)

index <- sample(1:nrow(data.norm), 0.7 * nrow(data.norm))
data.train <- data.norm[index,]
data.test <- data.norm[-index,]

print('treino: ')
summary(data.train$Winter.Training.Camp)
print('teste: ')
summary(data.test$Winter.Training.Camp)

numnodes <- 2
nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes, stepmax = 1e6 )

plot(nn.model)

# Performance do modelo
nn.pred <- compute(nn.model, data.test[, -4])

# Desnormalização
nn.pred.winterTrainingCamp <- minmaxdesnorm(nn.pred$net.result, dataset$Winter.Training.Camp)
test.winterTrainingCamp <- minmaxdesnorm(data.test$Winter.Training.Camp, dataset$Winter.Training.Camp)

RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

RMSE(nn.pred.winterTrainingCamp, test.winterTrainingCamp)

# Matriz de confusão - Avaliando o desempenho do modelo
cfmatrix <- table(nn.pred.winterTrainingCamp, test.winterTrainingCamp)
nn_accuracy <- sum(diag(cfmatrix)) / sum(cfmatrix) * 100 # 33.3%


#A) 
# K-folds

# Dados normalizados
data <- data.norm

# Número de folds 
k <- 10 

# Gera array contendo número de folds para cada amostra
folds <- sample(1:k, nrow(data), replace = TRUE)

numnodes = 2

# Tamanho de cada fold
table(folds)

# Matriz com nrFolds linhas e 2 colunas (2 modelos)
cv.error <- matrix(nrow = k, ncol = 2)

for (i in 1:k) {
  # Divisão da amostra
  train.cv <- data.norm[folds != i, ]
  test.cv <- data.norm[folds == i, ]
  
  # Árvore de decisão
  rpart.model <- rpart(Winter.Training.Camp ~ . , data = train.cv)
  rpart.pred <- predict(rpart.model, newdata = test.cv)
  rpart.pred <- minmaxdesnorm(rpart.pred, dataset$Winter.Training.Camp)
  
  # Rede neuronal
  nn.model <- neuralnet(Winter.Training.Camp ~ ., data = train.cv, hidden = numnodes, stepmax = 1e6)
  nnet.pred <- compute(nn.model, test.cv[, -4])
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

wilcox.test(cv.error[, 1], cv.error[, 2]) # p-value = 0.02323
# Como o p-value é inferior a alfa (0.05) rejeitamos a hipótese nula. 
# Ou seja, os modelos os modelos são significativamente diferentes

friedman.test(cv.error) #p-value = 0.05778

# Teste Paramétrico
t.test(cv.error[, 1], cv.error[, 2]) # p-value = 0.01187


#C)

# Árvore de decisão

sample <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
data.train <- dataset[sample, ] 
data.test <- dataset[!sample, ]

tree.model <- rpart(Winter.Training.Camp ~ ., data = data.train, method = "class")

tree.pred <- predict(tree.model, newdata = data.test, type = "class")
tree_cfmatrix <- table(tree.pred, data.test$Winter.Training.Camp)

# Cálculo dos critérios para a árvore de decisão
tree_accuracy <- sum(diag(tree_cfmatrix)) / sum(tree_cfmatrix) # Accuracy
tree_sensitivity <- tree_cfmatrix[2, 2] / sum(tree_cfmatrix[2, ]) # Sensitivity
tree_specificity <- tree_cfmatrix[1, 1] / sum(tree_cfmatrix[1, ]) # Specificity
tree_f1 <- 2 * tree_sensitivity * tree_specificity / (tree_sensitivity + tree_specificity) # F1

cat("Árvore de Decisão:\n")
cat("Accuracy:", tree_accuracy, "\n") # Accuracy: 0.6642599 
cat("Sensitivity:", tree_sensitivity, "\n") # Sensitivity: 0.6808511
cat("Specificity:", tree_specificity, "\n") # Specificity: 0.5714286 
cat("F1:", tree_f1, "\n\n") # F1: 0.6213592 


# Rede Neuronal

sample <- sample(c(TRUE, FALSE), nrow(data.norm), replace=TRUE, prob=c(0.7,0.3))
data.train <- data.norm[sample, ]
data.test <- data.norm[!sample, ]

nn.model <- neuralnet(Winter.Training.Camp ~ ., data = data.train, hidden = numnodes <- 2, stepmax = 1e6)

#plot(nn.model)
nn.pred <- compute(nn.model, data.test[, -4])

nn.pred.winterTrainingCamp <- minmaxdesnorm(nn.pred$net.result, dataset$Winter.Training.Camp)
test.winterTrainingCamp  <- minmaxdesnorm(data.test$gender, dataset$Winter.Training.Camp)

RMSE(nn.pred.winterTrainingCamp, test.winterTrainingCamp)

nn_cfmatrix <- table(nn.pred.winterTrainingCamp, test.winterTrainingCamp)

# Cálculo dos critérios para a rede neuronal
nn_accuracy <- sum(diag(nn_cfmatrix)) / sum(nn_cfmatrix) # Accuracy
nn_sensitivity <- nn_cfmatrix[2, 2] / sum(nn_cfmatrix[2, ]) # Sensitivity
nn_specificity <- nn_cfmatrix[1, 1] / sum(nn_cfmatrix[1, ]) # Specificity
nn_f1 <- 2 * nn_sensitivity * nn_specificity / (nn_sensitivity + nn_specificity) # F1

cat("Rede Neural:\n")
cat("Accuracy:", nn_accuracy, "\n") # Accuracy: 0.003448276 
cat("Sensitivity:", nn_sensitivity, "\n") # Sensitivity: 0 
cat("Specificity:", nn_specificity, "\n") # Specificity: 1 
cat("F1:", nn_f1, "\n") # F1: 0 


