

#EX3


RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}


set.seed(123)
index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
data.train <- normalized_dataset[index, ]
data.test <- normalized_dataset[-index, ]

train_labels <- normalized_dataset[index,"gender"]
test_labels <- normalized_dataset[-index,"gender"]

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

#accuracy

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

#100%



#NN

set.seed(123)
index <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))
data.train <- normalized_dataset[index, ]
data.test <- normalized_dataset[-index, ]

train_labels <- normalized_dataset[index,"gender"]
test_labels <- normalized_dataset[-index,"gender"]


nn.model <- neuralnet(gender ~ ., data = data.train, hidden = numnodes <- 2,stepmax = 1e6 )

#plot(nn.model)
nn.pred <- compute(nn.model, data.test[, -1])

nn.pred.gender <- minmaxdesnorm(nn.pred$net.result, dataset$gender)
test.gender  <- minmaxdesnorm(data.test$gender, dataset$gender)

RMSE(nn.pred.gender, test.gender)

cfmatrix2 <- table(nn.pred.gender, test.gender )
nn_accuracy <- sum(diag(cfmatrix2)) / sum(cfmatrix2) * 100
#33%

#A

cvf <- 10
folds <- sample(1:cvf, nrow(dataset), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)
k <- 20
ncols <- dim(dataset)[2]

for (i in 1:cvf){
  
  train.cv <- normalized_dataset[folds != i, ]
  test.cv <- normalized_dataset[folds == i, ]
  
  train_labels <- dataset[folds != i, "gender"]
  tst_labels <- dataset[folds == i, "gender"]
  
  knn.pred <- knn(train=train.cv[,-ncols], test=test.cv[,-ncols], cl=train_labels, k) 
  cfmatknn <- table(tst_labels,knn.pred)
  
  nn.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes <- 2 )
  
  #plot(nn.model)
  nn.pred <- compute(nn.model, test.cv[, -1])
  
  nn.pred.gender <- minmaxdesnorm(nn.pred$net.result, dataset$gender)
  test.gender  <- minmaxdesnorm(test.cv$gender, dataset$gender)
  
  cfmatnn <- table(nn.pred.gender,test.gender)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatnn))/sum(cfmatnn)) 
}

#accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)




#B
# H0: modelos são significativamente iguais VS H1: modelos são significativamente diferentes
# alfa = 5%
#p-value = 6.34e-05
# Como o p-value é inferior a alfa rejeitamos a hipótese nula. Ou seja, os modelos nao são significativamente iguais 
wilcox.test(accuracy[, 1], accuracy[, 2],exact = FALSE)


friedman.test(accuracy)
#p-value = 0.001565

#c

##K
index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
data.train <- normalized_dataset[index, ]
data.test <- normalized_dataset[-index, ]

train_labels <- normalized_dataset[index,"gender"]
test_labels <- normalized_dataset[-index,"gender"]

knn.pred <- knn(train=data.train, test=data.test, cl=train_labels, k=10) 

knn_cfmatrix <- table(test_labels,knn.pred)
#knn_predictions <- as.factor(knn_model)
#knn_cfmatrix <- table(knn_predictions, test_data$gender)

# Calcular métricas para k-NN
knn_accuracy <- sum(diag(knn_cfmatrix)) / sum(knn_cfmatrix)  # Accuracy
knn_sensitivity <- knn_cfmatrix[2, 2] / sum(knn_cfmatrix[2, ])  # Sensitivity
knn_specificity <- knn_cfmatrix[1, 1] / sum(knn_cfmatrix[1, ])  # Specificity
knn_f1 <- 2 * knn_sensitivity * knn_specificity / (knn_sensitivity + knn_specificity)  # F1 score

# Imprimir resultados
cat("k-NN:\n")
cat("Accuracy:", knn_accuracy, "\n")
cat("Sensitivity:", knn_sensitivity, "\n")
cat("Specificity:", knn_specificity, "\n")
cat("F1 score:", knn_f1, "\n")

# Modelo de Rede Neural

index <- sample(1:nrow(normalized_dataset), 0.7 * nrow(normalized_dataset))
data.train <- normalized_dataset[index, ]
data.test <- normalized_dataset[-index, ]

train_labels <- normalized_dataset[index,"gender"]
test_labels <- normalized_dataset[-index,"gender"]


nn.model <- neuralnet(gender ~ ., data = data.train, hidden = numnodes <- 2,stepmax = 1e6 )

#plot(nn.model)
nn.pred <- compute(nn.model, data.test[, -1])

nn.pred.gender <- minmaxdesnorm(nn.pred$net.result, dataset$gender)
test.gender  <- minmaxdesnorm(data.test$gender, dataset$gender)

RMSE(nn.pred.gender, test.gender)

nn_cfmatrix <- table(nn.pred.gender, test.gender )

# Calcular métricas para Rede Neural
nn_accuracy <- sum(diag(nn_cfmatrix)) / sum(nn_cfmatrix)  # Accuracy
nn_sensitivity <- nn_cfmatrix[2, 2] / sum(nn_cfmatrix[2, ])  # Sensitivity
nn_specificity <- nn_cfmatrix[1, 1] / sum(nn_cfmatrix[1, ])  # Specificity
nn_f1 <- 2 * nn_sensitivity * nn_specificity / (nn_sensitivity + nn_specificity)  # F1 score

cat("Rede Neural:\n")
cat("Accuracy:", nn_accuracy, "\n")
cat("Sensitivity:", nn_sensitivity, "\n")
cat("Specificity:", nn_specificity, "\n")
cat("F1 score:", nn_f1, "\n")