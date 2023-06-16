#EX3

set.seed(42)
minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data.norm <- as.data.frame(lapply(dataset, minmaxnorm))



set.seed(42)
index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))

data.train <- data.norm[index, -which(names(data.norm) == "gender")]
data.test <- data.norm[-index,-which(names(data.norm) == "gender")]

train_labels <- data.norm[index,"gender"]
test_labels <- data.norm[-index,"gender"]

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
#k=49 rmse=0.492
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

#k=17 accuracy=0.58



#NN

set.seed(42)
index <- sample(1:nrow(data.norm), 0.7 * nrow(data.norm))
data.train <- data.norm[index,  ]
data.test <- data.norm[-index,  ]

numnodes <- 2

nn.model <- neuralnet(gender ~ ., data = data.train, hidden = numnodes ,stepmax = 1e6 )

#plot(nn.model)
nn.pred <- compute(nn.model, data.test[,  -which(names(data.norm) == "gender")])

#nn.pred.gender <- minmaxdesnorm(nn.pred$net.result, dataset$gender)
#test.gender  <- minmaxdesnorm(data.test$gender, dataset$gender)

#RMSE(nn.pred.gender, test.gender)
predicted_labels <- ifelse(nn.pred$net.result > 0.5, 1, 0)  # Convert probabilities to class labels

cfmatrix2 <- table(predicted_labels,  data.test$gender)
nn_accuracy <- sum(diag(cfmatrix2) / sum(cfmatrix2)) * 100
#0.33

#A

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
  
  other[i, ] <- c(knn_results$accuracy,knn_results$`recall/sensitivity`,knn_results$precision,knn_results$F1,nn_results$accuracy,nn_results$`recall/sensitivity`,nn_results$precision,nn_results$F1)
  
  
}

#accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)


# Media Knn = 55.329%
# Desvio Knn = 4.147%
# Media nn = 87.204%
# Desvio nn = 4.755%
# Ambos os modelos têm uma média de precisão em torno de 67% e desvio padrão baixo
# O que indica consistência dos resultados ao longo das k-folds
# E apresentam um desempenho semelhante no calculo da previsao do "Pro.level"


#B
# H0: os modelos sÃ£o significativamente iguais 
# H1: os modelos sÃ£o significativamente diferentes

t.test(accuracy[, 1], accuracy[, 2])  #p-value = 1.316e-11
# Como o p-value Ã© inferior a alfa (0.05) rejeitamos a hipÃ³tese nula. 
# Ou seja, os modelos os modelos sÃ£o significativamente diferentes


#C
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

# resultados resultantes do ex do k fold
medidas <- matrix(nrow = 2, ncol = 4)

medidas[1,1] <- mean(other[,1])
medidas[1,2] <- mean(other[,2])
medidas[1,3] <- mean(other[,3])
medidas[1,4] <- mean(other[,4])
medidas[2,1] <- mean(other[,5])
medidas[2,2] <- mean(other[,6])
medidas[2,3] <- mean(other[,7])
medidas[2,4] <- mean(other[,8])

rownames(medidas) <- c("KNN", "NN")
colnames(medidas) <- c("Accuracy", "Sensitivity", "Precision", "F1")

View(medidas)
