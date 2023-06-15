
#4.2

#EX1
# Arvore
set.seed(123)
train_indices <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]



# Criando o modelo de árvore de decisão
decision_tree <- rpart(Pro.level ~ ., data = train_data, method = "class")

# Visualizando a árvore de decisão
#plot(decision_tree)
#text(decision_tree)

# Fazendo previsões no conjunto de teste
tree_predict <- predict(decision_tree, newdata = test_data, type = "class")

# Avaliando o desempenho do modelo

cfmatrix <- table(tree_predict, test_data$Pro.level)
tree_accuracy <- sum(diag(cfmatrix)) / sum(cfmatrix) * 100

#NN

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
train_data1  <- normalized_dataset[sample, ]
test_data1  <- normalized_dataset[!sample, ]

nn.model <- neuralnet(Pro.level ~ ., data = train_data1, hidden = numnodes <- 5)

#plot(nn.model)

nn.pred <- compute(nn.model,test_data1)

denormalized_pred <- minmaxdesnorm(nn.pred$net.result,dataset$Pro.level)

# Denormalizar
denormalized_actual <- minmaxdesnorm(test_data1$Pro.level,dataset$Pro.level)

cfmatrix2 <- table(denormalized_pred, denormalized_actual )
nn_accuracy <- sum(diag(cfmatrix2)) / sum(cfmatrix2) * 100


##K
knn_model <- knn(train = train_data[, -ncol(train_data)],
                 test = test_data[, -ncol(test_data)],
                 cl = train_data$Pro.level,
                 k = 5)

#Make predictions on the test data
knn_predict <- as.factor(knn_model)


cfmatrix3 <- table(knn_predict, test_data$Pro.level)
knn_accuracy <- sum(diag(cfmatrix3)) / sum(cfmatrix3) * 100

tree_accuracy
nn_accuracy
knn_accuracy

# EX1.a)

# Numero de folds
k <- 10  
# dois melhores modelos: Arvore e Kn
tree_accuracy <- numeric(k)
knn_accuracy <- numeric(k)

#set.seed(123) # se necessario

# Gera indices aleatorios para o cross-validation
indices <- sample(1:k, nrow(dataset), replace = TRUE)  

# k-folds
for (i in 1:k) {
  # Divide amostra em train/test para cada fold
  train_data <- dataset[indices != i, ]
  test_data <- dataset[indices == i, ]
  
  # Treina o modelo Arvore Decisao
  tree_model <- rpart(Pro.level ~ ., data = train_data, method = "class")
  
  # Treina o modelo K-vizinhos
  knn_model <- knn(train = train_data[, -ncol(train_data)],
                   test = test_data[, -ncol(test_data)],
                   cl = train_data$Pro.level,
                   k = 5)
  
  # Calcula Accuracy
  tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
  knn_predictions <- knn_model
  
  tree_accuracy[i] <- mean(tree_predictions == test_data$Pro.level)
  knn_accuracy[i] <- mean(knn_predictions == test_data$Pro.level)
}

tree_mean <- mean(tree_accuracy)
tree_sd <- sd(tree_accuracy)

knn_mean <- mean(knn_accuracy)
knn_sd <- sd(knn_accuracy)

# Media Arvore = 0.6744161
# Desvio Arvore = 0.05291416
# Media Knn = 0.672229
# Desvio Knn = 0.02810806
# Ambos os modelos têm uma média de precisão em torno de 67% e desvio padrão baixo
# O que indica consistência dos resultados ao longo das k-folds
# E apresentam um desempenho semelhante no calculo da previsao do "Pro.level"

# EX1.b

# Lazy Learning -> K-vizinhis mais proximos (Knn)
# Porque armazena todos os dados de treino na fase de treinamento
# e apenas os processo na fase de classificação
# - Simple e facil de implementar
# - Pode implicar um grande espaço de armazenamento para guardar todos os dados de treino
# - Pode exigir um processamento longo e computacionalmente intensivo, já que o modelo calcula as distâncias do dados
# de treino e de teste na fase de classificação
# - Considera todos os dados, incluindo os irrelevantes ou com erros, o que pode prejudicar a previsão
# - o valor k é necessário ser escolhido adequadamente, valor muito altopode causar perda de precisão enquanto que um
# valor baixo pode causar instabilidade nos dados
# - Conjunto de dados de altas dimensões -> Maldição da Dimensão


# EX1.c

# Teste de Shapiro-Wilk para o primeiro conjunto de dados (tree_accuracy)
shapiro.test(tree_accuracy)
# p-value = 0.5843 > alfa
# segue distribuição normal

# Teste de Shapiro-Wilk para o segundo conjunto de dados (knn_accuracy)
shapiro.test(knn_accuracy)
# p-value = 0.9185 > alfa
# segue distribuição normal

# Teste Parametrico
t.test(tree_accuracy, knn_accuracy, paired = T)
# p-value > alfa=0.05
# diferença não significativa


# EX1.d

#Compare os resultados dos modelos. Discuta em detalhe qual o modelo que
#apresentou melhor e pior desempenho de acordo com os critérios: Accuracy;
#Sensitivity; Specificity e F1.

# Arvore
set.seed(123)
train_indices <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.7,0.3))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# Criando o modelo de árvore de decisão
decision_tree <- rpart(Pro.level ~ ., data = train_data, method = "class")

# Modelo de Árvore de Decisão
tree_predictions <- predict(decision_tree, newdata = test_data, type = "class")
tree_cfmatrix <- table(tree_predictions, test_data$Pro.level)

# Calcular métricas para Árvore de Decisão
tree_accuracy <- sum(diag(tree_cfmatrix)) / sum(tree_cfmatrix)  # Accuracy
tree_sensitivity <- tree_cfmatrix[2, 2] / sum(tree_cfmatrix[2, ])  # Sensitivity
tree_specificity <- tree_cfmatrix[1, 1] / sum(tree_cfmatrix[1, ])  # Specificity
tree_f1 <- 2 * tree_sensitivity * tree_specificity / (tree_sensitivity + tree_specificity)  # F1 score


# Imprimir resultados
cat("Árvore de Decisão:\n")
cat("Accuracy:", tree_accuracy, "\n")
cat("Sensitivity:", tree_sensitivity, "\n")
cat("Specificity:", tree_specificity, "\n")
cat("F1 score:", tree_f1, "\n\n")


##K
knn_model <- knn(train = train_data[, -ncol(train_data)],
                 test = test_data[, -ncol(test_data)],
                 cl = train_data$Pro.level,
                 k = 5)

knn_predictions <- as.factor(knn_model)
knn_cfmatrix <- table(knn_predictions, test_data$Pro.level)

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

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(normalized_dataset), replace=TRUE, prob=c(0.7,0.3))
train_data1  <- normalized_dataset[sample, ]
test_data1  <- normalized_dataset[!sample, ]

nn.model <- neuralnet(Pro.level ~ ., data = train_data1, hidden = numnodes <- 5)

#plot(nn.model)

nn.pred <- compute(nn.model,test_data1)

denormalized_pred <- minmaxdesnorm(nn.pred$net.result,dataset$Pro.level)

# Denormalizar
denormalized_actual <- minmaxdesnorm(test_data1$Pro.level,dataset$Pro.level)

nn_cfmatrix <- table(denormalized_pred, denormalized_actual )

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
