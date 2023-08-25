matrimonios <- read.csv('./resultados/conjunto_de_datos/matrimonios.csv')
set.seed(210129)

# Función para evaluar los resultados de KNN

eval <- function (confusion.matrix) {
  accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
  precision <- confusion.matrix[2, 2] / sum(confusion.matrix[, 2])
  recall <- confusion.matrix[2, 2] / sum(confusion.matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)

  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1_score:", f1_score))
}

# Obtener datos completos por contrayente

matrimonios.completo.1 <- matrimonios[!is.na(matrimonios$ocup_con1),]
matrimonios.completo.2 <- matrimonios[!is.na(matrimonios$ocup_con2),]

# ALGORITMO SUPERVISADO - CLASIFICACIÓN - KNN

## Generar datos de entrenamiento y prueba para dos modelos

library(caret)

indices.entrenamiento <- createDataPartition(matrimonios.completo.1$ocup_con1, times = 1, p = 0.8, list = FALSE)

matrimonios.entrenamiento.1 <- matrimonios.completo.1[indices.entrenamiento, -c(7, 13, 24)]
matrimonios.prueba.1 <- matrimonios.completo.1[-indices.entrenamiento, -c(7, 13, 24)]
ocupaciones.entrenamiento.1 <- matrimonios.completo.1[indices.entrenamiento, 13]
ocupaciones.prueba.1 <- matrimonios.completo.1[-indices.entrenamiento, 13]

indices.entrenamiento <- createDataPartition(matrimonios.completo.2$ocup_con2, times = 1, p = 0.8, list = FALSE)

matrimonios.entrenamiento.2 <- matrimonios.completo.2[indices.entrenamiento, -c(7, 13, 24)]
matrimonios.prueba.2 <- matrimonios.completo.2[-indices.entrenamiento, -c(7, 13, 24)]
ocupaciones.entrenamiento.2 <- matrimonios.completo.2[indices.entrenamiento, 24]
ocupaciones.prueba.2 <- matrimonios.completo.2[-indices.entrenamiento, 24]

## Aplicar algoritmo con diferentes valores de k

library(class)

k3.1 <- knn(scale(matrimonios.entrenamiento.1), scale(matrimonios.prueba.1), ocupaciones.entrenamiento.1, k = 3)
k5.1 <- knn(scale(matrimonios.entrenamiento.1), scale(matrimonios.prueba.1), ocupaciones.entrenamiento.1, k = 5)
k7.1 <- knn(scale(matrimonios.entrenamiento.1), scale(matrimonios.prueba.1), ocupaciones.entrenamiento.1, k = 7)
k10.1 <- knn(scale(matrimonios.entrenamiento.1), scale(matrimonios.prueba.1), ocupaciones.entrenamiento.1, k = 10)

k3.2 <- knn(scale(matrimonios.entrenamiento.2), scale(matrimonios.prueba.2), ocupaciones.entrenamiento.2, k = 3)
k5.2 <- knn(scale(matrimonios.entrenamiento.2), scale(matrimonios.prueba.2), ocupaciones.entrenamiento.2, k = 5)
k7.2 <- knn(scale(matrimonios.entrenamiento.2), scale(matrimonios.prueba.2), ocupaciones.entrenamiento.2, k = 7)
k10.2 <- knn(scale(matrimonios.entrenamiento.2), scale(matrimonios.prueba.2), ocupaciones.entrenamiento.2, k = 10)

## Evaluar resultados

eval(table(Observado = ocupaciones.prueba.1, Prediccion = k3.1))
eval(table(Observado = ocupaciones.prueba.1, Prediccion = k5.1))
eval(table(Observado = ocupaciones.prueba.1, Prediccion = k7.1))
eval(table(Observado = ocupaciones.prueba.1, Prediccion = k10.1))

eval(table(Observado = ocupaciones.prueba.2, Prediccion = k3.2))
eval(table(Observado = ocupaciones.prueba.2, Prediccion = k5.2))
eval(table(Observado = ocupaciones.prueba.2, Prediccion = k7.2))
eval(table(Observado = ocupaciones.prueba.2, Prediccion = k10.2))

## Aplicar knn entrenado con todos los datos disponibles sobre dataset original usando el valor de k óptimo

matrimonios[is.na(matrimonios$ocup_con1), 13] <- knn(
  scale(matrimonios.completo.1[,-c(7, 13, 24)]),
  matrimonios[is.na(matrimonios$ocup_con1), -c(7, 13, 24)],
  matrimonios.completo.1[,13], k = 10
)

matrimonios[is.na(matrimonios$ocup_con1), 24] <- knn(
  scale(matrimonios.completo.2[,-c(7, 13, 24)]),
  matrimonios[is.na(matrimonios$ocup_con1), -c(7, 13, 24)],
  matrimonios.completo.2[,24], k = 5
)

# ALGORITMO NO SUPERVISADO - REDUCCIÓN DE DIMENSIONALIDAD - PCA

## Aplicar PCA al dataset correspondiente a cada contrayente

pca.1 <- prcomp(matrimonios.completo.1[,-c(7, 13, 24)], center = TRUE, scale = TRUE)
pca.2 <- prcomp(matrimonios.completo.2[,-c(7, 13, 24)], center = TRUE, scale = TRUE)

## Determinar los componentes necesarios para explicar al menos el 85% de varianza

summary(pca.1)$importance
#PC1:  0.15824
#PC2:  0.10419
#PC3:  0.10218
#PC4:  0.09695
#PC5:  0.07849
#PC6:  0.06245
#PC7:  0.04425
#PC8:  0.04119
#PC9:  0.03885
#PC10: 0.03605
#PC11: 0.03526
#PC12: 0.03284
#PC13: 0.03136
#PC14: 0.03013

summary(pca.2)$importance
#PC1:  0.14947
#PC2:  0.10192
#PC3:  0.09803
#PC4:  0.09434
#PC5:  0.07869
#PC6:  0.06456
#PC7:  0.04873
#PC8:  0.04615
#PC9:  0.03840
#PC10: 0.03612
#PC11: 0.03530
#PC12: 0.03322
#PC13: 0.03218

## Agregar los componentes seleccionados a los datasets

matrimonios.completo.1$PCA1 <- apply(pca.1$rotation[,1] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA2 <- apply(pca.1$rotation[,2] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA3 <- apply(pca.1$rotation[,3] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA4 <- apply(pca.1$rotation[,4] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA5 <- apply(pca.1$rotation[,5] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA6 <- apply(pca.1$rotation[,6] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA7 <- apply(pca.1$rotation[,7] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA8 <- apply(pca.1$rotation[,8] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA9 <- apply(pca.1$rotation[,9] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA10 <- apply(pca.1$rotation[,10] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA11 <- apply(pca.1$rotation[,11] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA12 <- apply(pca.1$rotation[,12] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.1$PCA13 <- apply(pca.1$rotation[,13] * matrimonios.completo.1[,-c(7, 13, 24)], 1, sum)

matrimonios.completo.2$PCA1 <- apply(pca.2$rotation[,1] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA2 <- apply(pca.2$rotation[,2] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA3 <- apply(pca.2$rotation[,3] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA4 <- apply(pca.2$rotation[,4] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA5 <- apply(pca.2$rotation[,5] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA6 <- apply(pca.2$rotation[,6] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA7 <- apply(pca.2$rotation[,7] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA8 <- apply(pca.2$rotation[,8] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA9 <- apply(pca.2$rotation[,9] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA10 <- apply(pca.2$rotation[,10] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA11 <- apply(pca.2$rotation[,11] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA12 <- apply(pca.2$rotation[,12] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)
matrimonios.completo.2$PCA13 <- apply(pca.2$rotation[,13] * matrimonios.completo.2[,-c(7, 13, 24)], 1, sum)