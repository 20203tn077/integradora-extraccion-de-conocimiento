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

matrimonios.completo.1 <- matrimonios[!is.na(matrimonios$ocup_con1), -24]
matrimonios.completo.2 <- matrimonios[!is.na(matrimonios$ocup_con2), -13]

# ALGORITMO DE CLASIFICACIÓN - KNN

## Generar datos de entrenamiento y prueba para dos modelos

library(caret)

indices.entrenamiento <- createDataPartition(matrimonios.completo.1$ocup_con1, times = 1, p = 0.8, list = FALSE)

matrimonios.entrenamiento.1 <- matrimonios.completo.1[indices.entrenamiento, -c(7, 13)]
matrimonios.prueba.1 <- matrimonios.completo.1[-indices.entrenamiento, -c(7, 13)]
ocupaciones.entrenamiento.1 <- matrimonios.completo.1[indices.entrenamiento, 13]
ocupaciones.prueba.1 <- matrimonios.completo.1[-indices.entrenamiento, 13]

indices.entrenamiento <- createDataPartition(matrimonios.completo.2$ocup_con2, times = 1, p = 0.8, list = FALSE)

matrimonios.entrenamiento.2 <- matrimonios.completo.2[indices.entrenamiento, -c(7, 23)]
matrimonios.prueba.2 <- matrimonios.completo.2[-indices.entrenamiento, -c(7, 23)]
ocupaciones.entrenamiento.2 <- matrimonios.completo.2[indices.entrenamiento, 23]
ocupaciones.prueba.2 <- matrimonios.completo.2[-indices.entrenamiento, 23]

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

