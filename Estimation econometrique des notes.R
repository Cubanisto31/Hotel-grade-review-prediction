library(dplyr)
library(ggplot2)
library(formattable)
library(readr)
library(MASS)

#Ce script vise à voir s'il est possible de prédire économétriquement une note sur la base de la longueur du commentaire 

#Necessaire de me renseigner mieux sur le fontionnement des modèles pour comprendre comment les construire et les interpréter 

#Dans la mesure où Rating est une variable discrète ordonnée je commence par un modèle de régression ordinal

model1 <- polr(as.factor(Rating) ~ character_number, data = df, method = "logistic")
model1

model2 <- polr(as.factor(Rating) ~ words_number, data = df, method = "logistic")
model2

model3 <- polr(as.factor(Rating) ~ class_Ch_N, data = df, method = "logistic")
model3

model4 <- polr(as.factor(Rating) ~ class_W_N, data = df, method = "logistic")
model4

model5 <- polr(as.factor(Rating) ~ W_length_mean, data = df, method = "logistic")
model5
