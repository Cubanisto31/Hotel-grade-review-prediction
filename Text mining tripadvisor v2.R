library(tidyverse)
library(tidytext)
library(caret)
library(wordcloud)
library(tm)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(textrecipes)

#Script v2 (car df v1 trop mauvaise)

#Importer la base de données 
df  <- read_csv("New_Delhi_reviews.csv")

#Mettre en forme pour pouvoir recycler le code de la v1 
df <- df[, c(2, 1)]

colnames(df) <- c("Review", "Rating")

df <- na.omit(df)

#Mise au format
df$Rating <- as.factor(df$Rating)

#Informations sur la df
str(df)
summary(df)


#Préparation des données pour l'analyse textuelle
df_words <- df %>% 
  mutate(
    Review = tolower(Review), #Conversion de tout le texte en minuscules
    Review = str_replace_all(Review, "[[:punct:]]", " " ), #Remplacement de toute la ponctuation par des espaces
    Review = str_replace_all(Review, "[[:digit:]]", " "), #Remplacement de tous les chiffres par des espaces
    Review = str_squish(Review) #Suppression des espaces multiples et des espaces en début/fin de texte
  )

#On tokenise par mot, chaque mot du corpus obtient donc une fréquence pour chaque note  
df_words <- df_words %>% 
  unnest_tokens(word, Review)

#Suppression des mots trop courants qui n'apportent pas de sens particulier
stopwords_list <- get_stopwords()

#Liste des mots de df qui font partie de la stopword_list
stopwords_in_df <- df_words %>%
  semi_join(stopwords_list, by = c("word" = "word"))

#Enlever ces mots de df_words
df_words_clean <- df_words %>%
  anti_join(stopwords_list, by = c("word" = "word"))

#Suppression des mots d'un seul caractère 
df_words_clean <- df_words_clean %>%
  filter(nchar(word) > 1)

#Comptage des mots les plus fréquents par note

#Top 30 mots les plus fréquents dans tout le corpus
top_words_corpus <- df_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 30)

#Représentation graphique 
ggplot(top_words_corpus, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "skyblue", color="black") +
  coord_flip() +
  labs(title = "Top 10 mots les plus fréquents dans le corpus",
       x = "Mots",
       y = "Fréquence") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Top 10 mots par note
top_words_by_rating <- df_words_clean %>%
  group_by(Rating, word) %>%
  count() %>%
  group_by(Rating) %>%
  slice_max(n, n = 10) %>%
  arrange(Rating, desc(n))

#Représentation graphique 
ggplot(top_words_by_rating, aes(x = reorder_within(word, n, Rating), y = n, fill = as.factor(Rating))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Rating, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 10 des mots les plus fréquents par note",
    x = "Mots",
    y = "Fréquence",
    fill = "Note"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Réalisation d'un TF-IDF pour éviter le pb lié au fait que ce soit toujours les mêmes mots qui reviennent
#TF-IDF = Term Frequency - Inverse Document Frequency


#Filtrage et calcul du TF-IDF amélioré
tf_idf_words <- df_words_clean %>%
  # Compter la fréquence totale de chaque mot
  group_by(word) %>%
  mutate(total_occurrences = n()) %>%
  ungroup() %>%
  # Filtrer pour garder uniquement les mots qui :
  filter(
    # Apparaissent au moins X fois dans l'ensemble du corpus
    total_occurrences >= 20,
    # Ont une longueur minimum
    str_length(word) >= 3,
    # Ne contiennent que des lettres (retire les caractères spéciaux)
    str_detect(word, "^[a-z]+$")
  ) %>%
  # Continuer avec le calcul TF-IDF
  count(Rating, word) %>%
  bind_tf_idf(word, Rating, n) %>%
  group_by(Rating) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  arrange(Rating, desc(tf_idf))


ggplot(tf_idf_words, aes(x = reorder_within(word, tf_idf, Rating), y = tf_idf, fill = as.factor(Rating))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Rating, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 10 des mots les plus fréquents par note (TF-IDF)",
    x = "Mots",
    y = "Fréquence",
    fill = "Note"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Envie de faire de la reconnaissance d'entité nommée pour voir ensuite les bar-café que les clients préfèrent

#Création du modèle d'estimation des notes

# 1. Préparation des données
# Diviser les données en ensembles d'entraînement et de test
set.seed(123) 
indices_train <- createDataPartition(df$Rating, p = 0.8, list = FALSE)
train_data <- df[indices_train, ]
test_data <- df[-indices_train, ]

# 2. Préparation du texte avec une recette
recipe_text <- recipe(Rating ~ Review, data = train_data) %>%
  step_tokenize(Review) %>%
  step_stopwords(Review) %>%
  step_tokenfilter(Review, max_tokens = 1000) %>%
  step_tfidf(Review)

# 3. Configuration du contrôle d'entraînement
ctrl <- trainControl(
  method = "cv",        # Validation croisée
  number = 5,          # Nombre de plis
  verboseIter = TRUE   # Pour voir la progression
)

# 4. Entraînement du modèle (Random Forest comme exemple)
model_rf <- train(
  recipe_text,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy"
)
#Manque de RAM pour le faire tourner 

model_glm <- train(
  recipe_text,
  data = train_data,
  method = "glmnet",  # régression logistique régularisée
  trControl = ctrl,
  metric = "Accuracy"
)

#######
# Sauvegarder le modèle
#saveRDS(model_glm, "model_glm.rds")

# Plus tard, pour recharger le modèle :
#model_glm <- readRDS("model_glm.rds")
#######

# 5. Évaluation du modèle
# Prédictions sur l'ensemble de test
predictions <- predict(model_glm, newdata = test_data)

# Matrice de confusion
confusionMatrix(predictions, test_data$Rating)

# Prédictions sur l'ensemble de test
conf_matrix <- confusionMatrix(predictions, test_data$Rating)

# Visualisation de la matrice de confusion
#Pas bonne car ne montre pas vraiment ce que je veux
conf_matrix_df <- as.data.frame(conf_matrix$table)

ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq)) +
  theme_minimal() +
  labs(title = "Matrice de Confusion",
       x = "Note réelle",
       y = "Note prédite")+
  theme(plot.title = element_text(hjust = 0.5))

# Calculer précision, rappel et F1-score par classe
perf_by_class <- data.frame(
  Classe = levels(test_data$Rating),
  Précision = conf_matrix$byClass[,"Precision"],
  Rappel = conf_matrix$byClass[,"Recall"],
  F1 = conf_matrix$byClass[,"F1"]
)

# Visualisation en barres groupées
perf_by_class_long <- tidyr::pivot_longer(perf_by_class, 
                                          cols = c("Précision", "Rappel", "F1"),
                                          names_to = "Métrique",
                                          values_to = "Valeur")

ggplot(perf_by_class_long, aes(x = Classe, y = Valeur, fill = Métrique)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Métriques de performance par classe",
       x = "Note",
       y = "Score")+
  theme(plot.title = element_text(hjust = 0.5))
