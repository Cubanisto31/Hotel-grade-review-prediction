library(dplyr)
library(ggplot2)
library(formattable)
library(readr)

## Ce script permet de manipuler la df pour préparer l'estimation économétrique des notes 

#Importer la base de données 
tripadvisor_hotel_reviews <- read_csv("tripadvisor_hotel_reviews.csv")

df <- tripadvisor_hotel_reviews
 
df <- df %>% 
  mutate(character_number = nchar(df$Review)) %>% 
  mutate(words_number = sapply(strsplit(df$Review, "\\s+"), length))

#Voir si je décide de ne pas arrondir pour regarder si ça peut donner des messages 
# Ajouter variable longueur moyenne des mots 
df <- df %>% 
  mutate(W_length_mean = round(character_number / words_number))

table(df$W_length_mean)

df <- df %>% 
  mutate(
    class_Ch_N = case_when(
      character_number <= 337 ~ "Très petit",
      character_number <= 535 ~ "Petit",
      character_number <= 857 ~ "Grand",
      TRUE ~ "Très grand"))



df <- df %>%
  mutate(
    class_W_N = case_when(
      words_number <= 48 ~ "Très petit",
      words_number <= 77 ~ "Petit",
      words_number <= 124 ~ "Grand",
      TRUE ~ "Très grand"))


#Tableau de contingence entre le nombre de lettres et le nombre de mots 
a <- table(df$class_Ch_N, df$class_W_N)

#Voir si je ne peux pas un peu optimiser ce bout infra
a <- as.data.frame(a)  

# Réordonner les modalités de Var1 dans un ordre spécifique
a$Var1 <- factor(a$Var1, levels = c("Très petit", "Petit", "Grand", "Très grand"))

# Trier les lignes du dataframe selon l'ordre de Var1
a <- a %>%
  arrange(Var1)

a$Var2 <- factor(a$Var2, levels = c("Très petit", "Petit", "Grand", "Très grand"))

# Trier les lignes du dataframe selon l'ordre de Var2
a <- a %>%
  arrange(Var2)

# Tracer le tableau de contingence 
ggplot(a, aes(x = Var1, y= Var2, fill= Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Character_number", y = "Word_number", title = "Tableau de contingence nombre de lettres vs nombre de mots") +
  theme_minimal()


#Tracer la répartition des notes attribuées 
grade_rep <- table(df$Rating) 

grade_rep <- as.data.frame(grade_rep)


ggplot(grade_rep, aes(x = Var1, y = Freq ))+
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution des notes", x = "Note", y = "Fréquence") +
  theme_minimal()


