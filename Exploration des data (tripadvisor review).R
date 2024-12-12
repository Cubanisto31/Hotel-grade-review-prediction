library(dplyr)
library(ggplot2)
library(formattable)
#Importer la base de données 
tripadvisor_hotel_reviews <- read_csv("~/Test IA avis hotel/tripadvisor_hotel_reviews.csv")

df <- tripadvisor_hotel_reviews
 
df <- df %>% 
  mutate(character_number = nchar(df$Review)) %>% 
  mutate(words_number = sapply(strsplit(df$Review, "\\s+"), length))

summary(df$words_number)

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


ggplot(a, aes(x = Var1, y= Var2, fill= Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Character_number", y = "Word_number", title = "Tableau de contingence nombre de mots vs nombre de lettres") +
  theme_minimal()





