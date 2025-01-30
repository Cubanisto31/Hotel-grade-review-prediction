library(dplyr)
library(ggplot2)
library(formattable)
library(readr)
library(tidyr)

#Script v2 (car df v1 trop mauvaise)


#Importer la base de données 
df  <- read_csv("New_Delhi_reviews.csv")

#Mettre en forme pour pouvoir recycler le code de la v1 
df <- df[, c(2, 1)]

colnames(df) <- c("Review", "Rating")

df <- na.omit(df)

df <- df %>% 
  mutate(character_number = nchar(df$Review)) %>% 
  mutate(words_number = sapply(strsplit(df$Review, "\\s+"), length))

#Longueur moyenne des mots par commentaire 
df <- df %>% 
  mutate(W_length_mean = round(character_number / words_number))

table(df$W_length_mean)

#Afficher le summary des character number et des word number pour créer des classes
summary(df$character_number)

summary(df$words_number)

#Création des classes des tailles de commentaires 
df <- df %>% 
  mutate(
    class_Ch_N = case_when(
      character_number <= 163 ~ "Très petit",
      character_number <= 263 ~ "Petit",
      character_number <= 462 ~ "Grand",
      TRUE ~ "Très grand"))



df <- df %>%
  mutate(
    class_W_N = case_when(
      words_number <= 29 ~ "Très petit",
      words_number <= 47 ~ "Petit",
      words_number <= 84 ~ "Grand",
      TRUE ~ "Très grand"))

#Tableau de contingence entre le nombre de lettres et le nombre de mots 
tab_conting <- table(df$class_Ch_N, df$class_W_N) %>% 
  as.data.frame()

#Réordonner les modalités de Var1 dans un ordre spécifique
tab_conting$Var1 <- factor(tab_conting$Var1, levels = c("Très petit", "Petit", "Grand", "Très grand"))

#Trier les lignes du dataframe selon l'ordre de Var1
tab_conting <- tab_conting %>%
  arrange(Var1)

tab_conting$Var2 <- factor(tab_conting$Var2, levels = c("Très petit", "Petit", "Grand", "Très grand"))


#Trier les lignes du dataframe selon l'ordre de Var2
tab_conting <- tab_conting %>%
  arrange(Var2)

#Tracer le tableau de contingence 
ggplot(tab_conting, aes(x = Var1, y= Var2, fill= Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Character_number", y = "Word_number", title = "Tableau de contingence nombre de caractères vs nombre de mots") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#Tracer la répartition des notes attribuées 
grade_rep <- table(df$Rating) %>% as.data.frame()

ggplot(grade_rep, aes(x = Var1, y = Freq ))+
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution des notes", x = "Note", y = "Fréquence") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Nombre de mots par note
nb_mots_par_note <- df %>%
  group_by(Rating) %>%
  summarise(words_number = sum(words_number))

ggplot(nb_mots_par_note, aes(x = Rating, y = words_number)) +
  geom_col(fill = "skyblue", color="black") +
  labs(title = "Nombre de mots par note",
       x = "Note",
       y = "Nombre de mots") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Tracer la répartition des notes en fonction du nombre de mots
ggplot(df, aes( x = words_number, y = Rating))+
  geom_point(stat = "identity", color = "black")+
  labs(title = "Répartition des notes en fonction du nombre de charactères", x= "Nombre de charactères", y="Notes")+
  theme_minimal()


#Nombre de charactères par note 
nb_ch_par_note <- df %>%
  group_by(Rating) %>%
  summarise(ch_number = sum(character_number))

ggplot(nb_ch_par_note, aes(x = Rating, y = ch_number)) +
  geom_col(fill = "skyblue", color="black") +
  labs(title = "Nombre de caractères par note",
       x = "Note",
       y = "Nombre de caractères") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Tracer la répartition des notes en fonction du nombre de charactères 
ggplot(df, aes( x = character_number, y = Rating))+
  geom_point(stat = "identity", color = "black")+
  labs(title = "Répartition des notes en fonction du nombre de charactères", x= "Nombre de charactères", y="Notes")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Fréquence des commentaires en fonction de leur note
grade_by_size <- table(df$Rating, df$class_Ch_N) %>% as.data.frame()

#Ajouter des colonnes qui permettent de voir la répartition des notes dans chaque class de commentaires (Class_ch_number)
grade_by_size <- grade_by_size %>% 
  group_by(Var2) %>% 
  mutate(prop_rate_by_size = Freq / sum(Freq)) %>% 
  ungroup()  

grade_by_size <- grade_by_size %>% 
  mutate(percent_rate_by_size = round(prop_rate_by_size * 100, 2))

#Ajouter des colonnes qui permettent de voir la répartition des class de commentaires  dans chaque note
grade_by_size <- grade_by_size %>% 
  group_by(Var1) %>% 
  mutate(prop_size_by_rate = Freq / sum(Freq)) %>% 
  ungroup()  

grade_by_size <- grade_by_size %>% 
  mutate(percent_size_by_rate = round(prop_size_by_rate * 100, 2))

#Création des diagrammes en camembert pour observer la part des notes dans les class de commentaires 
ggplot(grade_by_size, aes(x = "", y = percent_rate_by_size, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barres empilées
  coord_polar(theta = "y") +                                # Transformation en camembert
  facet_wrap(~Var2) +                                       # Un camembert par modalité de Var2
  labs(title = "Proportion des notes par taille de commentaire",
       fill = "Notes",
       x = NULL,
       y = NULL) +
  theme_void() +                                            # Thème minimaliste pour camembert
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))

# Création des diagrammes en camembert pour observer la part des class de commentaires dans les notes
grade_by_size$Var2 <- factor(grade_by_size$Var2, levels = c("Très petit", "Petit", "Grand", "Très grand"))

ggplot(grade_by_size, aes(x = "", y = percent_size_by_rate, fill = Var2)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barres empilées
  coord_polar(theta = "y") +                                # Transformation en camembert
  facet_wrap(~Var1) +                                       # Un camembert par modalité de Var2
  labs(title = "Proportion des tailles de commentaire par notes",
       fill = "Taille de commentaires",
       x = NULL,
       y = NULL) +
  theme_void() +                                            # Thème minimaliste pour camembert
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = paste0(percent_size_by_rate, "%")),
            position = position_stack(vjust = 0.5),
            size = 3)  


#Test du chi 2


