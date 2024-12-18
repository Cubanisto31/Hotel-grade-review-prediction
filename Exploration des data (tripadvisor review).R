library(dplyr)
library(ggplot2)
library(formattable)
library(readr)
library(tidyr)

#########################
## Ce script a pour objectif de (i) préparer la df et (ii) visualiser la repartition des tailles de commentaires parmi les notes attribuées (1-5)
#########################

##############
#(i) Préparation de la df
##############

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
tab_conting <- table(df$class_Ch_N, df$class_W_N) %>% 
  as.data.frame()

# Réordonner les modalités de Var1 dans un ordre spécifique
tab_conting$Var1 <- factor(tab_conting$Var1, levels = c("Très petit", "Petit", "Grand", "Très grand"))

# Trier les lignes du dataframe selon l'ordre de Var1
tab_conting <- tab_conting %>%
  arrange(Var1)

tab_conting$Var2 <- factor(tab_conting$Var2, levels = c("Très petit", "Petit", "Grand", "Très grand"))

# Trier les lignes du dataframe selon l'ordre de Var2
tab_conting <- tab_conting %>%
  arrange(Var2)


# Tracer le tableau de contingence 
ggplot(tab_conting, aes(x = Var1, y= Var2, fill= Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Character_number", y = "Word_number", title = "Tableau de contingence nombre de lettres vs nombre de mots") +
  theme_minimal()




#Tracer la répartition des notes attribuées 
grade_rep <- table(df$Rating) %>% as.data.frame()

ggplot(grade_rep, aes(x = Var1, y = Freq ))+
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution des notes", x = "Note", y = "Fréquence") +
  theme_minimal()


#Tracer la répartition des notes en fonction du nombre de charactères 
ggplot(df, aes( x = character_number, y = Rating))+
  geom_point(stat = "identity", color = "black")+
  labs(title = "Répartition des notes en fonction du nombre de charactères", x= "Nombre de charactères", y="Notes")+
  theme_minimal()



#Tracer la fréquence des commentaires en fonction de leur note
grade_by_size <- table(df$Rating, df$class_Ch_N) %>% as.data.frame()

ggplot(grade_by_size, aes(x = Var1, y = Freq, fill = Var2 ))+
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Distribution des notes", x = "Note", y = "Fréquence") +
  theme_minimal()
#Il semble que les class_ch sont réparties de manière similaire au sein des notes 

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

# Création des diagrammes en camembert pour observer la part des notes dans les class de commentaires 
ggplot(grade_by_size, aes(x = "", y = percent_rate_by_size, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barres empilées
  coord_polar(theta = "y") +                                # Transformation en camembert
  facet_wrap(~Var2) +                                       # Un camembert par modalité de Var2
  labs(title = "Proportion des notes par taille de commentaire",
       fill = "Notes",
       x = NULL,
       y = NULL) +
  theme_void() +                                            # Thème minimaliste pour camembert
  theme(legend.position = "bottom")

# Création des diagrammes en camembert pour observer la part des class de commentaires dans les notes
ggplot(grade_by_size, aes(x = "", y = percent_size_by_rate, fill = Var2)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Barres empilées
  coord_polar(theta = "y") +                                # Transformation en camembert
  facet_wrap(~Var1) +                                       # Un camembert par modalité de Var2
  labs(title = "Proportion des tailles de commentaire par notes",
       fill = "Taille de commentaires",
       x = NULL,
       y = NULL) +
  theme_void() +                                            # Thème minimaliste pour camembert
  theme(legend.position = "bottom")


#S'intéresser au distribution des classes de tailles de commentaire pour chaque note

contingence_table <- grade_by_size[,c(1, 2, 7)]

contingence_table <- contingence_table %>% 
  pivot_wider(names_from = Var2, values_from = percent_size_by_rate) %>%
  select(-Var1) %>%
  as.matrix()

# Test du Chi²
chi2_result <- chisq.test(contingence_table)
print(chi2_result)

#p-value = 0,74 > 0,05 ; je ne peux pas rejeter H0 (avec un seuil d'erreur à 5%) selon laquelle la répartition des tailles des messages pour chaque note serait dûe au hasard


