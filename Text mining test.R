library("tm")
library("tidytext")
library("proustr")
library("tidyverse")
library("devtools")
devtools::install_github("ThinkRstat/stopwords")
library("stopwords")
library("dplyr")
library("ggplot2")

books <- proust_books()


books_tidy <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_iso$fr) %>%
  count(word, sort = TRUE) %>%
  head(10)

df <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " "))

#Visualisation 1 
barplot(height=books_tidy$n, names.arg= books_tidy$word, xlab="Mots", ylab="Fréquence", col="#973232", main="À la recherche du temps perdu")

#Visualisation 2
ggplot(books_tidy, aes(x=fct_reorder(word, n, .desc=TRUE), y=n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black")+
  labs(title = "La recherche du temps perdu", x="Mots", y="Fréquence")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Transformer le texte en corpus 
corpus <- df$text %>%
  VectorSource() %>% 
  VCorpus()
  
       