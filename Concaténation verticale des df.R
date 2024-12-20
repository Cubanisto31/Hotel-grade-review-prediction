######Concaténation verticale des bases de données

library(dplyr)
library(openxlsx)
library(openxlsx)

#Fonction pour ajouter une colonne année à chaque df

add_year_column <- function(df, year) {
  df <- df %>% 
    mutate(Année=year)
  return(df)
}


# Liste des dataframes et années
dataframes <- list(df2010, df2011, df2012, df2013, df2014, df2015, df2016, df2017, df2018, 
                   df2019, df2020, df2021, df2022, df2023, df2024)

years <- 2010:2024

# Ajouter une colonne "Année" à chaque dataframe
dataframes_with_year <- mapply(add_year_column, dataframes, years, SIMPLIFY = FALSE)

#Concaténer toutes les df verticalement
final_df <- bind_rows(dataframes_with_year)

#Replacer la colonne année en 1ere position 
final_df <- final_df %>% relocate(Année, .before = 1)


#Mettre les modalités de la réplication en minuscule pour éviter les pb de casses
final_df$Réplication <- tolower(final_df$Réplication)
final_df$`Réplication towerCast` <- tolower(final_df$`Réplication towerCast`)

#Transformer les variables Réplication et Réplication towerCast afin que ce soit plus compréhensible


final_df <- final_df %>%
  # Créer un identifiant de scénario basé sur les modalités des deux variables
  mutate(scenario = case_when(
    is.na(Réplication) & is.na(`Réplication towerCast`) ~ 1,
    Réplication == "towercast" & is.na(`Réplication towerCast`) ~ 2,
    is.na(Réplication) & `Réplication towerCast` == "non" ~ 3,
    is.na(Réplication) & `Réplication towerCast` == "site" ~ 4,
    is.na(Réplication) & `Réplication towerCast` == "oui" ~ 5,
    Réplication == "itas" & is.na(`Réplication towerCast`) ~ 6,
    is.na(Réplication) & `Réplication towerCast` == "antenne" ~ 7,
    Réplication == "groupe itas" & is.na(`Réplication towerCast`) ~ 8,
    `Réplication towerCast` == "itas / towercast" ~ 9,
    Réplication == "onecast" ~ 10,
    TRUE ~ NA_real_  # Tout autre cas qui ne correspond pas à un scénario spécifié
  )) %>%
  
  # Appliquer les transformations spécifiques pour chaque scénario
  mutate(
    `Réplication` = case_when(  
      scenario == 4 ~ "towercast",   # Scénario 4 : Remplacer NA de Réplication par "towercast"
      scenario == 5 ~ "towercast",   # Scénario 5 : Remplacer NA de Réplication par "towercast"
      TRUE ~ Réplication              # Sinon, ne rien faire
    ),
    `Réplication towerCast` = case_when(
      scenario == 2 ~ "oui",         # Scénario 2 : Remplacer NA de `Réplication towerCast` par "Oui"
      scenario == 3 ~ NA,            # Scénario 3 : Remplacer "Non" de `Réplication towerCast` par NA
      TRUE ~ `Réplication towerCast`  # Sinon, ne rien faire
    )
  ) %>%
  # Supprimer la colonne de scénario
  select(-scenario)

  #Uniformiser la modalité itas 

final_df <- final_df %>% 
  mutate(Réplication = ifelse(Réplication == "groupe itas", "itas", Réplication))


#Exporter au format xlsx en arrondissant les tarifs annuels à l'affichage 

# Créer un workbook
wb <- createWorkbook()

# Ajouter une feuille de travail
addWorksheet(wb, "Concaténation")

# Écrire les données
writeData(wb, "Concaténation", final_df)

# Appliquer un format pour afficher sans décimales
style_no_decimal <- createStyle(numFmt = "0") # Format sans décimales
addStyle(
  wb, 
  sheet = "Concaténation", 
  style = style_no_decimal, 
  cols = which(names(final_df) == "Tarif annuel"), 
  rows = 1:(nrow(final_df) + 1), # Inclure l'en-tête et toutes les lignes
  gridExpand = TRUE
)


# Sauvegarder le fichier Excel
saveWorkbook(wb, file ="S:/P_Broadcast_transition/6. Offres de référence/0. Concaténation des ODR TDF/Concaténation verticale des ODR.xlsx", overwrite = TRUE)

#Supprimer les objets inutiles
rm(dataframes, dataframes_with_year, years,add_year_column, style_no_decimal, wb)


