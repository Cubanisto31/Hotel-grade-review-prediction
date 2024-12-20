######Importation et nettoyage des bases de données pour chaque année

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)

#2024
#Importer les ODR
RP_NREP20 <- read_excel("2024/20240502_ODR 2024_ARCEP_DiffHF RP NREP.xlsx", range="B3:I73") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2024/20240502_ODR 2024_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2024/20240515_ODR 2024_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2024/20240515_ODR 2024_ARCEP_DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner 
df2024 <- bind_rows(RP_NREP20,RC_NREP20,RP_REP20,RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2024 <- df2024 %>% relocate(Réseau, Nature, .after = last_col())

#Supp les ODR de construction

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Hauteur
df2024 <- df2024 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2024$`Hauteur max (m)` <- df2024$`Hauteur max (m)`+ df2024$`Hauteur max`

#Tarifs
df2024 <- df2024 %>% replace_na(list(`Tarif annuel` = 0,Tarifs = 0))
df2024$`Tarif annuel` <- df2024$`Tarif annuel`+ df2024$Tarifs

#Fusionner Réplications 
df2024 <- df2024 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))

#Supprimer les colonnes de construction
df2024 <- df2024[,-c(9,10,13,14)]



#2023

#Importer les ODR
RP_NREP20 <- read_excel("2023/20230510_ODR 2023_ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non Réplicable"    # Valeur pour la colonne Nature
  ) 

RC_NREP20 <- read_excel("2023/20230510_ODR 2023_ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2023/20230517_ODR 2023_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2023/20230517_ODR 2023_ARCEP_DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner 
df2023 <- bind_rows(RP_NREP20,RC_NREP20,RP_REP20,RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2023 <- df2023 %>% relocate(Réseau, Nature, .after = last_col())


#Supp les ODR de construction

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Hauteur
df2023 <- df2023 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2023$`Hauteur max (m)` <- df2023$`Hauteur max (m)`+ df2023$`Hauteur max`

#Tarifs
df2023 <- df2023 %>% replace_na(list(`Tarif annuel` = 0,Tarifs = 0))
df2023$`Tarif annuel` <- df2023$`Tarif annuel`+ df2023$Tarifs

#Fusionner Réplications 
df2023 <- df2023 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))

df2023 <- df2023[,-c(9,10,13,14)]

#2022
#Importer les ODR
RP_NREP20 <- read_excel("2022/20220504_ODR 2022_ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2022/20220504_ODR 2022_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2022/20220504_ODR 2022_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2022/20221110_ODR 2022_ARCEP_DiffHF RC REP_V2.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner 
df2022 <- bind_rows(RP_NREP20,RC_NREP20,RP_REP20,RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2022 <- df2022 %>% relocate(Réseau, Nature, .after = last_col())

#Supp les ODR de construction
rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur
df2022 <- df2022 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2022$`Hauteur max (m)` <- df2022$`Hauteur max (m)`+ df2022$`Hauteur max`

#Tarifs
df2022 <- df2022 %>% replace_na(list(`Tarif annuel` = 0,Tarifs = 0))
df2022$`Tarif annuel` <- df2022$`Tarif annuel`+ df2022$Tarifs

#Fusionner Réplications 
df2022 <- df2022 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))

#Supprimer les colonnes de construction
df2022 <- df2022[,-c(9,10,13,14)]



#2021

RP_NREP20 <- read_excel("2021/20210712_ODR 2021_ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2021/20210712_ODR 2021_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2021/20210712_ODR 2021_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2021/20210712_ODR 2021_ARCEP_DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner 
df2021 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2021 <- df2021 %>% relocate(Réseau, Nature, .after = last_col())

#Supp les ODR de construction
rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Hauteur 
df2021 <- df2021 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2021$`Hauteur max (m)` <- df2021$`Hauteur max (m)`+ df2021$`Hauteur max`

#Tarifs
df2021 <- df2021 %>% replace_na(list(`Tarif annuel` = 0,Tarifs = 0))
df2021$`Tarif annuel` <- df2021$`Tarif annuel`+ df2021$Tarifs

#Fusionner Réplications 
df2021 <- df2021 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))

#Supprimer les colonnes de construction
df2021 <- df2021[,-c(9,10,13,14)]



#2020

RP_NREP20 <-  read_excel("2020/20200630_ODR 2020_ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2020/20200630_ODR 2020_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2020/20200630_ODR 2020_ARCEP_DiffHF RP REP_V1.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2020/20200630_ODR 2020_ARCEP_DiffHF RC REP_V1.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner 
df2020 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2020 <- df2020 %>% relocate(Réseau, Nature, .after = last_col())

#Supp les ODR de construction
rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2020 <- df2020 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2020$`Hauteur max (m)` <- df2020$`Hauteur max (m)`+ df2020$`Hauteur max`

#Tarifs
df2020 <- df2020 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2020-2024` = 0,Tarifs = 0))
df2020$`Tarif annuel, moyenné sur la période 2020-2024` <- df2020$`Tarif annuel, moyenné sur la période 2020-2024`+ df2020$Tarifs
df2020<- df2020 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2020-2024`)

#Fusionner Réplications 
df2020 <- df2020 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))


#Supprimer les colonnes de construction

df2020 <- df2020[,-c(7:11,14,15,18)]



#2019

RP_NREP20 <- read_excel("2019/20190429_ODR 2019_ARCEP_DiffHF RP NREP.xlsx", range = "A3:Q74") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2019/20190429_ODR 2019_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2019/20190522_ODR 2019_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2019/20190529_ODR 2019_ARCEP_DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Fusionner

df2019 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)


#Replacer les colonnes Réseau et Nature en fin de df 
df2019 <- df2019 %>% relocate(Réseau, Nature, .after = last_col())

#Supp les ODR de construction
rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2019 <- df2019 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2019$`Hauteur max (m)` <- df2019$`Hauteur max (m)`+ df2019$`Hauteur max`

#Tarifs
df2019 <- df2019 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2019-2023` = 0,Tarifs = 0))
df2019$`Tarif annuel, moyenné sur la période 2019-2023` <- df2019$`Tarif annuel, moyenné sur la période 2019-2023`+ df2019$Tarifs
df2019<- df2019 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2019-2023`)

#Fusionner Réplications 
df2019 <- df2019 %>% mutate(`Réplication towerCast` = coalesce(`Réplication towerCast (non publié)`, `Réplication towerCast`))

#Supprimer les colonnes de construction

df2019 <- df2019[,-c(1,8:12,15:18,21:24)]

#2018

RP_NREP20 <- read_excel("2018/20180504_ODR 2018_ARCEP_DiffHF RP NREP.xlsx", range = "B3:N76") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2018/20180504_ODR 2018_ARCEP_DiffHF RC NREP.xlsx", range = "B3:N5") %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2018/20180516_ODR 2018_ARCEP_DiffHF RP REP.xlsx", range = "B3:N78") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2018/20180516_ODR 2018_ARCEP_DiffHF RC REP V2.xlsx", range = "B3:P1353") %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2018 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2018 <- df2018 %>% relocate(Réseau, Nature, .after = last_col())

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)


#Nettoyer

#Hauteur 
df2018 <- df2018 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2018$`Hauteur max (m)` <- df2018$`Hauteur max (m)`+ df2018$`Hauteur max`

#Tarifs
df2018 <- df2018 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2018-2022` = 0,Tarifs = 0))
df2018$`Tarif annuel, moyenné sur la période 2018-2022` <- df2018$`Tarif annuel, moyenné sur la période 2018-2022`+ df2018$Tarifs
df2018<- df2018 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2018-2022`)

#Fusion Réplication
df2018 <- df2018 %>% mutate(Réplication = coalesce(Opérateur, Réplication))

#Supprimer les colonnes de construction et inutiles 

df2018 <- df2018[,-c(7:11,14:16,19,20,22,23)]


#2017

RP_NREP20 <- read_excel("2017/ODR 2017__ARCEP_DiffHF RP NREP.xlsx", range="B5:N78") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2017/ODR 2017_ARCEP_DiffHF RC NREP.xlsx", range="B5:Q7") %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2017/ODR 2017_ARCEP_DiffHF RP REP_Final.xlsx", 
                       sheet = "Tarifs RP REP au 1er juin 2017", 
                       range = "B3:M78") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2017/ODR 2016_ARCEP_DiffHF RC REP_Final.xlsx", 
                       sheet = "Tarifs RC REP au 1er juin 2017 ", range ="B3:P1466") %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2017 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2017 <- df2017 %>% relocate(Réseau, Nature, .after = last_col())


rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2017 <- df2017 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2017$`Hauteur max (m)` <- df2017$`Hauteur max (m)`+ df2017$`Hauteur max`

#Tarifs
df2017 <- df2017 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2017-2021` = 0,Tarifs = 0))
df2017$`Tarif annuel, moyenné sur la période 2017-2021` <- df2017$`Tarif annuel, moyenné sur la période 2017-2021`+ df2017$Tarifs
df2017<- df2017 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2017-2021`)

#Fusionner opérateurs
df2017 <- df2017 %>% mutate(Réplication = coalesce(Opérateur, Réplication))

#Supprimer les colonnes de construction et inutiles 

df2017 <- df2017[,-c(7:11,14:19,22,24:26)]


#2016

RP_NREP20 <- read_excel("2016/20160518_ODR 2016_V2__ARCEP_DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2016/20160518_ODR 2016_V2_ARCEP_DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2016/20160524_ODR 2016_V2_ARCEP_DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2016/20160524_ODR 2016_ARCEP_DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2016 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2016 <- df2016 %>% relocate(Réseau, Nature, .after = last_col())

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2016 <- df2016 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2016$`Hauteur max (m)` <- df2016$`Hauteur max (m)`+ df2016$`Hauteur max`

#Tarifs
df2016 <- df2016 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2016-2020` = 0,Tarifs = 0))
df2016$`Tarif annuel, moyenné sur la période 2016-2020` <- df2016$`Tarif annuel, moyenné sur la période 2016-2020`+ df2016$Tarifs
df2016<- df2016 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2016-2020`)

#Puissance
df2016 <- df2016 %>% replace_na(list(`Puissance max numérique (w)` = 0,`Puissance max numérique` = 0))
df2016$`Puissance max numérique (w)` <- df2016$`Puissance max numérique (w)`+ df2016$`Puissance max numérique`


#Supprimer les colonnes de construction et inutiles 

df2016 <- df2016[,-c(7:11,14:18,21)]

#2015

RP_NREP20 <- read_excel("2015/20150424_ODR 2015_ARCEP_Tarifs DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2015/20150424_ODR 2015_ARCEP_Tarifs DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2015/20150505_ODR 2015_ARCEP_Tarifs DiffHF RP REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2015/20150505_ODR 2015_ARCEP_Tarifs DiffHF RC REP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2015 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2015 <- df2015 %>% relocate(Réseau, Nature, .after = last_col())


rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2015 <- df2015 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2015$`Hauteur max (m)` <- df2015$`Hauteur max (m)`+ df2015$`Hauteur max`

#Tarifs
df2015 <- df2015 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2015-2019` = 0,Tarifs = 0))
df2015$`Tarif annuel, moyenné sur la période 2015-2019` <- df2015$`Tarif annuel, moyenné sur la période 2015-2019`+ df2015$Tarifs
df2015<- df2015 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2015-2019`)

#Puissance
df2015 <- df2015 %>% replace_na(list(`Puissance max numérique (w)` = 0,`Puissance max numérique` = 0))
df2015$`Puissance max numérique (w)` <- df2015$`Puissance max numérique (w)`+ df2015$`Puissance max numérique`

#Supprimer les colonnes de construction et inutiles 
df2015 <- df2015[,-c(7:12,15:18)]

#2014

RP_NREP20 <- read_excel("2014/20141205_ODR 2014_ARCEP_Tarifs DiffHF RP NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2014/20141205ODR 2014_ARCEP_Tarifs DiffHF RC NREP.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2014/20141205_ODR 2014_ARCEP_Tarifs DiffHF RP REP_feeder.xlsx", skip=1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2014/20141205_ODR 2014_ARCEP_Tarifs DiffHF RC REP_feeder.xlsx", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2014 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2014 <- df2014 %>% relocate(Réseau, Nature, .after = last_col())

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

#Hauteur 
df2014 <- df2014 %>% replace_na(list(`Hauteur max (m)` = 0,`Hauteur max` = 0))
df2014$`Hauteur max (m)` <- df2014$`Hauteur max (m)`+ df2014$`Hauteur max`

#Tarifs
df2014 <- df2014 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2014-2018` = 0,Tarifs = 0))
df2014$`Tarif annuel, moyenné sur la période 2014-2018` <- df2014$`Tarif annuel, moyenné sur la période 2014-2018`+ df2014$Tarifs
df2014<- df2014 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2014-2018`)

#Puissance
df2014 <- df2014 %>% replace_na(list(`Puissance max numérique (w)` = 0,`Puissance max numérique` = 0))
df2014$`Puissance max numérique (w)` <- df2014$`Puissance max numérique (w)`+ df2014$`Puissance max numérique`

#Fusionner modification tarifs 
df2014 <- df2014 %>% mutate(`Modification tarif` = coalesce(`Modification tarif`, `Modification tarifs`))

#Supprimer les colonnes de construction et inutiles 

df2014 <- df2014[,-c(7:12,15:20)]

#2013

NREP20 <- read_excel("2013/Tarifs DiffHF NREP ODR 2013_ARCEP 04062013.xlsx", skip=1)%>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )
NREP20 <- NREP20 %>% mutate(Réseau = ifelse(NREP20$`Nom du site` %in% c("Millau:Levezou", "Mt-Vial", "Paris Tour Eiffel"), "complémentaire", Réseau)) 


RP_REP20 <- read_excel("2013/Tarifs DiffFH REP ODR 2013_ARCEP 18022014.xlsx", 
                       sheet = "Publication_RP_R_20130601", skip=1, range = "A3:M59") %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2013/Tarifs DiffFH REP ODR 2013_ARCEP 18022014.xlsx", 
                       sheet = "Publication_RC_R_20140218", skip=1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2013 <- bind_rows(NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2013 <- df2013 %>% relocate(Réseau, Nature, .after = last_col())

rm(NREP20, RP_REP20, RC_REP20)

#Nettoyage 

df2013 <- df2013[,-c(1,6,8,10:22, 25, 26,28,29)]


#Tarifs
df2013 <- df2013 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2013-2017` = 0,'Tarifs en Euros HT 2013' = 0))
df2013$`Tarif annuel, moyenné sur la période 2013-2017` <- df2013$`Tarif annuel, moyenné sur la période 2013-2017`+ df2013$`Tarifs en Euros HT 2013`
df2013<- df2013 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2013-2017`)
df2013 <- df2013 %>% rename("Puissance max numérique (w)"= `Puissance max numérique`)
df2013 <- df2013 %>% rename("Hauteur max (m)"= `Hauteur max`)

df2013 <- df2013[,-c(9)]



#2012

RP_NREP20 <- read_excel("2012/Publication_Diff_HF_13022013.xlsx", 
                        sheet = "RP NR", skip = 1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2012/Publication_Diff_HF_13022013.xlsx", 
                        sheet = "RC NR", skip = 1) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )
RC_NREP20$`Code IG` <- as.character(RC_NREP20$`Code IG`)

RP_REP20 <- read_excel("2012/Publication_Diff_HF_13022013.xlsx", 
                       sheet = "RP R", skip = 1) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2012/Publication_Diff_HF_13022013.xlsx", 
                       sheet = "RC R", skip = 2) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

#Nettoyer


df2012 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2012 <- df2012 %>% relocate(Réseau, Nature, .after = last_col())

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

df2012 <- df2012[,-c(5,7,9:21,26)]

df2012 <- df2012 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2012-2016` = 0,'Tarifs en Euros HT 2012' = 0,'Tarif annuel moyenné sur la période 2012-2016 1' = 0))
df2012$`Tarif annuel, moyenné sur la période 2012-2016` <- df2012$`Tarif annuel, moyenné sur la période 2012-2016`+ df2012$`Tarifs en Euros HT 2012`+ df2012$`Tarif annuel moyenné sur la période 2012-2016 1`
df2012<- df2012 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2012-2016`)
df2012 <- df2012 %>% rename("Puissance max numérique (w)"= `Puissance max numérique`)
df2012 <- df2012 %>% rename("Hauteur max (m)"= `Hauteur max`)

df2012 <- df2012[,-c(9:12)]

#2011

RP_NREP20 <- read_excel("2011/RP_NR_DiffHF_TNT_ tarifs 2011 ODR 2011.xls", skip=3) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RC_NREP20 <- read_excel("2011/RC_NR_DiffHF_TNT_ tarifs 2011 ODR 2011.xls", skip =3) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2011/RP_R_DiffHF_TNT_ tarifs 2011 ODR 2011.xls", skip = 3) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

RC_REP20 <- read_excel("2011/RC_R_DiffHF_TNT_ tarifs 2011_ODR 2011.xls", 
                       sheet = "Publication_RC_R_20111228", skip=3) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )

df2011 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2011 <- df2011 %>% relocate(Réseau, Nature, .after = last_col())

rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Nettoyer

df2011 <- df2011[,-c(5,7,9:19,24:27)]

df2011 <- df2011 %>% replace_na(list(`Tarif annuel, moyenné sur la période 2011-2015` = 0,'Tarifs en Euros HT 2011' = 0,'Tarif annuel moyenné sur la période 2011-2015 1' = 0))
df2011$`Tarif annuel, moyenné sur la période 2011-2015` <- df2011$`Tarif annuel, moyenné sur la période 2011-2015`+ df2011$`Tarifs en Euros HT 2011`+ df2011$`Tarif annuel moyenné sur la période 2011-2015 1`
df2011<- df2011 %>% rename("Tarif annuel" = `Tarif annuel, moyenné sur la période 2011-2015`)
df2011 <- df2011 %>% rename("Puissance max numérique (w)"= `Puissance max numérique`)
df2011 <- df2011 %>% rename("Hauteur max (m)"= `Hauteur max`)

df2011 <- df2011[,-c(9,10)]

#2010


RP_NREP20 <- read_excel("2010/RP_NR_DiffHF_TNT_ tarifs 2010_V1 offre référence 2010.xls", skip=3) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )
RP_NREP20$`Hauteur max` <- as.numeric(RP_NREP20$`Hauteur max`)
RP_NREP20$`Puissance max` <- as.numeric(RP_NREP20$`Puissance max`)
RP_NREP20$`Tarifs en Euros HT 2010` <- as.numeric(RP_NREP20$`Tarifs en Euros HT 2010`)
RP_NREP20 <- RP_NREP20[RP_NREP20$`Code IG` != "Code IG",]


RC_NREP20 <- read_excel("2010/RC_NR_DiffHF_TNT_ tarifs 2010_V1 offre référence 2010.xls", skip=3) %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "non réplicable"    # Valeur pour la colonne Nature
  )

RP_REP20 <- read_excel("2010/RP_R_DiffHF_TNT_ tarifs 2010_V1 offre référence 2010.xls", skip=3) %>%
  mutate(
    Réseau = "principal",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  )
RP_REP20$`Hauteur max` <- as.numeric(RP_REP20$`Hauteur max`)
RP_REP20$`Puissance max numérique` <- as.numeric(RP_REP20$`Puissance max numérique`)
RP_REP20$`Tarifs en Euros HT 2010` <- as.numeric(RP_REP20$`Tarifs en Euros HT 2010`)
RP_REP20 <- RP_REP20[RP_REP20$`Code IG` != "Code IG",]

RC_REP20 <- read_excel("2010/RC_R_DiffHF_TNT_ tarifs 2010_V1 offre référence 2010.xls", 
    sheet = "Publication_RC_R_20110411", range = "B5:N1722") %>%
  mutate(
    Réseau = "complémentaire",        # Valeur pour la colonne Réseau
    Nature = "réplicable"    # Valeur pour la colonne Nature
  ) 
RC_REP20 <- RC_REP20[RC_REP20$`Code IG` != "Code IG",]
RC_REP20$`Hauteur max` <- as.numeric(RC_REP20$`Hauteur max`)
RC_REP20$`Puissance max numérique` <- as.numeric(RC_REP20$`Puissance max numérique`)
RC_REP20$`Tarifs en Euros HT 2010` <- as.numeric(RC_REP20$`Tarifs en Euros HT 2010`)

df2010 <- bind_rows(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)

#Replacer les colonnes Réseau et Nature en fin de df 
df2010 <- df2010 %>% relocate(Réseau, Nature, .after = last_col())

#Puissance
df2010 <- df2010 %>% replace_na(list(`Puissance max` = 0,`Puissance max numérique` = 0))
df2010$`Puissance max` <- df2010$`Puissance max`+ df2010$`Puissance max numérique`


df2010<- df2010 %>% rename("Tarif annuel" = "Tarifs en Euros HT 2010")
df2010 <- df2010 %>% rename("Puissance max numérique (w)"= `Puissance max`)
df2010 <- df2010 %>% rename("Hauteur max (m)"= `Hauteur max`)

df2010 <- df2010[,-c(5,7,9,12:14)]



rm(RP_NREP20, RC_NREP20, RP_REP20, RC_REP20)


