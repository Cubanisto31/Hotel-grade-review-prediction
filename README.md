# Hotel grade review prediction

## Contexte 
*Officiellement ce projet a pour objectif d’explorer la relation qu’il existe
entre un commentaire et la note que les clients laissent sur Tripadvisor.
Officieusement ce projet était surtout un cas pratique me permettant de
reprendre en main différents outils tels que R et Rmd.*

La base de données New_Delhi_reviews provient de 
[Kaggle]([https://huggingface.co/datasets/patrickbdevaney/tripadvisor_hotel_reviews](https://www.kaggle.com/datasets/arnabchaki/tripadvisor-reviews-2023).

## Structure du dépôt
Le dépôt contient quatre fichiers (hors README.md).

### Presentation-vF.pdf 
Ce fichier est la présentation permettant de restituer le travail réalisé dans le cadre de ce cas pratique. Il synthétise les grands messages et les résultats. **À lire en priorité**.

### Presentation vF.Rmd
Ce fichier est le code au format Rmd qui permet de générer le pdf mentionné _supra_. 

### Exploration des data v2.R 
Ce fichier est le code brut qui a permis d'alimenter la première partie du ficher mentionné _supra_ relative à l'estimation des notes par taille de commentaire. 

### Text mining tripadvisor v2.R 
Ce fichier est le code qui a permis d'alimenter la seconde partie du fichier mentionné _supra_ relative à l'observation de la fréquence d'apparition des différents mots **et** à la création d'un modèle d'estimation des notes. 

## Conclusion 
En l'espèce ce travail manque d'un support théorique, d'une problématique, d'un terrain mieux documenté etc.. toutefois il permet de s'initier à différent concepts tels que l'entrainement d'un modèle de catégorisation, les métriques de mesure des performances d'un modèle de catégorisation, la statistique du Chi 2 et la méthode du TF-IDF. 
