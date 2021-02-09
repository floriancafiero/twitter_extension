## Importation du réseau

On isole ici les comptes Twitter ayant produit 5 tweets ou plus concernant la vaccination.

```{r import}
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
sup5_nodes <- read.csv("~/Desktop/Twitter/sup10_tweetos.csv", encoding="UTF-8", comment.char="#")
sup5_edges <- read.csv("~/Desktop/Twitter/sup10_edges.csv", encoding="UTF-8", comment.char="#")
sup5_nodes <- sup5_nodes[,-1] #supprime la première colonne inutile
sup5_edges <- sup5_edges[,-1] #supprime la première colonne inutile
sup5_nodes <- sup5_nodes[,c(2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)] # On intervertit les deux premières colonnes, pour commencer par l'user_id
sup5_edges <- sup5_edges[,c(1,4,2,3,5,6,7)] # On se débrouille pour avoir source et target comme première et deuxième colonnes.
```

## Fusion avec BDD politique

We realize a left outer join, simply using the base R function, to merge political positioning indicators and our tweeters base.

```{r political}
political <-read.csv("~/Downloads/newest_engaged_account_unique_follower_sets_oordinates 2 3.csv")
sup5_nodes_political <- merge(sup5_nodes, political, by=c("user_id"), all.x = TRUE)
```

## Fusion avec codage quali

We realize a left outer join, simply using the base R function, to merge political positioning indicators and our tweeters base.

```{r qualit}

quali <-read.csv("~/Downloads/codage_quali_tweetos.csv", sep=";", comment.char="#")
bddfinale <- merge(sup5_nodes_political, quali, by=c("user_id"), all.x = TRUE)
bddfinale <- bddfinale[-c(38042, 44613,  44656, 45618, 45835,  46433, 46970, 46985, 47538, 47590, 47871, 48530, 49395, 49412,  49830,  50158, 50177,  50396,  50674, 50991, 51183, 51356, 51488, 51935, 52603, 52679, 52916, 53135, 53276,53493),]
n_occur <- data.frame(table(bddfinale$user_id))
n_occur[n_occur$Freq > 1,] #on vérifie l'absence de doublons
```

## Création de périodes de temps et de bases de données associées

```{r period}
library("dplyr")
library("forcats")
library("questionr")
sup5_edges <- filter(sup5_edges, sup5_edges[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges <- filter(sup5_edges, sup5_edges[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges <- filter(sup5_edges, sup5_edges[,1]%in% bddfinale$user_id & sup5_edges[,2] %in% bddfinale$user_id)
sup5_edges$timeperiod <- as.Date(sup5_edges$time) #on pense à bien mettre la date au format date pour permettre les comparaisons, et on se permet de négliger l'heure
sup5_edges$timeperiod = case_when(
    sup5_edges$timeperiod < "2016-06-01" ~ "avant_juin_2016",
    "2016-05-31" < sup5_edges$timeperiod & sup5_edges$timeperiod < "2017-01-01" ~ "2016_SEM2",
    "2016-12-31" < sup5_edges$timeperiod & sup5_edges$timeperiod < "2017-06-01" ~ "2017_SEM1",
    "2017-05-31" < sup5_edges$timeperiod & sup5_edges$timeperiod < "2018-01-01" ~ "2017_SEM2",
    "2017-12-31" < sup5_edges$timeperiod & sup5_edges$timeperiod < "2018-06-01" ~ "2018_SEM1",
    "2018-05-31" < sup5_edges$timeperiod & sup5_edges$timeperiod < "2019-01-01" ~ "2018_SEM2",
    "2019-01-01" < sup5_edges$timeperiod ~ "apres_janv_2019")

sup5_edges_period1 <- sup5_edges[sup5_edges$timeperiod =="2016_SEM2", ]
sup5_edges_period2 <- sup5_edges[sup5_edges$timeperiod =="2017_SEM1", ]
sup5_edges_period3 <- sup5_edges[sup5_edges$timeperiod =="2017_SEM2", ]
sup5_edges_period4 <- sup5_edges[sup5_edges$timeperiod =="2018_SEM1", ]
sup5_edges_period5 <- sup5_edges[sup5_edges$timeperiod =="2018_SEM2", ]

exam <- c(sup5_edges[,1], sup5_edges[,2]) %in% bddfinale[,1]
exam <- as.data.frame(exam)
```


 


##Création des réseaux par période

```{r network_creation}
library(igraph)
library(geomnet)
network_vax <- graph.data.frame(sup5_edges, directed=TRUE, vertices = bddfinale)
network_vax_p1 <- graph.data.frame(sup5_edges_period1, directed=TRUE, vertices = bddfinale)
network_vax_p2 <- graph.data.frame(sup5_edges_period2, directed=TRUE, vertices = bddfinale)
network_vax_p3 <- graph.data.frame(sup5_edges_period3, directed=TRUE, vertices = bddfinale)
network_vax_p4 <- graph.data.frame(sup5_edges_period4, directed=TRUE, vertices = bddfinale)
network_vax_p5 <- graph.data.frame(sup5_edges_period5, directed=TRUE, vertices = bddfinale)
```


## Détection de communautés

```{r clustering, echo=FALSE}
library(cluster)

```


## Préparation analyse de réseau réplicable

To make our computations replicable, we set an arbitrary seed.

```{r replication, echo=FALSE}
library(cluster)
library(statnet)
set.seed(12345)
statnet::update_statnet()
```




## STERGM

We use Separable Temporal Exponential family Random Graph Models (STERGM) to model the creation and destruction of links across the period. Two equations for each process are estimated.
