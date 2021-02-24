---
title: "Twitter_extension"
author: "Florian Cafiero"
date: "03/01/2021"
output: html_document
---
 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Importation du réseau

On isole ici les comptes Twitter ayant produit 5 tweets ou plus concernant la vaccination.

```{r import}
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
sup5_nodes <- read.csv("~/Desktop/Twitter/sup10_tweetos.csv", encoding="UTF-8", comment.char="#")
sup5_edges_glob <- read.csv("~/Desktop/Twitter/sup10_edges.csv", encoding="UTF-8", comment.char="#")
sup5_nodes <- sup5_nodes[,-1] #supprime la première colonne inutile
sup5_edges_glob <- sup5_edges_glob[,-1] #supprime la première colonne inutile
sup5_nodes <- sup5_nodes[,c(2,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)] # On intervertit les deux premières colonnes, pour commencer par l'user_id
sup5_edges_glob <- sup5_edges_glob[,c(1,4,2,3,5,6,7)] # On se débrouille pour avoir source et target comme première et deuxième colonnes.
library(dplyr)
sup5_edges <- filter(sup5_edges_glob, sup5_edges_glob[,5]!= "mention") #on filtre pour ne garder que les RT et commenté
sup5_edges <- filter(sup5_edges, sup5_edges[,5]!= "RT_comment") #on filtre pour ne garder que les RT
```

## Fusion avec BDD politique

We realize a left outer join, simply using the base R function, to merge political positioning indicators and our tweeters base.

```{r political}
political <-read.csv("~/Downloads/newest_engaged_account_unique_follower_sets_oordinates_2_3.csv")
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
sup5_edges_period1 <- filter(sup5_edges_period1, sup5_edges_period1[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period1 <- filter(sup5_edges_period1, sup5_edges_period1[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period1 <- filter(sup5_edges_period1, sup5_edges_period1[,1]%in% bddfinale$user_id & sup5_edges_period1[,2] %in% bddfinale$user_id)


sup5_edges_period2 <- sup5_edges[sup5_edges$timeperiod =="2017_SEM1", ]
sup5_edges_period2 <- filter(sup5_edges_period2, sup5_edges_period2[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period2 <- filter(sup5_edges_period2, sup5_edges_period2[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period2 <- filter(sup5_edges_period2, sup5_edges_period2[,1]%in% bddfinale$user_id & sup5_edges_period2[,2] %in% bddfinale$user_id)

sup5_edges_period3 <- sup5_edges[sup5_edges$timeperiod =="2017_SEM2", ]
sup5_edges_period3 <- filter(sup5_edges_period3, sup5_edges_period3[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period3 <- filter(sup5_edges_period3, sup5_edges_period3[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period3 <- filter(sup5_edges_period3, sup5_edges_period3[,1]%in% bddfinale$user_id & sup5_edges_period3[,2] %in% bddfinale$user_id)


sup5_edges_period4 <- sup5_edges[sup5_edges$timeperiod =="2018_SEM1", ]
sup5_edges_period4 <- filter(sup5_edges_period4, sup5_edges_period4[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period4 <- filter(sup5_edges_period4, sup5_edges_period4[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period4 <- filter(sup5_edges_period4, sup5_edges_period4[,1]%in% bddfinale$user_id & sup5_edges_period4[,2] %in% bddfinale$user_id)

sup5_edges_period5 <- sup5_edges[sup5_edges$timeperiod =="2018_SEM2", ]
sup5_edges_period5 <- filter(sup5_edges_period5, sup5_edges_period5[,1]!= "NA") #on filtre les sources des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period5 <- filter(sup5_edges_period5, sup5_edges_period5[,2]!= "NA") #on filtre les destinations des liens dont l'identité n'a pas pu être  correctement crawlée
sup5_edges_period5 <- filter(sup5_edges_period5, sup5_edges_period5[,1]%in% bddfinale$user_id & sup5_edges_period5[,2] %in% bddfinale$user_id)
```

##Création des réseaux par période

```{r network_creation}
library(igraph)
network_vax <- graph.data.frame(sup5_edges, directed=TRUE, vertices = bddfinale)
network_vax_noiso <- delete_vertices (network_vax, V(network_vax)[degree(network_vax)==0]) # on enlève les isolats, pour ne pas les prendre en compte dans la détection de communauté etc. 
network_vax_p1 <- graph.data.frame(sup5_edges_period1, directed=TRUE, vertices = bddfinale)
network_vax_p2 <- graph.data.frame(sup5_edges_period2, directed=TRUE, vertices = bddfinale)
network_vax_p3 <- graph.data.frame(sup5_edges_period3, directed=TRUE, vertices = bddfinale)
network_vax_p4 <- graph.data.frame(sup5_edges_period4, directed=TRUE, vertices = bddfinale)
network_vax_p5 <- graph.data.frame(sup5_edges_period5, directed=TRUE, vertices = bddfinale)
```

## Détection de communautés - Louvain non pondéré

Pour la détection de communautés, on s'assure que l'on travaille avec le minimum d'informations requise. On retire d'abord tous les doublons potentiels:

```{r removing duplicates}
library(dplyr)
sup5_edges_community <- sup5_edges[,-c(6,7,8)]
sup5_edges_community <- unique(sup5_edges_community)
network_vax_final <- graph.data.frame(sup5_edges_community, directed=TRUE, vertices = bddfinale)
network_vax_final <- delete_vertices (network_vax, V(network_vax)[degree(network_vax)==0])
```

On s'intéresse seulement au plus grands composants du réseaux, on élimine les plus petits composants (le second plus grand composant contient 12 noeuds, le 3ème noeuds, le 4ème 6 noeuds etc.).

```{r removing small components}
components_final <- components(graph = network_vax_final)
components_final
biggest_cluster_id <- which.max(components_final$csize)
vert_ids <- V(network_vax_final)[components_final$membership == biggest_cluster_id]
network_vax_final_component <- igraph::induced_subgraph(network_vax_final, vert_ids)
```

On peut ensuite lancer la détection de communautés sur le réseau global. On tente d'abord d'aplatir le réseau, sous forme de réseau non dirigé, et d'exécuter un algorithme de Louvain.

```{r undirected approximation and Louvain}
network_vax_undirected <- as.undirected(network_vax_final_component, mode = c("collapse"))
louvain_undirected <- cluster_louvain(network_vax_undirected)
sizes(louvain_undirected)
```


## Détection de communautés - Louvain pondéré


On peut choisir de pondérer les liens entre compte par le nombre de RT.

```{r finding weight}
library(plyr)
weight <- ddply(sup5_edges,.(source_id,target_id), nrow)
weight$V1
library(dplyr)
sup5_edges_community <- left_join(sup5_edges_community, weight, by=c("source_id","target_id"))
```

On rééxecute alors l'algorithme de Louvain en intégrant au calcul le poids nouvellement créé.

```{r undirected approximation and Louvain}
network_vax_final_weighted <- graph.data.frame(sup5_edges_community, directed=TRUE, vertices = bddfinale)
network_vax_final_weighted <- delete_vertices (network_vax, V(network_vax)[degree(network_vax)==0])
components_final_weighted <- components(graph = network_vax_final_weighted)
biggest_cluster_id_w <- which.max(components_final_weighted$csize)
vert_ids_w <- V(network_vax_final_weighted)[components_final$membership == biggest_cluster_id_w]
network_vax_final_component_w <- igraph::induced_subgraph(network_vax_final_weighted, vert_ids_w)
network_vax_undirected_w <- as.undirected(network_vax_final_component_w, mode = c("collapse"))
louvain_undirected_w <- cluster_louvain(network_vax_undirected_w, weights= network_vax_undirected_w$V1 )
sizes(louvain_undirected_w)
```


## Autres algorithmes de détection de communautés 


Trop de communautés par défaut, on change d'algorithme, et on fixe le nombre de communautés, soit avec spinglass

```{r spinglass}
spinglass_undirected <- network_vax_undirected %>% cluster_spinglass(spins = 3)
```

Soit avec walktrap (marche sur CPU ISC):

```{r walktrap}
walk <- network_vax_undirected %>% cluster_walktrap() %>% cut_at(no = 3) 
```

Ou edge betweenness:

```{r edge betweenness}
eb <- network_vax_undirected %>% cluster_edge_betweenness() %>% cut_at(no = 3)
```

```{r clustering, echo=FALSE}
library(cluster)
infomap_global <- igraph::cluster_infomap(network_vax_final, nb.trials = 10, modularity = FALSE) 
```


```{r clustering, echo=FALSE}
library(igraph)
library(cluster)
edge_betweenness_global <- cluster_edge_betweenness(network_vax_noiso, directed=TRUE)
```



```{r clustering, echo=FALSE}
library(DirectedClustering)
clustering_global <- ClustF(network_vax_noiso, type = "directed", isolates = "zero", norm=1)
```

```{r clustering, echo=FALSE}
library(cluster)
infomap_p1 <- igraph::cluster_infomap(network_vax_p1, nb.trials = 7, modularity = FALSE) 
infomap_p2 <- igraph::cluster_infomap(network_vax_p2, nb.trials = 7, modularity = FALSE)
infomap_p3 <- igraph::cluster_infomap(network_vax_p3, nb.trials = 7, modularity = FALSE)
infomap_p4 <- igraph::cluster_infomap(network_vax_p4, nb.trials = 7, modularity = FALSE)
infomap_p5 <- igraph::cluster_infomap(network_vax_p5, nb.trials = 7, modularity = FALSE)
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
