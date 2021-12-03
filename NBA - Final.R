library(dplyr)
library(factoextra)
library(tidyr)

nba_players_data_original <- read.csv(file.choose(),header=TRUE)

View(nba_players_data_original)

nba_player_data <- nba_players_data_original [,c(5,7,8)]

View(nba_player_data)

nba_player_data[nba_player_data < 0] <- NA
nba_player_data[nba_player_data == 0] <- NA
nba_player_data <- na.omit(nba_player_data)

View(nba_player_data)

nba_player_data$X.POSITION[nba_player_data$X.POSITION == "PG"] <- "Guard" 
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "G"] <- "Guard" 
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "SG"] <- "Guard" 
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "F"] <- "Foward" 
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "SF"] <- "Foward" 
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "PF"] <- "Foward"
nba_player_data$X.POSITION[nba_player_data$X.POSITION == "C"] <- "Center"

#View(nba_player_data)

nba_player.pos=nba_player_data$X.POSITION
table(nba_player.pos)
View(nba_player.pos)

#selecionar variaveis a estudar
nba_profile <-nba_player_data [,c(2,3)]

#scale data 
nba_profile_norm <- scale(nba_profile)
View(nba_profile_norm)

"normalize data
normalize <- function(x) {
  + return((x-min(x))/(max(x)-min(x)))}
nba_profile_norm<-as.data.frame(lapply(nba_profile, normalize))"



#Metodo inercia
fviz_nbclust(nba_profile_norm, kmeans, method = "wss")

#KMEANS
km.out <- kmeans (nba_profile_norm, centers = 3, nstart = 1000 )
print(km.out)

#visualizar os resultados
km.clusters <- km.out$cluster
rownames(nba_profile_norm)<-nba_player_data$X.POSITION
fviz_cluster(list(data=nba_profile_norm, cluster = km.clusters))
table(km.clusters,nba_player_data$X.POSITION )
table(nba_player.pos)

