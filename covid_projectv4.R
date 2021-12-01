
covid_data_original <- read.csv(file.choose(),header=TRUE)

View(covid_data_original)

covid_data <- na.omit(covid_data_original)

View(covid_data)

covid <- covid_data [,c(1,9,7)]

View(covid)
#normalizar

covid$Total.Cases.1M.population<-normalize(covid$Total.Cases.1M.population)

#----------------------------------------
View(covid)
view(covid_norm)

covid.country=covid$Country 

covid_numbers <- covid [2:4]

View(covid_numbers)

#distance 
?dist
covid_numbers_dist <- dist(covid_numbers)

covid_numbers_dist

#how many clusters we need
library(factoextra)
fviz_nbclust(covid_numbers, kmeans, method = "wss")

#kmeans

km.out <- kmeans (covid_numbers, centers = 3, nstart = 10 )

print(km.out)

#visualize the clustering algorithm results
km.clusters <- km.out$cluster
rownames(covid_numbers)<-covid$Country
fviz_cluster(list(data=covid_numbers, cluster = km.clusters))

