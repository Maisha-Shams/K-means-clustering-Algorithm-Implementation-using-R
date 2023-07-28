
#loading dataset from drive 
mydata<- read.csv('D:/wine.csv',header = TRUE,sep = ',')
mydata


#Printing maximum no. of rows 
options(max.print = 10000)

#scaling the wine data
wine_data_scaled <- scale(mydata)

#library to visualize data clusters
install.packages("factoextra")
library(factoextra)

#Finding the Distance matrix
wine_data <- dist(wine_data_scaled)
wine_data 

#using the within sum squares to visualize the optimum no. of clusters
fviz_nbclust(wine_data_scaled, kmeans, method = "wss")+
  labs(subtitle="Elbow Method")


#using the silhouette width to visualize the optimum no. of clusters
fviz_nbclust(wine_data_scaled, kmeans, method = "silhouette")+
  labs(subtitle="silhouette Method")


#using the gap statistic to visualize the optimum no. of clusters
fviz_nbclust(wine_data_scaled, kmeans, method = "gap_stat")+
  labs(subtitle="Gap Method")

# create cluster biplot using the principle components
fviz_cluster(kmeans(wine_data_scaled, centers = 3, iter.max = 100, nstart = 100), 
             data = wine_data_scaled)


install.packages("dplyr")
library(dplyr)
# visualize clusters using original variables
clusters <- kmeans(wine_data_scaled, centers = 3, iter.max = 100, nstart = 100)
Wine  <- mydata |> mutate(cluster = clusters$cluster)
Wine |> ggplot(aes(x = Alcohol, y = Mg, col = as.factor(cluster))) + geom_point()



