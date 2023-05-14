#kMeans clustering
cluster_data <- perConvictions 
cluster <- cluster_data[,2:13]
head(cluster)
library(caret)
dummy <- dummyVars("~.",data = cluster)
newdata <- data.frame(predict(dummy,newdata=cluster))
newdata
km = kmeans(newdata,4, nstart=25)
head(km)
summary(km)
km

#cluster plot
fviz_cluster(km, data = newdata,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#FF6347"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

#Silhouttee cluster plot
set.seed(123)
fviz_nbclust(newdata, kmeans, method = "silhouette")

#Silhoutte plot
require("cluster")
sil <- silhouette(km$cluster, dist(cluster))
fviz_silhouette(sil)

