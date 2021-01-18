### Universidad Politécnica Salesiana ###
### Clustering ###
### Deber 1 ##

library(readr)
segmentation <- read_csv("C:/Users/userac/Desktop/HERRAMIENTAS PARA CONTROL DE LA PRODUCCIÓN/U1/D1/segmentation.csv")
View(segmentation)

s <- segmentation[,-c(1),drop=FALSE] 
d = dist(s[,1:3], method = "euclidean") 
c=cor(s)

library(corrplot)
corrplot(c)
fits = cmdscale(d,eig=TRUE, k=2)
x = fits$points[,1] 
y = fits$points[,2]
plot(x,y, col=c("yellow","blue","orange","red"), main = "clientes Original")

## K-Means
grupos2 = kmeans(s,4)
g1 = grupos2$cluster
g2 = grupos2$size 
plot(x,y, col=c("yellow","blue","orange","red" )[g1], main = "CLIENTES K-Means")

## DHC
library("dendextend")
hcs= hclust(d, method = "complete" )
clus = cutree(hcs, 4)
dends = as.dendrogram(hcs)
dends = color_branches(dends, 4)
colors = c("yellow", "blue","orange","red")
plot(dends, fill = colors[clus], cex = 0.1 , main = "CLIENTES DHC JERARQUICO")

## Elbow
wis = c()
for (i in 1:10) 
{
  g = kmeans(s,i) 
  wis[i] = g$tot.withinss
}
plot((1:length(wis)),wis, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

##validacion interna
library(cluster)
library(clValid)
du1 = dunn(d,g1)
du2= dunn(d,clus)

sil1s = silhouette(g1,d)
plot(sil1s,col=1:4, border=NA)
sil2s = silhouette(clus,d)
plot(sil2s,col=5:8, border=NA)


##validacion externa
library(aricode)
library(plyr)
ARI1= ARI(g1,g1)
ARI2= ARI(g1,clus)
AMI1= AMI(g1,g1)
AMI2= AMI(g1,clus)
NMI1= NMI(g1,g1,variant = c("joint"))
NMI2= NMI(g1,clus,variant = c("joint"))
