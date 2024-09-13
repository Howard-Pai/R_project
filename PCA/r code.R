library(reshape2)
library(ggplot2)
library(stats)
library(nsprcomp)
library(base)
library(ggfortify)
library(tidyverse)
data = read.csv("financialdata.csv",sep=",")
for(i in c(12,15,16)){
  for(j in 1:nrow(data)){
    data[j,i] = gsub('[,]','',data[j,i])
  }
  data[,i] = as.numeric(data[,i])
}

pca <- prcomp(data[,-1],center = T,scale = T)
screeplot(pca)
summary(pca)

pve =(pca$sdev)^2/sum(pca$sdev^2)
plot(cumsum(pve),xlab="Principle Component", ylab="Cumulative Propotion of Variance Explained",ylim=c(0,1),type = 'b')
abline(h=0.8,col = "blue")

ggplot(melt(pca$rotation[,1:3]),aes(Var2,Var1))+
  geom_tile(aes(fill = value),colour = "white")+
  scale_fill_gradient2(low = "darkblue",high = "khaki",
                       mid = "white",midpoint = 0)+
  guides(fill=guide_legend(title = "Correlation"))+
  theme_bw()+
  theme(axis.text = element_text(angle=45,hjust = 1,vjust = 1),
        axis.title = element_blank())


autoplot(prcomp(data,center = T,scale = T),x=1,y=2,
         loadings = T,loadings.colour = "red",
         loadings.label = T)
biplot(pca,scale = T,choices = 2:3)

data2 <- cbind(data,pca$x)
head(data2[order(data2$PC1,decreasing = T),]$comp_id)
head(data2[order(data2$PC2,decreasing = T),]$comp_id)
head(data2[order(data2$PC3,decreasing = F),]$comp_id)


k <- kmeans(data2[,18:20],centers = 8)
group <- as.factor(k$cluster)
data3 <- cbind(data2[,c(1,18:20)],group)
library(plotly)
plot_ly(data3, x = ~PC1, y = ~PC2, z = ~PC3,color = group, size=20,text = ~paste("Comp_id:",comp_id)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))
