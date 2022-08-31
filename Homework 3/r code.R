data = read.csv("airline_survey.csv",sep = ",")
library(tidyverse)
#1
for(i in c(3,4,6,7,9:22,25)){
  data[,i] = as.factor(data[,i])
}
subdata <- data[1:1000,-c(1,2)]
str(subdata)

library(randomForest)
rf <- randomForest(satisfaction ~.,data = subdata, importance=T ,na.action = na.omit,nstart = 10)
rf

plot(rf)
legend("topright", colnames(rf$err.rate),col = 1:3,cex = 0.8,fill = 1:3)

importance(rf)
varImpPlot(rf)

#2

newdata <- subdata[,c(2,4,5,7,12,14,23)] %>% 
  mutate(
    Customer.Type = ifelse(Customer.Type=="Loyal Customer",1,0),
    Type.of.Travel = ifelse(Type.of.Travel=="Personal Travel",1,0),
    Eco_Class = ifelse(Class =="Eco",1,0),
    Eco_Plus = ifelse(Class == "Eco Plus",1,0),
    score = as.integer(Inflight.wifi.service)+as.integer(Online.boarding)+as.integer(Inflight.entertainment)-3,
    w_score = (score-min(score))/(max(score)-min(score))
    )
summary(newdata)

library(factoextra)
fviz_nbclust(newdata[,c(1,2,8,9,11)],
             FUNcluster = kmeans, #k-Means
             nstart =20,
             method ="wss",  #total within sum of square
             k.max = 20 #max number of clusters to consider
            )+
  labs(title = "Elbow Method for K-Means")+
  geom_vline(xintercept = 8,linetype =2)

fviz_nbclust(newdata[,c(1,2,8,9,11)],
             FUNcluster = kmeans, #k-Means
             nstart =20,
             method ="silhouette",  #total within sum of square
             k.max = 20 #max number of clusters to consider
)+
  labs(title = "Elbow Method for K-Means")

k = kmeans(newdata[,c(1,2,8,9,11)],centers = 8, nstart = 20)
k$cluster
fviz_cluster(k,data = newdata[,c(1,2,8,9,11)])
newdata$group = as.factor(k$cluster)
str(newdata)

ggplot(newdata,aes(group,fill = satisfaction))+
  geom_bar()

k$centers
mean(newdata$w_score)


ggplot(newdata,aes(x=group,y=score,fill = group)) + geom_boxplot()+theme(legend.position = "none")

group <- newdata %>% 
  group_by(group) %>% 
  summarise(mean_wifi = mean(as.integer(Inflight.wifi.service)),
            mean_online = mean(as.integer(Online.boarding)),
            mean_entertainment = mean(as.integer(Inflight.entertainment))) %>% 
  mutate(group = as.character(group))

summarise1 <- newdata %>% 
  summarise(mean_wifi = mean(as.integer(Inflight.wifi.service)),
            mean_online = mean(as.integer(Online.boarding)),
            mean_entertainment = mean(as.integer(Inflight.entertainment)))
group[nrow(group)+1,] <- list("ALL",summarise1$mean_wifi,summarise1$mean_online,summarise1$mean_entertainment)

library(RColorBrewer)
display.brewer.all()
rcols <- sample(brewer.pal(12,name = "Set3"),3)
barplot(data = group,cbind(mean_wifi,mean_online,mean_entertainment)~group,beside =T,col = rcols)
legend("topleft",inset = c(-0.05,-0.05), legend = c("mean_wifi","mean_online","mean_entertainment"),fill = rcols,cex=0.8,bty = "n",xpd=T,x.intersp = 0.1)
            
