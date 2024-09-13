#1 a.
set.seed(1)
a=c()
e=c()
x=c()
i = 1

repeat{
  a_tmp = sample(0:10,1)  #隨機抽樣
  e_tmp = rnorm(1,0,sqrt(2))   #常態分布
  tmp = a_tmp+e_tmp
  
  if(tmp<0 | tmp>11) next
  
  a[i] = a_tmp
  e[i] = e_tmp
  x[i] = tmp
  
  i = i+1
  
  if (length(x) == 20){
    break 
  }
}
#1 b.
f <- function(theta,x){
  result <- 0
  for( i in x){
    result <- (theta-i)/(1+(theta-i)^2) + result
    }
  return(-2*result)
}

#1 c.
f(theta = 0.3,x)

#2 a.
houseprice <- read.csv("houseprice.csv",sep = ',')
library(tidyverse)
houseprice <- houseprice %>% 
  mutate( year_type = ifelse(Build_year<1900,"centennial",
                             ifelse(Build_year<1960,"old","new")))

#2 b.
library(broom)
summary(houseprice)
houseprice$Type <- as.factor(houseprice$Type)
houseprice$year_type <- as.factor(houseprice$year_type)
houseprice$Town<- as.factor(houseprice$Town)
houseprice$University <- as.factor(houseprice$University)

plot(lm(Sale_amount~Sqft_home,data = houseprice))

edited <-houseprice[-c(2308,2320,5759),]

ggplot(edited,aes(x=Sqft_home ,y=Sale_amount/1000))+geom_point()+
  geom_smooth(method = "lm")+
  labs(y='Sale Amount (thousand)')

fit <- lm(Sale_amount~Sqft_home,data = edited)
glance(fit)
anova(fit)

fit2 <- lm(Sale_amount~., data=edited)
step(fit2)
fit3 <- lm(Sale_amount ~ Beds + Baths + Sqft_home + Sqft_lot + 
             Type + Build_year + Town + year_type, data = edited)
anova(fit,fit3)
glance(fit3)
tidy(fit3)
