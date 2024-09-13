client <- read.csv("client_list.csv", sep = ",")
product <- read.csv("product_list.csv", sep = ",")
sales <- read.csv("salesdata.csv", sep = ",")
library(tidyverse)

#1
product %>% 
  separate(Item , c ( "Product","Item" ), sep = "_") -> product

#2
product$Product <- as.numeric(product$Product)
full.table <- client %>% 
  inner_join(sales) %>% 
  inner_join( product)

#3
full.table %>% 
  mutate( spend = UnitPrice * Quantity) -> full.table

#4
full.table %>% 
  mutate( group = ifelse( Membership =="gold" | Membership =="diamond","gold & diamond","others")) %>% 
  group_by(group) %>% 
  summarise(mean(Age), male = sum(Gender=="male"),mean(spend))

#5
full.table %>% 
  filter(Gender == "female") %>% 
  summarise(mean(Age),mean(spend),median(spend),sum(spend))

Totals <- full.table %>% 
  group_by(Product) %>% 
  filter(Gender == "female") %>% 
  summarise (TotalSpend= sum(spend))

Totals %>% 
  ggplot(aes(x= Product, y= TotalSpend), fill = Product)+
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits = Totals$Product) 
