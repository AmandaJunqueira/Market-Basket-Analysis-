
# header


#load libraries
library(RMySQL)
library(arules)
library(dplyr)
library(ggplot2)
library(plyr)
library(arulesViz)
library(recommenderlab)
library(data.table)
library(tidyverse)
library(knitr)

#connect to the local host
con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'root1234',
                 host = '127.0.0.1',
                 dbname='imarket_sql1')
dbListTables(con)
dbWriteTable(con, name='table_name', value=data.frame.name)

#read the data - converting them
#line_item = data
rs = dbSendQuery(con, "select * from line_item")
data = fetch(rs, n=-1)

#products = data2
rs2 = dbSendQuery(con, "select * from products")
data2 = fetch(rs2, n=-1)

#orders = data3
rs3 = dbSendQuery(con, "select * from orders")
data3 = fetch(rs3, n=-1)

#Check that all orders in line_item are present in our orders dataset. 
#Exclude from line_item any rows that do not meet that condition.
datamerge <- merge(data3, data)
mergeName <- merge(data, data2)

#Exclude from line_item any rows from orders that are not “Completed”.
datamerge <- subset(datamerge,state == "Completed")

#Check that all products in line_item are present in the products dataset. 
#Exclude from line_item any rows that do not meet that condition.
mergeProducts <- merge(datamerge, data2)


#Creating A New Talble to Compare Values Of Purchases
mergeProducts$unit_price <- as.numeric(gsub(",", ".", gsub("\\.", "", mergeProducts$unit_price)))
mergeProducts$total_paid <- as.numeric(gsub(",", ".", gsub("\\.", "", mergeProducts$total_paid)))

ComparingPrices <- select(mergeProducts, id_order, unit_price, product_quantity, total_paid)

ComparingPrices$OrderResult <- ComparingPrices$unit_price * ComparingPrices$product_quantity

ComparingPrices <- ComparingPrices[,c(1,3,2,5,4)]

Comparison <- select(ComparingPrices, id_order, OrderResult, total_paid)

#Final Table to Compare Values
ComparingValues <- Comparison %>% group_by(id_order) %>% transmute(Total=sum(OrderResult), total_paid)
ComparingValues$TotalDifference <- (ComparingValues$total_paid - ComparingValues$Total)
ComparingValues$TotalDifference <- as.vector(ComparingValues$TotalDifference)
ComparingValues <- ComparingValues[,c(1,2,3,4)]



#Sorting By Biggest Difference 
abs(as.numeric(ComparingValues$TotalDifference))
sorted <- abs(transform(ComparingValues, TotalDifference = as.numeric(TotalDifference)))

SortedValues <- arrange(sorted, TotalDifference, decreasing = TRUE)

#Deletind Difference Bigger Than 30
TransactionalTable <- SortedValues %>% filter(TotalDifference < 30)

#Merging with another table to have the  "sku"
Transactional_Table <- merge(TransactionalTable, mergeName)
TransactionalDf <- select(Transactional_Table, id_order, sku, name_en)

#CREATING THE TRANSACTIONAL FILE
#mergeProducts <- select(mergeProducts, id_order, sku)
write.csv(TransactionalDf, "data/MyData.csv", row.names = FALSE)
MyData <- read.csv("data/MyData.csv")



transactional <-
  read.transactions("data/MyData.csv", 
                    format = "single", 
                    sep = ",",
                    header = TRUE,
                    cols = c("id_order", "name_en"))


association.rules <- apriori(transactional, parameter = list(supp = 0.0003, conf = 0.03))

plot(association.rules, method = "grouped")

itemFrequencyPlot(transactional, topN = 5,cex.names=0.5, type = "absolute")
inspect(association.rules)


#RecommenderTable <- Transactional_Table %>% 
  #select(id_order, sku)

#Erase Duplicates
n_occur <- Transactional_Table %>% 
  mutate(skul = paste(id_order, sku, sep = ' '))
n_occur <- n_occur[!duplicated(n_occur$skul),] %>% 
  select(-skul)


#RecommenderLab
#Matrix
Pre.Matrix <- n_occur %>%
  select(id_order, sku) %>% 
  mutate(value = 1) %>% 
  spread(sku, value, fill = 0) %>% 
  select(-id_order) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")

#CV
scheme <- Pre.Matrix %>% 
  evaluationScheme(method = "cross",
                   k = 1,
                   train = 0.8,
                   given = -1)

algorithms <- list(
  "association rules" = list(name = "AR",
                             param = list(supp = 0.0003, conf = 0.03)),
  #"random items" = list(name = "RANDOM", param = NULL),
  #"popular items" = list(name = "POPULAR", param = NULL),
  #"item-based CF" = list (name = "IBCF", param = list(k = 1)),
  "user-based CF" = list(names = "UBCF", param = list(method = "Cosine", nn = 500)))

results <- recommenderlab::evaluate(scheme, 
                                    algorithms,
                                    type = "topNList",
                                    n = c(1, 5, 10))




