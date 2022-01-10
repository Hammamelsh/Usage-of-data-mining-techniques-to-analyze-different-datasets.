
#association rule
install.packages("plyr")
library(plyr)

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

install.packages("rattle")
library(rattle)

install.packages("dplyr")
library(dplyr)

Groceries_data<-read.csv("Groceries_dataset.csv", header = T,colClasses = "factor")
names(Groceries_data)
head(Groceries_data, 20)

yes <- colSums(Groceries_data == "Yes")
yes

no <-colSums(Groceries_data == "No")
no

purchased <-rbind(yes,no)
purchased

barplot(purchased,legend=rownames(purchased)) #Plot 1
barplot(purchased, beside=T,legend=rownames(purchased))

sum(is.na(Groceries_data))

#convert member number to numeric
sorting<- Groceries_data[order(Groceries_data$Member_number),]
#convert item description to categorical format
sorting$Member_number<-as.numeric(sorting$Member_number)
str(sorting)
#Group all the item that were brought together by the same customer on the same date
Listofitems <- ddply(sorting, c("Member_number","Date"), 
                     function(df1)paste(df1$itemDescription,collapse = ","))
head(Listofitems,15)

#remove member number and date
Listofitems$Member_number<- NULL
Listofitems$Date <- NULL
colnames(Listofitems)<-c("Listofitems")

write.csv(Listofitems,"Listofitems.csv", quote = FALSE, row.names = TRUE)
head(Listofitems)

#Convert CSV file to Basket Format
distoftxn = read.transactions(file="Listofitems.csv", rm.duplicates= TRUE, 
                              format="basket",sep=',',cols=1);
print(distoftxn)
itemFrequencyPlot(distoftxn, topN =20)
#remove quotes from Transaction
distoftxn@itemInfo$labels <- gsub("\" ","",distoftxn@itemInfo$labels)

#Apriori Algorithm 
rules_basket <- apriori(distoftxn, parameter = list(minlen=2, sup = 0.001, conf = 0.05, target="rules"))
#Total rules generated
print(length(rules_basket))

summary(rules_basket)

inspect(rules_basket[1:20])
inspect(head(sort(rules_basket, by ="lift"),5))
#Visulation of the association rule
plot(rules_basket, jitter = 0)

plot(rules_basket, method = "grouped", control = list(k = 5))

plot(rules_basket[1:20], method="graph")

plot(rules_basket[1:20], method="paracoord")

plot(rules_basket@quality)
yes<-colSums(Groceries_data == "yes")

yes