
#Decision tree using ctree
install.packages("party")
library(party)

Brcancer_data <- read.csv ("data 2.csv" , header = TRUE)
names(Brcancer_data)
dim(Brcancer_data)
head(Brcancer_data)
str(Brcancer_data)
attributes(Brcancer_data)
tail(Brcancer_data)
summary(Brcancer_data)
nrow(Brcancer_data)
ncol(Brcancer_data)

#set column 33 to NUL

any(is.na(Brcancer_data))
Brcancer_data$diagnosis<-as.factor(Brcancer_data$diagnosis)
Brcancer_data[,33]<- NULL

#visulaize using corrplot
install.packages("corrplot")
library(corrplot)
prop.table(table(Brcancer_data$diagnosis))

Matrix_corr <- cor(Brcancer_data[,3:ncol(Brcancer_data)])
corrplot(Matrix_corr, order = "hclust", tl.cex = 1, addrect = 8)

#
set.seed(1234)
pd<-sample(2, nrow(Brcancer_data),replace = TRUE,prob = c(0.8,0.2))
pd
train<- Brcancer_data[pd==1,]
validate <- Brcancer_data[pd==2,]

dim(train)
dim(validate)

#Pca is applied for the pre-processing

pca_res <- prcomp(Brcancer_data[,3:ncol(Brcancer_data)], center = TRUE, scale = TRUE)
plot(pca_res, type="l")


#applying of package party to design decision tree
library(party)
Brcancer_tree <- ctree(diagnosis ~ ., data=train)
plot(Brcancer_tree)

predict(Brcancer_tree)
tab<- table(predict(Brcancer_tree),train$diagnosis)
tab

sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)
test_table<- table(predict(Brcancer_tree, newdata=validate),validate$diagnosis)
test_table
sum(diag(test_table))/sum(test_table)
1-sum(diag(test_table))/sum(test_table)
#https://www.kaggle.com/ljanjughazyan/breast-cancer-classification
