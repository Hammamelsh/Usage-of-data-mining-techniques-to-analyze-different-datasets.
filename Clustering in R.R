


install.packages("ggplot2")
library(ggplot2)

install.packages("cluster")
library(cluster)

install.packages("factoextra")
library(factoextra)




#read the raw data
order_of_customers_summary= read.csv("SampleAssessment.csv", header = TRUE)
#inspecting dataset
names(order_of_customers_summary)
#rename the column names so that they can be more understandable2
colnames(order_of_customers_summary) = c("Customer_Id","First_order_DateTime","Recent_Order_DateTime","Orders_All","Orders_Last_7_days","Orders_Last_4_weeks","Amount_All","Amount_Last_7_days","Amount_Last_4_weeks","Avg_DistanceFromResturant","Avg_DeliveryTime")
#
str(order_of_customers_summary)
#summary of the data
summary(order_of_customers_summary)

#cleaning of the data and making new columns
order_of_customers_summary$First_order_Date = as.Date(order_of_customers_summary$First_order_DateTime ,format = "%m/%d/%y")
order_of_customers_summary$Recent_Order_Date = as.Date(order_of_customers_summary$Recent_Order_DateTime ,format = "%m/%d/%y")

order_of_customers_summary$Current_Date = max(order_of_customers_summary$Recent_Order_Date) + 1
order_of_customers_summary$Days_Since_Last_Order = as.numeric(order_of_customers_summary$Current_Date - order_of_customers_summary$Recent_Order_Date)
order_of_customers_summary$Days_Since_First_Order = as.numeric(order_of_customers_summary$Current_Date - order_of_customers_summary$First_order_Date)

#filtering for the cases where the order value is NA for the last 4 weeks and last 7 days for these user the minimum day is taken out from the last order
none_order_7Days = order_of_customers_summary[ is.na(order_of_customers_summary$Orders_Last_7_days),]
none_order_4Weeks = order_of_customers_summary[ is.na(order_of_customers_summary$Orders_Last_4_weeks),]

print(paste("For all the Users who had NA value in the last orders for the 7 days, the minimum value for recent order placed is",min(none_order_7Days$Days_Since_Last_Order),paste("Days"),sep = " "))
print(paste("For all the Users who had NA value in the last orders for the 4 Weeks, the minimum value for recent order placed is",min(none_order_4Weeks$Days_Since_Last_Order),paste("Days"),sep = " "))


#when looking at data we realize the minimum days are more than 28 days and 7 days for the recent orders. due to this we have the capability to assume that the NA values not missing but are actually 0. next step is to replace the values with 0.
order_of_customers_summary$Orders_Last_7_days = ifelse(is.na(order_of_customers_summary$Orders_Last_7_days),0,order_of_customers_summary$Orders_Last_7_days)
order_of_customers_summary$Orders_Last_4_weeks = ifelse(is.na(order_of_customers_summary$Orders_Last_4_weeks),0,order_of_customers_summary$Orders_Last_4_weeks)
#Average distance of restaurants is negative in a number of cases, to make it easier I am calling them as 0.also the Average order value (AOV) column was created, this will be used instead of the total order value.
order_of_customers_summary$Avg_DistanceFromResturant = ifelse(order_of_customers_summary$Avg_DistanceFromResturant<0,0,order_of_customers_summary$Avg_DistanceFromResturant)

order_of_customers_summary$AOV_All = round(order_of_customers_summary$Amount_All/order_of_customers_summary$Orders_All,0)

order_of_customers_summary$AOV_Last_7_Days = round(ifelse(order_of_customers_summary$Orders_Last_7_days ==0 ,0,order_of_customers_summary$Amount_Last_7_days/order_of_customers_summary$Orders_Last_7_days),0)

order_of_customers_summary$AOV_Last_4_Weeks = round(ifelse(order_of_customers_summary$Orders_Last_4_weeks ==0 ,0,order_of_customers_summary$Amount_Last_4_weeks/order_of_customers_summary$Orders_Last_4_weeks),0)

# segmentation customers
Q1 = 100 - round(100*sum(order_of_customers_summary$Orders_Last_7_days==0)/nrow(order_of_customers_summary),0)

Q2 = 100 - round(100*sum(order_of_customers_summary$Orders_Last_4_weeks==0)/nrow(order_of_customers_summary),0)


#filtering all data to only use the relevant columns for the building of the model
#in this cluster analysis I will be using AOV , Order Count , Avg distance from Restaurant, Avg Delivery Time and Days since 1st and last orders as the relevant columns. 
#The principal component analysis is applied to help reduce the variables more.

Filtered_data = order_of_customers_summary[ ,c(1,4:6,10,11,15:19)]
set.seed(1234)

Pca_data1 = prcomp(Filtered_data[,-1], center = T,scale. = T)
plot(Pca_data1, type = "l",
     main="Variance of PCA")
#Results from plot show that the variance is still high when you look at the 5th and 6th principal component.
# the purpose is to build a clustering model using k-means clustering.
set.seed(00909)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

filtered_data_log = log(Filtered_data[,-1]+2)

filtered_data_normal = as.data.frame(lapply(Filtered_data[,-1], normalize))


score_wss_log <- (nrow(filtered_data_log)-1)*sum(apply(filtered_data_log,2,var))
for (i in 2:15) score_wss_log[i] <- sum(kmeans(filtered_data_log,
                                               centers=i)$withinss)

plot(1:15, score_wss_log[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Log Data",
     pch=20, cex=2)

score_wss_normal <- (nrow(filtered_data_normal)-1)*sum(apply(filtered_data_normal,2,var))
for (i in 2:15) score_wss_normal[i] <- sum(kmeans(filtered_data_normal,
                                                  centers=i)$withinss)

plot(1:15, score_wss_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Normalized Data",
     pch=20, cex=2)

head(score_wss_normal)
head(score_wss_log)

distance <- dist(score_wss_normal, method = "euclidean")
print(distance)


#From the plots above we can see that the normalized data makes more sense to go ahead with. 
#Because the within group sum of squares is at much lesser scale for it.
#Along with that 3 or 4 cluster seems to be optimum number as the incremental benefit after that is minimal. 
#We are going ahead with 3 clusters.

minvec <- sapply(Filtered_data[,-1],min)
maxvec <- sapply(Filtered_data[,-1],max)
denormalize <- function(x,minval,maxval) {
  return(x*(maxval-minval) + minval)
}
set.seed(009)
km_3_cluster_normal = kmeans(filtered_data_normal,3,nstart = 100)  

km_3_cluster_actual = NULL
test1 = NULL

for(i in 1:10)
{
  test1 = (km_3_cluster_normal$centers[,i] * (maxvec[i] - minvec[i])) + minvec[i]
  km_3_cluster_actual = cbind(km_3_cluster_actual,test1)
}

colnames(km_3_cluster_actual) = colnames(Filtered_data[-1])
print("Mean value of all variables in each cluster is given below")
km_3_cluster_actual
print("Numbers of customers in each cluster is given below")
km_3_cluster_normal$size
#km_3_cluster_normal$centers
#fviz_cluster(km_3_cluster_normal,data = filter_data_normal)

#filtering out users who did not interact in the last 4 weeks would give a better cluster which is more accurate.
filtered_order_data = Filtered_data[ Filtered_data$Orders_Last_4_weeks !=0,]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

filtered_order_data_normal = as.data.frame(lapply(filtered_order_data[,-1], normalize))

score_wss_order_normal <- (nrow(filtered_order_data_normal)-1)*sum(apply(filtered_order_data_normal,2,var))
for (i in 2:15) score_wss_order_normal[i] <- sum(kmeans(filtered_order_data_normal,
                                                        centers=i)$withinss)

plot(1:15, score_wss_order_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Active Users",
     pch=20, cex=2)

#using 4 so we can get a better distinction in user segiments.

minvec1 <- sapply(filtered_order_data[,-1],min)
maxvec1 <- sapply(filtered_order_data[,-1],max)
denormalize <- function(x,minval,maxval) {
  return(x*(maxval-minval) + minval)
}

set.seed(0091)
km_2_cluster_normal_order_data = kmeans(filtered_order_data_normal,2,nstart = 100)  
km_4_cluster_normal_order_data = kmeans(filtered_order_data_normal,4,nstart = 100)  

km_2_cluster_actual_order_data = NULL
km_4_cluster_actual_order_data = NULL


test1 = NULL

for(i in 1:10)
{
  test1 = (km_2_cluster_normal_order_data$centers[,i] * (maxvec1[i] - minvec1[i])) + minvec1[i]
  km_2_cluster_actual_order_data = cbind(km_2_cluster_actual_order_data,test1)
}

colnames(km_2_cluster_actual_order_data) = colnames(Filtered_data[-1])


test1 = NULL

for(i in 1:10)
{
  test1 = (km_4_cluster_normal_order_data$centers[,i] * (maxvec1[i] - minvec1[i])) + minvec1[i]
  km_4_cluster_actual_order_data = cbind(km_4_cluster_actual_order_data,test1)
}

colnames(km_4_cluster_actual_order_data) = colnames(Filtered_data[-1])
km_2_cluster_actual_order_data = NULL
km_4_cluster_actual_order_data = NULL

#km_2_cluster_actual_order_data
#km_2_cluster_normal_order_data$size
print("Mean value of all variables in each cluster is given below")
km_4_cluster_actual_order_data

print("Numbers of customers in each cluster is given below")
km_4_cluster_normal_order_data$size

#km_3_cluster_normal$centers
#fviz_cluster(km_2_cluster_normal_order_data,data = filtered_order_data_normal)
print("Graph below gives the clusters visubalization on 4 clusters")
fviz_cluster(km_4_cluster_normal_order_data,data = filtered_order_data_normal)
