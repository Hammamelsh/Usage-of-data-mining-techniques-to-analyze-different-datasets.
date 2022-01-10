install.packages("shinydashboard")
install.packages("shiny")
library(shiny)
library(arules)

library(shinydashboard)

Groceries_data<-read.csv("Groceries_dataset.csv", header = T,colClasses = "factor")
names(Groceries_data)
head(Groceries_data, 20)


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


ui <- dashboardPage(
  dashboardHeader(title = "My dashboard"),
  dashboardSidebar(
    sidebarMenu(),
  dashboardBody(
    box(plotOutput("MBA"), width = 8)
  )
)

server <- function(input, output){
  output$MBA <- renderPlot({
    plot(rules_basket, method = "grouped", control = list(k = 5))
    
  })
  
}

shinyApp(ui, server)

plot(rules_basket, jitter = 0))