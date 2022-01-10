Hotel_reviews1 = read.csv("hotel_reviews.csv")
Distinct_hotel_names <- unique()
install.packages("tidyverse")
library(tidyverse)

Hotel_reviews1[, c("Hotel.Restaurant.name")]
colnames(Hotel_reviews1)

Hotelname_column <- Hotel_reviews1$Hotel.Restaurant.name
view(Hotelname_column)
length(Hotelname_column)

Hotelname_unique <- unique(Hotelname_column)
length(Hotelname_unique)
view(Hotelname_unique)

Hotel_reviews1[, c("Location")]

unique(c(Hotelname_unique,Hotel_reviews1[,2]))
colnames(Hotel_reviews1)

          
library(dplyr)
Ndf <- Hotel_reviews1 %>% group_by(Hotel.Restaurant.name, Location)%>%
tally()
view(Ndf)

#use Chalong thale as a location
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
Anns_Kitchen_Bar<-subset(Hotel_reviews1,
                        Hotel.Restaurant.name=="Ann's Kitchen Bar and Grill")
head(Anns_Kitchen_Bar$Review)

Audy_Restaurant_data<-subset(Hotel_reviews1,
                           Hotel.Restaurant.name=="Audy Restaurant")
head(Audy_Restaurant_data$Review)

Bampot_Kitchen_Bar<-subset(Hotel_reviews1,
                           Hotel.Restaurant.name=="Bampot Kitchen & Bar")
head(Bampot_Kitchen_Bar$Review)

Bennys_AmericanBar_Grill<-subset(Hotel_reviews1,
                        Hotel.Restaurant.name=="Benny's American Bar & Grill")
head(Bennys_AmericanBar_Grill$Review)


Bodega_Grill<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Bodega & Grill")
head(Bodega_Grill$Review)

DaVinci_Restaurant<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="DaVinci Restaurant")
head(DaVinci_Restaurant$Review)

DeDos<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="DeDos")
head(DeDos$Review)


Don_Vito_Trattoria<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Don Vito Trattoria")
head(Don_Vito_Trattoria$Review)

Flame_data<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Flame")

head(Flame_data$Review)

Black_Cat<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Black Cat")
head(Black_Cat$Review)

Golden_Paradise_Restaurant<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Golden Paradise Restaurant")

head(Golden_Paradise_Restaurant$Review)

Bocconcino<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Bocconcino")
head(Bocconcino$Review)


Cafe_de_Bangtao<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Cafe de Bangtao")
head(Cafe_de_Bangtao$Review)

Chilli_Kitchen_data<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Chilli Kitchen")
head(Chilli_Kitchen_data$Review)

Cut_Grill_Lounge<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Cut Grill & Lounge")
head(Cut_Grill_Lounge$Review)

D_Restaurant<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="D Restaurant")

head(D_Restaurant$Review)

Kata_Country_House_Restaurant<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Kata Country House Restaurant")
head(Kata_Country_House_Restaurant$Review)


KEE_Sky_Lounge<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="KEE Sky Lounge & Restaurant")
head(KEE_Sky_Lounge$Review)

Little_Paris_data<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Little Paris")
head(Little_Paris_data$Review)

Siam_Supper_Club<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="Siam Supper Club")
head(Siam_Supper_Club$Review)

The_Beach_Cuisine<-subset(Hotel_reviews1,
                      Hotel.Restaurant.name=="The Beach Cuisine")
head(The_Beach_Cuisine$Review)

#Create text vectors
Anns_Kitchen_Bar<-Anns_Kitchen_Bar$Review

Audy_Restaurant_data<-Audy_Restaurant_data$Review 

Bampot_Kitchen_Bar<-Bampot_Kitchen_Bar$Review

Bennys_AmericanBar_Grill<-Bennys_AmericanBar_Grill$Review

Bodega_Grill<-Bodega_Grill$Review

DaVinci_Restaurant<-DaVinci_Restaurant$Review

DeDos<-DeDos$Review

Don_Vito_Trattoria<-Don_Vito_Trattoria$Review

Flame_data<-Flame_data$Review

Black_Cat<-Black_Cat$Review

Golden_Paradise_Restaurant<-Golden_Paradise_Restaurant$Review

Cafe_de_Bangtao<-Cafe_de_Bangtao$Review

Chilli_Kitchen_data<-Chilli_Kitchen_data$Review

Cut_Grill_Lounge<-Cut_Grill_Lounge$Review

D_Restaurant<-D_Restaurant$Review

Kata_Country_House_Restaurant<-Kata_Country_House_Restaurant$Review

KEE_Sky_Lounge<-KEE_Sky_Lounge$Review

Little_Paris_data<-Little_Paris_data$Review

Siam_Supper_Club<-Siam_Supper_Club$Review

The_Beach_Cuisine<-The_Beach_Cuisine$Review

#Change all reviews to lower case
Anns_Kitchen_Bar<-tolower(Anns_Kitchen_Bar)

Audy_Restaurant_data<-tolower(Audy_Restaurant_data)

Bampot_Kitchen_Bar<-tolower(Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill<-tolower(Bennys_AmericanBar_Grill)

Bodega_Grill<-tolower(Bodega_Grill)

DaVinci_Restaurant<-tolower(DaVinci_Restaurant)

DeDos<-tolower(DeDos)

Don_Vito_Trattoria<-tolower(Don_Vito_Trattoria)

Flame_data<-tolower(Flame_data)

Black_Cat<-tolower(Black_Cat)

Golden_Paradise_Restaurant<-tolower(Golden_Paradise_Restaurant)

Cafe_de_Bangtao<-tolower(Cafe_de_Bangtao)

Chilli_Kitchen_data<-tolower(Chilli_Kitchen_data)

Cut_Grill_Lounge<-tolower(Cut_Grill_Lounge)

D_Restaurant<-tolower(D_Restaurant)

Kata_Country_House_Restaurant<-tolower(Kata_Country_House_Restaurant)

KEE_Sky_Lounge<-tolower(KEE_Sky_Lounge)

Little_Paris_data<-tolower(Little_Paris_data)

Siam_Supper_Club<-tolower(Siam_Supper_Club)

The_Beach_Cuisine<-tolower(The_Beach_Cuisine)

#use gsub function to remove any links in the reviews
Anns_Kitchen_Bar <- gsub("http\\S+\\s*", "", Anns_Kitchen_Bar)

Audy_Restaurant_data <- gsub("http\\S+\\s*", "", Audy_Restaurant_data)

Bampot_Kitchen_Bar <- gsub("http\\S+\\s*", "", Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill <- gsub("http\\S+\\s*", "", Bennys_AmericanBar_Grill)

Bodega_Grill <- gsub("http\\S+\\s*", "", Bodega_Grill)

DaVinci_Restaurant <- gsub("http\\S+\\s*", "", DaVinci_Restaurant)

DeDos <- gsub("http\\S+\\s*", "", DeDos)

Don_Vito_Trattoria <- gsub("http\\S+\\s*", "", Don_Vito_Trattoria)

Flame_data <- gsub("http\\S+\\s*", "", Flame_data)

Black_Cat <- gsub("http\\S+\\s*", "", Black_Cat)

Golden_Paradise_Restaurant <- gsub("http\\S+\\s*", "", Golden_Paradise_Restaurant)

Cafe_de_Bangtao <- gsub("http\\S+\\s*", "", Cafe_de_Bangtao)

Chilli_Kitchen_data <- gsub("http\\S+\\s*", "", Chilli_Kitchen_data)

Cut_Grill_Lounge <- gsub("http\\S+\\s*", "", Cut_Grill_Lounge)

D_Restaurant <- gsub("http\\S+\\s*", "", D_Restaurant)

Kata_Country_House_Restaurant <- gsub("http\\S+\\s*", "", Kata_Country_House_Restaurant)

KEE_Sky_Lounge <- gsub("http\\S+\\s*", "", KEE_Sky_Lounge)

Little_Paris_data <- gsub("http\\S+\\s*", "", Little_Paris_data)

Siam_Supper_Club <- gsub("http\\S+\\s*", "", Siam_Supper_Club)

The_Beach_Cuisine <- gsub("http\\S+\\s*", "", The_Beach_Cuisine)

#Remove the punctuation using gsub
Anns_Kitchen_Bar <- gsub("[[:punct:]]", "", Anns_Kitchen_Bar) 

Audy_Restaurant_data <- gsub("[[:punct:]]", "", Audy_Restaurant_data) 

Bampot_Kitchen_Bar <- gsub("[[:punct:]]", "", Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill <- gsub("[[:punct:]]", "", Bennys_AmericanBar_Grill)

Bodega_Grill <- gsub("[[:punct:]]", "", Bodega_Grill)

DaVinci_Restaurant <- gsub("[[:punct:]]", "", DaVinci_Restaurant)

DeDos <- gsub("[[:punct:]]", "", DeDos)

Don_Vito_Trattoria <- gsub("[[:punct:]]", "", Don_Vito_Trattoria)

Flame_data <- gsub("[[:punct:]]", "", Flame_data)

Black_Cat <- gsub("[[:punct:]]", "", Black_Cat)

Golden_Paradise_Restaurant <- gsub("[[:punct:]]", "", Golden_Paradise_Restaurant)

Cafe_de_Bangtao <- gsub("[[:punct:]]", "", Cafe_de_Bangtao)

Chilli_Kitchen_data <- gsub("[[:punct:]]", "", Chilli_Kitchen_data)

Cut_Grill_Lounge <- gsub("[[:punct:]]", "", Cut_Grill_Lounge)

D_Restaurant <- gsub("[[:punct:]]", "", D_Restaurant)

Kata_Country_House_Restaurant <- gsub("[[:punct:]]", "", Kata_Country_House_Restaurant)

KEE_Sky_Lounge <- gsub("[[:punct:]]", "", KEE_Sky_Lounge)

Little_Paris_data <- gsub("[[:punct:]]", "", Little_Paris_data)

Siam_Supper_Club <- gsub("[[:punct:]]", "", Siam_Supper_Club)

The_Beach_Cuisine <- gsub("[[:punct:]]", "", The_Beach_Cuisine)


#Remove digits from reviews

Anns_Kitchen_Bar <- gsub("[[:digit:]]", "", Anns_Kitchen_Bar)

Audy_Restaurant_data <- gsub("[[:digit:]]", "", Audy_Restaurant_data)
  
Bampot_Kitchen_Bar <- gsub("[[:digit:]]", "", Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill <- gsub("[[:digit:]]", "", Bennys_AmericanBar_Grill)

Bodega_Grill <- gsub("[[:digit:]]", "", Bodega_Grill)

DaVinci_Restaurant <- gsub("[[:digit:]]", "", DaVinci_Restaurant)

DeDos <- gsub("[[:digit:]]", "", DeDos)

Don_Vito_Trattoria <- gsub("[[:digit:]]", "", Don_Vito_Trattoria) 

Flame_data <- gsub("[[:digit:]]", "", Flame_data)

Black_Cat <- gsub("[[:digit:]]", "", Black_Cat)

Golden_Paradise_Restaurant <- gsub("[[:digit:]]", "", Golden_Paradise_Restaurant)

Cafe_de_Bangtao <- gsub("[[:digit:]]", "", Cafe_de_Bangtao)

Chilli_Kitchen_data <- gsub("[[:digit:]]", "", Chilli_Kitchen_data)

Cut_Grill_Lounge <- gsub("[[:digit:]]", "", Cut_Grill_Lounge)

D_Restaurant <- gsub("[[:digit:]]", "", D_Restaurant)

Kata_Country_House_Restaurant <- gsub("[[:digit:]]", "", Kata_Country_House_Restaurant)

KEE_Sky_Lounge <- gsub("[[:digit:]]", "", KEE_Sky_Lounge)

Little_Paris_data <- gsub("[[:digit:]]", "", Little_Paris_data)

Siam_Supper_Club <- gsub("[[:digit:]]", "", Siam_Supper_Club)

The_Beach_Cuisine <- gsub("[[:digit:]]", "", The_Beach_Cuisine)

#remove lead blank space in the begining of a review
Anns_Kitchen_Bar <- gsub("^ ", "", Anns_Kitchen_Bar) 
  
Audy_Restaurant_data <- gsub("^ ", "", Audy_Restaurant_data) 
  
Bampot_Kitchen_Bar <- gsub("^ ", "", Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill <- gsub("^ ", "", Bennys_AmericanBar_Grill)

Bodega_Grill <- gsub("^ ", "", Bodega_Grill)

DaVinci_Restaurant <- gsub("^ ", "", DaVinci_Restaurant)

DeDos <- gsub("^ ", "", DeDos)

Don_Vito_Trattoria <- gsub("^ ", "", Don_Vito_Trattoria)

Flame_data <- gsub("^ ", "", Flame_data)

Black_Cat <- gsub("^ ", "", Black_Cat)

Golden_Paradise_Restaurant <- gsub("^ ", "", Golden_Paradise_Restaurant)

Cafe_de_Bangtao <- gsub("^ ", "", Cafe_de_Bangtao)

Chilli_Kitchen_data <- gsub("^ ", "", Chilli_Kitchen_data)

Cut_Grill_Lounge <- gsub("^ ", "", Cut_Grill_Lounge)

D_Restaurant <- gsub("^ ", "", D_Restaurant)

Kata_Country_House_Restaurant <- gsub("^ ", "", Kata_Country_House_Restaurant)

KEE_Sky_Lounge <- gsub("^ ", "", KEE_Sky_Lounge)

Little_Paris_data <- gsub("^ ", "", Little_Paris_data)

Siam_Supper_Club <- gsub("^ ", "", Siam_Supper_Club)

The_Beach_Cuisine <- gsub("^ ", "", The_Beach_Cuisine)

#Remove blank space at the end of the reviews
Anns_Kitchen_Bar <- gsub(" $", "", Anns_Kitchen_Bar)
  
Audy_Restaurant_data <- gsub(" $", "", Audy_Restaurant_data)
  
Bampot_Kitchen_Bar <- gsub(" $", "", Bampot_Kitchen_Bar)

Bennys_AmericanBar_Grill <- gsub(" $", "", Bennys_AmericanBar_Grill)

Bodega_Grill <- gsub(" $", "", Bodega_Grill)

DaVinci_Restaurant <- gsub(" $", "", DaVinci_Restaurant)

DeDos <- gsub(" $", "", DeDos)

Don_Vito_Trattoria <- gsub(" $", "", Don_Vito_Trattoria

Flame_data <- gsub(" $", "", Flame_data)

Black_Cat <- gsub(" $", "", Black_Cat)

Golden_Paradise_Restaurant <- gsub(" $", "", Golden_Paradise_Restaurant)

Cafe_de_Bangtao <- gsub(" $", "", Cafe_de_Bangtao)

Chilli_Kitchen_data <- gsub(" $", "", Chilli_Kitchen_data)

Cut_Grill_Lounge <- gsub(" $", "", Cut_Grill_Lounge)

D_Restaurant <- gsub(" $", "", D_Restaurant)

Kata_Country_House_Restaurant <- gsub(" $", "", Kata_Country_House_Restaurant)

KEE_Sky_Lounge <- gsub(" $", "", KEE_Sky_Lounge)

Little_Paris_data <- gsub(" $", "", Little_Paris_data)

Siam_Supper_Club <- gsub(" $", "", Siam_Supper_Club)

The_Beach_Cuisine <- gsub(" $", "", The_Beach_Cuisine)

#Inspect the vectorsafter the cleaning
head(Anns_Kitchen_Bar)
head(Audy_Restaurant_data)




#convert the text vectors to corpus
library(tm)

corpus_Anns_Kitchen_Bar <- Corpus(VectorSource(Anns_Kitchen_Bar))

corpus_Audy_Restaurant <- Corpus(VectorSource(Audy_Restaurant_data))

corpus_Bampot_KitchenBar <- Corpus(VectorSource(Bampot_Kitchen_Bar))

corpus_Bennys_AmericanBar_Grill <- Corpus(VectorSource(Bennys_AmericanBar_Grill))

corpus_Bodega_Grill <- Corpus(VectorSource(Bodega_Grill))

corpus_DaVinci_Restaurant <- Corpus(VectorSource(DaVinci_Restaurant))

corpus_DeDos <- Corpus(VectorSource(DeDos))

corpus_Don_Vito_Trattoria <- Corpus(VectorSource(Don_Vito_Trattoria))

corpus_Flame_data <- Corpus(VectorSource(Flame_data))

corpus_Black_Cat <- Corpus(VectorSource(Black_Cat))

corpus_Golden_Paradise_Restaurant <- Corpus(VectorSource(Golden_Paradise_Restaurant))

corpus_Cafe_de_Bangtao <- Corpus(VectorSource(Cafe_de_Bangtao))

corpus_Chilli_Kitchen_data <- Corpus(VectorSource(Chilli_Kitchen_data))

corpus_Cut_Grill_Lounge <- Corpus(VectorSource(Cut_Grill_Lounge))

corpus_D_Restaurant <- Corpus(VectorSource(D_Restaurant))

corpus_Kata_Country_House_Restaurant <- Corpus(VectorSource(Kata_Country_House_Restaurant))

corpus_KEE_Sky_Lounge <- Corpus(VectorSource(KEE_Sky_Lounge))

corpus_Little_Paris_data <- Corpus(VectorSource(Little_Paris_data))

corpus_Siam_Supper_Club <- Corpus(VectorSource(Siam_Supper_Club))

corpus_The_Beach_Cuisine <- Corpus(VectorSource(The_Beach_Cuisine))

#inspect the following commands to ispect the corpus
corpus_Anns_Kitchen_Bar

corpus_Audy_Restaurant

corpus_Bampot_KitchenBar

corpus_Bennys_AmericanBar_Grill

corpus_Bodega_Grill

corpus_DaVinci_Restaurant

corpus_DeDos

corpus_Don_Vito_Trattoria

corpus_Flame_data

corpus_Black_Cat

corpus_Golden_Paradise_Restaurant

corpus_Cafe_de_Bangtao

corpus_Chilli_Kitchen_data

corpus_Cut_Grill_Lounge

corpus_D_Restaurant

corpus_Kata_Country_House_Restaurant

corpus_KEE_Sky_Lounge

corpus_Little_Paris_data

corpus_Siam_Supper_Club

corpus_The_Beach_Cuisine

#clean up corpus to remove stop words and whitespace
corpus_Anns_Kitchen_Bar <- tm_map(corpus_Anns_Kitchen_Bar, removeWords,stopwords("english"))
corpus_Anns_Kitchen_Bar <- tm_map(corpus_Anns_Kitchen_Bar, stripWhitespace)
inspect(corpus_Anns_Kitchen_Bar)

corpus_Audy_Restaurant <- tm_map(corpus_Audy_Restaurant, removeWords,stopwords("english"))
corpus_Audy_Restaurant <- tm_map(corpus_Audy_Restaurant, stripWhitespace)
inspect(corpus_Audy_Restaurant)

corpus_Bampot_KitchenBar <- tm_map(corpus_Bampot_KitchenBar, removeWords,stopwords("english"))
corpus_Bampot_KitchenBar <- tm_map(corpus_Bampot_KitchenBar, stripWhitespace)
inspect(corpus_Bampot_KitchenBar)

corpus_Bennys_AmericanBar_Grill <- tm_map(corpus_Bennys_AmericanBar_Grill, removeWords,stopwords("english"))
corpus_Bennys_AmericanBar_Grill <- tm_map(corpus_Bennys_AmericanBar_Grill, stripWhitespace)
inspect(corpus_Bennys_AmericanBar_Grill)

corpus_Bodega_Grill <- tm_map(corpus_Bodega_Grill, removeWords,stopwords("english"))
corpus_Bodega_Grill <- tm_map(corpus_Bodega_Grill, stripWhitespace)
inspect(corpus_Bodega_Grill)

corpus_DaVinci_Restaurant <- tm_map(corpus_DaVinci_Restaurant, removeWords,stopwords("english"))
corpus_DaVinci_Restaurant <- tm_map(corpus_DaVinci_Restaurant, stripWhitespace)
inspect(corpus_DaVinci_Restaurant)

corpus_DeDos <- tm_map(corpus_DeDos, removeWords,stopwords("english"))
corpus_DeDos <- tm_map(corpus_DeDos, stripWhitespace)
inspect(corpus_DeDos)

corpus_Don_Vito_Trattoria <- tm_map(corpus_Don_Vito_Trattoria, removeWords,stopwords("english"))
corpus_Don_Vito_Trattoria <- tm_map(corpus_Don_Vito_Trattoria, stripWhitespace)
inspect(corpus_Don_Vito_Trattoria)

corpus_Flame_data <- tm_map(corpus_Flame_data, removeWords,stopwords("english"))
corpus_Flame_data <- tm_map(corpus_Flame_data, stripWhitespace)
inspect(corpus_Flame_data)

corpus_Black_Cat <- tm_map(corpus_Black_Cat, removeWords,stopwords("english"))
corpus_Black_Cat <- tm_map(corpus_Black_Cat, stripWhitespace)
inspect(corpus_Black_Cat)

corpus_Golden_Paradise_Restaurant <- tm_map(corpus_Golden_Paradise_Restaurant, removeWords,stopwords("english"))
corpus_Golden_Paradise_Restaurant <- tm_map(corpus_Golden_Paradise_Restaurant, stripWhitespace)
inspect(corpus_Golden_Paradise_Restaurant)

corpus_Cafe_de_Bangtao <- tm_map(corpus_Cafe_de_Bangtao, removeWords,stopwords("english"))
corpus_Cafe_de_Bangtao <- tm_map(corpus_Cafe_de_Bangtao, stripWhitespace)
inspect(corpus_Cafe_de_Bangtao)

corpus_Chilli_Kitchen_data <- tm_map(corpus_Chilli_Kitchen_data, removeWords,stopwords("english"))
corpus_Chilli_Kitchen_data <- tm_map(corpus_Chilli_Kitchen_data, stripWhitespace)
inspect(corpus_Chilli_Kitchen_data)

corpus_Cut_Grill_Lounge <- tm_map(corpus_Cut_Grill_Lounge, removeWords,stopwords("english"))
corpus_Cut_Grill_Lounge <- tm_map(corpus_Cut_Grill_Lounge, stripWhitespace)
inspect(corpus_Cut_Grill_Lounge)

corpus_D_Restaurant <- tm_map(corpus_D_Restaurant, removeWords,stopwords("english"))
corpus_D_Restaurant <- tm_map(corpus_D_Restaurant, stripWhitespace)
inspect(corpus_D_Restaurant)

corpus_Kata_Country_House_Restaurant <- tm_map(corpus_Kata_Country_House_Restaurant, removeWords,stopwords("english"))
corpus_Kata_Country_House_Restaurant <- tm_map(corpus_Kata_Country_House_Restaurant, stripWhitespace)
inspect(corpus_Kata_Country_House_Restaurant)

corpus_KEE_Sky_Lounge <- tm_map(corpus_Audy_Restaurant, removeWords,stopwords("english"))
corpus_KEE_Sky_Lounge <- tm_map(corpus_KEE_Sky_Lounge, stripWhitespace)
inspect(corpus_KEE_Sky_Lounge)

corpus_Little_Paris_data <- tm_map(corpus_Little_Paris_data, removeWords,stopwords("english"))
corpus_Little_Paris_data <- tm_map(corpus_Little_Paris_data, stripWhitespace)
inspect(corpus_Little_Paris_data)

corpus_Siam_Supper_Club <- tm_map(corpus_Siam_Supper_Club, removeWords,stopwords("english"))
corpus_Siam_Supper_Club <- tm_map(corpus_Siam_Supper_Club, stripWhitespace)
inspect(corpus_Siam_Supper_Club)

corpus_The_Beach_Cuisine <- tm_map(corpus_The_Beach_Cuisine, removeWords,stopwords("english"))
corpus_The_Beach_Cuisine <- tm_map (corpus_The_Beach_Cuisine, stripWhitespace)
inspect(corpus_The_Beach_Cuisine)

#Stem the words to the root of all reviews in the corpus
library(tm)
update.packages("tm", checkBuilt = TRUE)
install.packages("SnowballC")
library(SnowballC)

stem_corpus_Anns_Kitchen_Bar <- tm_map(corpus_Anns_Kitchen_Bar, stemDocument)

stem_corpus_Audy_Restaurant <- tm_map(corpus_Audy_Restaurant, stemDocument)

stem_corpus_Bampot_KitchenBar <- tm_map(corpus_Bampot_KitchenBar, stemDocument)

stem_corpus_Bennys_AmericanBar_Grill <- tm_map(corpus_Bennys_AmericanBar_Grill, stemDocument)

stem_corpus_Bodega_Grill <- tm_map(corpus_Bodega_Grill, stemDocument)

stem_corpus_DaVinci_Restaurant <- tm_map(corpus_DaVinci_Restaurant, stemDocument)

stem_corpus_DeDos <- tm_map(corpus_DeDos, stemDocument)

stem_corpus_Don_Vito_Trattoria <- tm_map(corpus_Don_Vito_Trattoria, stemDocument)

stem_corpus_Flame_data <- tm_map(corpus_Flame_data, stemDocument)

stem_corpus_Black_Cat <- tm_map(corpus_Black_Cat, stemDocument)

stem_corpus_Golden_Paradise_Restaurant <- tm_map(corpus_Golden_Paradise_Restaurant, stemDocument)

stem_corpus_Cafe_de_Bangtao <- tm_map(corpus_Cafe_de_Bangtao, stemDocument)

stem_corpus_Chilli_Kitchen_data <- tm_map(corpus_Chilli_Kitchen_data, stemDocument)

stem_corpus_Cut_Grill_Lounge <- tm_map(corpus_Cut_Grill_Lounge, stemDocument)

stem_corpus_D_Restaurant <- tm_map(corpus_D_Restaurant, stemDocument)

stem_corpus_Kata_Country_House_Restaurant <- tm_map(corpus_Kata_Country_House_Restaurant, stemDocument)

stem_corpus_KEE_Sky_Lounge <- tm_map(corpus_KEE_Sky_Lounge, stemDocument)

stem_corpus_Little_Paris_data <- tm_map(corpus_Little_Paris_data, stemDocument)

stem_corpus_Siam_Supper_Club <- tm_map(corpus_Siam_Supper_Club, stemDocument)

stem_corpus_The_Beach_Cuisine <- tm_map(corpus_The_Beach_Cuisine, stemDocument)

#Lexicon
positive_lexicon <- read.csv("positive-lexicon.txt")
negative_lexicon <- read.csv("negative-lexicon.txt")


head(positive_lexicon)
tail(positive_lexicon)

head(negative_lexicon)
tail(negative_lexicon)

library(wordcloud)

sentiment <- function(stem_corpus)
{
#generate wordclouds
wordcloud(stem_corpus,
          min.freq = 3,
          colors=brewer.pal(8, "Dark2"),
          random.color = TRUE,
          max.words = 100)
#Calculating the count of total positive and negative words in each review

#create variables and vectors
total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()
#Calculate the size of the corpus
size <- length(stem_corpus)

for(i in 1:size)
{
#All the words in current review
corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
    
#positive words in current review
pos_count <-length(intersect(unlist(corpus_words), unlist(positive_lexicon)))

#negative words in current review
neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    
total_pos_count <- total_pos_count + pos_count ## overall positive count
total_neg_count <- total_neg_count + neg_count ## overall negative count
    
}

#Calculating overall percentage of positive and negative words of all the reviews
total_pos_count ## overall positive count
total_neg_count ## overall negative count
total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count
overall_positive_percentage ## overall positive percentage
#Create a dataframe with all the positive and negative reviews
df<-data.frame(Review_Type=c("Postive","Negitive"),
               Count=c(total_pos_count ,total_neg_count ))
print(df) #Print
overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                   round(overall_positive_percentage,2),"%")

return(overall_positive_percentage)

}

sentiment(stem_corpus_D_Restaurant)
sentiment(stem_corpus_Anns_Kitchen_Bar)
sentiment(stem_corpus_Bampot_KitchenBar)
sentiment(stem_corpus_Bennys_AmericanBar_Grill)
sentiment(stem_corpus_Black_Cat)
sentiment(stem_corpus_Bodega_Grill)
sentiment(stem_corpus_Cafe_de_Bangtao)
sentiment(stem_corpus_Chilli_Kitchen_data)
sentiment(stem_corpus_Cut_Grill_Lounge)
sentiment(stem_corpus_D_Restaurant)
sentiment(stem_corpus_DaVinci_Restaurant)
sentiment(stem_corpus_DeDos)
sentiment(stem_corpus_Don_Vito_Trattoria)
sentiment(stem_corpus_Flame_data)
sentiment(stem_corpus_Golden_Paradise_Restaurant)
sentiment(stem_corpus_Kata_Country_House_Restaurant)
sentiment(stem_corpus_KEE_Sky_Lounge)
sentiment(stem_corpus_Little_Paris_data)
sentiment(stem_corpus_Siam_Supper_Club)
sentiment(stem_corpus_The_Beach_Cuisine)