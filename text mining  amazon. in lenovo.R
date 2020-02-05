 
library(rvest) 
library(XML)

library(magrittr) 
library(tm) 
library(wordcloud)

library(wordcloud2)
library(syuzhet) 
library(lubridate)
 
library(ggplot2)
 
library(scales)
 
library(reshape2)
library(dplyr)



# install.packages('devtools')
# devtools::install_github("lchiffon/wordcloud2")


# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Lenovo-Legion-Graphics-Windows-81SY00CKIN/dp/B07W6H9YM9/ref=sr_1_5?keywords=laptop&qid=1578658991&sr=8-5#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)


setwd("C:/Users/win10/Desktop/assignments/text mining")
write.table(amazon_reviews,"lenovolaptop.txt",row.names = F)


lenovolaptop_Lap <- read.delim('lenovolaptop.TXT')
str(lenovolaptop_Lap)


View(lenovolaptop_Lap)

# Build Corpus and DTM/TDM
library(tm)
corpus <- lenovolaptop_Lap[-1,]
head(corpus)


class(corpus) 
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
 
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
 
inspect(corpus[1:5])



corpus <- tm_map(corpus,removeNumbers) 
inspect(corpus[1:5])



corpus_clean<-tm_map(corpus,stripWhitespace)
 
inspect(corpus[1:5])


cleanset<-tm_map(corpus,removeWords, stopwords('english'))
 inspect(cleanset[1:5])

 
 removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
 cleanset <- tm_map(cleanset, content_transformer(removeURL))
  
 inspect(cleanset[1:5]) 

 
 
 cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))
 ## Warning in tm_map.SimpleCorpus(cleanset, removeWords, c("laptop", "can")):
 ## transformation drops documents
 # Since the word Laptop and can is used more, this can be removed as we are 
 # mining the tweets for this laptop only.Also the word "Can" is common english word.
 # we can pull back the word "can"  if needed.
 
 
 cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine') 

 
 
 # the barplot pulls both Computer and Machine as separate words. this should be 
 # counted as one as both holds the same synonym.
 
 inspect(cleanset[1:5]) 

 
 
 
 cleanset <- tm_map(cleanset,stripWhitespace)
  
 inspect(cleanset[1:5]) 

 
 #Term Document Matrix :
 # Convert the unstructured data to structured data :
 tdm <- TermDocumentMatrix(cleanset)
  
   tdm <- as.matrix(tdm)
 tdm[1:10,1:20] 
 
 
 # Bar Plot 
 
 w <- rowSums(tdm)  # provides the no of times a particular word has been used.
 w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
 barplot(w, las = 2, col = rainbow(50))

 
 
 #the word Machine as the highest frequency. This implies
 # that MSI_Laptop has got more reviews about the Machine's performance or
 # battery or about their specifications.
 
 # Word Cloud :
 library(wordcloud)
 w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
 set.seed(123)
 wordcloud(words = names(w), freq = w, 
           max.words = 250,random.order = F,
           min.freq =  3, 
           colors = brewer.pal(8, 'Dark2'),
           scale = c(5,0.3),
           rot.per = 0.6)

 
 
  
 
 # Sentiment Analysis for tweets:
 library(syuzhet)
 library(lubridate)
 library(ggplot2)
 library(scales)
 library(reshape2)
 library(dplyr)
 
 # install.packages("syuzhet")
 
 # Read File 
 Amzn_reviews <- read.delim('lenovolaptop.TXT')
 reviews <- as.character(Amzn_reviews[-1,])
 class(reviews)
 
 s <- get_nrc_sentiment(reviews)
 head(s)

 reviews[6] 
 get_nrc_sentiment('Love')

 # Love has one Joy and one positive 
 get_nrc_sentiment('glaring') #1 Anger and 1 Negavite
 
 
 # barplot 
 
 barplot(colSums(s), las = 2.5, col = rainbow(10),
         ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        Lenovo Legion Y540  Laptop')

  
  