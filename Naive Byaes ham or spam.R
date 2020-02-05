library(rmarkdown) 
library(tm) 
library(e1071) 
library(gmodels)

library(caret)
library(dplyr)

sms.data<-read.csv(file.choose(),stringsAsFactors = F)
class(sms.data)
 
str(sms.data)
sms.data$type<-as.factor(sms.data$type)
str(sms.data)
table(sms.data$type)
#preparing corpus

sms_corpous<-VCorpus(VectorSource(sms.data$text))
class(sms_corpous)

#cleaning and organising corpus
corpus_clean<-tm_map(sms_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
class(corpus_clean)


#creating documetn termed matrix


sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)




# creating training and test datasets
sms_raw_train <- sms.data[1:4169, ]
sms_raw_test  <- sms.data[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# chechking proportion 

prop.table(table(sms_raw_train$type))


# indicator features for frequent words
# if the word has been referred to 5 times or more
sms_dict<-findFreqTerms(sms_dtm, 5)
 

# Now apply this particular dictionary of words to training and testing data.
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary=sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))



temp <- as.data.frame(as.matrix(sms_train))


View(temp)
dim(sms_train)
 
dim(sms_test) 


#inspect(sms_corpus_train[1:100])
#list(sms_dict[1:100])
# convert counts to a factor
# Create a custom function to show that if a specific word as been used more than once.
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)
View(sms_train)
View(sms_test)

##  Training a model on the data ----
# Now apply naiveBayes Model on the new sms_train and original data
# on Type (Classifier for ham or spam)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier



##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))


confusionMatrix(sms_test_pred,sms_raw_test$type)
 