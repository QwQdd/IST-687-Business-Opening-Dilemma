library(readr)
yelp_academic_dataset_business <- read.csv("yelp_academic_dataset_business.json", header=FALSE)
#import the business data set
library(dplyr)
#start cleaning the data set
yelpBusiness <- select(yelpBusiness, -c(12:41))

yelpBusiness <- rename(yelpBusiness, c(business_id=X1, name=X2, address=X3, city=X4, state=X5, postal_code=X6, lat=X7, long=X8, stars=X9, review_count=X10, is_open=X11))
yelpBusiness$business_id <- gsub('"', '', yelpBusiness$business_id)
yelpBusiness$business_id <- gsub('{business_id:', '', yelpBusiness$business_id, fixed = TRUE)
yelpBusiness$name <- gsub('name:', '', yelpBusiness$name)
yelpBusiness$address <- gsub('address:', '', yelpBusiness$address)
yelpBusiness$city <- gsub('city:', '', yelpBusiness$city)
yelpBusiness$state <- gsub('state:', '', yelpBusiness$state)
yelpBusiness$postal_code <- gsub('postal_code:', '', yelpBusiness$postal_code)
yelpBusiness$lat <- gsub('latitude:', '', yelpBusiness$lat)
yelpBusiness$long <- gsub('longitude:', '', yelpBusiness$long)
yelpBusiness$stars <- gsub('stars:', '', yelpBusiness$stars)
yelpBusiness$review_count <- gsub('review_count:', '', yelpBusiness$review_count)
yelpBusiness$is_open <- gsub('is_open:', '', yelpBusiness$is_open)

yelpBusiness$lat <- as.numeric(yelpBusiness$lat)
yelpBusiness$long <- as.numeric(yelpBusiness$long)
yelpBusiness$review_count <- as.numeric(yelpBusiness$review_count)
yelpBusiness$stars <- as.factor(yelpBusiness$stars)
yelpBusiness$state <- tolower(yelpBusiness$state)
yelpBusiness$city <- tolower(yelpBusiness$city)

yelpOregon <- yelpBusiness[yelpBusiness$state == "OR",]
yelpOregon$state <- gsub('OR', 'oregon', yelpOregon$state)

colSums(is.na(yelpBusiness))

#import the review data set

yelp_academic_dataset_review <- read.csv("yelp_academic_dataset_review.json", header=FALSE)
yelpReview <- yelp_academic_dataset_review
yelpReview <- yelpReview[, -c(1:2, 4:7, 9)]
yelpReview <- rename(yelpReview, c( business_id=V3, text=V8))
yelpReview$business_id <- gsub('business_id:', '', yelpReview$business_id)
yelpReview$text <- gsub('text:', '', yelpReview$text)
#merge the review data set with business data set
yelpText <- merge(yelpReview, yelpOregon)

#filter For Portland 
yelpText <- yelpText[yelpText$city == "Portland",]


library(quanteda)
#changing the reviews into document term matrix 
dfCor <- corpus(yelpText$text)
dftok <- tokens(dfCor)
dfDFM <- dfm(dftok, remove_punct=TRUE, remove=c(stopwords("english"), "n", "$"), removeNumbers = TRUE, tolower = TRUE)
dfDFM


library(quanteda.textplots)
#do word cloud on review data 
textplot_wordcloud(dfDFM, min_termfeq = 10)
#import the positive and negative word list
posWords <- scan("https://intro-datascience.s3.us-east-2.amazonaws.com/positive-words.txt", character(0), sep = "\n") 
negWords <- scan("https://intro-datascience.s3.us-east-2.amazonaws.com/negative-words.txt", character(0), sep = "\n") 
posWords <- posWords[-c(1:34)]
negWords <- negWords[-c(1:34)]
#match the word list with the review from yelp data set
posDfm <- dfm_match(dfDFM, posWords)
negDfm <- dfm_match(dfDFM, negWords)
posDfm
negDfm
textplot_wordcloud(posDfm, min_termfeq = 2)
textplot_wordcloud(negDfm, min_termfeq = 2)

#sum up the frequency for the text used 
dfFreq <- textstat_frequency(dfDFM)
posFreq <- textstat_frequency(posDfm)
negFreq <- textstat_frequency(negDfm)
#frequency list for word cloud positive and negative
dfFreq[1:10,]
posFreq[1:10,]
negFreq[1:10,]
