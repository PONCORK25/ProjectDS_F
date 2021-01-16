library(Rstem)
library(twitteR)
library(NLP)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(SnowballC)
library(sentiment)

#mengakses API Twitter di developer.twitter.com
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
api_key <- "f8QAtE9P5sIvpUvYpARuxxlsd" 
api_key_secret <- "x30wLXI0oxVTos1hbThibCrgPl4cZsghNaiEX8IvuyyPtVOaby" 
a_token <- "1192644329140998145-Dypm394tg9iGuVb6P0HkhPj1q87qP8" 
a_token_secret <- "ek4MHmguoXG3Hf5MFrjIhQMH36eQsLStqeqjtyGxSzQJS" 
setup_twitter_oauth(api_key, api_key_secret, a_token, a_token_secret)

#mengambil tweet
lf_tweets <- searchTwitter('sj182', n=1000, lang="en")
tweets <- sapply(lf_tweets, function(x) x$getText()) 
write.csv(tweets, file = 'C:/Users/Allief F/Documents/Rstudio/DSproject/dataTwitter.csv')

#menghilangkan entitas tweet yg tidak penting
tweets = sapply(lf_tweets, function(x) x$getText())
tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
tweets = gsub("@\\w+", "", tweets)
tweets = gsub("[[:punct:]]", "", tweets)
tweets = gsub("[[:digit:]]", "", tweets)
tweets = gsub("http\\w+", "", tweets)
tweets = gsub("[ \t]{2,}", "", tweets)
tweets = gsub("^\\s+|\\s+$", "", tweets)
tweets = gsub("note", "", tweets)

#mengantisipasi data yg error akibat dari langkah sebelumnya
try.error = function(x)
{
  #pemberian nilai
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
tweets = sapply(tweets, try.error)
tweets = tweets[!is.na(tweets)]
names(tweets) = NULL

write.csv(tweets, file = 'C:/Users/Allief F/Documents/Rstudio/DSproject/dataBersih.csv')

#pengaplikasian naive bayes untuk menentukan sentimen
class_emo = classify_emotion(tweets, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "neutral"
class_pol = classify_polarity(tweets, algorithm="bayes")
polarity = class_pol[,4]

#data sentimen
sent_df = data.frame(text=tweets, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'C:/Users/Allief F/Documents/Rstudio/DSproject/dataSentimen.csv')
View(sent_df)
table(sent_df$emotion)
table(sent_df$polarity)

#visualisasi data
plotSentiments1 <- function(sent_df, title) 
{
  ggplot(sent_df, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}
plotSentiments1(sent_df, "Tweets of SJ182 By Emotion")

plotSentiments2 <- function(sent_df, title)
{
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position="right") +
    ylab("Number of Tweets") +
    xlab("Polarity Categories")
}
plotSentiments2(sent_df, "Tweets of SJ182 By Polarity")
