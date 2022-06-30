#library(usethis) 
#usethis::edit_r_environ()
rm(list=ls())

library(tm)
library(topicmodels)
library(dplyr)
library(readxl)
library(data.table)
library(wordcloud)
library(SnowballC)
#library(tibble)
library(ggplot2)
library(SentimentAnalysis)
#library(syuzhet)
library(RColorBrewer)
library(wordcloud2)

set.seed(1234)

###################################
#read files=========================
######################################
data_file=list.files("C:/Users/Ningyu Wang/Documents/R/UR/CIS 434R/Project/data")                    
data=data.table()

for(i in 1:length(data_file)) {                              
  dataTemp=read_excel(paste0("C:/Users/Ningyu Wang/Documents/R/UR/CIS 434R/Project/data/",
                          data_file[i]))
  data=rbind(dataTemp,data,fill=TRUE)
}

#date
data$date=as.Date(as.factor(data$date))
data$year = format(data$date,format="%Y")
data$month = format(data$date,format="%m")
data$date_new = format(data$date,format="%d")

#data(only english)
data=data[data$lang=="en",]
dataSub=data[,c("tweet_id","date","text","user_id","user_location","user_description")][1:20000]

#randomly choose dataset
dataSubSize <- sample(1:nrow(data),30000,replace=FALSE)
dataSub = data[dataSubSize]

dataSub=dataSub[order(dataSub$year,dataSub$month,dataSub$date_new),]

#########################################################
#create sub data and clean reviews===========================
############################################################

#clean tweet function
cleanTweets <- function(tweets_1){
  removeRT <- function(x){gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)}
  tweets_2 = tm_map(tweets_1,content_transformer(removeRT))
  removeHashtag <- function(x){gsub("#\\S+", "", x)}
  tweets_3 = tm_map(tweets_2,content_transformer(removeHashtag))
  removeURL <- function(x){gsub("http[^[:space:]]*", "", x)}
  tweets_4 = tm_map(tweets_3,content_transformer(removeURL))
  unescapeHTML <- function(str) {return(gsub("<.*?>", "", str))}
  tweets_5 = tm_map(tweets_4,content_transformer(unescapeHTML))
  removeMention <- function(x){gsub("@\\w+", "", x)}
  tweets_6 = tm_map(tweets_5,content_transformer(removeMention))
  removeCarriage <- function(x){gsub("[\r\n]", "", x)}
  tweets_7 = tm_map(tweets_6,content_transformer(removeCarriage))
  removeEmoticon <- function(x){gsub("[^\x01-\x7F]", "", x)}
  tweets_8 = tm_map(tweets_7,content_transformer(removeEmoticon))
  return(tweets_8)
}

#create corpus
docs = Corpus(VectorSource(dataSub$text))
docs = cleanTweets(docs)

#clean tweet
dtm.tfidf <- DocumentTermMatrix(docs, 
                                control=list(
                                         weighting=weightTfIdf,
                                         tolower=T, removePunctuation=T,
                                         removeNumbers=T, 
                                         stripWhitespace=T,
                                         stopwords=c(stopwords("english"))))

############################################################
#sentiment==================================================
############################################################
send_mega <- analyzeSentiment(dtm.tfidf, language = "english")
send <- send_mega[,1:4]
send <- as.data.frame(send)
send1 <- convertToDirection(send)

sum(send1$SentimentGI=='negative')/nrow(send)
sum(send1$SentimentGI=='neutral')/nrow(send)
sum(send1$SentimentGI=='positive')/nrow(send)

#summary data
send_table <- table(send1$SentimentGI)
send_table1 <- as.data.frame(send_table)
send_table1 <- rownames_to_column(send_table1)
colnames(send_table1) <- c("count","polarity","freq")

#emotion analysis
sent2 <- get_nrc_sentiment(dataSub$text)
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")

#word cloud---------------------------------
###############################
#pure frequency without weight
##################################
dtm <- DocumentTermMatrix(docs, 
                                control=list(
                                  #weighting=weightTfIdf,
                                  tolower=T, removePunctuation=T,
                                  removeNumbers=T, 
                                  stripWhitespace=T,
                                  stopwords=c(stopwords("english"))))


sums <- as.data.frame(colSums(as.matrix(dtm)))
sums <- rownames_to_column(sums)
colnames(sums) <- c("word", "count")
sums <- arrange(sums, desc(count))
head <- sums[1:200,]

##visualize text (2 wordcloud approaches)
wordcloud(words = sums$word, freq = sums$count, min.freq = 60,
          max.words=100, random.order=FALSE, rot.per= 0.35,
          scale=c(3.5,0.5), use.r.layout=FALSE,
          random.color = FALSE, colors=brewer.pal(8, "Dark2"))

wordcloud2(data=head,color = rep(brewer.pal(8, "Blues"),nrow(head)),
           backgroundColor = "black",size = 0.7,rotateRatio = 0,
           shape="circle")

##ggpolt bar chart----------------------
modified_data=head[1:10,]

ggplot(modified_data) +
  geom_bar(mapping = aes(reorder(word, -count), y = count), stat = "identity", fill = "lightblue", color = "black", width = 0.5) + 
  geom_text(aes(x = word, y = count, label = count), 
            stat = "identity", vjust = -0.5, 
            position = position_dodge(width=1)) +
  theme_classic() + 
  labs(title = "Word Frequency", x = "Word", y = "Frequency") + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45,hjust = 1), 
        plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))
  #annotation_custom(l, xmin = 6.5, xmax = 10.5, ymin = -50, ymax = -120) +
  #coord_cartesian(clip = "off") +
  #theme(plot.margin = unit(c(1,1,4,1),"lines"))

ggplot(send_table1)+
  geom_bar(aes(polarity,freq),stat = "identity",width=0.3,
           fill=brewer.pal(7, "Set1")[2])+
  theme_classic() + 
  labs(title = "Sentiment Analysis", x = "Polarity", y = "Frequency") + 
  theme(text = element_text(size=15), 
        plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))











