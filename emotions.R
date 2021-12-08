library(printr)
library(tm)
library(ggplot2)
library(reshape2)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(dplyr)

OriginalData=read.csv('webmd.csv', header = T)
webmd = filter(OriginalData, Drug == 'adderall')
webmd


REmotion =data.frame(webmd$Satisfaction, webmd$Reviews,
                       get_nrc_sentiment(as.character(webmd$Reviews)))
colnames(REmotion)=c('Satisfaction', 'Reviews', 'Anger', 'Anticipation', 'Disgust', 'Fear', 'Joy', 'Sadness', 'Surprise', 'Trust', 'Negative', 'Positive')
head(REmotion)



#preprocess
preprocessing <-function(Corp) {
  StripString=content_transformer(function(x, pattern) gsub(pattern, ' ', x))
  SearchReplace=content_transformer(function(x, pattern1, pattern2) gsub(pattern1, pattern2, x))
  latin2Ascii=content_transformer(function(x) iconv(x, 'latin1', 'ASCII', sub=' '))
  Corp=tm_map(Corp, content_transformer(tolower))
  Corp=tm_map(Corp, removeWords, stopwords("english"))
  Corp=tm_map(Corp, removePunctuation)
  Corp=tm_map(Corp, removeNumbers)
  Corp=tm_map(Corp, stripWhitespace)
  Corp=tm_map(Corp, StripString, 'http[[:alnum:]]*')
  Corp=tm_map(Corp, StripString, '[\r\n]')
  Corp=tm_map(Corp, StripString, '[\t]')
  Corp=tm_map(Corp, SearchReplace, 'star wars', 'starwars')
  Corp=tm_map(Corp, latin2Ascii)
  # Corp=tm_map(Corp, removeWords, c('whereisrey', 'rey', 'starwars'))
  Corp=tm_map(Corp, stemDocument)
  return(Corp)
}



#plot the graph
REmotionME=melt(REmotion[c(1,2,3,4,5,6,7,8,9,10)], id.var=c('Satisfaction', 'Reviews'))

#stack bar
p1<-ggplot(REmotionME, aes(x=Satisfaction, y=value, fill=variable)) +
  ggtitle("Satisfaction Level VS. Emotions of Drug: Adderall") +
  geom_bar(position="stack", stat='summary', fun.y='mean')+
  labs(x='Satisfaction', y='Average Emotion', fill='Emotion Type')+ scale_x_continuous(breaks=seq(1,10,1))

gridExtra::grid.arrange(p1, nrow=1)


#investigate more
ggplot(REmotionME,aes(x=Satisfaction, y=value, fill=variable)) +
  ggtitle("Satisfaction Level VS. Emotions of Drug: Adderall") +
  geom_bar( position="dodge", stat='summary', fun.y='mean')+
  labs(x='Satisfaction', y='Average Emotion', fill='Emotion Type')+
  facet_wrap(~variable)+ scale_x_continuous(breaks=seq(1,10,1))
 
  

#determine the dominant
for (i in 1:nrow(REmotion))
{
  REmotion$Emotion[i]=ifelse((table(as.numeric(REmotion[i,3:10]))
                                [names(table(as.numeric(REmotion[i, 3:10])))==max(REmotion[i, 3:10])])==1,
                               'FindEmotion', '') # try to determine whether to findemotion or not for each Reviews
  for (column in 3:10)
  {
    REmotion$Emotion[i]=ifelse(REmotion$Emotion[i]=='FindEmotion',
                                 ifelse(REmotion[i, column]==max(REmotion[i, 3:10]),
                                        colnames(REmotion[column]),REmotion$Emotion[i]), REmotion$Emotion[i])
  } # if 'FindEmotion', then find and assign the emotion name that matches the maximum value
}



RCompareEmo=c(anger=paste(subset(REmotion, Emotion=='Anger')$Reviews,
                            sept='\n', collapse = ' '), paste(subset(REmotion, Emotion=='Disgust')$Reviews,
                                                              sept='\n', collapse = ' ') )
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Fear')$Reviews,
                                     sept='\n', collapse = ' '))
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Sadness')$Reviews,
                                     sept='\n', collapse = ' '))
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Surprise')$Reviews,
                                     sept='\n', collapse = ' '))
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Joy')$Reviews,
                                     sept='\n', collapse = ' '))
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Anticipation')$Reviews,
                                     sept='\n', collapse = ' '))
RCompareEmo=c(RCompareEmo, paste(subset(REmotion, Emotion=='Trust')$Reviews,
                                     sept='\n', collapse = ' '))

#
RCompCorp=Corpus(VectorSource(RCompareEmo))
RCompCorp=preprocessing(RCompCorp)
RCompTDM=TermDocumentMatrix(RCompCorp)
RCompTDM=removeSparseTerms(RCompTDM, 0.99)
colnames(RCompTDM)=c('Anger', 'Disgust', 'Fear', 'Sadness', 'Surprise', 'Joy', 'Anticipation', 'Trust')
RCompTDM=as.matrix(RCompTDM)
comparison.cloud(RCompTDM, max.words = 1500, vfont=c('serif', 'plain'), random.order = F, rot.per = .25, scale = c(2.5, 0.85), title.size = 1.5)

