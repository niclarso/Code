install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)
#Reading the the data
RenameOCP <- read.csv("C:/Users/Larso/Documents/Research/OCPRclass.csv", stringsAsFactors = FALSE)

#Combing all the occupation titles together
occupationText = paste(RenameOCP$Text,collapse = " ")

#Setting up source and corpus
OccupationSource = VectorSource(occupationText)
Corp = Corpus(OccupationSource)

#CLeaning
Corp = tm_map(Corp, content_transformer(tolower))
Corp = tm_map(Corp, removePunctuation)
Corp = tm_map(Corp, stripWhitespace)
corp = tm_map(Corp, removeWords,stopwords("english"))


#Making a document matrix
dtm = DocumentTermMatrix(Corp)
dtm2 = as.matrix(dtm)



freq = colSums(dtm2)
freq = sort(freq,decreasing = TRUE)

words = names(freq)
wordcloud(words[1:50],freq[1:50])


wf <- data.frame(word=names(freq), freq=freq)   
library(ggplot2)
p <- ggplot(subset(wf, freq>10), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p   


