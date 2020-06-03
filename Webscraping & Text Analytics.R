rm(list=ls())
install.packages("XML")
library(XML)
address = "http://apps.saferoutesinfo.org/legislation_funding%20/state_apportionment.cfm"
webhtml<-htmlParse(address)
raw.table<- readHTMLTable(webhtml,stringsAsFactors = FALSE)
data_state <- sapply(raw.table[[1]][,-1], FUN= function(x) 
  as.character(gsub(",", "", as.character(x), fixed = TRUE) ))
data_state <-as.data.frame(substring(data_state,2), stringsAsFactors=FALSE)
data_state$State<-c(raw.table[[1]][,1]) # add state name
data_state<-data_state[,c(10,1:9)] # re arrange columns
names(data_state)[2:10]<-c("Actual2005","Actual2006","Actual2007","Actual2008",
                           "Actual2009","Actual2010","Actual2011", "Actual2012", "total")
data_state = as.data.frame(data_state)
str(data_state)
data_state = data_state[1:51,] # remove totals row
for (i in 2:10){
  data_state[,i] = as.numeric(data_state[,i])}
str(data_state)
# Now we can do the usual EDA/Viz stuff # top 5 states in 2010?
data_state$State[order(-data_state$total)[1:5]]
library(ggplot2)
ggplot(data_state,aes(x=1:nrow(data_state),y=Actual2005,label=State))+geom_point()+
  geom_text()
ggplot(data_state,aes(x=1:nrow(data_state),y=Actual2006,label=State))+geom_point()+
  geom_text()
ggplot(data_state,aes(x=1:nrow(data_state),y=total,label=State))+geom_point()+
  geom_text()

# text analytics
rm(list=ls())
install.packages("tm")
install.packages("wordcloud")
library("tm")
library("wordcloud")
##### look at a real example #####
reut <- system.file("texts", "crude", package = "tm")
reuters = VCorpus(DirSource(reut),readerControl = list(reader = readReut21578XMLasPlain))
reuters
reuters[[1]]$content
reuters[[1]]$meta
reuters[[1]]$meta$heading
reuters[[2]]$content
reuters[[2]]$meta$heading
# basic corrections/transformations
reuters[[1]]$content
reuters = tm_map(reuters, stripWhitespace) # strip extra white space
reuters[[1]]$content
# remove stopwords
reuters = tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]]$content # still problems, see company -- copany

#create wordcloud
reut <- system.file("texts", "crude", package = "tm")
crude = VCorpus(DirSource(reut),readerControl = list(reader = readReut21578XMLasPlain))
reut <- system.file("texts", "acq", package = "tm")
acq = VCorpus(DirSource(reut),readerControl = list(reader = readReut21578XMLasPlain))

# look at some examples
crude[[1]]$content
acq[[1]]$content

par(mfrow=c(1,2))
wordcloud(crude, max.words = 100, random.order = FALSE,colors="blue")
wordcloud(acq, max.words = 100, random.order = FALSE,colors="red")
crude = tm_map(crude, removeWords, stopwords("english"))
acq = tm_map(acq, removeWords, stopwords("english"))
par(mfrow=c(1,2))
wordcloud(crude, max.words = 100, random.order = FALSE,colors="blue")
wordcloud(acq, max.words = 100, random.order = FALSE,colors="red")
crude = tm_map(crude, removeWords, c("said","reuter","the"))
acq = tm_map(acq, removeWords, c("said","reuter","the"))
par(mfrow=c(1,2))
wordcloud(crude, max.words = 100, random.order = FALSE,colors="blue")
wordcloud(acq, max.words = 100, random.order = FALSE,colors="red")
          
          