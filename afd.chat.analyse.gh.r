# --------------------------------------
# Housekeeping
# --------------------------------------

rm(list=ls())
print(Sys.time())
setwd(".")

# --------------------------------------
# Computational time	
# --------------------------------------

help.time.1 <- proc.time()

# --------------------------------------
# working directiory
# --------------------------------------

wd<-"xxx"

# --------------------------------------
# Load package(s)
# --------------------------------------

# option change if packet download isnt working
# options(download.file.method = "libcurl")

# check what is needed to install
# 
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("topicmodels")
# install.packages("wordcloud")
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("slam")
# install.packages("xlsx")
# install.packages("tm")
# install.packages("stringi")
# install.packages("stringr")

library("tm")
library("SnowballC")
require("topicmodels")
library("wordcloud")
library("reshape2")
library("ggplot2")
library("slam")
library("xlsx")
library("tm")
library("stringi")
library("stringr")
library("xml2")
library(openNLP)
library(Matrix)


# Load german stopwords

filePath <- paste(wd,"/textvorgaben/stopwords_de.txt", sep = "")
germanStopwords <- readLines(filePath, encoding = "UTF-8")

# --------------------------------------
# Load files and build corpus
# --------------------------------------

options(stringsAsFactors = FALSE)
# Corpus file in CSV format
sourceFile <- paste(wd,"/afd.chat.protocol.v2.csv", sep="")

# Einlesen der Datei in ein Objekt
textdata <- read.csv(
  sourceFile, 
  header = TRUE, 
  sep = ";", 
  quote="\"", 
  encoding = "UTF-8"
)

# ---

# Dimensionen des eingelesenen Data-Frame
dim(textdata)
# Spaltennamen der Metadaten
colnames(textdata)

# ändere namensfehler
colnames(textdata)[1]<-c("Datum")

# ---

# entferne umlaute und emojis

texttemp <- gsub("ü", "ue", textdata[,4])
texttemp <- gsub("ö", "oe", texttemp)
texttemp <- gsub("ä", "ae", texttemp)
texttemp <- gsub("ß", "ss", texttemp)
texttemp <- gsub("[^\x01-\x7F]", "", texttemp)

textdata$text_plain_cleaned <- texttemp

# Erstelle corpus

df_title <- data.frame(doc_id=textdata$Uhrzeit,
                       text=textdata$text_plain_cleaned)


afd.chat.corpus <- VCorpus(DataframeSource(df_title), readerControl = list(language = "german"))

# Corpus
afd.chat.corpus
# Examples
as.character(afd.chat.corpus[[4198]])
as.character(afd.chat.corpus[[2512]])
as.character(afd.chat.corpus[[8256]])
# Note: Multiple questionmarks mark emojicons in the chat message.

# ---

# Preprocessing

afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus, removePunctuation, preserve_intra_word_dashes = TRUE )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, content_transformer( tolower ) )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeNumbers )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, germanStopwords ) 
# L?sche standardeintrag im chat, wenn bilder etc. gepostet wurden
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, "bild weggelassen" )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, "video weggelassen" )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, stripWhitespace )
#afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, stemDocument, language ="german"  )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, PlainTextDocument )


# Add Meta data

#metadata
meta( afd.chat.corpus, type = "local", tag="date" ) <- textdata$Datum
meta( afd.chat.corpus, type = "local", tag="time" ) <- textdata$Uhrzeit
# Meta "author": name or phone number in chat protocol
meta( afd.chat.corpus, type = "local", tag="author" ) <- textdata$verfasser_plain
# Meta "messagedummy": 1 if text is an message, 0 otherwise (status updates like XY joined etc.)
meta( afd.chat.corpus, type = "local", tag="messagedummy" ) <- textdata$beitrags_dummy
meta( afd.chat.corpus, type = "local", tag="origin" ) <- "https://linksunten.indymedia.org/de/system/files/data/2017/06/3098700935.txt"


# Erstellen einer Dokument-Term-Matrix (kann ein wenig dauern)
DTM <- DocumentTermMatrix(afd.chat.corpus)
DTM.mod1 <- DocumentTermMatrix(afd.chat.corpus.mod1)
# Aufrufen von Informationen ?ber die Dokument-Term Matrix
DTM
# Dimensionen der DTM (Spalten/Zeilen: Anzahl der Dokumente/Types)
dim(DTM)


# L?sche Eintr?ge aus dem Corpus mit zu wenigen Eintr?gen

# Word count per document
head(rowSums(as.matrix(DTM.mod1)),n=10)
summary(rowSums(as.matrix(DTM.mod1)))

# selection vector for posts < 5 words after preproc.
selectvec <- (rowSums(as.matrix(DTM.mod1))>5)
textdata_filtered <-textdata[selectvec,]

# compare raw and filtered textdata
dim(textdata_filtered)
dim(textdata)
# ~75% of all posts are filtered out



# Make a new corpus with the filtered data
# gleicher name wie der *.mod1 corpus

df_title <- data.frame(doc_id=textdata_filtered$Uhrzeit,
                       text=textdata_filtered$text_plain_cleaned)


afd.chat.corpus.mod1 <- VCorpus(DataframeSource(df_title), readerControl = list(language = "german"))

# Preprocessing

afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removePunctuation, preserve_intra_word_dashes = TRUE )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, content_transformer( tolower ) )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeNumbers )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, germanStopwords ) 
# L?sche standardeintrag im chat, wenn bilder etc. gepostet wurden
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, "bild weggelassen" )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, removeWords, "video weggelassen" )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, stripWhitespace )
#afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, stemDocument, language ="german"  )
afd.chat.corpus.mod1 <- tm_map( afd.chat.corpus.mod1, PlainTextDocument )


# Add Meta data


#metadata
meta( afd.chat.corpus.mod1, type = "local", tag="date" ) <- textdata$Datum
meta( afd.chat.corpus.mod1, type = "local", tag="time" ) <- textdata$Uhrzeit
# Meta "author": name or phone number in chat protocol
meta( afd.chat.corpus.mod1, type = "local", tag="author" ) <- textdata$verfasser_plain
# Meta "messagedummy": 1 if text is an message, 0 otherwise (status updates like XY joined etc.)
meta( afd.chat.corpus.mod1, type = "local", tag="messagedummy" ) <- textdata$beitrags_dummy
meta( afd.chat.corpus.mod1, type = "local", tag="origin" ) <- "https://linksunten.indymedia.org/de/system/files/data/2017/06/3098700935.txt"


# Erstellen einer Dokument-Term-Matrix (kann ein wenig dauern)
DTM.mod1 <- DocumentTermMatrix(afd.chat.corpus.mod1)



#-------------------------------------------------------------------------------
#
# Analyse von politischen Themen die in der Chatgruppe besprochen wurden
#
# ------------------------------------------------------------------------------

# Alle W?rter

# Berechnen der Summen aller Spalten in der Dokument Term Matrix
freqs <- slam::col_sums(DTM.mod1)
# Erzeugen eines Vektors mit den Namen der Einzelw?rter
words <- colnames(DTM.mod1)
# Erzeugen einer Datenstruktur, deren Spalten die Wortbezeichnung und dessen H?ufigkeit enth?lt
wordlist <- data.frame(words, freqs)
# Absteigende Sortierung der Wortliste anhand der H?ufigkeiten
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
wordlist <- wordlist[wordIndexes, ]
# Anzeigen der 25 h?ufigsten W?rter
head(wordlist, 25)

# subselections of words

plot(wordlist$freqs, type = "l", log="xy",lwd=2, main = "Rank-Frequency plot", xlab="Rank", ylab = "Frequency")
stopwords_idx <- which(wordlist$words %in% germanStopwords)
low_frequent_idx <- which(wordlist$freqs < 10)
insignificant_idx <- union(stopwords_idx, low_frequent_idx)
meaningful_range_idx <- setdiff(1:nrow(wordlist), insignificant_idx)
lines(meaningful_range_idx, wordlist$freqs[meaningful_range_idx], col = "green", lwd=2, type="p", pch=20)

wordlist_filtered <- wordlist[meaningful_range_idx, ]
head(wordlist_filtered, 25)



#---------------------------------------------
# Bestimmte Themen
#---------------------------------------------

auslaenderExpression <- "fluechtling|auslaender|afrika|flucht"
allTypes <- colnames(DTM.mod1)
allRegExpMatchings <- grepl(auslaenderExpression, allTypes)
auslaenderTerms <- allTypes[allRegExpMatchings]
#exclude references to weblinks
auslaenderTerms<-grep("^(?!http)", auslaenderTerms, value=T, perl=T)
# Show select termsed 
auslaenderTerms
terms <- c(auslaenderTerms)
# Subselektion der Spalten aus der Dokument-Term Matrix, die den Namen der gesuchten Terme haben. Spalten in R k?nnen auch ?ber deren Spaltennamen selektiert werden. Selektion ?ber den Spaltenindex w?re aber auch m?glich.
DTM.mod1.auslaenderTerms <- DTM.mod1[,terms]
# Berechnen der Summen aller Spalten in der Dokument Term Matrix
freqs <- slam::col_sums(DTM.mod1.auslaenderTerms)
# Erzeugen eines Vektors mit den Namen der Einzelw?rter
words <- colnames(DTM.mod1.auslaenderTerms)
# Erzeugen einer Datenstruktur, deren Spalten die Wortbezeichnung und dessen H?ufigkeit enth?lt
wordlist <- data.frame(words, freqs)
# Absteigende Sortierung der Wortliste anhand der H?ufigkeiten
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
auslaenderwordlist <- wordlist[wordIndexes, ]
# Anzeigen der h?ufigsten W?rter
auslaenderwordlist


deutschExpression <- "deutsch|heimat|patriot"
allTypes <- colnames(DTM.mod1)
alldeutschExpression <- grepl(deutschExpression, allTypes)
deutschTerms <- allTypes[alldeutschExpression]
#exclude references to weblinks
deutschTerms<-grep("^(?!http)", deutschTerms, value=T, perl=T)
# Show select termsed 
deutschTerms
terms <- c(deutschTerms)
# Subselektion der Spalten aus der Dokument-Term Matrix, die den Namen der gesuchten Terme haben. Spalten in R k?nnen auch ?ber deren Spaltennamen selektiert werden. Selektion ?ber den Spaltenindex w?re aber auch m?glich.
DTM.mod1.deutschTerms <- DTM.mod1[,terms]
# Berechnen der Summen aller Spalten in der Dokument Term Matrix
freqs <- slam::col_sums(DTM.mod1.deutschTerms)
# Erzeugen eines Vektors mit den Namen der Einzelw?rter
words <- colnames(DTM.mod1.deutschTerms)
# Erzeugen einer Datenstruktur, deren Spalten die Wortbezeichnung und dessen H?ufigkeit enth?lt
wordlist <- data.frame(words, freqs)
# Absteigende Sortierung der Wortliste anhand der H?ufigkeiten
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
deutschwordlist <- wordlist[wordIndexes, ]
# Anzeigen der h?ufigsten W?rter
deutschwordlist


mediaExpression <- "presse|medien"
allTypes <- colnames(DTM.mod1)
allmediaExpMatchings <- grepl(mediaExpression, allTypes)
mediaTerms <- allTypes[allmediaExpMatchings]
#exclude references to weblinks
mediaTerms<-grep("^(?!http)", mediaTerms, value=T, perl=T)
# Show selected terms 
mediaTerms
terms <- c(mediaTerms)
# Subselektion der Spalten aus der Dokument-Term Matrix, die den Namen der gesuchten Terme haben. Spalten in R k?nnen auch ?ber deren Spaltennamen selektiert werden. Selektion ?ber den Spaltenindex w?re aber auch m?glich.
DTM.mod1.mediaTerms <- DTM.mod1[,terms]
# Berechnen der Summen aller Spalten in der Dokument Term Matrix
freqs <- slam::col_sums(DTM.mod1.mediaTerms)
# Erzeugen eines Vektors mit den Namen der Einzelw?rter
words <- colnames(DTM.mod1.mediaTerms)
# Erzeugen einer Datenstruktur, deren Spalten die Wortbezeichnung und dessen H?ufigkeit enth?lt
wordlist <- data.frame(words, freqs)
# Absteigende Sortierung der Wortliste anhand der H?ufigkeiten
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
mediawordlist <- wordlist[wordIndexes, ]
# Anzeigen der 100 h?ufigsten W?rter
mediawordlist


EuroExpression <- "euro"
allTypes <- colnames(DTM.mod1)
allEuroExpMatchings <- grepl(EuroExpression, allTypes)
EuroTerms <- allTypes[allEuroExpMatchings]
#exclude references to weblinks
EuroTerms<-grep("^(?!http)", EuroTerms, value=T, perl=T)
#exclude references to "european"-topics
EuroTerms<-grep("^(?!europ)", EuroTerms, value=T, perl=T)
# Show selected terms 
EuroTerms
terms <- c(EuroTerms)
# Subselektion der Spalten aus der Dokument-Term Matrix, die den Namen der gesuchten Terme haben. Spalten in R k?nnen auch ?ber deren Spaltennamen selektiert werden. Selektion ?ber den Spaltenindex w?re aber auch m?glich.
DTM.mod1.EuroTerms <- DTM.mod1[,terms]
# Berechnen der Summen aller Spalten in der Dokument Term Matrix
freqs <- slam::col_sums(DTM.mod1.EuroTerms)
freqs


# ---



# Wer sind die flei?igsten Poster?

verfasser <- table(textdata[, "verfasser_plain"])
verfasserlist <- data.frame(verfasser)
# Absteigende Sortierung der Wortliste anhand der H?ufigkeiten
verfasserIndexes <- order(verfasserlist[, "Freq"], decreasing = TRUE)
verfasserlist <- verfasserlist[verfasserIndexes, ]
# Anzeigen der 25 h?ufigsten W?rter
head(verfasserlist, 25)

# posts von einem verfasser
andrepoggenburgposts <- which(textdata$verfasser_plain == "Andre Poggenburg")

# wichtige Vokabeln eines verfassers, gewichtete via tf_idf

# Compute IDF: log(N / n_i)
number_of_docs <- nrow(DTM.mod1)
term_in_docs <- col_sums(DTM.mod1 > 0) # true/false matrix bzw 0/1 matrix
idf <- log2(number_of_docs / term_in_docs) #number devided by a vector => elementwise calculation

# Compute TF

selector_logical_idx <- textdata$verfasser_plain == "Andre Poggenburg"

andreDTM <- DTM.mod1[selector_logical_idx, ]
tf <- col_sums(andreDTM)

# Compute TF-IDF
tf_idf <- tf * idf
names(tf_idf) <- colnames(DTM.mod1)

sort(tf_idf, decreasing = T)[1:20]



# Topic model

# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 1
DTM_minfreq <- DocumentTermMatrix(afd.chat.corpus.mod1, control = list(bounds = list(global = c(minimumFrequency, Inf))))

# have a look at the number of documents and terms in the matrix
dim(DTM_minfreq)

# number of topics (number chosen without any background)
K <- 40
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 500 iterations of Gibbs sampling
topicModel <- LDA(DTM_minfreq, K, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 0.2))

# Results

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
nTerms(DTM_minfreq)              # lengthOfVocab

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

rowSums(beta)            # rows in beta sum to 1

nDocs(DTM_minfreq)               # size of collection

# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10]     # rows in theta sum to 1

terms(topicModel, 5)

# concat the five most likely terms of each topic to a string,
# representing a pseudo-name for each topic.

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")



# Visualization of Words and Topics


# visualize topics as word cloud
topicToViz <- 21 # change for your own topic of interest
topicToViz <- grep('presse', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)


# Let us first take a look at the contents of three sample documents:

exampleIds <- c(200, 1011, 1915)
lapply(afd.chat.corpus.mod1[exampleIds], as.character)

# visualize the topic distributions within the documents.

# load libraries for visualization

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)


# Topic ranking

# sort topics according to their probability within the entire collection:

# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(DTM_minfreq)     # mean probablities over all documents
names(topicProportions) <- topicNames               # assign the topic names we created before
sort(topicProportions, decreasing = TRUE)           # show summed proportions in decreased order


# count how often a topic appears as a primary topic within a document. This method is also called Rank-1.

countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs(DTM_minfreq)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)


# Unfertiges...

# Ideen wie/welche Methoden auf den afd chat Datensatz anzuwenden sein k?nnten
# .	Clustering over verfasser_plain
# .	Cooc. Analysis over wording in posts
# .	Heatmap for words over verfasser_plain
# .	Sentimentanalysis over whole corpus/verfasser_plain
# .	Topic model over posts to see what different kinds of discussions there are

# sentiment list

url <- paste(wd,"/textvorgaben/OPdict.xml", sep = "")
html_document <- read_xml(url)



test<-as.matrix(DTM_minfreq)
