library(tm) 
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(factoextra)
library(NbClust)
library(topicmodels)
#library(philentropy)
getwd()
setwd("/Users/gwennguyen/Downloads/ITD")
doc_admin <- VCorpus(DirSource("docs"))
##### Processing the doc_adminument #####
#Check number of doc_admins loaded
print(doc_admin)
#inspect a particular doc_adminument
writeLines(as.character(doc_admin[[1]]))
#Remove punctuation
doc_admin <- tm_map(doc_admin, removePunctuation) 
#Transform to lower case
doc_admin <- tm_map(doc_admin,content_transformer(tolower))
#Strip digits
doc_admin <- tm_map(doc_admin, removeNumbers)
#Remove stopwords from standard stopword list 
doc_admin <- tm_map(doc_admin, removeWords, stopwords("english"))
#Since this is English doc_adminument, removing all except alphabets and numbers would be fine
eng <- content_transformer(function (x) gsub("[^a-zA-Z0-9 ]"," ",x))
doc_admin <- tm_map(doc_admin, eng)
#Strip whitespace
doc_admin <- tm_map(doc_admin, stripWhitespace) 
#Stem doc_adminument
doc_admin <- tm_map(doc_admin,stemdoc_adminument)
#inspect
writeLines(as.character(doc_admin[[1]]))
#Clean
rm(eng)

##### Types of DTM #####
#Regular DTM
dtm <- DocumentTermMatrix(doc_admin)
#Weighted DTM
dtm_tfidf <- DocumentTermMatrix(doc_admin,control = list(weighting = weightTfIdf))
#Bigram DTM
BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
dtmbi <- DocumentTermMatrix(doc_admin, control = list(tokenize = BigramTokenizer))
rm(BigramTokenizer)

#Choose topics: Most frequent words
#Purpose: These words might be used to give hints of topics after clustering
#Record the most frequent used words in order
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#Write to disk and inspect file
write.csv(file="admin_hardware2_most frequent words.csv",freq[ord])

#Choose topics: Most weighted words
#Purpose: These words might be used to give hints of topics after clustering
#Record the most weighted words in order
wt_tot_tfidf <- colSums(as.matrix(dtm_tfidf))
ord_tfidf <- order(wt_tot_tfidf,decreasing=TRUE)
#inspect most frequently occurring terms
wt_tot_tfidf[head(ord_tfidf)]
#write to disk and inspect file
write.csv(file="FEIT_most weighted words.csv",wt_tot_tfidf[ord_tfidf])

#### LDA ####

best <- TRUE
burnin <- 1000
iter <- 2000
thin <- 500
nstart <- 5
k <- 5
seed <- list(100,5,123,234,789)
ldaOut <- LDA(dtmbi,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("FEIT",k,"DocsToTopics.csv"))
terms(ldaOut,8)
ldaOut.terms <- as.matrix(terms(ldaOut,8))
write.csv(ldaOut.terms,file=paste("FEIT",k,"TopicsToTerms.csv"))

