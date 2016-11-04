






#### extracting postags

public = read.delim("public_leaderboard_rel_2.tsv")

pset1= public[public$EssaySet=="7",]

View(pset1)

pset1$nounCount= rep(0, 599)
pset1$verbCount= rep(0, 599)
pset1$adjectiveCount= rep(0, 599)
pset1$adverbCount= rep(0, 599)



tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

for(i in 1:nrow(pset1)){
  tagged_str= tagPOS(pset1$EssayText[i])
  
  nouns <- strsplit(unlist(tagged_str$POStagged),'/NN')
  
  verb= strsplit(unlist(tagged_str$POStagged),'/VB')
  
  adjective= strsplit(unlist(tagged_str$POStagged),'/JJ')
  
  adverb= strsplit(unlist(tagged_str$POStagged),'/RB')
  
  pset1$nounCount[i]= NROW(nouns[[1]])-1
  pset1$verbCount[i]= NROW(verb[[1]])-1
  pset1$adjectiveCount[i]= NROW(adjective[[1]])-1
  pset1$adverbCount[i]= NROW(adverb[[1]])-1
  
  gc(reset= TRUE)
}



write.csv(pset1 , "pset7.csv", row.names = F)





##### making predictions 


###################################

### combining train and public leaderboard data
str(pset1)


pset1=read.csv("pset7.csv")
newset1= read.csv("Set7.csv")
newset1$Score2=NULL
str(newset1)

pset1$Score1= -1


comb= rbind(newset1, pset1)
View(comb)

### making bag of words 
pcorpus= Corpus(VectorSource(comb$EssayText))

pcorpus = tm_map(pcorpus, content_transformer(tolower))
pcorpus= tm_map(pcorpus, removeNumbers)
pcorpus = tm_map(pcorpus, PlainTextDocument)
pcorpus= tm_map(pcorpus, removePunctuation)
pcorpus= tm_map(pcorpus, stripWhitespace)
pcorpus= tm_map(pcorpus, removeWords, stopwords("english"))
pcorpus= tm_map(pcorpus, stemDocument)
ptdm1 = DocumentTermMatrix(pcorpus)

psparse = removeSparseTerms(ptdm1, 0.997)

pset1Sparse= as.data.frame(as.matrix(psparse))
colnames(pset1Sparse) = make.names(colnames(psparse))




## adding number of words

pset1Sparse$wordcount =rowSums(pset1Sparse)



## finding average word length
avgword= as.data.frame(sapply(pcorpus, function(x) { mean(nchar(x))}))

pset1Sparse$wordLength  =avgword$`sapply(pcorpus, function(x) {     mean(nchar(x)) })`




### finding spelling error
comb$EssayText= as.character(comb$EssayText)
pset1Sparse$spellerror= sapply(comb$EssayText, function(x) { length(hunspell_find(x)[[1]])})


## sentence count

pset1Sparse$sentenceCount =sapply(comb$EssayText, function(x) { NROW(sent_detect(x))})


### comma count 

pset1Sparse$commaCount= sapply(comb$EssayText, function(x) { NROW(sent_detect(x, endmarks = c(",")))-1 })


### posTags
pset1Sparse$nounCount= comb$nounCount
pset1Sparse$verbCount= comb$verbCount
pset1Sparse$adjectiveCount= comb$adjectiveCount
pset1Sparse$adverbCount= comb$adverbCount


### adding Score1 column
pset1Sparse$Score1= comb$Score1
pset1Sparse$Score1= as.factor(pset1Sparse$Score1)

dim(pset1Sparse)


#### again dividing data back into train and test
testingdata= subset(pset1Sparse, Score1== -1)

trainingdata= subset(pset1Sparse, Score1!= -1)

testingdata$Score1=NULL


####################  modelling

train.h2o= as.h2o(trainingdata)
test.h2o= as.h2o(testingdata)

colnames(train.h2o)
colnames(test.h2o)
which(colnames(train.h2o)== "Score1")
ncol(train.h2o)

y.dep= 675
x.dep= c(1:674)

rforest.model <- h2o.randomForest(y=y.dep, x=x.dep, training_frame = train.h2o, ntrees = 500)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

table( predict.rforest$predict)

pset1$Score1= predict.rforest$predict
write.csv(pset1, "pset7.csv", row.names = F)



