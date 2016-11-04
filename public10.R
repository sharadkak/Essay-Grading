


#### very bad prediction on this set

#### postags and bigrams didnt help in this one

public = read.delim("public_leaderboard_rel_2.tsv")

pset1= public[public$EssaySet=="10",]


##### making predictions 


###################################

### combining train and public leaderboard data
str(pset1)

newset1= read.csv("Set10.csv")
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

View(pset1Sparse)



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
View(testingdata)
trainingdata= subset(pset1Sparse, Score1!= -1)
View(trainingdata)
testingdata$Score1=NULL


####################  modelling




ruleModel <- C5.0(Score1~. , data = trainingdata, rules = TRUE, trails=10)
ruleModel
summary(ruleModel)

c50.pred <- predict(ruleModel, testingdata)
c50.pred
table(c50.pred)

table(Set10$Score1)

pset1$Score1= c50.pred
write.csv(pset1, "pset10.csv", row.names = F)

