
options(java.parameters = "- Xmx1024m")

library(caTools)
library(Metrics)
library(openNLPdata)
library(openNLP)
library(NLP)
library(RWeka)
library(qdap)
library(openNLPmodels.en)
library(tm)
library(qdap)
library(hunspell)

train <- read.delim("train_rel_2.tsv")


str(train)


train$EssaySet= as.factor(train$EssaySet)
train$Score1= as.factor(train$Score1)


Set1= train[train$EssaySet=="1",]
Set2= train[train$EssaySet=="2",]
Set3= train[train$EssaySet=="3",]
Set4= train[train$EssaySet=="4",]
Set5= train[train$EssaySet=="5",]
Set6= train[train$EssaySet=="6",]
Set7= train[train$EssaySet=="7",]
Set8= train[train$EssaySet=="8",]
Set9= train[train$EssaySet=="9",]
Set10= train[train$EssaySet=="10",]


pset1= public[public$EssaySet==1,]
View(Set1)



View(as.data.frame(tdm1))
corpus= Corpus(VectorSource(Set1$EssayText))

corpus = tm_map(corpus, content_transformer(tolower))
corpus= tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)
corpus= tm_map(corpus, removePunctuation)
corpus= tm_map(corpus, removeWords, stopwords("english"))
corpus= tm_map(corpus, stemDocument)
tdm1 = DocumentTermMatrix(corpus)
findFreqTerms(tdm1, lowfreq = 20)
tdm1
corpus[[1]]$content
sparse = removeSparseTerms(tdm1, 0.997)
sparse

set1Sparse= as.data.frame(as.matrix(sparse))
View(set1Sparse)

colnames(set1Sparse) = make.names(colnames(sparse))
set1Sparse$Score1= Set1$Score1
set1Sparse$Score1= as.factor(set1Sparse$Score1)


## adding number of words
set1Sparse$Score1=NULL
set1Sparse$wordcount =rowSums(set1Sparse)
set1Sparse$wordcount[1:10]


## finding average word length
avgword= as.data.frame(sapply(corpus, function(x) { mean(nchar(x))}))
avgword
set1Sparse$wordLength  =avgword$`sapply(corpus, function(x) {     mean(nchar(x)) })`
set1Sparse$wordLength= NULL

## character count
charcount= as.data.frame(sapply(corpus, function(x) { nchar(x)}))[1]
charcount



### finding spelling error
Set1$EssayText= as.character(Set1$EssayText)
set1Sparse$spellerror= sapply(Set1$EssayText, function(x) { length(hunspell_find(x)[[1]])})


## sentence count

set1Sparse$sentenceCount =sapply(Set1$EssayText, function(x) { NROW(sent_detect(x))})


### comma count 

set1Sparse$commaCount= sapply(Set1$EssayText, function(x) { NROW(sent_detect(x, endmarks = c(",")))-1 })


### punctuation count



##### n grams



### POS tags
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

s= Set1$EssayText[1]
tagged_str <-  tagPOS(s)
tagged_str$POStagged
unique(tagged_str$POStags)


View(Set7)

######### finding pos tags
Set8$nounCount= rep(0, 1799)
Set8$verbCount= rep(0, 1799)
Set8$adjectiveCount= rep(0, 1799)
Set8$adverbCount= rep(0, 1799)

for(i in 1:nrow(Set8)){
  tagged_str= tagPOS(Set8$EssayText[i])
  
  nouns <- strsplit(unlist(tagged_str$POStagged),'/NN')
  
  verb= strsplit(unlist(tagged_str$POStagged),'/VB')
  
  adjective= strsplit(unlist(tagged_str$POStagged),'/JJ')
  
  adverb= strsplit(unlist(tagged_str$POStagged),'/RB')

  Set8$nounCount[i]= NROW(nouns[[1]])-1
  Set8$verbCount[i]= NROW(verb[[1]])-1
  Set8$adjectiveCount[i]= NROW(adjective[[1]])-1
  Set8$adverbCount[i]= NROW(adverb[[1]])-1
  
  gc(reset= TRUE)
}


write.csv(Set8, "Set8.csv", row.names = F)




## modelling

library(caTools)
set1Sparse$Score1= as.factor(set1Sparse$Score1)
set.seed(144)
split = sample.split(set1Sparse$Score1, SplitRatio = 0.7)
trainsparse = subset(set1Sparse, split==T)
testsparse = subset(set1Sparse, split==F)

library(rpart.plot)
library(rpart)

set1Cart = rpart(Score1 ~ . , data = trainsparse, method = "class")
prp(set1Cart)

predictCart = predict(set1Cart, testsparse, type = "class")
predictCart
table(testsparse$Score1, predictCart)


(82+44+76+55)/ nrow(testsparse) ## with just bag of words (0.999)
(89+49+79+47)/ nrow(testsparse) ## with bag of words (0.997)

(93+ 41+ 88+ 44)/ nrow(testsparse) ## with wordcount

(104+51+77+44)/ nrow(testsparse)  ## after adding charcount

(88+28+105+37)/nrow(testsparse)  ### with wordcount and average word length, also gives good kappa score


(100+53+93+44)/ nrow(testsparse) #### with wordcount and average word length, nounCount, verbCount, also gives good kappa score (best so far)
ScoreQuadraticWeightedKappa(testsparse$Score1, predictCart, 0, 1)



## using svm for classification 
library(e1071)


svm_model <- svm(Score1~. , data = trainsparse)
summary(svm_model)

svm.pred <- predict(svm_model, testsparse)
table(testsparse$Score1, svm.pred)


(76+51+111+26)/ nrow(testsparse)


(89+43+ 109+30)/ nrow(testsparse) ## bag of words
(77+58+107+24)/ nrow(testsparse) ## with wordcount
(68+18+124+5)/nrow(testsparse)


(78+55+110+27)/nrow(testsparse)  ## with wordcount, average length fof word
(88+55+103+32)/nrow(testsparse)## with wordcount, average length of word, noun count

ScoreQuadraticWeightedKappa(testsparse$Score1, svm.pred, 0, 1)





#### tuning SVM
x= subset(trainsparse, select = -Score1)
y=trainsparse$Score1

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(Score1~. , data = trainsparse, kernel="radial", cost=10, gamma=0.5)
summary(svm_model_after_tune)

svm.pred <- predict(svm_model_after_tune, testsparse)
table(testsparse$Score1, svm.pred)



#### cross validation
library(caret)
tc <- trainControl("cv",5)
rpart.grid <- expand.grid(.cp=0.2)

memory.size(TRUE)
train.rpart <- train(Score1~. , data = train, method="rpart",
                      trControl=tc,tuneGrid=rpart.grid)

help(memory.size)



#### random forest model
library(randomForest)
rf.model= randomForest(as.factor(Score1)~. , data = trainsparse)
rf.model

rf.pred <- predict(rf.model, testsparse, type = "class")
table(testsparse$Score1, rf.pred)
(87+51+122+30)/ nrow(testsparse)
(88+52+119+31)/nrow(testsparse)


importance(rf.model)

ScoreQuadraticWeightedKappa(testsparse$Score1, rf.pred, 0, 1)



#### using C5.0
library(C50)
ruleModel <- C5.0(Score1~. , data = trainsparse, rules = TRUE)
ruleModel
summary(ruleModel)

c50.pred <- predict(ruleModel, testsparse)
c50.pred
table(testsparse$Score1, c50.pred)

(89+55+97+41)/nrow(testsparse)
ScoreQuadraticWeightedKappa(testsparse$Score1, c50.pred, 0, 1)


#### using KNN
library(class)
names(testsparse)[365:374]
knn.model= knn(train = trainsparse[,-370], test =testsparse[,-370], cl= trainsparse$Score1, k=3)
knn.model

table(testsparse$Score1, knn.model)

ScoreQuadraticWeightedKappa(testsparse$Score1, knn.model, 0, 1)
