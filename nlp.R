

DATA
qview(DATA)
htruncdf(DATA)



data("crude")
crude[[1]][2]
View(crude)
tdm=TermDocumentMatrix(crude)
findFreqTerms(tdm,2 ,3)
inspect(crude)

tdm[[1]]
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))



tm_dat <- qdap_dat <- DATA[1:4, c(1, 4)]
rownames(tm_dat) <- paste("docs", 1:nrow(tm_dat))
tm_dat <- Corpus(DataframeSource(tm_dat[, 2, drop=FALSE]))


qdap_dat


with(qdap_dat, wfm(state, person))


### extracting entities
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")


text= "In 1804, after several months of profound spiritual anxiety, Jarena Lee moved from New Jersey to Philadelphia. There she labored as a domestic and worshiped among white congregations of Roman Catholics and mixed congregations of Methodists. On hearing an inspired sermon by the Reverend Richard Allen, founder of the Bethel African Methodist Episcopal Church, Lee joined the Methodists. She was baptized in 1807. Prior to her baptism, she experienced the various physical and emotional stages of conversion: terrifying visions of demons and eternal perdition; extreme feelings of ecstasy and depression; protracted periods of meditation, fasting, and prayer; ennui and fever; energy and vigor. In 1811 she married Joseph Lee, who pastored an African-American church in Snow Hill, New Jersey. They had six children, four of whom died in infancy."

pipeline <- list(sent_ann, word_ann, person_ann, location_ann, organization_ann)
bio_annotations <- annotate(text, pipeline)
bio_doc <- AnnotatedPlainTextDocument(text, bio_annotations)
annotations(bio_doc)

entities= function(doc, kind){
  
  ss= doc$content
  a= annotations(doc)[[1]]
  if(hasArg(kind)){
    k= sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  }else{
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc, kind = "location")




#####sentence count using openNLP

s= as.String(Set1$EssayText[8])
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(s, list(sent_ann, word_ann))

class(bio_annotations)
head(bio_annotations)


bio_doc= AnnotatedPlainTextDocument(s, bio_annotations)
sents(bio_doc)





## for  postags information visit this...... "https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html"
### extracting nouns

tagged_str <-  tagPOS(Set1$EssayText[1])
tagged_str$POStagged
unique(tagged_str$POStags)

acqTagSplit= strsplit(tagged_str$POStagged, " ")
acqTagSplit
strsplit(acqTagSplit[[1]], "/")


q <- strsplit(unlist(tagged_str$POStagged),'/VB')
q

NROW(q[[1]])-1  ## this is noun count

qq= vector(mode="character", length = length(q[[1]]))
for(i in 1:length(q[[1]])){
  
  qq[i] <-tail(strsplit(unlist(q[1])," ")[[i]],1)
}
unique(qq)




########### n grams
install.packages("quanteda")
library(quanteda)

Text <- c("Ab Hello world", "Hello ab", "ab")

dfm(Set1$EssayText, ngrams = 2, verbose = FALSE)

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

str(Set1)
Set1$EssayText= as.character(Set1$EssayText)
bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
View(bigram)

bigram$dimnames$Terms

bigram= as.data.frame(bigram)

testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
the newspaper from a boy named Seamus, in his mouth."
testCorpus <- corpus(testText)
# note: "also" is not in the default stopwords("english")
features(dfm(testCorpus, ignoredFeatures = stopwords("english")))
# for ngrams

features(dfm(Set1$EssayText, ngrams = 3, ignoredFeatures = stopwords("english")))
features(dfm(testCorpus, ngrams = 1:2, ignoredFeatures = stopwords("english")))

