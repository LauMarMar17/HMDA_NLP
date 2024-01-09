#########################
# 1. Subwork tokenization
#########################
# read Qcaps.rds. This object is a list of strings. Each string is a chapter.
caps <-readRDS(file="data/Qcaps.rds")

# The novel has 2 parts. part1: Capítulo primero to Capítulo XXXVII
#                        part2: Capítulo XXXVIII to the end
text1 <- paste(unlist(caps[53:89]), collapse="\n")
text2 <- paste(unlist(caps[90:126]), collapse="\n")

## BPE model

library(tokenizers.bpe)

model <- bpe(unlist(caps[53:89])) # train the model with the first part (text1)
subtoks2 <- bpe_encode(model, x=text2, type="subwords") # use text2 to test the model
head(unlist(subtoks2), n=20)

###########################
# 2. Distance between texts
###########################
# create a TF-IDF matrix with quanteda
# Using as docs the Don Quijote chapters

library(quanteda)
texts_caps <- unlist(caps)
names(texts_caps) <- paste("Chap.", 1:length(texts_caps))
corpus_capsQ <-  corpus(texts_caps)
docvars(corpus_capsQ, field="Chapter") <- 1:length(texts_caps)
corpus_capsQ

# doc-feature matrix (dfm)
dfm_capsQ <- dfm(tokens(corpus_capsQ))

# Dendrogram for distance (euclidean)
disMatrix <- dist(as.matrix(dfm_capsQ), method="euclidean")
groups <- hclust(disMatrix, method="ward.D")
plot(groups, 
    cex=0.25,    # label's size
    hang=-1,     # same hight labels
    xlab="",
    ylab="", 
    main="Dendrogram")
rect.hclust(groups, k=6)

# Most frequent and infrequent features
topfeatures(dfm_capsQ)
dfm_capsQ_1 <- dfm(tokens(corpus_capsQ,
                          remove_punct=TRUE)
                   )
dfm_capsQ_2 <- dfm_remove(dfm_capsQ_1, stopwords("es"))# remove stopwords 
topfeatures(dfm_capsQ_2)
# less frequent
topfeatures(dfm_capsQ_2, decreasin=FALSE)

#######################
# 3. Sentiment Analysis
#######################
