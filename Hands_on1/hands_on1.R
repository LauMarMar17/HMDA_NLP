##################################################
# 1. Read text from Internet and selection of text
##################################################

urlQuijoteGutenberg <- "https://www.gutenberg.org/files/2000/2000-0.txt"
lines <- readLines(urlQuijoteGutenberg,
                   encoding = "UTF-8") # few seconds
# Delete header. Last and first line of the header contain "***"

grep(pattern = "***",
     lines, 
     fixed=TRUE)# Whithout fixed the regex is "\\*\\*\\*"

    # [1] 24 37704 37706

linesQ <- lines[25:37703]
length(linesQ)
    # [1] 37679

# Delete the prologe. We are interested in "En un lugar de La Mancha, de cuyo..."

grep(pattern = "En un lugar de",
     linesQ, 
     fixed = TRUE)
    # [1]  1045 13513 --> It repeats two times. The first one is the begining.

linesQ <- linesQ[-c(1:1044)] # removes the prologue
length(linesQ)
    # [1] 36635

# join lines
paste(linesQ[1:5], collapse=" ")

################
# 2. Basic Check
################

library(utf8)
# Check encoding
linesQ[!utf8_valid(linesQ)] # if "character(0)", all lines are encoded with UTF-8

# Check character normalization (NFC)
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ)
    # [1] 0

# 3. Basic structuration
########################

# Obtain a vector with the paragraphs in the text (not empty text block separated form another by two lines)

stringQ <- paste(linesQ, collapse="\n")
paragraphs <- unlist(strsplit(stringQ, "\n\n\n", fixed=TRUE))

parEmpty <- which(paragraphs == "") # Look for empty paragraphs
length(parEmpty)
    # [1] 0 --> No empty paragraphs

# paragraphs <- paragraphs[-parEmpty]

length(paragraphs)

# First 200 characters of the first paragraph:
substring(paragraphs[1], 1, 200)

##################
# 4. Some Cleaning
##################

# replace any sequence of one or more \n by " ".
paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs)
substring(paragraphswoNL[1], 1, 200)

# replace any double-space by single-space
paragraphs <- gsub("[ ]{2, }", " ", paragraphswoNL)
substring(paragraphs[1], 1, 200)

#################
# 5. Some numbers
#################
library(spacyr)
spacy_install() # This creates an enviroment on his own.
spacy_download_langmodel("es") # download the es_core_news_sm to disk
spacy_initialize(model = "es_core_news_sm")

# Get sentences form paragraphs
phrases <- spacy_tokenize(paragraphs, 
                          what="sentence",
                          # default:remove_separators = TRUE
                          )
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases)
sum(v_phrases=="")
    # [1] 121

v_phrases<-v_phrases[-which(v_phrases=="")]

# histogram
hist(nchar(v_phrases),
     main="Histogram of sentences size",
     xlab="Sentence size (no. characters",
     ylab="Ocurrences")

tokens <- spacy_tokenize(paragraphs)
v_tokens <- unlist(tokens)
v_tokens[1:10]
length(v_tokens)
    # [1] 442164 (many repeated)
length(unique(v_tokens))
    # [1] 24130

# As a list
head(sort(table(v_tokens),
          decreasing = TRUE,
          n = 25))

# As a simple plot 
plot(head(sort(table(v_tokens),
               decreasing = TRUE),
               n = 10),
          xlab="Token",
          ylab="Ocurrences")

###########################
# 6. Sentence analysis. POS
###########################

# using spacyr
# POS of the first 100 sentences of D.Quijote

# Spacyr is better for performing POS in spanish and NE recognition

begin <- Sys.time()
names(v_phrases)<-NULL
res <- spacy_parse(v_phrases,
                   dependency = TRUE,
                   nounphrase = TRUE)
Sys.time()-begin
tic <- Sys.time()

res <- lapply(v_phrases[1:100],
              spacy_parse,
              dependency=TRUE,
              nounphrse=TRUE)

df <- res[[1]] # df with the first results
for (i in 2:length(res)){
    df <- rbind(df, res[[1]])
}

# save df as rds
saveRDS(df, file="spacy_parse_Quixote.rds")

library(kableExtra)
kable_styling(kable(df[1:20, c(2:ncol(df))]),
              font_size = 7)

############### 
# 7. Finishing
###############
spacy_finalize() #Donotforgetthis

sessionInfo()
