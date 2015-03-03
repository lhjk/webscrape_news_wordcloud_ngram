## Modified from code taken from below site
## http://hack-r.com/n-gram-wordclouds-in-r/
## Modified by Rimno 2015.02.27

require(RWeka)
require(tau)
require(tm)
require(tm.plugin.webmining)
require(wordcloud)
require(RMeCab)


# Setup search word

search_word <- "PwC"

# Words to exclude from word cloud

wordlist <- c("PwC", "PricewaterhouseCoopers")


# Scrape News ------------------------------------------------------

#nytimes_appid <- "94eb7d0ca1fec4ed43ce05a7f46331c0:16:71423764"

#nytimes <- WebCorpus(NYTimesSource(query = search_word, appid = nytimes_appid))
yahoonews <- WebCorpus(YahooNewsSource(search_word))
googlenews <- WebCorpus(GoogleNewsSource(search_word))
#yahooinplay <- WebCorpus(YahooInplaySource(search_word))
#googlefinance <- WebCorpus(GoogleFinanceSource("NASDAQ:LFVN"))
#reutersnews <- WebCorpus(ReutersNewsSource("PwC"))
#yahoofinance <- WebCorpus(YahooFinanceSource("LFVN"))



# Text Mining the Results -------------------------------------------------
corpus <- c(googlenews, yahoonews)


inspect(corpus)



ds0.1g <- tm_map(corpus, content_transformer(tolower))
ds1.1g <- tm_map(ds0.1g, content_transformer(removeWords), wordlist)
ds1.1g <- tm_map(ds1.1g, content_transformer(removeWords), stopwords("english"))
ds2.1g <- tm_map(ds1.1g, stripWhitespace)
ds3.1g <- tm_map(ds2.1g, removePunctuation)
ds4.1g <- tm_map(ds3.1g, stemDocument)


tdm.1g <- TermDocumentMatrix(ds4.1g)
dtm.1g <- DocumentTermMatrix(ds4.1g)




#findFreqTerms(tdm.1g, 40)
#findFreqTerms(tdm.1g, 60)
#findFreqTerms(tdm.1g, 80)
#findFreqTerms(tdm.1g, 100)


#findAssocs(dtm.1g, "advisory", .75)
#findAssocs(dtm.1g, "audit", .5)
#findAssocs(dtm.1g, "consult", .75) 


tdm89.1g <- removeSparseTerms(tdm.1g, 0.89)
tdm9.1g  <- removeSparseTerms(tdm.1g, 0.9)
tdm91.1g <- removeSparseTerms(tdm.1g, 0.91)
tdm92.1g <- removeSparseTerms(tdm.1g, 0.92)


tdm2.1g <- tdm92.1g


# Creates a Boolean matrix (counts # docs w/terms, not raw # terms)
tdm3.1g <- inspect(tdm2.1g)
tdm3.1g[tdm3.1g>=1] <- 1 


# Transform into a term-term adjacency matrix
termMatrix.1gram <- tdm3.1g %*% t(tdm3.1g)


# inspect terms numbered 5 to 10
termMatrix.1gram[5:10,5:10]
termMatrix.1gram[1:10,1:10]


# Create a WordCloud to Visualize the Text Data ---------------------------
notsparse <- tdm3.1g
m = as.matrix(notsparse)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)


# Create the word cloud
pal = brewer.pal(9,"BuPu")
wordcloud(words = d$word,
          freq = d$freq,
          scale = c(3,.8),
          random.order = F,
          colors = pal)




####################################################

# Fix for mac OSX
options(mc.cores=1)

# Tokenizer_ngrams ####

tokenize_ngrams <- function(x, n=3) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

# BigramTokenizer ####
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))


# Create an n-gram Word Cloud ----------------------------------------------
tdm.ng <- TermDocumentMatrix(ds4.1g, control = list(tokenize = BigramTokenizer))
dtm.ng <- DocumentTermMatrix(ds4.1g, control = list(tokenize = BigramTokenizer))


# Try removing sparse terms at a few different levels
tdm89.ng <- removeSparseTerms(tdm.ng, 0.89)
tdm9.ng  <- removeSparseTerms(tdm.ng, 0.9)
tdm91.ng <- removeSparseTerms(tdm.ng, 0.91)
tdm92.ng <- removeSparseTerms(tdm.ng, 0.92)


notsparse <- tdm91.ng
m = as.matrix(notsparse)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)


# Create the word cloud
pal = brewer.pal(9,"BuPu")
wordcloud(words = d$word,
          freq = d$freq,
          scale = c(3,.8),
          random.order = F,
          colors = pal)
