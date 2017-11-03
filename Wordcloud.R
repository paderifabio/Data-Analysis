text <- read.delim(file.choose(), header=F, sep = "\n",stringsAsFactors = F)
colnames(text) <- c("MyCol")
docs <- text$MyCol
a <- VCorpus(VectorSource(docs))
jeopCorpus <- tm_map(a, PlainTextDocument)
jeopCorpus <- tm_map(a, removePunctuation)
jeopCorpus <- tm_map(a, removeWords, stopwords('italian'))
jeopCorpus <- tm_map(a, stemDocument)
jeopCorpus <- tm_map(a, removeWords, c('sono','che', 'non','questionario'))
wordcloud(a,scale=c(5,0.5), max.words = 100, 
          random.order = FALSE, rot.per=0.35, 
          use.r.layout=FALSE,
          colors=brewer.pal(8,"Dark2"))

