Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

library("RWeka")
library("tm")

sample1.src<-VectorSource(random.sample1)
sample1.corpus<-Corpus(sample1.src)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm1 <- TermDocumentMatrix(sample1.corpus, control = list(tokenize = BigramTokenizer))
findFreqTerms(tdm1, 800)

inspect(tdm1[1:50,1:10])