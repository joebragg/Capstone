raw.txt<-readLines("raw.txt")

suppressMessages(library(qdap))
suppressMessages(library("RWeka"))
suppressMessages(library("tm"))

set.seed(1234)

random.sample<-sample(raw.txt,length(raw.txt)/100)
random.sample<-replace_contraction(random.sample,sent.cap=FALSE)

ptd<- PlainTextDocument(random.sample)
sample.1Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
sample.2Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
sample.3Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
sample.4Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
sample.5Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

sample.1Gfreq <- termFreq(ptd, control = list(tokenize = sample.1Gtokenizer, wordLengths=c(1,Inf)))
sample.2Gfreq <- termFreq(ptd, control = list(tokenize = sample.2Gtokenizer, wordLengths=c(1,Inf)))
sample.3Gfreq <- termFreq(ptd, control = list(tokenize = sample.3Gtokenizer, wordLengths=c(1,Inf)))
sample.4Gfreq <- termFreq(ptd, control = list(tokenize = sample.4Gtokenizer, wordLengths=c(1,Inf)))
sample.5Gfreq <- termFreq(ptd, control = list(tokenize = sample.5Gtokenizer, wordLengths=c(1,Inf)))


library(data.table)

df.1Gfreq<-data.table(W1=names(sample.1Gfreq),C1=sample.1Gfreq,P1=sample.1Gfreq/sum(sample.1Gfreq))
setorder(df.1Gfreq, -C1)

df.2Gfreq<-data.table(C12=sample.2Gfreq)
df.2Gfreq<-cbind(df.2Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.2Gfreq))," ",fixed=TRUE))))
setnames(df.2Gfreq,c("V1","V2"),c("W1","W2"))
df.2Gfreq<-merge(df.2Gfreq,df.1Gfreq,by="W1")
df.2Gfreq$P2g1<-df.2Gfreq$C12/df.2Gfreq$C1
setcolorder(df.2Gfreq, c("W1","C1","P1","W2","C12","P2g1"))
setorder(df.2Gfreq, -C12,-C1)

PPW2G<-exp(-(1/nrow(df.2Gfreq))*sum(df.2Gfreq$P2g1))


df.3Gfreq<-data.table(C123=sample.3Gfreq)
df.3Gfreq<-cbind(df.3Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.3Gfreq))," ",fixed=TRUE))))
setnames(df.3Gfreq,c("V1","V2","V3"),c("W1","W2","W3"))
df.3Gfreq<-merge(df.3Gfreq,df.2Gfreq,by=c("W1","W2"))
df.3Gfreq$P3g12<-df.3Gfreq$C123/df.3Gfreq$C12
setcolorder(df.3Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12"))
setorder(df.3Gfreq, -C123,-C12,-C1)

PPW3G<-exp(-(1/nrow(df.3Gfreq))*sum(df.3Gfreq$P3g12))

df.4Gfreq<-data.table(C1234=sample.4Gfreq)
df.4Gfreq<-cbind(df.4Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.4Gfreq))," ",fixed=TRUE))))
setnames(df.4Gfreq,c("V1","V2","V3","V4"),c("W1","W2","W3","W4"))
df.4Gfreq<-merge(df.4Gfreq,df.3Gfreq,by=c("W1","W2","W3"))
df.4Gfreq$P4g123<-df.4Gfreq$C1234/df.4Gfreq$C123
setcolorder(df.4Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12","W4","C1234","P4g123"))
setorder(df.4Gfreq,-C1234,-C123,-C12,-C1)

PPW4G<-exp(-(1/nrow(df.4Gfreq))*sum(df.4Gfreq$P4g123))

df.5Gfreq<-data.table(C12345=sample.5Gfreq)
df.5Gfreq<-cbind(df.5Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.5Gfreq))," ",fixed=TRUE))))
setnames(df.5Gfreq,c("V1","V2","V3","V4","V5"),c("W1","W2","W3","W4","W5"))
df.5Gfreq<-merge(df.5Gfreq,df.4Gfreq,by=c("W1","W2","W3","W4"))
df.5Gfreq$P5g1234<-df.5Gfreq$C12345/df.5Gfreq$C1234
setcolorder(df.5Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12","W4","C1234","P4g123","W5","C12345","P5g1234"))
setorder(df.5Gfreq,-C12345,-C1234,-C123,-C12,-C1)

PPW5G<-exp(-(1/nrow(df.5Gfreq))*sum(df.5Gfreq$P5g1234))

df.4Gfreq<-data.table(C5432=sample.4Gfreq)
df.4Gfreq<-cbind(df.4Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.4Gfreq))," ",fixed=TRUE))))
setnames(df.4Gfreq,c("V1","V2","V3","V4"),c("W2","W3","W4","W5"))
df.4Gfreq<-merge(df.5Gfreq,df.4Gfreq,by=c("W2","W3","W4","W5"))
df.4Gfreq$P5g432<-df.4Gfreq$C12345/df.4Gfreq$C5432
setcolorder(df.4Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12","W4","C1234","P4g123","W5","C12345","P5g1234",
                         "C5432","P5g432"))

df.3Gfreq<-data.table(C543=sample.3Gfreq)
df.3Gfreq<-cbind(df.3Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.3Gfreq))," ",fixed=TRUE))))
setnames(df.3Gfreq,c("V1","V2","V3"),c("W3","W4","W5"))
df.3Gfreq<-merge(df.4Gfreq,df.3Gfreq,by=c("W3","W4","W5"))
df.3Gfreq$P5g43<-df.4Gfreq$C5432/df.3Gfreq$C543
setcolorder(df.3Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12","W4","C1234","P4g123","W5","C12345","P5g1234",
                         "C5432","P5g432","C543","P5g43"))

df.2Gfreq<-data.table(C54=sample.2Gfreq)
df.2Gfreq<-cbind(df.2Gfreq,data.table(do.call('rbind', strsplit(as.character(names(sample.2Gfreq))," ",fixed=TRUE))))
setnames(df.2Gfreq,c("V1","V2"),c("W4","W5"))
df.2Gfreq<-merge(df.3Gfreq,df.2Gfreq,by=c("W4","W5"))
df.2Gfreq$P5g4<-df.3Gfreq$C543/df.2Gfreq$C54
setcolorder(df.2Gfreq, c("W1","C1","P1","W2","C12","P2g1","W3","C123","P3g12","W4","C1234","P4g123","W5","C12345","P5g1234",
                         "C5432","P5g432","C543","P5g43","C54","P5g4"))

df.1Gfreq<-data.table(W5=names(sample.1Gfreq),C5=sample.1Gfreq,P5=sample.1Gfreq/sum(sample.1Gfreq))
df.1Gfreq<-merge(df.2Gfreq,df.1Gfreq,by="W5")
df.1Gfreq$P5<-df.2Gfreq$C54/df.1Gfreq$C5
setcolorder(df.1Gfreq, c("W1","W2","W3","W4","W5",
                         "C1","C12","C123","C1234","C12345","C5432","C543","C54","C5",
                         "P1","P2g1","P3g12","P4g123","P5g1234","P5g432","P5g43","P5g4","P5"))

NGdata<-df.1Gfreq
write.table(NGdata, "Shiny/data/NGdata3.csv", sep=",")

