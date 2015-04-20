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

df.1Gfreq<-data.table(W1=names(sample.1Gfreq),C1=sample.1Gfreq,P1=log(sample.1Gfreq/sum(sample.1Gfreq)))
setorder(df.1Gfreq, -C1)

df.2Gfreq<-data.table(Phrase12=names(sample.2Gfreq),C12=sample.2Gfreq)
df.2Gfreq<-cbind(df.2Gfreq,data.table(do.call('rbind', strsplit(as.character(df.2Gfreq$Phrase)," ",fixed=TRUE))))
setnames(df.2Gfreq,c("V1","V2"),c("W1","W2"))
df.2Gfreq<-merge(df.2Gfreq,df.1Gfreq,by="W1")
df.2Gfreq$P2g1<-log(df.2Gfreq$C12/df.2Gfreq$C1)
setcolorder(df.2Gfreq, c("W1","C1","P1","Phrase12","C12","W2","P2g1"))
setorder(df.2Gfreq, -C12,-C1)

PPW2G<-exp(-(1/nrow(df.2Gfreq))*sum(df.2Gfreq$P2g1))


df.3Gfreq<-data.table(Phrase123=names(sample.3Gfreq),C123=sample.3Gfreq)
df.3Gfreq<-cbind(df.3Gfreq,data.table(do.call('rbind', strsplit(as.character(df.3Gfreq$Phrase)," ",fixed=TRUE))))
setnames(df.3Gfreq,c("V1","V2","V3"),c("W1","W2","W3"))
df.3Gfreq<-merge(df.3Gfreq,df.2Gfreq,by=c("W1","W2"))
df.3Gfreq$P3g12<-log(df.3Gfreq$C123/df.3Gfreq$C12)
setcolorder(df.3Gfreq, c("W1","C1","P1","Phrase12","C12","Phrase123","C123","W2","W3","P2g1","P3g12"))
setorder(df.3Gfreq, -C123,-C12,-C1)

PPW3G<-exp(-(1/nrow(df.3Gfreq))*sum(df.3Gfreq$P3g12))

df.4Gfreq<-data.table(Phrase1234=names(sample.4Gfreq),C1234=sample.4Gfreq)
df.4Gfreq<-cbind(df.4Gfreq,data.table(do.call('rbind', strsplit(as.character(df.4Gfreq$Phrase)," ",fixed=TRUE))))
setnames(df.4Gfreq,c("V1","V2","V3","V4"),c("W1","W2","W3","W4"))
df.4Gfreq<-merge(df.4Gfreq,df.3Gfreq,by=c("W1","W2","W3"))
df.4Gfreq$P4g123<-log(df.4Gfreq$C1234/df.4Gfreq$C123)
setcolorder(df.4Gfreq, c("W1","C1","P1","Phrase12","C12","Phrase123","C123","Phrase1234","C1234","W2","W3","W4","P2g1","P3g12","P4g123"))
setorder(df.4Gfreq,-C1234,-C123,-C12,-C1)

PPW4G<-exp(-(1/nrow(df.4Gfreq))*sum(df.4Gfreq$P4g123))

df.5Gfreq<-data.table(Phrase12345=names(sample.5Gfreq),C12345=sample.5Gfreq)
df.5Gfreq<-cbind(df.5Gfreq,data.table(do.call('rbind', strsplit(as.character(df.5Gfreq$Phrase)," ",fixed=TRUE))))
setnames(df.5Gfreq,c("V1","V2","V3","V4","V5"),c("W1","W2","W3","W4","W5"))
df.5Gfreq<-merge(df.5Gfreq,df.4Gfreq,by=c("W1","W2","W3","W4"))
df.5Gfreq$P5g1234<-log(df.5Gfreq$C12345/df.5Gfreq$C1234)
setcolorder(df.5Gfreq, c("W1","C1","P1","Phrase12","C12","Phrase123","C123","Phrase1234","C1234","Phrase12345","C12345","W2","W3","W4","W5","P2g1","P3g12","P4g123","P5g1234"))
setorder(df.5Gfreq,-C12345,-C1234,-C123,-C12,-C1)

PPW5G<-exp(-(1/nrow(df.5Gfreq))*sum(df.5Gfreq$P5g1234))

NGdata<-with(df.5Gfreq, data.table(W1=W1,P1=C1,W2=W2,P2=C12,W3=W3,P3=C123,W4=W4,P4=C1234,W5=W5,P5=C12345))

NGdata<-setorder(NGdata,-P5,-P4,-P3,-P2,-P1)

write.table(NGdata, "Shiny/data/NGdata.csv", sep=",")

g<-ggplot(df.1Gfreq[3:103,],aes(x=Phrase,y=Freq))
g<-g+geom_bar(stat="identity")+coord_flip()
g

g<-ggplot(df.2Gfreq[1:100,],aes(x=Phrase,y=Freq))
g<-g+geom_bar(stat="identity")+coord_flip()
g

g<-ggplot(df.3Gfreq[1:100,],aes(x=Phrase,y=Freq))
g<-g+geom_bar(stat="identity")+coord_flip()
g

g<-ggplot(df.4Gfreq[1:100,],aes(x=Phrase,y=Freq))
g<-g+geom_bar(stat="identity")+coord_flip()
g

g<-ggplot(df.5Gfreq[1:25,],aes(x=Phrase,y=Freq))
g<-g+geom_bar(stat="identity")+coord_flip()
g