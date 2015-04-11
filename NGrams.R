raw.txt<-readLines("raw.txt")

suppressMessages(library(qdap))
suppressMessages(library("RWeka"))
suppressMessages(library("tm"))

set.seed(1234)

random.sample<-sample(raw.txt,length(raw.txt)/100)
random.sample<-replace_contraction(random.sample)

ptd<- PlainTextDocument(random.sample)
sample.1Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
sample.2Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
sample.3Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
sample.4Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
sample.5Gtokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

sample.1Gfreq <- sort(termFreq(ptd,control = list(tokenize = sample.1Gtokenizer)),
                      decreasing=TRUE)
sample.2Gfreq <- sort(termFreq(ptd, control = list(tokenize = sample.2Gtokenizer)),
                      decreasing=TRUE)
sample.3Gfreq <- sort(termFreq(ptd, control = list(tokenize = sample.3Gtokenizer)),
                      decreasing=TRUE)
sample.4Gfreq <- sort(termFreq(ptd, control = list(tokenize = sample.4Gtokenizer)),
                      decreasing=TRUE)
sample.5Gfreq <- sort(termFreq(ptd, control = list(tokenize = sample.5Gtokenizer)),
                      decreasing=TRUE)

df.1Gfreq<-data.frame(Phrase=names(sample.1Gfreq),Freq=sample.1Gfreq,LogProb=log(sample.1Gfreq/length(sample.1Gfreq)))
df.1Gfreq$Phrase <- factor(df.1Gfreq$Phrase, levels = df.1Gfreq$Phrase[order(df.1Gfreq$Freq)])

df.2Gfreq<-data.frame(Phrase=names(sample.2Gfreq),Freq=sample.2Gfreq,LogProb=log(sample.2Gfreq/length(sample.2Gfreq)))
df.2Gfreq$Phrase <- factor(df.2Gfreq$Phrase, levels = df.2Gfreq$Phrase[order(df.2Gfreq$Freq)])

df.3Gfreq<-data.frame(Phrase=names(sample.3Gfreq),Freq=sample.3Gfreq,LogProb=log(sample.3Gfreq/length(sample.3Gfreq)))
df.3Gfreq$Phrase <- factor(df.3Gfreq$Phrase, levels = df.3Gfreq$Phrase[order(df.3Gfreq$Freq)])

df.4Gfreq<-data.frame(Phrase=names(sample.4Gfreq),Freq=sample.4Gfreq,LogProb=log(sample.4Gfreq/length(sample.4Gfreq)))
df.4Gfreq$Phrase <- factor(df.4Gfreq$Phrase, levels = df.4Gfreq$Phrase[order(df.4Gfreq$Freq)])

df.5Gfreq<-data.frame(Phrase=names(sample.5Gfreq),Freq=sample.5Gfreq,LogProb=log(sample.5Gfreq/length(sample.5Gfreq)))
df.5Gfreq$Phrase <- factor(df.5Gfreq$Phrase, levels = df.5Gfreq$Phrase[order(df.5Gfreq$Freq)])


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