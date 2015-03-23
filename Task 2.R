if(!file.exists("data/Coursera-SwiftKey.zip")) {
  download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                destfile = "data/Coursera-SwiftKey.zip",mode="wb")
  unzip("data/Coursera-SwiftKey.zip",exdir="data")
}
blogs.con<-file("data/final/en_US/en_US.blogs.txt",open="r", encoding="windows-1252")
news.con<-file("data/final/en_US/en_US.news.txt",open="r", encoding="windows-1252")
twitter.con<-file("data/final/en_US/en_US.twitter.txt",open="r", encoding="windows-1252")

blogs.txt<-readLines(blogs.con, skipNul = TRUE, warn=FALSE)
news.txt<-readLines(news.con, skipNul = TRUE, warn=FALSE)
twitter.txt<-readLines(twitter.con, skipNul = TRUE, warn=FALSE)

close(blogs.con)
close(news.con)
close(twitter.con)

rm(blogs.con);rm(news.con);rm(twitter.con)

raw.txt<-c(blogs.txt,news.txt,twitter.txt)
rm(blogs.txt);rm(news.txt);rm(twitter.txt)

#Split out individual sentences
raw.txt<-unlist(strsplit( raw.txt, "\\. |! |\\? "))

#Remove sentences with non-english words/non-ansii characters
sentences.non.ascii<-grep("nonascii", iconv(raw.txt, "windows-1252", "ASCII", sub="nonascii"))
length(sentences.non.ascii)/length(raw.txt)
raw.txt<-raw.txt[-sentences.non.ascii]
rm(sentences.non.ascii)

#Substitute numbers with hash symbol
length(grep("[0-9]",raw.txt))/length(raw.txt)
sentences.numbers<-grep("[0-9]",raw.txt)
raw.txt<-raw.txt[-sentences.numbers]
rm(sentences.numbers)

#convert to all lower case letters
raw.txt<-tolower(raw.txt)

# Remove punctuation except apostrophes in contractions & dashes in hyphenated words
raw.txt<-gsub("[^[:alnum:][:space:]'-]", " ", raw.txt)
raw.txt<-gsub(" '+", " ", raw.txt)
raw.txt<-gsub("'+ ", " ", raw.txt)
raw.txt<-gsub(" -+", " ", raw.txt)
raw.txt<-gsub("-+ ", " ", raw.txt)
raw.txt<-gsub("^[-' ]", "", raw.txt)
raw.txt<-gsub("[-' ]$", "", raw.txt)
raw.txt<-gsub("-", " ", raw.txt)

#Substitute (CENSORED) for profane words
bad.words<-" anal | anus | arse | ass | asses | asshole | assholes | bastard | bitch | bitches | bitching | bloody | blowjob | blowjobs | bollock | boner | boob | boobs | bugger | bum | butt | butthole | clit | clitoris | clits | cock | cocks | cocksuck | cocksucker | cocksucking | coon | crap | cum | cumming | cumshot | cunt | cunts | damn | dick | dickhead | dildo | dildos | duche | ejaculate | ejaculated | ejaculating  | fuck | fucking | fag | faggitt | faggot | faggs | fagot | fagots | fags | fatass | fucked | fucker | fuckers | fuckin | fucking | gangbang | gangbanged  | god-dam | god-damned | goddamn | goddamned | hardcoresex  | hell | hoar | homo | horny | hotsex | jackoff | labia | lmfao | masterbate | masterbation | masterbations | masturbate | mofo | mothafucker | mothafuckers | mothafuckin | mothafucking  | motherfuck | motherfucked | motherfucker | motherfuckers | motherfucking | motherfuckings | muther | nigga | niggas | nigger | niggers  | orgasim  | orgasims  | orgasm | orgasms  | pecker | pissed | pisser | pissers | pissing | pussies | pussy | s.o.b. | sex | shag | shaggin | shagging | shit | shithead | shitted | shitter | shitters  | shitting | shitty  | slut | sluts | snatch | son-of-a-bitch | tit | tits | titties | turd | twat | wank | wanker | whore "
raw.txt<-gsub(bad.words, "(CENSORED)", raw.txt)
rm(bad.words)

#Remove extra spaces
raw.txt<-gsub(" {2,}", " ", raw.txt)

#Remove short sentences
word.counts<-sapply(gregexpr(" ", raw.txt), length) + 1
raw.txt<-raw.txt[!word.counts<3]

writeLines(raw.txt,"raw.txt")

mean(word.counts)
median(word.counts)
sd(word.counts)
max(word.counts)
sum(word.counts)
hist(word.counts,col="blue",breaks=100)
rm(word.counts)

raw.txt<-readLines("raw.txt")

library(qdap)
set.seed(1234)
sample<-vector('character')
ng.1 <- data.frame(W1=character())
ng.2 <- data.frame(W1=character(),W2=character())
ng.3 <- data.frame(W1=character(),W2=character(),W3=character())
ng.4 <- data.frame(W1=character(),W2=character(),W3=character(),W4=character())
ng.5 <- data.frame(W1=character(),W2=character(),W3=character(),W4=character(),W5=character())

for(i in 1:10){
binom.var<-as.logical(rbinom(length(raw.txt),1,0.001))
sample<-raw.txt[binom.var]
sample<-replace_contraction(sample)
ng<-ngrams(sample, n = 5)

tng.1 <- data.frame(matrix(unlist(ng$all_n$n_1), nrow=length(ng$all_n$n_1), byrow=TRUE))
names(tng.1)<-c("W1")
ng.1<-rbind(ng.1,tng.1)
rm(tng.1)
tng.2 <- data.frame(matrix(unlist(ng$all_n$n_2), nrow=length(ng$all_n$n_2), byrow=TRUE))
names(tng.2)<-c("W1","W2")
ng.2<-rbind(ng.2,tng.2)
rm(tng.2)
tng.3 <- data.frame(matrix(unlist(ng$all_n$n_3), nrow=length(ng$all_n$n_3), byrow=TRUE))
names(tng.3)<-c("W1","W2","W3")
ng.3<-rbind(ng.3,tng.3)
rm(tng.3)
tng.4 <- data.frame(matrix(unlist(ng$all_n$n_4), nrow=length(ng$all_n$n_4), byrow=TRUE))
names(tng.4)<-c("W1","W2","W3","W4")
ng.4<-rbind(ng.4,tng.4)
rm(tng.4)
tng.5 <- data.frame(matrix(unlist(ng$all_n$n_5), nrow=length(ng$all_n$n_5), byrow=TRUE))
names(tng.5)<-c("W1","W2","W3","W4","W5")
ng.5<-rbind(ng.5,tng.5)
rm(tng.5)
}

rm(i);rm(ng);rm(sample);rm(binom.var)


plot(freq_terms(sample, top = 20))
