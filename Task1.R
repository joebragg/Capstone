if(!file.exists("data/Coursera-SwiftKey.zip")) {
   download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                 destfile = "data/Coursera-SwiftKey.zip",mode="wb")
   unzip("data/Coursera-SwiftKey.zip",exdir="data")
}
blogs.con<-file("data/final/en_US/en_US.blogs.txt",open="r", encoding="windows-1252")

blogs.txt<-readLines(blogs.con, skipNul = TRUE, warn=FALSE)

close(blogs.con)

#Split out individual sentences
blogs.txt<-unlist(strsplit( blogs.txt, "\\. |! |\\? "))

#Remove sentences with non-english words/non-ansii characters
non.ascii<-grep("nonascii", iconv(blogs.txt, "windows-1252", "ASCII", sub="nonascii"))
length(non.ascii)/length(blogs.txt)
blogs.txt<-blogs.txt[-non.ascii]
rm(non.ascii)

#convert to all lower case letters
blogs.txt<-tolower(blogs.txt)

# Remove punctuation except apostrophes in contractions & dashes in hyphenated words
blogs.txt<-gsub("[^[:alnum:][:space:]'-]", " ", blogs.txt)
blogs.txt<-gsub(" '+", " ", blogs.txt)
blogs.txt<-gsub("'+ ", " ", blogs.txt)
blogs.txt<-gsub(" -+", " ", blogs.txt)
blogs.txt<-gsub("-+ ", " ", blogs.txt)
blogs.txt<-gsub("^[-' ]", "", blogs.txt)
blogs.txt<-gsub("[-' ]$", "", blogs.txt)
blogs.txt<-gsub("-", " ", blogs.txt)

#Substitute numbers with hash symbol
blogs.txt<-gsub("[0-9]", "#", blogs.txt)

#Remove extra spaces
blogs.txt<-gsub(" {2,}", " ", blogs.txt)

#Remove short strings (non-value)
blogs.txt<-blogs.txt[!nchar(blogs.txt[1:length(blogs.txt)])<5]

#Substitute (CENSORED) for profane words
bad.words<-" anal | anus | arse | ass | asses | asshole | assholes | bastard | bitch | bitches | bitching | bloody | blowjob | blowjobs | bollock | boner | boob | boobs | bugger | bum | butt | butthole | clit | clitoris | clits | cock | cocks | cocksuck | cocksucker | cocksucking | coon | crap | cum | cumming | cumshot | cunt | cunts | damn | dick | dickhead | dildo | dildos | duche | ejaculate | ejaculated | ejaculating  | fuck | fucking | fag | faggitt | faggot | faggs | fagot | fagots | fags | fatass | fucked | fucker | fuckers | fuckin | fucking | gangbang | gangbanged  | god-dam | god-damned | goddamn | goddamned | hardcoresex  | hell | hoar | homo | horny | hotsex | jackoff | labia | lmfao | masterbate | masterbation | masterbations | masturbate | mofo | mothafucker | mothafuckers | mothafuckin | mothafucking  | motherfuck | motherfucked | motherfucker | motherfuckers | motherfucking | motherfuckings | muther | nigga | niggas | nigger | niggers  | orgasim  | orgasims  | orgasm | orgasms  | pecker | pissed | pisser | pissers | pissing | pussies | pussy | s.o.b. | sex | shag | shaggin | shagging | shit | shithead | shitted | shitter | shitters  | shitting | shitty  | slut | sluts | snatch | son-of-a-bitch | tit | tits | titties | turd | twat | wank | wanker | whore "
blogs.txt<-gsub(bad.words, " (CENSORED) ", blogs.txt)
rm(bad.words)

tokenized.txt<-strsplit(blogs.txt," ")

set.seed(1234)
binom.var<-as.logical(rbinom(length(blogs.txt),1,.1))
random.sample<-blogs.txt[binom.var]
rm(binom.var)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

library("RWeka")
library("tm")

sample.src<-VectorSource(random.sample)
sample.corpus<-Corpus(sample.src)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(sample.corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm[842900:842964,1:10])

