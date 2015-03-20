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

# blogs.txt<-blogs.txt[!nchar(blogs.txt[1:length(blogs.txt)])<3]
# news.txt<-news.txt[!nchar(news.txt[1:length(news.txt)])<3]
# twitter.txt<-twitter.txt[!nchar(twitter.txt[1:length(twitter.txt)])<3]

raw.txt<-c(blogs.txt,news.txt,twitter.txt)
rm(blogs.txt);rm(news.txt);rm(twitter.txt)

raw.txt<-unlist(strsplit( raw.txt, "\\. |! |\\? "))

non.ascii<-grep("nonascii", iconv(raw.txt, "windows-1252", "ASCII", sub="nonascii"))
length(non.ascii)/length(raw.txt)
raw.txt<-raw.txt[-non.ascii]
rm(non.ascii)

#Remove numbers
length(grep("[0-9]",raw.txt))/length(raw.txt)
raw.txt<-gsub("[0-9]", " ", raw.txt)

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

#Remove extra spaces
raw.txt<-gsub(" {2,}", " ", raw.txt)

raw.txt<-raw.txt[!nchar(raw.txt[1:length(raw.txt)])<5]

bad.words<-"anal|anus|arse|ass|asses|asshole|assholes|bastard|bitch|bitches|bitching|bloody|blowjob|blowjobs|bollock|boner|boob|boobs|bugger|bum|butt|butthole|clit|clitoris|clits|cock|cocks|cocksuck|cocksucker|cocksucking|coon|crap|cum|cumming|cumshot|cunt|cunts|damn|dick|dickhead|dildo|dildos|duche|ejaculate|ejaculated|ejaculating |fuck|fucking|fag|faggitt|faggot|faggs|fagot|fagots|fags|fatass|fucked|fucker|fuckers|fuckin|fucking|gangbang|gangbanged |god-dam|god-damned|goddamn|goddamned|hardcoresex |hell|hoar|homo|horny|hotsex|jackoff|labia|lmfao|masterbate|masterbation|masterbations|masturbate|mofo|mothafucker|mothafuckers|mothafuckin|mothafucking |motherfuck|motherfucked|motherfucker|motherfuckers|motherfucking|motherfuckings|muther|nigga|niggas|nigger|niggers |orgasim |orgasims |orgasm|orgasms |pecker|pissed|pisser|pissers|pissing|pussies|pussy|s.o.b.|sex|shag|shaggin|shagging|shit|shithead|shitted|shitter|shitters |shitting|shitty |slut|sluts|snatch|son-of-a-bitch|tit|tits|titties|turd|twat|wank|wanker|whore"
raw.txt<-gsub(bad.words, "(CENSORED)", raw.txt)

raw.txt[5000000:5000010]

# library(tm)
# raw.src<-VectorSource(raw.txt)
# raw.corpus<-Corpus(raw.src)
# 
# raw.corpus <- tm_map(raw.corpus, removeNumbers)
# raw.corpus <- tm_map(raw.corpus, removePunctuation)
# raw.corpus <- tm_map(raw.corpus, tolower)