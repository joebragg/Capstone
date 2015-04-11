First.Word.Freq<-df.2Gfreq[grep("<s>",df.2Gfreq$Phrase),]

phrase<-"thanks for the follow"

phrase<-paste("<s>",phrase)
splphrase<-unlist(strsplit(phrase," "))

C1<-df.1Gfreq[df.1Gfreq$Phrase==splphrase[1],]$Freq
C12<-df.2Gfreq[df.2Gfreq$Phrase==paste(splphrase[1],splphrase[2],sep=" "),]$Freq
C123<-df.3Gfreq[df.3Gfreq$Phrase==paste(splphrase[1],splphrase[2],splphrase[3],sep=" "),]$Freq
C1234<-df.4Gfreq[df.4Gfreq$Phrase==paste(splphrase[1],splphrase[2],splphrase[3],splphrase[4],sep=" "),]$Freq
C12345<-df.5Gfreq[df.5Gfreq$Phrase==paste(splphrase[1],splphrase[2],splphrase[3],splphrase[4],splphrase[5],sep=" "),]$Freq


LP2g1<-log(C12 / C1)
LP3g12<-log(C123 / C12)
LP4g123<-log(C1234 / C123)
LP5g1234<-log(C12345 / C1234)

LP2g1+LP3g12+LP4g123+LP5g1234

df.5Gfreq[grep(paste("^",splphrase[1]," ",splphrase[2]," ",splphrase[3]," ",splphrase[4],sep=""),df.5Gfreq$Phrase),]

