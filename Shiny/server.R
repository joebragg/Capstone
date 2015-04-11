library(data.table)
library(wordcloud)
NGdata <- read.table("data/table.txt", header=TRUE, sep=",")

shinyServer(function(input, output,session) {
  phrase<-reactive({
    phrase<-tolower(input$phrase)
    phrase<-gsub("[^[:alnum:][:space:]'-]", " ", phrase)
    phrase<-gsub(" {2,}", " ", phrase)
    phrase<-unlist(strsplit(phrase,split=" "))
    phrase
  })
  
  wc<-reactive({
    length(unlist(strsplit(as.character(phrase())," ",fixed=TRUE)))
  })
  
  pred<-reactive({
    if (wc()==0){
      tempt<-data.table(unique(NGdata[,c("W1","C1")]))
      pred<-setorder(tempt, -C1)
      pred
    }
    if (wc()==1){
      tempt<-data.table(unique(NGdata[NGdata$W1==phrase()[1],c("W2","C2")]))
      if(nrow(tempt)==0) {
        tempt<-data.table(W2="UNK",C2=0)
      }
      pred<-setorder(tempt, -C2)
      pred
    }
    if (wc()==2){
      tempt<-data.table(unique(NGdata[(NGdata$W1==phrase()[1] & NGdata$W2==phrase()[2]),c("W3","C3")]))
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[NGdata$W2==phrase()[2],c("W3","C3")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(W3="UNK",C3=0)
      }
      pred<-setorder(tempt, -C3)
      pred
    }
    if (wc()==3){
      tempt<-data.table(unique(NGdata[(NGdata$W1==phrase()[1] & NGdata$W2==phrase()[2] & NGdata$W3==phrase()[3]),c("W4","C4")]))
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W2==phrase()[2] & NGdata$W3==phrase()[3]),c("W4","C4")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[NGdata$W3==phrase()[3],c("W4","C4")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(W4="UNK",C4=0)
      }
      pred<-setorder(tempt, -C4)
      pred
    }
    if (wc()==4){
      tempt<-data.table(unique(NGdata[(NGdata$W1==phrase()[1] & NGdata$W2==phrase()[2] & NGdata$W3==phrase()[3] & NGdata$W4==phrase()[4]),c("W5","C5")]))
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W2==phrase()[2] & NGdata$W3==phrase()[3] & NGdata$W4==phrase()[4]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W3==phrase()[3] & NGdata$W4==phrase()[4]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W4==phrase()[4]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(W5="UNK",C5=0)
      }
      pred<-setorder(tempt, -C5)
      pred
    }
    if (wc()>4){
      phl<-length(phrase())
      tempt<-data.table(unique(NGdata[(NGdata$W1==phrase()[phl-3] & NGdata$W2==phrase()[phl-2] & NGdata$W3==phrase()[phl-1] & NGdata$W4==phrase()[phl]),c("W5","C5")]))
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W2==phrase()[phl-2] & NGdata$W3==phrase()[phl-1] & NGdata$W4==phrase()[phl]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W3==phrase()[phl-1] & NGdata$W4==phrase()[phl]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(unique(NGdata[(NGdata$W4==phrase()[phl]),c("W5","C5")]))
      }
      if(nrow(tempt)==0) {
        tempt<-data.table(W5="UNK",C5=0)
      }
      pred<-setorder(tempt, -C5)
      pred
    }
    pred
  })
  wordcloud_rep<-repeatable(wordcloud)
  
  output$text <- renderText(input$phrase)
  output$wordcnt <- renderText(wc())
  output$pred<-renderText(as.matrix(pred())[1,1])
  output$cloud<-renderPlot({
    wordcloud_rep(as.matrix(pred())[,1],as.integer(as.matrix(pred())[,2]),
                  max.words=7,colors=brewer.pal(8, "Dark2"),scale=c(6,.2))
  })
 })