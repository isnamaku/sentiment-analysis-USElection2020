library(twitteR)
library(rtweet)
library(tm)
library(wordcloud)
library(e1071)
library(gmodels)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(gmodels) 
library(Rstem)
library(sentiment)
library(caret) 

reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "ohUn4VZDPTgFU7TffZcpu9Xv5" 
CUSTOMER_SECRET <- "PXVIp5LPbdUznJVQdGtsf2VVzjKbwTs0ZpMuzR29NEi3VZtxKM" 
ACCESS_TOKEN <- "511827320-FgsZ224KeFya5aVOt4VePmFAhX6Po806BmdQkHTD" 
ACCESS_secret <- "El5FMDwrfpiduLwbylPsWcQtdnpfVOnpw6WznWG4Sv3DQ" 
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

#mengambil tweet
tweets <- searchTwitter('#uselection2020', n=10000, lang="en")
tweets.data <- twListToDF(tweets)

text <- sapply(tweets, function(x) x$getText()) 
write.csv(text, file = 'usElection2020.csv')

data.frame <- read.csv('usElection2020.csv')

#DATA PREPARATION
#raw data
corpus <- Corpus(VectorSource(data.frame$x))
raw.corpus.dtm <- DocumentTermMatrix(corpus)
inspect(corpus[1:5])

#Preprocessing
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

clean.corpus <- tm_map(corpus, tolower)
clean.corpus <- tm_map(clean.corpus, removeNumbers)
inspect(clean.corpus[1:5])


clean.corpus <- tm_map(clean.corpus, removeWords, stopwords())
clean.corpus <- tm_map(clean.corpus, removePunctuation)
inspect(clean.corpus[1:5])

clean.corpus <- tm_map(clean.corpus, stripWhitespace)
inspect(clean.corpus[1:5])

clean.corpus.dtm <- DocumentTermMatrix(clean.corpus)

dataframe=data.frame(text=unlist(sapply(clean.corpus, `[`)), stringsAsFactors=F)

write.csv(dataframe,file = 'cleandata.csv')
dataframe.clean <- read.csv('cleandata.csv')


#SENTIMEN
# Klasifikasi Emosi
class_emo = classify_emotion(dataframe.clean, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"


#Klasifikasi Polarisasi
class_pol = classify_polarity(dataframe.clean, algorithm="bayes")
polarity = class_pol[,4]


hasil = data.frame(text=dataframe.clean, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
table(hasil$emotion)

review_combine<-cbind(hasil)
par(mar=rep(3,4))
table(hasil$polarity)

#data frame snetimen
sent_df = data.frame(text=review_combine$text.text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


write.csv(hasil, file = "sentiment-data.csv")

head(sent_df,20)
table(sent_df$emotion)


df<-read.csv("cleandata.csv",stringsAsFactors = FALSE) #membaca file CSV
glimpse(df)

#Atur seed generator bilangan acak R, yang berguna untuk membuat simulasi atau objek acak yang dapat direproduksi.
set.seed(20) 
df<-df[sample(nrow(df)),] 
df<-df[sample(nrow(df)),]
glimpse(df) #melihat tipe data
df$X=as.factor(df$X) #mengubah menjadi faktor
#menampilkan semua tweet yang kita mining
corpus<-Corpus(VectorSource(df$text)) 
corpus
#melihat data yang telah i corpus
inspect(corpus[1:10])
#prepocessing data
corpus.clean<-corpus%>%
  tm_map(content_transformer(tolower))%>% 
  tm_map(removeNumbers)%>% 
  tm_map(removeWords,stopwords(kind="en"))%>% 
  tm_map(stripWhitespace) 
dtm<-DocumentTermMatrix(corpus.clean)
n<-nrow(df)


#NAIVE BAYES

#split data
df.train<-df[1:round(.8 * n),]
df.test<-df[(round(.8 * n)+1):n,]
dtm.train<-dtm[1:round(.8 * n),]
dtm.test<-dtm[(round(.8 * n)+1):n,]
glimpse(df.test)
glimpse(df.train)
corpus.clean.train <- corpus.clean[1:76]
corpus.clean.test<-corpus.clean[77:306]
glimpse(corpus.clean.train)

dim(dtm.train)
fivefreq<-findFreqTerms(dtm.train,5) #frekuensi kemunculan kata tersebut pada dokumen
length(fivefreq)


dtm.train.nb<-DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))
dtm.test.nb<-DocumentTermMatrix(corpus.clean.test,control = list(dictionary=fivefreq))
dim(dtm.test.nb)

convert_count <- function(x){
  y<-ifelse(x>0,1,0)
  y<-factor(y,levels=c(0,1),labels=c("no","yes"))
  y
}
#data train dan test naive bayes
trainNB<-apply(dtm.train.nb,2,convert_count)
testNB<-apply(dtm.test.nb,1,convert_count)



#SHINY

library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Wordcloud", icon = icon("th"), tabName = "wordcloud"),
      menuItem("Emotion", icon = icon("th"), tabName = "emotion"),
      menuItem("Polarity", icon = icon("th"), tabName = "polarity"),
      menuItem("Sentiment Table", icon = icon("th"), tabName = "sentiment")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(
                  title = "Wordcloud", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plot1", height = 400, width = 400)
                ),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 300, 50)
                ),
                
              ),
              
              
      ),
      tabItem(tabName = "emotion",
              box(
                title = "Emotion", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plotData1", height = 500, width = 400)
                
              )
      ),
      tabItem(tabName = "polarity",
              box(
                title = "Polarity", status="primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plotData2", height = 500, width = 400)
              )
      ),
      tabItem(tabName = "sentiment",
              box(
                title = "Sentiment Table", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                tableOutput("table", height = 600, width = 600)
                
              )
      )
      # Boxes need to be put in a row (or column)
      
    )
  )
  
)



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    data <- wordcloud(clean.corpus, min.freq = 5, max.words = input$slider, random.order = FALSE,text=14, colors=brewer.pal(8, "Dark2"))
    
  })
  output$plotData1 <- renderPlot({
    #PLOT EMOTION
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis",
           plot.title = element_text(size=12))

  })
  output$plotData2 <- renderPlot({
    #PLOT POLARITY
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis",
           plot.title = element_text(size=12))

    
    
  })
  output$tbl = DT::renderDataTable({ 
    sentiment <- read.csv(file="sentiment-data.csv",header=TRUE)
    DT::datatable(sentiment, options = list(lengthChange = FALSE)) # data akan ditampilkan dalam beberapa halaman.
  })
  output$table <-  renderDataTable(hasil)
  
}

shinyApp(ui, server)

