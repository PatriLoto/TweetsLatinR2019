# Load packages
library(rtweet) # interface with Twitter API
library(tidytext) # sentiment analysis
library(tidyverse) # ggplot
library(wordcloud)
library(here)
library(jpeg)
library(png)
library(twitteR)
library(syuzhet)
extrafont::loadfonts(device = "win")
library(showtext) # for google fonts
font_add_google("Fira Mono", "firamono")
#install.packages("syuzhet")
#install.packages("wordcloud")
#------------------------------------------------------------------------------------

set_here(path ="E:/PROYECTOS_CON_R/TweetsLatinR")
# Connect to Twitter
# *~* API witchcraft *~*

# Get tweets 
conf_tweets <- search_tweets(
  "#LatinR2019", n = 100000, include_rts = FALSE) #LatinR2019

conf_tweets

conf19_tweets <- search_tweets(
  "@LatinR_Conf", n = 10000, include_rts = FALSE) #LatinR_Conf

conf19_tweets

conf <- rbind(conf_tweets, conf19_tweets) %>%
  select(created_at, text) %>%
  mutate(id = c(1:700), text = tolower(text))  #1129
conf
#------------------------------------------------------------------------------
#Convirtiendo los tweets en un data frame
tweets_df <- twListToDF(tweets) 
head(tweets_df)
tweets


tweets <- conf %>% separate(col = created_at, into = c("fecha", "hora"), sep = " ")
#%>% mutate(fechaT= dmy('fecha'))
tweets
#funcionaaaa
#Quitando los links en los tweets
tweets_df2 <- gsub("http.*","",tweets$text)
tweets_df2 <- gsub("https.*","",tweets_df2)

#Quitando los hashtags y usuarios en los tweets
tweets_df2 <- gsub("#\\w+","",tweets_df2)
tweets_df2 <- gsub("@\\w+","",tweets_df2)

#Quitando los signos de puntuación, números y textos con números
tweets_df2 <- gsub("[[:punct:]]","",tweets_df2)
tweets_df2 <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets_df2)
tweets_df2

palabra.df <- as.vector(tweets_df2)
palabra.df

#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")
emocion.df
#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets_df2, emocion.df)
head(emocion.df2)

#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))
emocion.df3
#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)
emocion.df3
#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)


sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n con los Tweets de LatinR2019",
       x = "Sentimiento", y = "Frecuencia", scale_x_discrete(limits=c("enojo","anticipacion","disgusto", "miedo", "alegría",
                                                                      "tristeza", "sorpresa","confianza")))+
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size =4) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5, family= "Fira Mono"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
        #, labels=colnames("enojo", anticipacion","disgusto", "miedo", "alegría","tristeza","sorpresa", "confianza"))
  
sentimientos1
