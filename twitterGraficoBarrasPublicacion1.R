#Cargo las librerías 

library(rtweet)      #interfaz con la API de twitter
library(tidyverse)   #ggplot
library(here)
library(jpeg)
library(png)
library(grid)

set_here(path ="E:/PROYECTOS_CON_R/TweetsLatinR")

# Obtengo los tweets con el hashtag #LatinR2019
conferencia19_tweets <- search_tweets(
  "#LatinR2019", n = 100000, include_rts = FALSE)  #LatinR2019

# Obtengo los tweets que nombren a @LatinR_Conf
conferencia_tweets <- search_tweets(
  "@LatinR_Conf", n = 10000, include_rts = FALSE) #@LatinR_Conf

#------------------------------------------------------------------------------
# Unifico en `conferencia` el resultado de las búsquedas anteriores
conferencia <- rbind(conferencia19_tweets, conferencia_tweets) %>%
  select(created_at, text) %>%
  mutate(id = c(1:1135), text = tolower(text))  
conferencia
# ** Obtuvé un dataset con 1135 filas al 02/10 

#------------------------------------------------------------------------------
# Separo la columna created_at en `fecha` y `hora'
tweets <- conferencia %>% separate(col = created_at, into = c("fecha", "hora"), sep = " ")
View(tweets)  

#-------------------------------------------------
# Gráfico 
#-------------------------------------------------
# Verifico los colores disponibles para el gráfico
colours()

# gráfico de barras con la cantidad de publicaciones por día entre el 23/09 y el 02/10
grafico1 <-ggplot(tweets, aes(x = fecha)) +
  geom_bar(fill = "blue", alpha = .5) + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.subtitle = element_text(color = "black", hjust = 0.5),
        plot.caption = element_text(color = "darkblue",size = 9,  hjust =0.9,family ="Atma Light"),
        plot.title = element_text(size = 20, hjust = 0.5,colour = "#562457",face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  labs(x = "",
       y = "Tweets",
       title =("Publicaciones en Twitter") ,
       subtitle = "Entre el 23 de septiembre y el 2 de octubre",
       caption = str_c(str_wrap("Para el scrapping se tuvieron en cuenta las menciones a #LatinR2019 y        a @LatinR_Conf.", width = 120),
                       "\n Data Source: Scrapping con rtweet\nVisualización: @PatriLoto"))+ 
  scale_y_continuous(breaks =seq(0, 320, by = 50))

grafico1

#--------------------------------------------------------------------------------
# inserto logo
# opción con logo 1
imagen <- readPNG("E:/PROYECTOS_CON_R/TweetsLatinR/latinr.png", FALSE)      #logolatinr2019
marca <- rasterGrob(imagen, interpolate=F,height=unit(2, "cm"),hjust=-3.5,  vjust=-0.4)
graficoFinal <-grafico1 + annotation_custom(marca,xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
graficoFinal
ggsave("graficoLogoLatinR2019.png",width = 10, height = 5, dpi = "retina")
#--------------------------------------------------------------------------------
#opción 2
imagen2 <-readJPEG("E:/PROYECTOS_CON_R/TweetsLatinR/latinr2019.jpg", FALSE)  #logolatinrHexagonal
marca2 <- rasterGrob(imagen2, interpolate=F,height=unit(3, "cm"),hjust=-1.5,  vjust=0)
pmarca2 <-grafico1 + annotation_custom(marca2,xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
pmarca2
ggsave("graficoLogoHexagonal.png",width = 10, height = 5, dpi = "retina")




