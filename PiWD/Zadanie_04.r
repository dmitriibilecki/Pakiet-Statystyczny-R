# Prezentacja biblioteka ktуra uzywa ggplo2
# Interaktywna biblioteka, ktуra umozliwa doglebnie zbadac wykres
# po utworzeniu kodu za pomoca ggplot2 wystarczy tylko dodac kod "ggplotly(__nasz__WYKRES___)"
#biblioteka ggthemes - pozwala tworzyc wlasne motywy lub korzystac z gotowych wariantуw
#==============================
# 1 sekcja bibliotek
library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)
library(streamgraph)
library(ggthemes)
data(HairEyeColor)
data("mtcars")
#1 koniec 

#=======================================
# 2.1 Wykres Interaktywny + motyw
interactions_cars <-( ggplot(
 cars, aes(x=speed)) + geom_line(aes(y=dist))
 +theme_tufte()
)
print(ggplotly(interactions_cars))
#==================================
#2.2 Wykres Interaktywny + motyw
head(HairEyeColor)
par <- as.data.frame(HairEyeColor)
ee<- (ggplot(
  par, aes(Freq,Hair))
  + geom_count()
  +theme_economist()
)
print(ggplotly(ee))
#==========================
# 2.3 Wykres interaktywny + motyw
head(gapminder)
dd <- as.data.frame(gapminder) %>% filter(year==1997) %>%  ggplot( aes(x=gdpPercap, y=lifeExp, color =country )) + geom_point() +theme_hc()
print(ggplotly(dd))
