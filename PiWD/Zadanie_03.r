rm(list=ls())
list.of.packages <- c("ggplot2", "tidyverse", "patchwork", "ggplot2", "grid", "ggpubr", "ggeasy" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(tidyverse)
library(dplyr)
library(patchwork)
library(ggplot2)
library(grid)
library(ggpubr)
library(ggeasy)
# pobiera dane z NBP
getNBPData <- function(year=rok){
  
  ret <- data.frame()
  
  if(year>=2013){
    
    fileName <- paste0(year,"_NBP_data.csv")
    
    try({
      if(file.exists(fileName)){
        if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
          cat(paste("Reading data from local file\n"))
          ret<-read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F)
          colnames(ret) <- gsub("X","",colnames(ret))
          return(ret)
        }
      }
    })
    
    cat(paste("Downloading data\n"))
    
    res <- try({
      
      d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
      d <- d[-2]
      head(d)
      d <- d[-c((length(d)-3):length(d))]
      head(d)
      tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
      tmpColnames
      tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
      temp_col <- str_replace_all(tmpColnames,c('1'='', '0'=''))
      #robi macierz 
      d <- do.call("rbind",
                   lapply(strsplit(d[-1],";"),
                          function(x){
                            matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                          })
      )
      colnames(d) <- temp_col
      
      d <- as.data.frame(d)
      
      d$data <- as.Date(as.character(d$data),format="%Y%m%d")
      ret <- d
      write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
      
    },silent=T)
    
    if(inherits(res,"try-error")){
      cat(paste("An error occurred while downloading data!!!\n")) 
    }
    
    
  }
  
  return(ret)
  
}
#--------------------------------------
rok <-readline(prompt="Wprowadz rok z 2013-2021: ")



ret <- getNBPData(rok)
#ret <- bind_rows(list(getNBPData(2013), getNBPData(2014)), list(getNBPData(2015), getNBPData(2016)), list(getNBPData(2017), getNBPData(2018)), 
#                 list(getNBPData(2019), getNBPData(2020)), getNBPData(2021))
head(ret)
tail(ret)
ret <- ret[,grep("data|EUR|USD|GBP",colnames(ret))] 

names(ret)[names(ret) == "1USD"] <- "USD"
names(ret)[names(ret) == "1EUR"] <- "EUR"
names(ret)[names(ret) == "1GBP"] <- "GBP"
names(ret)[names(ret) == "data"] <- "data"
print(head(ret))
print(tail(ret))

#ret %>% group_by(data) %>% summarise(srednia_usd = sum(USD, na.rm= TRUE)) %>% arrange(data)
print(head(ret))

#pierwszy wykres
wykres1 <- (
  ggplot(ret, mapping =  aes(x=as.Date(data)))
  + geom_line(mapping = aes(x=as.Date(data), y=USD, color="USD"), size =0.1)+ geom_line(mapping =  aes(x=as.Date(data), y=EUR, color="EUR")) + geom_line(aes(x=as.Date(data),y=GBP, color="GBP"))
  + labs(x=as.character(rok),y= "PLN", label="rok", title= "Wykres nr 1", subtitle = "EUR, USD, GPB wzgledem PLN", caption = "www.nbp.pl", colour = 'Legenda')
  #+ scale_y_continuous( name = "ti lox", sec.axis = sec_axis(trans = ~.*01, name = "ti lox sprawa"))
  + theme(legend.position = c(0.95,0.95), legend.justification = c("right","top"))
  + scale_colour_manual(name="Line Color",
                        values=c(USD="red", EUR="green3", GBP="purple"))
  + theme_minimal()
)

x11()
print(wykres1)



#=============
#drugi wykres
mini_usd <-(
  ggplot(ret, mapping=  aes(x=as.Date(data)))
  +geom_line(aes(y=USD))
  +labs(x='')
  + theme_bw()
)
mini_eur <-(
  ggplot(ret, mapping=  aes(x=as.Date(data)))
  +geom_line(aes(y=EUR))
  +labs(x='',title = "Wykres nr 2")
  + easy_center_title()
  +theme_bw()
)
mini_gbp <-(
  ggplot(ret, mapping=  aes(x=as.Date(data)))
  +geom_line(aes(y=GBP))
  +labs(y="GBP", x='')
  +theme_bw()
)

sumd <- ggarrange(mini_eur, mini_usd, mini_gbp, ncol = 1, nrows=3)
x11();print(sumd)
#grid.arrange(mini_eur, mini_gbp, mini_usd, nrow = 1)

# Zadanie za 2 pkt.
# Korzystajac z grupowania danych dostepnego w ggplot, wykonac dwa wykresy szeregow czasowych
# kursow EUR, USD, GPB wzgledem PLN. Wykres pierwszy powinien posiadac legende, podpisane osie i tytul.
# Powinien byc wykresem zbiorczym - w ramach jednego ukladu wspolrzednych.
# Wykres drugi powiniem przedstawiac kursy walut w ramach trzech osobnych ukladow wspolrzednych o 
# dobranych dla kursow osiach OY. 
# Wykresy powinien generowac sie takze dla danych z innych lat (od 2013 do 2021).
# Czas na rozwiazanie - do 2021.04.30 23:59:59.
# tytul maila: PiWD/XXXXX/zadanie_03.r, gdzie XXXXX - numer albumu
# Zadanie w formie skryptu zadanie_03.r (rozwiniecie tego skryptu)
# email: mkozak3@sgh.waw.pl






