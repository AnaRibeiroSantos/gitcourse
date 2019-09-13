#install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))
                 
                 requiredLibs <- c("ggplot2", "RODBC", "plyr", "dplyr", "reshape2", "data.table", "magrittr", "stringr", "lubridate", "tidyverse")
                 lapply(requiredLibs, require, character.only = TRUE)
                 for(i in unique(requiredLibs)) {
                   if(!i %in% (.packages())) {
                     print(paste0(i, " isn't loaded. installing package now"))
                     install.packages(i)
                   } else if(i %in% (.packages())) {
                     print(paste0(i, " loaded successfully"))
                   }
                 }
                 
library(wordcloud)
library(tm)
library (RColorBrewer)
library(RCurl)
library(XML)
library(SnowballC)
                 
load ("C:\\Users\\as15\\OneDrive - CEFAS\\BX004A\\On-Shore Programme\\BX004A John\\gariData_ENG_final.Rdata")

observer <- read.csv ("C:\\Users\\as15\\OneDrive - CEFAS\\AFST meetings\\AFST meeting 2018\\disc.samp.csv")
stocks <- read.csv ("C:\\Users\\as15\\OneDrive - CEFAS\\AFST meetings\\AFST meeting 2018\\stock_list16.csv")
unique(observer$Region)

str(stocks)
stocks$Stock <- as.character(stocks$Stock)
stocks$SPECIES_CODE <- as.character(stocks$SPECIES_CODE)

observer$IcesDivision <- as.character(observer$Region)
observer$IcesDivision[observer$Region=="104A"] <- "27.4.a"
observer$IcesDivision[observer$Region=="104B"] <- "27.4.b"
observer$IcesDivision[observer$Region=="104C"] <- "27.4.c"
observer$IcesDivision[observer$Region=="107A"] <- "27.7.a"
observer$IcesDivision[observer$Region=="107D"] <- "27.7.d"
observer$IcesDivision[observer$Region=="107E"] <- "27.7.e"
observer$IcesDivision[observer$Region=="107F"] <- "27.7.f"
observer$IcesDivision[observer$Region=="107G"] <- "27.7.g"
observer$IcesDivision[observer$Region=="107H"] <- "27.7.h"
observer$IcesDivision[observer$Region=="106A"] <- "27.6.a"
observer$IcesDivision[observer$Region=="107J"] <- "27.7.j"

observer <- merge (observer, stocks, all.x=T)
observer$Stock <- ifelse (is.na(observer$Stock) | observer$Stock=="no.defined.stock", paste (observer$SPECIES, observer$IcesDivision, sep="_"), observer$Stock)
observer$Stock <- tolower(observer$Stock)

FCK SH1T UP

# SampleID = LAnding_event
# SampleNo = Category sample (different species within a SampleID)

d <- gariData_ENG_final %>%
  group_by (Stock) %>%
  summarise (., number.len = sum(N.at.Len), no.samples = n_distinct(SampleID)) %>%
  ungroup () %>%
  group_by (Stock) %>%
  arrange (., desc(no.samples))

d <- as.data.frame (d)



d2 <- observer%>%
  group_by (Stock) %>%
  summarise (., number.len = sum(No.lengths.disc), no.trips= sum(No.trips)) %>%
  ungroup () %>%
  group_by (Stock) %>%
  arrange (., desc(no.trips))

d2 <- as.data.frame (d2)



barplot(d[1:10,]$no.samples, las = 2, 
        names.arg = d[1:10,]$Stock,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

pal2 <- brewer.pal(8,"Dark2")

wordcloud(d$Stock, d$no.samples, random.order= F, rot.per = 0.1, 
          scale = c (3,.5), font.main = 1, cex.main = 2, 
          max.words = 100, colors = pal2)  
par(mar = rep(0, 4))
wordcloud(d$Stock, d$number, random.order= F, 
          rot.per = 0.3, scale = c (4,.5), 
          font.main = 1, cex.main = 1.5, max.words = 100,
          colors = pal2)  


wordcloud(d2$Stock, d2$no.trips, random.order= F, rot.per = 0.1, 
          scale = c (2,.5), font.main = 1, cex.main = 2, 
          max.words = 100, colors = pal2)  
par(mar = rep(0, 4))

wordcloud(d2$Stock, d2$number.len, random.order= F, 
          rot.per = 0.3, scale = c (4,.5), 
          font.main = 1, cex.main = 1.5, max.words = 100,
          colors = pal2)   
