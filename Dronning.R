# Author: MHGottlieb
# Date: Sun Dec 16 17:08:06 2018
# --------------
# Description:
# Scraping, then analysing new years adresses from HM Queen Margrethe II of DK
# 
# --------------

# Dependencies ------------------------------------------------------------

  library(httr)
  library(XML)
  library(ggplot2)
  library(ggridges)
  library(RColorBrewer)
  library(stringr)
  library(reshape2)
  library(tm)
  library(tidytext)
  library(SnowballC)
  library(wordcloud)

# Fordi jeg ikke kan lide rød ---------------------------------------------

  options(warn=-1)  
  
# Downloader taler --------------------------------------------------------

  base <- "http://dronningens-nytaarstale.dk/taler/nytaarstale_"
  years <- c(1972:2012)
  
  urls <- c("http://kongehuset.dk/menu/nyheder/las-nytarstalen-2013",
            "http://kongehuset.dk/menu/nyheder/las-nytarstalen-2014",
            "http://kongehuset.dk/menu/nyheder/laes-nytaarstalen-2015",
            "http://kongehuset.dk/nyheder/laes-nytaarstalen-2016",
            "http://kongehuset.dk/nyheder/laes-hm-dronningens-nytaarstale-2017")
  
  taler <- NULL # placeholder
  taler2 <- NULL # placeholder
  
  for(i in 1:length(years)){
    temp <- paste0(base, years[i], ".html")
    temp <- GET(temp, add_headers("user-agent" = "Mozilla/5.0"))
    temp <- htmlParse(temp, encoding = "UTF-8")
    text <- xpathSApply(temp, "//body", xmlValue)
    text <- paste(text, collapse=" ")
    text <- iconv(text, from="UTF-8", to="latin1")
    text <- gsub('[\n\r]',' ', text)
    taler$text[i] <- text
    taler$year[i] <- years[i]
    message(years[i])
  }
  
  for(i in 1:length(urls)){
    temp <- GET(urls[i], add_headers("user-agent" = "Mozilla/5.0"))
    temp <- htmlParse(temp)
    text <- xpathSApply(temp, "//p", xmlValue)
    text <- paste(text, collapse=c(" ", "\n"))
    text <- iconv(text, from="UTF-8", to="latin1")
    text <- gsub('[\n\r]',' ', text)
    taler2$text[i] <- text
    taler2$year[i] <- 2012+i
    message(2012+i)
  }

# Omformer til Data Frame -------------------------------------------------
  
  taler <- as.data.frame(taler)
  taler2 <- as.data.frame(taler2)
  taler <- rbind(taler, taler2)
  taler$text <- as.character(taler$text)
  taler$text <- gsub("\\s+", " ", str_trim(taler$text))

  # Manuel oprydning for en god ordens skyld
  taler$text[42:46] <- substring(taler$text[42:46], 25) #Fjerner dato og tag
  taler$text[42] <- substring(taler$text[42], 1, nchar(taler$text[42])-117)
  
  # Fjerner lidt støj fra enden af de seneste taler
  taler$text <- substring(taler$text, 1, unlist(gregexpr("GUD BEVARE DANMARK", 
                                                          taler$text))+18)
  
  # Tæller ord og udregner sætningslængder
  taler$nwords <- lengths(strsplit(taler$text, "\\W+"))
  taler$sentences <- str_count(taler$text, "\\.|\\?")
  taler$sen_lengths <- taler$nwords/taler$sentences
  
  # Ordlængder
  taler$wl <- sapply(strsplit(taler$text, c(" ")), FUN=nchar) # counting word lenghts
  taler$wl <- lapply(taler$wl, function(x) {x[x!=0]}) # removes zeros

  # Rydder op
  remove(taler2, base, i, temp, text, urls, years)
  
# Simple search solution for single word in "taler" -----------------------

  taler$year[grep("Pakistan", taler$text, ignore.case=TRUE)]

# Beregner lix ------------------------------------------------------------

  taler$long_words <- unlist(lapply(taler$wl, function(x){sum(x>6)}))
  taler$lix <- taler$nwords/taler$sentences + taler$long_words*100/taler$nwords

# Plot1: antal ord ---------------------------------------------------------

  plot1 <- ggplot(data=taler, aes(x=year, y=nwords)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Ord", x="År") +
    geom_text(aes(label=ifelse(year==2004,as.character("Frederik og Mary Bryllup,\nJoachim og Alexandra seperation"),'')),
              hjust=0,vjust=0, size=3, nudge_x = -13, nudge_y=-33) +
    geom_text(aes(label=ifelse(year==1998,as.character("Ingen begivenheder i \ndet forgangne år nævnes"),'')),
              hjust=0,vjust=0, size=3, nudge_x = 1, nudge_y=-23) +
    xlim(1972,2020) + ylim(0, 2000)
  
  lm1 <- lm(nwords ~ year, data=taler)
  #summary(lm1) #linreg

# Plot2 : Sætningslængder -------------------------------------------------
  
  plot2 <- ggplot(data=taler, aes(x=year, y=sen_lengths)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Gns. ord per sætning", x="År") +
    xlim(1972,2020) + ylim(0,40)
  
  lm2 <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linreg
  
  
# Plot3: Lix --------------------------------------------------------------
 
  plot3 <- ggplot(data=taler, aes(x=year, y=lix)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Lix", x="År") +
    xlim(1972,2020) + ylim(0, 50)
  
  lm3 <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linregs
  
# Eksempler på sætninger ---------------------------------------------------

  ex1 <- paste(unlist(str_split(taler$text[9], "\\.|\\?"))[1:5], collapse=".")
  ex2 <- paste(unlist(str_split(taler$text[43], "\\.|\\?"))[1:5], collapse=".")

# New data frame with word lengths and years ------------------------------

  years <- c(1972:2017)
  wl <- unlist(lapply(taler$wl, length)) # number of words per year
  wl <-  data.frame(wl = matrix(unlist(taler$wl)), year = rep(years, times=len))
  

# Density plot ------------------------------------------------------------
  
  plot4 <- ggplot(wl, aes(x=wl, y=year, group=year)) +
    stat_density_ridges(scale = 8,rel_min_height=0.07) +
    xlim(0, 15)
  
# Ordanalyse --------------------------------------------------------------

  #hvor mange gange nævnes div. lande

  lande <- GET("www.globalis.dk/Lande")
  lande <- htmlParse(lande)
  lande <- xpathSApply(lande, "//ul/li/a", xmlValue)
  
  lande <- c(lande, "Grønland", "Færøerne")
  
  tomatch <- lande
  tomatch <- c("terror", "jordskælv", "flodbølge", "tsunami", "storm", "orkan", 
               "vulkan", "eksplosion", "krig", "klimaforandringer")
  
  result <- lapply(taler$text, str_count, tomatch)
  result <- as.data.frame(do.call(rbind, result))
  
  colnames(result) <- tomatch
  rownames(result) <- c(1972:2017)
  
  result <- result[,order(colSums(result), decreasing=FALSE)]
  result <- result[,colSums(result)>0]
  result <- as.matrix(result)

# Matrixplot --------------------------------------------------------------

  long <- melt(result)
  long <- long[long$value!=0,]

  #Farvede felter
  plot5 <- ggplot(long, aes(x = Var1, y = Var2)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="grey90", high="blue", guide="legend", na.value="grey90") +
    labs(x="", y="", title="Lande nævnt i Dronningens Nytårstale") +
    theme_bw() + theme(axis.text.x=element_text(size=12, angle=45, vjust=0.3),
                       axis.text.y=element_text(size=12, hjust=0),
                       plot.title=element_text(size=22))

  #Punktstørrelser
  plot6 <- ggplot(long, aes(x = Var1, y = Var2)) + 
    geom_point(aes(size=value), color="blue", alpha=0.5) + 
    scale_size(name="Gange nævnt", breaks=c(1,2,3,5), labels=c(1,2,3,"4+")) +
    labs(x="", y="", title="Lande nævnt i Dronningens Nytårstale") +
    theme_gray() + theme(axis.text.x=element_text(size=12, angle=45, vjust=0.3),
                         axis.text.y=element_text(size=12, hjust=0),
                         plot.title=element_text(size=22))
  
  remove(lande, tomatch, years, result, long)

# Wordcloud ----------------------------------------------------------

  sw <- read.csv2("https://raw.githubusercontent.com/stopwords-iso/stopwords-da/master/stopwords-da.txt", 
                    encoding="UTF-8", header=FALSE)
  sw <- as.character(sw[,1]) #170 stopord fra MIT
  
  docs <- Corpus(VectorSource(taler$text))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, sw)
  
  dtm <- as.matrix(TermDocumentMatrix(docs))
  dtm <- sort(rowSums(dtm),decreasing=TRUE)
  dtm <- data.frame(word = names(dtm),freq=dtm) #taget fra sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
  head(dtm, 10)
  
  set.seed(1)
  wordcloud(words = dtm$word, freq = dtm$freq, min.freq = 1,
            max.words=300, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
# Sentiment Analysis ------------------------------------------------------
  
  taler$text_stem <- str_split(taler$text)
  taler$text_stem <- 
    
  af <- get_sentiments("afinn")
  
# Saving images -----------------------------------------------------------

  gridExtra::grid.arrange(plot1, plot2, plot3, ncol=3) 
  gridExtra::grid.arrange(plot6)
