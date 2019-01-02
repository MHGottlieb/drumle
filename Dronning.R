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

  #options(warn=-1)  

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
    geom_smooth(method="lm", col="firebrick", se=FALSE) +
    labs(title="Dronningens nytårstale bliver længere", y="Ord", x="År") +
    geom_text(aes(label=ifelse(year==2004,as.character("Frederik og Mary Bryllup,\nJoachim og Alexandra seperation"),'')),
              hjust=0,vjust=0, size=3, nudge_x = -5, nudge_y=45) +
    geom_text(aes(label=ifelse(year==1998,as.character("Ingen begivenheder i \ndet forgangne år nævnes"),'')),
              hjust=0,vjust=0, size=3, nudge_x = 0.5, nudge_y=-100) +
    xlim(1972,2020) + ylim(0, 2000) +
    theme(text = element_text(size=22))
  
  png("plot1.png", width=1024, height=512)
  plot1
  dev.off()
  
  lm1 <- lm(nwords ~ year, data=taler)
  #summary(lm1) #linreg

# Plot2 : Sætningslængder -------------------------------------------------
  
  plot2 <- ggplot(data=taler, aes(x=year, y=sen_lengths)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick", se=FALSE) +
    labs(title="Dronningens nytårstale: sætningerne bliver kortere", 
         y="Gns. ord per sætning", x="År") + xlim(1972,2020) + ylim(0,40) +
    theme(text = element_text(size=22))
  
  png("plot2.png", width=1014, height=512)
  plot2
  dev.off()
  
  lm2 <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linreg


# Plot3: Lix --------------------------------------------------------------
  
  plot3 <- ggplot(data=taler, aes(x=year, y=lix)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick", se=FALSE) +
    labs(title="Dronningens nytårstale: Lix", y="Lix", x="År") +
    xlim(1972,2020) + ylim(0, 50) +
    theme(text = element_text(size=22))
  
  png("plot3.png", width=1024, height=512)
  plot3
  dev.off()
  
  lm3 <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linregs

# Eksempler på sætninger ---------------------------------------------------

  ex1 <- paste(unlist(str_split(taler$text[9], "\\.|\\?"))[1:5], collapse=".")
  ex2 <- paste(unlist(str_split(taler$text[43], "\\.|\\?"))[1:5], collapse=".")

# New data frame with word lengths and years ------------------------------
  
  years <- c(1972:2017)
  wl <- unlist(lapply(taler$wl, length)) # number of words per year
  wl <-  data.frame(wl = matrix(unlist(taler$wl)), year = rep(years, times=wl))
  wl <- wl
  fill <- rep(taler$lix, times=unlist(lapply(taler$wl, length)))

# Density plot ------------------------------------------------------------
  
  plot4 <- ggplot(wl, aes(x=wl, y=year, group=year, fill=fill)) +
    stat_density_ridges(scale = 11,rel_min_height=0, bandwidth = 0.5, alpha=0.8) +
    scale_fill_viridis_c(name="Lix") +
    labs(title="Dronningens nytårstale: Fordeling af ordlængder", y="År", x="Bogstaver per ord") +
    xlim(0, 15) +
    theme(text = element_text(size=22))
  
  png("plot4.png", width=800, height=800)
  plot4
  dev.off()

# Ordanalyse --------------------------------------------------------------

  testwords <- NULL
  
  #hvor mange gange nævnes div. lande
  lande <- GET("www.globalis.dk/Lande")
  lande <- htmlParse(lande)
  lande <- xpathSApply(lande, "//ul/li/a", xmlValue)
  
  testwords$lande <- c(lande, "Grønland", "Færøerne")

  testwords$opt <- c("glæde", "varme", "sommer", "føler", "muligheder", 
                     "lykke")
  
  testwords$fam <- c("familie", "prins", "generationer", "børn", "prinsgemal", 
                     "joachim", "familien", "prinsesse", "dronning", 
                     "kronprinspar", "barn", "dreng", "kronprins", "kronprinsesse")
  
  testwords$krise <- c("storm", "orkan", "jordskælv", "flodbølge", "vulkan", "naturkatastrofe", 
                        "terror", "krig", "ufred", "krise")
  
  testwords$andet <- c("arbejde", "økonomiske", "soldater", "forsvaret", 
                       "forsvar", "politi")
  
  testwords$geo <- c("grønlandske", "lande", "grænsen", "syd", "færøske", "internationale", 
                     "grænser", "europæiske", "sydslesvig", "københavn", "grønlands", 
                     "irak", "aarhus", "rigsfællesskabet", "europas", "europæisk")
  
  testwords$sammen <- c("verden", "dansk", "tillid", "tryghed", "samfund", "sammen", 
                       "hinanden", "folk", "landet", "fremmede", "herhjemme", "fællesskab", 
                       "samfundet", "respekt", "fællesskabet", "traditioner", "trofasthed", 
                       "landsmænd", "offervilje", "medansvar", "danskheden", "egoisme")
  
# Matrixplot --------------------------------------------------------------
 
  tomatch <- testwords$opt
  
  result <- lapply(taler$text, str_count, tomatch)
  result <- as.data.frame(do.call(rbind, result))
  
  colnames(result) <- tomatch
  rownames(result) <- c(1972:2017)
  
  result <- result[,order(colSums(result), decreasing=FALSE)]
  result <- result[,colSums(result)>0]
  result <- as.matrix(result)
   
  long <- melt(result)
  long <- long[long$value!=0,]

#Farvede felter
#  plot5 <- ggplot(long, aes(x = Var1, y = Var2)) + 
#    geom_raster(aes(fill=value)) + 
#    scale_fill_gradient(low="grey90", high="blue", guide="legend", na.value="grey90") +
#    labs(x="", y="", title="Lande nævnt i Dronningens Nytårstale") +
#    theme_bw() + theme(axis.text.x=element_text(size=12, angle=45, vjust=0.3),
#                       axis.text.y=element_text(size=12, hjust=0),
#                       plot.title=element_text(size=22))

#Punktstørrelser
  cplot2 <- ggplot(long, aes(x = Var1, y = Var2)) + 
    geom_point(aes(size=value), color="blue", alpha=0.5) + 
    scale_size(name="Gange nævnt", breaks=c(1,2,3), labels=c(1,2,"3+")) +
    labs(x="", y="", title="Nævnt i Dronningens Nytårstale") +
    theme_gray() + 
    theme(axis.text.x=element_text(size=12, angle=45, vjust=0.3),
                         axis.text.y=element_text(size=12, hjust=0),
                         plot.title=element_text(size=22))
  
  remove(lande, tomatch, years, result, long)
  
  png("cplot2.png", width=1024, height=512)
  cplot2
  dev.off()

# Stemming ----------------------------------------------------------

  # omformer tekst
  taler$text_clean <- tolower(removePunctuation(taler$text))
  taler$text_stem <- stemDocument(taler$text_clean, language="danish")

  #fjerner stopord
  sw <- read.csv2("https://raw.githubusercontent.com/stopwords-iso/stopwords-da/master/stopwords-da.txt", 
                  encoding="UTF-8", header=FALSE)
  sw <- as.character(sw[,1]) #170 stopord fra MIT
  taler$text_stem <- removeWords(taler$text_clean, sw)
  
  docs <- Corpus(VectorSource(taler$text_stem))
  
  dtm <- as.matrix(TermDocumentMatrix(docs))
  dtm <- sort(rowSums(dtm),decreasing=TRUE)
  dtm <- data.frame(word = names(dtm), freq=dtm) #taget fra sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
  rownames(dtm) <- c(1:nrow(dtm))
  
  head(dtm, 10)
  
  set.seed(1)
  plot5 <- wordcloud(words = dtm$word, freq = dtm$freq, min.freq = 1,
            max.words=300, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))

# Sentiment Analysis ------------------------------------------------------

  #downloader AFINN danish:
  
  AFINN <- "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-da-32.txt"
  AFINN <- read.csv2(AFINN, encoding="UTF-8", sep="\t", header=FALSE)
  
  sentiments <- lapply(taler$text_clean, str_count, as.character(AFINN$V1))
  sentiments <- as.data.frame(do.call(rbind, sentiments))
  
  colnames(sentiments) <- AFINN$V1
  rownames(sentiments) <- c(1972:2017)
  sentiments <- as.data.frame(sentiments)
  sentiments2 <- mapply("*", sentiments, AFINN$V2)
  
  taler$sentiment <- rowSums(sentiments2)/taler$sentences
  
  plot5 <- ggplot(data=taler, aes(x=year, y=sentiment)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="loess", col="firebrick", se=TRUE, span=0.8) +
    labs(title="Dronningens nytårstale: Følelse", y="Gns. AFINN sentiment per sætning \n (>0 = positiv, <0 = negativ)", x="År") +
    xlim(1972,2020) +
    theme(text = element_text(size=20))
  
  abe <- loess(sentiment ~year, taler)
  
  png("plot5.png", width=1024, height=512)
  plot5
  dev.off()
