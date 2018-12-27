  library(httr)
  library(XML)
  library(ggplot2)
  library(ggridges)
  library(RColorBrewer)
  library(stringr)
  library(reshape2)

# Downloader taler --------------------------------------------------------

  base <- "http://dronningens-nytaarstale.dk/taler/nytaarstale_"
  years <- c(1972:2012)
  
  urls <- c("http://kongehuset.dk/menu/nyheder/las-nytarstalen-2013",
            "http://kongehuset.dk/menu/nyheder/las-nytarstalen-2014",
            "http://kongehuset.dk/menu/nyheder/laes-nytaarstalen-2015",
            "http://kongehuset.dk/nyheder/laes-nytaarstalen-2016",
            "http://kongehuset.dk/nyheder/laes-hm-dronningens-nytaarstale-2017")
  
  taler <- NULL # Tomt element som placeholder
  taler2 <- NULL # Tomt element som placeholder
  
  for(i in 1:length(years)){
    temp <- paste0(base, years[i], ".html")
    temp <- GET(temp, add_headers("user-agent" = "Mozilla/5.0"))
    temp <- htmlParse(temp)
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

  # manuel oprydning for en god ordens skyld
  taler$text[42:46] <- substring(taler$text[42:46], 25) #Fjerner dato og tag
  taler$text[42] <- substring(taler$text[42], 1, nchar(taler$text[42])-117)
  
  #fjerner lidt støj fra enden af hver tale
  taler$text1 <- substring(taler$text, 1, unlist(gregexpr("GUD BEVARE DANMARK", 
                                                          taler$text))+18)
  # Tæller ord og udregner sætningslængder
  taler$words <- lengths(strsplit(taler$text, "\\W+"))
  taler$sentences <- str_count(taler$text, "\\.|\\?")
  taler$sen_lengths <- taler$words/taler$sentences
  
  # Ordlængder
  
  taler$wl <- sapply(strsplit(taler$text, c(" ")), FUN=nchar) # counting word lenghts
  taler$wl <- lapply(taler$wl, function(x) {x[x!=0]}) # removes zeros

# Simple search solution for single word in "taler" -----------------------

  taler$year[grep("færøerne", taler$text, ignore.case=TRUE)]

# Beregner lix ------------------------------------------------------------

  taler$long_words <- unlist(lapply(taler$wl, function(x){sum(x>6)}))
  taler$lix <- taler$words/taler$sentences + taler$long_words*100/taler$words

# Plot1: antal ord ---------------------------------------------------------

  plot1 <- ggplot(data=taler, aes(x=year, y=words)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Ord", x="År") +
    geom_text(aes(label=ifelse(year==2004,as.character("Frederik og Mary Bryllup,\nJoachim og Alexandra seperation"),'')),
              hjust=0,vjust=0, size=3, nudge_x = -13, nudge_y=-33) +
    geom_text(aes(label=ifelse(year==1998,as.character("Ingen begivenheder i \ndet forgangne år nævnes"),'')),
              hjust=0,vjust=0, size=3, nudge_x = 1, nudge_y=-23) +
    xlim(1972,2020) + ylim(0, 2000)
  
  plot1 #plotter figuren
  
  linearMod <- lm(words ~ year, data=taler)
  #summary(linearMod) #linreg

# Plot2 : Sætningslængder -------------------------------------------------
  
  plot2 <- ggplot(data=taler, aes(x=year, y=sen_lengths)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Gns. ord per sætning", x="År") +
    xlim(1972,2020) + ylim(0,40)
  
  plot2 #plotter figuren
  
  linearMod <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linreg
  
  
# Plot3: Lix --------------------------------------------------------------
 
  plot3 <- ggplot(data=taler, aes(x=year, y=lix)) + 
    geom_point(col="steelblue", size=3) + 
    geom_smooth(method="lm", col="firebrick") +
    labs(title="Dronningens Nytårstaler", y="Lix", x="År") +
    xlim(1972,2020) + ylim(0, 50)
  
  plot3 #plotter figuren
  
  linearMod <- lm(sen_lengths ~ year, data=taler)
  #summary(linearMod) #linreg
  
# Eksempel på sætninger ---------------------------------------------------

  unlist(str_split(taler$text[taler$year==1980], "\\.|\\?"))[1:4]
  unlist(str_split(taler$text[taler$year==2014], "\\.|\\?"))[1:4]

# New data frame with word lengths and years ------------------------------

  years <- c(1972:2017)
  len <- unlist(lapply(taler$wl, length)) # number of words per year
  wl <-  data.frame(wl = matrix(unlist(taler$wl)), year = rep(years, times=len)) 

# Density plot ------------------------------------------------------------
  
  plot4 <- ggplot(wl, aes(x=wl, y=year, group=year)) +
    stat_density_ridges(scale = 8,rel_min_height=0.07) +
    xlim(0, 15)
  
  plot4 #plotter figuren

# Ordanalyse --------------------------------------------------------------

  #hvor mange gange nævnes div. lande

  temp <- GET("www.globalis.dk/Lande")
  lande <- htmlParse(temp)
  lande <- xpathSApply(lande, "//ul/li/a", xmlValue)
  
  lande <- c(lande, "Grønland", "Færøerne")
  
  tomatch <- lande
  
  result <- lapply(taler$text, str_count, tomatch)
  result <- as.data.frame(do.call(rbind, result))
  
  colnames(result) <- tomatch
  rownames(result) <- c(1972:2017)
  
  result <- result[,order(colSums(result), decreasing=FALSE)]
  result <- result[,colSums(result)>1]
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
                       axis.text.y=element_text(size=12),
                       plot.title=element_text(size=22))

  #Punktstørrelser
  plot6 <- ggplot(long, aes(x = Var1, y = Var2)) + 
    geom_point(aes(size=value), color="blue", alpha=0.5) + 
    scale_size(name="Gange nævnt", breaks=c(1,2,3,5), labels=c(1,2,3,"4+")) +
    labs(x="", y="", title="Lande nævnt i Dronningens Nytårstale") +
    theme_gray() + theme(axis.text.x=element_text(size=12, angle=45, vjust=0.3),
                         axis.text.y=element_text(size=12),
                         plot.title=element_text(size=22))
  
  gridExtra::grid.arrange(plot1, plot2, plot3, ncol=3) 
  gridExtra::grid.arrange(plot6) 
  
