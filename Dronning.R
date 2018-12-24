library(httr)
library(XML)
library(ggplot2)
library(ggridges)
library(RColorBrewer)


base <- "http://dronningens-nytaarstale.dk/taler/nytaarstale_"
years <- c(1972:2013)

urls <- c("http://kongehuset.dk/menu/nyheder/las-nytarstalen-2014",
          "http://kongehuset.dk/menu/nyheder/laes-nytaarstalen-2015",
          "http://kongehuset.dk/nyheder/laes-nytaarstalen-2016",
          "http://kongehuset.dk/nyheder/laes-hm-dronningens-nytaarstale-2017")

taler <- NULL
taler2 <- NULL

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
  #Sys.sleep(1)
}

for(i in 1:length(urls)){
  temp <- GET(urls[i], add_headers("user-agent" = "Mozilla/5.0"))
  temp <- htmlParse(temp)
  text <- xpathSApply(temp, "//p", xmlValue)
  text <- paste(text, collapse=c(" ", "\n"))
  text <- iconv(text, from="UTF-8", to="latin1")
  text <- gsub('[\n\r]',' ', text)
  taler2$text[i] <- text
  taler2$year[i] <- 2013+i
  message(2013+i)
  #Sys.sleep(1)
}

taler <- as.data.frame(taler)
taler2 <- as.data.frame(taler2)
taler <- rbind(taler, taler2)


taler$text <- as.character(taler$text)
taler$words <- lengths(strsplit(taler$text, "\\W+"))

taler$year[which(grepl("Søens Folk", taler$text, ignore.case=TRUE))]

plot <- ggplot(data=taler, aes(x=year, y=words)) + 
  geom_point(col="steelblue", size=3) + 
  geom_smooth(method="lm", col="firebrick") +
  labs(title="Dronningens Nytårstaler", y="antal ord", x="år") +
  geom_text(aes(label=ifelse(year==2004,as.character("Frederik og Mary Bryllup,\nJoachim og Alexandra seperation"),'')),
            hjust=0,vjust=0, size=3, nudge_x = -13, nudge_y=-33) +
  geom_text(aes(label=ifelse(year==1998,as.character("Ingen begivenheder i \ndet forgangne år nævnes"),'')),
            hjust=0,vjust=0, size=3, nudge_x = 1, nudge_y=-23) +
  as.character(as.expression(eq))
  
  xlim(1970,2020)

plot

linearMod <- lm(words ~ year, data=taler)
summary(linearMod)

remove(base, i, temp, text, urls, years, taler2)




# New data frame with word lengths and years ------------------------------

taler$wl <- sapply(strsplit(taler$text, c(" ")), FUN=nchar) # counting word lenghts
taler$wl <- lapply(taler$wl, function(x) {x[x!=0]}) # removes zeros
years_all <- c(1972:2017)
len <- unlist(lapply(taler$wl, length)) # number of words per year
wl <-  data.frame(wl = matrix(unlist(taler$wl)), year = rep(years_all, times=len)) 

# Density plot ------------------------------------------------------------


plot2 <- ggplot(wl, aes(x=wl, y=year, group=year)) +
  stat_density_ridges(scale = 8,rel_min_height=0.07) +
  xlim(0, 15)
plot2


