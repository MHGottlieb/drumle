  
  
  WordAffinity <- function(text, language){
    require(tm)
    text <- tolower(removePunctuation(text))
    words <- as.data.frame(unlist(strsplit(text, "\\W+")))
    words$row <- c(1:nrow(words))
    names(words) <- c("word", "row")
    if(language=="da"){
      AFINN <- "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-da-32.txt"
    } else {
      AFINN <- "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt"
    }
    AFINN <- read.csv2(AFINN, encoding="UTF-8", sep="\t", header=FALSE)
    names(AFINN) <- c("word", "sentiment")
    words <-  merge(words, AFINN, all.x=TRUE)
    words <- words[order(words$row),]
    words$sentiment[is.na(words$sentiment)] <- 0
    words$cum_sent <- cumsum(words$sentiment)
    return(words)
  }
  
  SenteceAffinity <- function(text, language){
    require(tm)
    text <- tolower(text)
    unlist(strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))
  }
  
  
