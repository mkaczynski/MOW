preprocessing = function()
{
  con <- file("SMSSpamCollection", open = "r")
  out <- file("SMSSpamCollection_out", open = "a")
  
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # [punct] - ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
    res <- gsub("\'t", "t", oneLine, ignore.case=TRUE)
    res <- gsub("[[:punct:]]+", " ", res, ignore.case=TRUE)
    res <- gsub("[ ]+", " ", res, ignore.case=TRUE)
    res <- tolower(res)
    writeLines(res, out)  
  } 
  close(con)
  close(out)
}

bayes = function(lset, x, probabilities)
{
  # lset - zbior uczacy,
  # x - probka do klasyfikacji
  # probabilities - wektor opisujacy prawdopdobienstwa poszczegolnych klas
}

setwd("C:/Users/Marcin/Desktop/studia/MOW/kod/")

con <- file("SMSSpamCollection_out", open = "r")
len <- 0

vec = c()
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  oneLine <- readLines(con, n = 1, warn = FALSE)
  line <- unlist(strsplit(oneLine, " |\t"))
  if((tmp <- length(line)) > len)
    len = tmp
  vec = union(vec, line)
} 
close(con)

len

cn <- paste("V",1:len,sep="")
tmp <- read.table("SMSSpamCollection_out", fill=TRUE, col.names=cn, quote="")

dict <- c()
for(i in 1:nrow(tmp)) {
  for(j in 1:ncol(tmp[i,])) {
    word <- tmp[i,j]
    x <- dict[word]
    print(x)
  }
}

library(hash)

dict <- hash()
o <- 0
for(i in 1:nrow(tmp)) {
  for(j in 1:ncol(tmp[i,])) {
    word <- as.character(tmp[[i,j]])
    if(word != "" && !is.na(word) && !has.key(word, dict)) {
      o = o + 1
      dict[word] <- o;  
    }
  }
}



