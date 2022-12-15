rm(list=ls(all = TRUE))
setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
#setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q15 <- read.csv("q15.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...
q15$sx <- as.numeric(substr(q15$V3,3,nchar(q15$V3)-1))
q15$sy <- as.numeric(substr(q15$V4,3,nchar(q15$V4)-1))
q15$bx <- as.numeric(substr(q15$V9,3,nchar(q15$V9)-1))
q15$by <- as.numeric(substr(q15$V10,3,nchar(q15$V10)))
q15 <- q15[c("sx","sy","bx","by")]
q15$dist <- abs(q15$bx - q15$sx) + abs(q15$by - q15$sy)
#q15$maxradius <- q15$dist * 2 + 1
targety <- 2000000
q15$ydiff <- abs(q15$sy - targety)
q15$targetdist <- q15$dist - q15$ydiff

data <- q15[q15$targetdist>0,]

blankedx <- c()

for (i in 1:nrow(data)) {
  startx <- data[i,]$sx - data[i,]$targetdist
  endx   <- data[i,]$sx + data[i,]$targetdist
  cat(startx,endx,"\n")
  blankedx <- c(blankedx,startx:endx)
}

a15 <- length(unique(blankedx))-1
