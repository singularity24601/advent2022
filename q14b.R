rm(list=ls(all = TRUE))
setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
library(plot.matrix)
library(Corbi)
#setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q14 <- read.csv("q14.txt",sep="W",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...

lines <- list()
nlines <- nrow(q14)

getx <- function(input) {
  commapos <- regexpr(",",input)[1]
  return(as.numeric(substr(input,1,commapos-1)))
}

gety <- function(input) {
  commapos <- regexpr(",",input)[1]
  return(as.numeric(substr(input,commapos+1,nchar(input))))
}

for (i in 1:nlines) {
  lines[[i]] <- unlist(strsplit(q14[i,],split=" -> "))
}

mapheight = 200
mapwidth = 1000
map1 <- matrix(0,nrow=mapheight,ncol=mapwidth)
penx <- 0
peny <- 0

movepen <- function(newx,newy,currentmap) {
  if (newx > penx) { #go right
    currentmap[newy,penx:newx] <- 1
  } else if (newx < penx) { #go left
    currentmap[newy,penx:newx] <- 1
  } else if (newy < peny) { #go up
    currentmap[newy:peny,newx] <- 1
  } else if (newy > peny) { #go down
    currentmap[peny:newy,newx] <- 1
  }
  penx <<- newx
  peny <<- newy
  return(currentmap)
}

for (i in 1:nlines) { #for (i in 1:nlines) {
  pathlength <- length(lines[[i]])
  for (j in 1:pathlength) { #for (j in 1:pathlength) {
    currentpos <- lines[[i]][j]
    #place down the pen
    if (j == 1) {
      penx <- getx(currentpos)
      peny <- gety(currentpos)
      map1[peny,penx] <- 1
    } else {
      newx <- getx(currentpos)
      newy <- gety(currentpos)
      map1 <- movepen(newx,newy,map1)
    }
  }
}

maxdepth <- 0
for (i in 1:mapheight) {
  if (sum(map1[i,]) > 0 ) {
    maxdepth <- i    
  }
}

map1[maxdepth+2,] <- 1

sandcount <- 0
sanddrop <- function(currentmap) {
  sandx <- 500
  sandy <- 0
  sandstatus <- "falldown"
  while (sandstatus != "stopped" & sandstatus != "gone") {
    if (sandy >= mapheight) {sandstatus <- "gone"}
    if (sandstatus == "falldown") {
      if (currentmap[sandy+1,sandx] == 0) {
        sandy <- sandy + 1 #successful falldown
      } else {
        sandstatus <- "fallleft"
      }
    } else if (sandstatus == "fallleft") {
      if (currentmap[sandy+1,sandx-1] == 0) {
        sandy <- sandy + 1
        sandx <- sandx - 1 #successful fallleft
        sandstatus <- "falldown"
      } else {
        sandstatus <- "fallright"
      }
    } else if (sandstatus == "fallright") {
      if (currentmap[sandy+1,sandx+1] == 0) {
        sandy <- sandy + 1
        sandx <- sandx + 1 #successful fallright
        sandstatus <- "falldown"
      } else {
        sandstatus <- "stopped"
        sandcount <<- sandcount + 1
        currentmap[sandy,sandx] <- 2
      }
    } else if (sandstatus == "stopped") {
 
      if (sandy == 0 & sandx == 500) {
        stop(paste0("source blocked after ",sandcount)) #this wasn't triggering properly...
      }
      #
    } else if (sandstatus == "gone") {
      stop(paste0("sand disappeared after ",sandcount))
    }
  }
  if (sandy == 0 & sandx == 500) {
    stop(paste0("source blocked after ",sandcount)) #...therefore, catch it here
  }
  return(currentmap)
}

for (k in 1:50000) {
  map1 <- sanddrop(map1)
}

plot(submatrix(map1,0:155,1:800),border=NA,asp=TRUE,col=c("black","blue","red"))

