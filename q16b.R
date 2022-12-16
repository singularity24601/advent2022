rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
library(stringr)

q16 <- read.csv("q16.txt",sep="@",header=F,stringsAsFactors=F,blank.lines.skip=F)

flowrates <- list()
connects <- list()
names <- list()
opened <- list()

for (i in 1:nrow(q16)) {
  valvename <- substr(q16[i,],7,8)
  flowrate <- substr(q16[i,],24,25)
  if (substr(flowrate,2,2) == ";") {flowrate <- substr(flowrate,1,1)}
  flowrate <- as.numeric(flowrate)
  tunnels <- substr(q16[i,],49,nchar(q16[i,]))
  if (substr(tunnels,1,1) == "s") {tunnels <- substr(tunnels,2,nchar(tunnels))}
  tunnels <- str_trim(unlist(strsplit(tunnels,split=",")))
  #cat(valvename,flowrate,tunnels,"\n",sep=":")
  names[[valvename]] <- valvename
  flowrates[[valvename]] <- flowrate
  connects[[valvename]] <- tunnels
  if (flowrate > 0) {
    opened[[valvename]] <- FALSE
  } else {
    opened[[valvename]] <- TRUE #useless valves
  }
}

distancefrom <- list()
for (i in names) {
  distancefrom[[i]] <- list()
}

crawler <- function(origin,currentlocation,generation) {
  if (is.null(distancefrom[[origin]][[currentlocation]])) { #undiscovered node
    distancefrom[[origin]][[currentlocation]] <<- generation #min(generation,distancefrom[[origin]][[currentlocation]])      
    destinations <- connects[[currentlocation]]
    for (destination in destinations) {
      crawler(origin,destination,generation+1)
    }
  } else if (distancefrom[[origin]][[currentlocation]] > generation) { #more efficient path
    distancefrom[[origin]][[currentlocation]] <<- generation #min(generation,distancefrom[[origin]][[currentlocation]])      
    destinations <- connects[[currentlocation]]
    for (destination in destinations) {
      crawler(origin,destination,generation+1)
    }
  }
}

cat("mapping distances...\n")
for (name in names) {
  crawler(name,name,0)
}

currentminutes <- 30

targetvalves <- names[opened == FALSE]

#MAP POSSIBILITIES
library(combinat)
library(gtools)
candidatestrings <- c()

remaining <- function(currenttime,opened) { #old pruning system, doesn't cut it
  ans <- sum(unlist(flowrates[unlist(names[opened == FALSE])]))
  return(ans*(26-currenttime))
}

remainingV2 <- function(yournewtime,elephantnewtime,yournewlocation,elephantnewlocation,newopened) { #new pruning system

  sum <- 0
  
  for (name in names[newopened == FALSE]) {
    yournewertime <- yournewtime+distancefrom[[yournewlocation]][[name]] + 1
    yourbest <- (26-yournewertime)*flowrates[[name]]
    
    elephantnewertime <- elephantnewtime + distancefrom[[elephantnewlocation]][[name]] + 1
    elephantbest <- (26-elephantnewertime)*flowrates[[name]]

    sum <- sum + max(yourbest,elephantbest,0) 
  }

  return(sum)

}

record <- 2323

explorer <- function(yourstring,elephantstring,yourtime,elephanttime,opened,expectedscore) {
  #locations are the last 2 characters of each string
  yourlocation <- substr(yourstring,nchar(yourstring)-1,nchar(yourstring))
  elephantlocation <- substr(elephantstring,nchar(elephantstring)-1,nchar(elephantstring))

  targetvalves <- names[opened == FALSE]

  if (length(targetvalves) == 1) {
    options <- matrix(rep(targetvalves[1],2))
  } else {
    if (yourlocation == "AA" & elephantlocation == "AA") {
      options <- combn(targetvalves,2) #symmetry at start
    } else {
      options <- t(permutations(length(targetvalves),2,unlist(targetvalves))) #combn(targetvalves,2)
    }
  }
  #print(options)
  if (class(options)[1] == "character") {
    options <- matrix(options,ncol=1)
  }
  
  viableoptions <- c()

  for (i in 1:ncol(options)) { #consider each combination
    slice <- options[,i]
    yournewlocation <- slice[1]
    yournewtime <- yourtime + distancefrom[[yourlocation]][[yournewlocation]] + 1 #no point going unless open valve
    elephantnewlocation <- slice[2]
    elephantnewtime <- elephanttime + distancefrom[[elephantlocation]][[elephantnewlocation]] + 1 #no point going unless open valve
    if (yournewtime >= 26 & elephantnewtime >= 26) { #no point opening valve unless time to release
      #do nothing
    } else if (yournewtime < 26 & elephantnewtime >= 26) {
      yourwinnings <- flowrates[[yournewlocation]]*(26-yournewtime)
      elephantwinnings <- 0
      newexpectedscore <- expectedscore + yourwinnings + elephantwinnings
      record <<- max(record,newexpectedscore)
      viableoptions <- c(viableoptions,paste0(yournewlocation,":",elephantlocation,":",yournewtime,":",elephanttime+1,":",newexpectedscore))
    } else if (yournewtime >= 26 & elephantnewtime < 26) {
      yourwinnings <- 0
      elephantwinnings <- flowrates[[elephantnewlocation]]*(26-elephantnewtime)
      newexpectedscore <- expectedscore + yourwinnings + elephantwinnings
      record <<- max(record,newexpectedscore)
      viableoptions <- c(viableoptions,paste0(yourlocation,":",elephantnewlocation,":",yourtime+1,":",elephantnewtime,":",newexpectedscore))
    } else if (yournewtime < 26 & elephantnewtime < 26) {
      yourwinnings <- flowrates[[yournewlocation]]*(26-yournewtime)
      elephantwinnings <- flowrates[[elephantnewlocation]]*(26-elephantnewtime)
      newexpectedscore <- expectedscore + yourwinnings + elephantwinnings
      record <<- max(record,newexpectedscore)
      viableoptions <- c(viableoptions,paste0(yournewlocation,":",elephantnewlocation,":",yournewtime,":",elephantnewtime,":",newexpectedscore))
    }
  }
  viableoptions <- unique(viableoptions)
  
  if (length(viableoptions) == 0) {
    candidatestrings <<- c(candidatestrings,paste0(yourstring,":",elephantstring,":",expectedscore))
  } else {
    for (i in 1:length(viableoptions)) {
      newoption <-unlist(strsplit(viableoptions[i],":"))
      
      yournewlocation <- newoption[1]
      elephantnewlocation <- newoption[2]
      
      yournewstring <- paste0(yourstring,yournewlocation)
      elephantnewstring <- paste0(elephantstring,elephantnewlocation)
      
      yournewtime <- as.numeric(newoption[3])
      elephantnewtime <- as.numeric(newoption[4])
      
      newopened <- opened
      newopened[[yournewlocation]] <- TRUE
      newopened[[elephantnewlocation]] <- TRUE
      
      newexpectedscore <- as.numeric(newoption[5])
      
      newupforgrabs <- remainingV2(yournewtime,elephantnewtime,yournewlocation,elephantnewlocation,newopened)
      
      if (newexpectedscore + newupforgrabs > record) {
        explorer(yournewstring,elephantnewstring,yournewtime,elephantnewtime,newopened,newexpectedscore)
      } #else {
        #cat("futility:",newexpectedscore,newupforgrabs," =",newexpectedscore+newupforgrabs,"\n")
      #}
    }
  }
}

cat("exploring options...\n")
explorer("AA","AA",0,0,opened,0)
saveRDS(candidatestrings,"candidatestringsb.rds") #this never happens because of the predictive pruning method


