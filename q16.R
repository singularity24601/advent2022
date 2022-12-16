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

#for (location in c("AA",targetvalves)) {
#  currentlocation <- location  
#  for (targetvalve in targetvalves) {
#    targetdistance <- distancefrom[[currentlocation]][[targetvalve]]
#    targetflow <- flowrates[[targetvalve]]
#    expectedopentime <- currentminutes - targetdistance - 1
#    expectedrelease <- expectedopentime * targetflow
#    cat(currentlocation,"to",targetvalve,"dist:",targetdistance,"flow:",targetflow,"expected release:",expectedrelease,"\n")
#  }
#}

#MAP POSSIBILITIES
candidatestrings <- c()

explorer <- function(currentstring,timeleft,currentlocation,opened) {
  targetvalves <- names[opened == FALSE]
  targetvalves <- targetvalves[targetvalves != currentlocation]
  options <- targetvalves
  viableoptions <- c()
  viablecosts <- c()
  
  if (opened[[currentlocation]] == FALSE) {
    options <- c("oo")
    optioncost <- 1
    if (optioncost < timeleft) { #no point opening unless time to release
      viableoptions <- c(options)
      viablecosts <- c(optioncost)
    }
    #if (currentstring == "AACWooFGooQWooUPooFD") {cat(currentstring,timeleft,currentlocation,options,optioncost,"\n")}
  } else {
    for (option in options) {
      optioncost <- distancefrom[[currentlocation]][[option]]  
      if (optioncost + 1 < timeleft) { #pointless to go unless open a valve #no point opening unless time to release
        viableoptions <- c(viableoptions,option)
        viablecosts <- c(viablecosts,optioncost)
      }
      #if (substr(currentstring,1,6) == "AADXoo") {cat(currentstring,timeleft,option,optioncost,"\n")}
      #if (currentstring == "AACWooFGooQWooUPooFD") {cat(currentstring,timeleft,currentlocation,option,optioncost,"\n")}
    }
  }
  
  if (length(viableoptions) == 0) {
    candidatestrings <<- c(candidatestrings,currentstring)
    #cat("candidate:",currentstring,"\n")
  } else {
    for (i in 1:length(viableoptions)) {
      newopened <- opened
      newlocation <- viableoptions[i]
      newstring <- paste0(currentstring,viableoptions[i])
      newtime <- timeleft - viablecosts[i]
      if ((viableoptions[i]) == "oo") {
        newopened[[currentlocation]] <- TRUE
        newlocation <- currentlocation
      } 
      #cat("viable",viableoptions[i],viablecosts[i],"\n")
      explorer(newstring,newtime,newlocation,newopened)
    }
  }
}

cat("exploring options...\n")
explorer("AA",30,"AA",opened)
saveRDS(candidatestrings,"candidatestrings.rds")

#restart everything
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

candidatestrings <- readRDS("candidatestrings.rds")

releasepressure <- function(flowrates,opened) {
  openvalves <- names[opened == TRUE]
  sum <- 0
  for (openvalve in openvalves) {
    sum <- sum + flowrates[[openvalve]]
  }
  return(sum)
}

parsestring <- function(candidatestring,currentopened=opened,echo=FALSE) {
  #cat(candidatestring,":")
  newword <- substr(candidatestring,1,2)
  candidatestring <- substr(candidatestring,3,nchar(candidatestring))
  timeleft <- 30
  lastlocation <- "AA"
  release <- 0
  
  while(nchar(candidatestring) > 0) {
    newword <- substr(candidatestring,1,2)
    candidatestring <- substr(candidatestring,3,nchar(candidatestring))
    if (newword == "oo") {
      timeelapsed <- 1
    } else {
      timeelapsed <- distancefrom[[lastlocation]][[newword]]
    }
    timeleft <- timeleft - timeelapsed
    release <- release + releasepressure(flowrates,currentopened)*timeelapsed
    if (newword == "oo") {
      currentopened[[lastlocation]] <- TRUE
    } else {
      lastlocation <- newword
    }
    if (echo) {cat(newword,"current=",release,"rate=",releasepressure(flowrates,currentopened),"timeleft=",timeleft,"\n")}
  }

  release  <- release + releasepressure(flowrates,currentopened)*timeleft
  if (echo) {cat("TOTAL =",release,"\n")}
  if (timeleft < 0) {stop("shouldn't happen")}
  return(release)
}

parsestringV <- Vectorize(parsestring,vectorize.args="candidatestring")

parsestring("AACHooVQooSGooMLooATooBDoo",echo=TRUE)

cat("calculating outcomes...")
answers <- parsestringV(candidatestrings)

cat(max(answers))
#893 is too low
#1021 is too low
#1997 is too low
