rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q13b <- read.csv("q13.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=T)
q13b<-rbind(q13b,"[[2]]")
q13b<-rbind(q13b,"[[6]]")
q13b<-unlist(q13b)
row.names(q13b) <- NULL
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...

knownorder <- 0
currentpair <- 1

toplevelsplit <- function(input) {
  if (substr(input,1,1) == "[") {
    input <- substr(input,2,nchar(input)-1)
  }
  level <- 0
  for (i in 1:nchar(input)) {
    if (substr(input,i,i) == "[") {
      level <- level + 1
    } else if (substr(input,i,i) == "]") {
      level <- level - 1
    } else if (substr(input,i,i) == "," & level == 0) {
      input <- paste0(substr(input,1,i-1),";",substr(input,i+1,nchar(input)))
    } 
  }
  ans <- unlist(strsplit(input,split=";"))
  return(ans[ans != ";"])
}

isunitaryint <- function(input) {
  return(regexpr(",",input)[1] == -1 & substr(input,1,1) != "[")
}

isnothing <- function(input) {
  if (length(input) == 0) {
    return(TRUE)
  } else if (is.na(input)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

compare <- function(left,right) {
  if (isnothing(right) & !isnothing(left)) {
    if (knownorder[currentpair] == 0) {    
      knownorder[currentpair] <<- 1
    } else {
    }
  } else if (!isnothing(right) & isnothing(left)) {
    if (knownorder[currentpair] == 0) {
      knownorder[currentpair] <<- -1
    } else {
    }
  } else if (isnothing(right) & isnothing(left)) {
  } else if (isunitaryint(left) & isunitaryint(right)) { #else if either arg is NA...
    if (as.numeric(right) < as.numeric(left)) {
      if (knownorder[currentpair] == 0) {
        knownorder[currentpair] <<- 1
      } else {
      }
    } else if (as.numeric(right) > as.numeric(left)) {
      if (knownorder[currentpair] == 0) {
        knownorder[currentpair] <<- -1
      } else {
      }
    } else if (as.numeric(right) == as.numeric(left)) {
    }  
  } else {
    if (isunitaryint(left) & !isunitaryint(right)) {left <- paste0("[",left,"]")}
    if (isunitaryint(right) & !isunitaryint(left)) {right <- paste0("[",right,"]")}
    leftlist <- toplevelsplit(left)
    rightlist <- toplevelsplit(right)
    for (i in 1:max(length(leftlist),length(rightlist))) {
      compare(leftlist[i],rightlist[i])
    }
  }
}


isordered <- function(a,b) { #kludged from part A
  knownorder <<- 0
  currentpair <<- 1
  compare(a,b)
  if (knownorder == -1) {
    return(TRUE)
  } else if (knownorder == 1) {
    return(FALSE)
  } else {
    stop("wtf")
  }
}

bubble <- length(q13b)-1

while (bubble > 1) {
  for (i in 1:bubble) {
    a <- q13b[i]
    b <- q13b[i+1]
    if (!isordered(a,b)) {
      q13b[i] <- b
      q13b[i+1] <- a
    }
  }
  bubble <- bubble - 1  
  cat(bubble,"\n")
}

a13b <- sum(1:302*(q13b=="[[2]]"))*sum(1:302*(q13b=="[[6]]"))
