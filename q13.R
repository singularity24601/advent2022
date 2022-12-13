rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q13 <- read.csv("q13.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...
q13a <- q13[seq(1,nrow(q13),3),]
q13b <- q13[seq(2,nrow(q13),3),]
knownorder <- rep(0,length(q13a))
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
  cat("INPUT = ",left," vs ",right,"\n")
  if (isnothing(right) & !isnothing(left)) {
    if (knownorder[currentpair] == 0) {    
      message("*RIGHT HAS RUN OUT* = not right order")
      knownorder[currentpair] <<- 1
    } else {
      cat("*RIGHT HAS RUN OUT*")
    }
  } else if (!isnothing(right) & isnothing(left)) {
    if (knownorder[currentpair] == 0) {
      message("*LEFT HAS RUN OUT* = right order")
      knownorder[currentpair] <<- -1
    } else {
      cat("*LEFT HAS RUN OUT*")
    }
  } else if (isnothing(right) & isnothing(left)) {
    #stop("wtf")
    #if (knownorder[currentpair] == 0) {
    #  message("*LEFT HAS RUN OUT (so has right)* = right order")
    #  knownorder[currentpair] <<- -1
    #} else {
      cat("*LEFT HAS RUN OUT (so has right)*")
    #}
  } else if (isunitaryint(left) & isunitaryint(right)) { #else if either arg is NA...
    if (as.numeric(right) < as.numeric(left)) {
      if (knownorder[currentpair] == 0) {
        message("*RIGHT IS SMALLER* = not right order")
        knownorder[currentpair] <<- 1
      } else {
        cat("*RIGHT IS SMALLER*")
      }
    } else if (as.numeric(right) > as.numeric(left)) {
      if (knownorder[currentpair] == 0) {
        message("*LEFT IS SMALLER* = right order")
        knownorder[currentpair] <<- -1
      } else {
        cat("*LEFT IS SMALLER*")
      }
    } else if (as.numeric(right) == as.numeric(left)) {
      cat("*SAME, keep going*")
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
  cat("\n")
}

message("EXAMPLE 1")
currentpair <- 1
compare("[1,1,3,1,1]","[1,1,5,1,1]")
message("EXAMPLE 2")
currentpair <- 2
compare("[[1],[2,3,4]]","[[1],4]")
message("EXAMPLE 3")
currentpair <- 3
compare("[9]","[[8,7,6]]")
message("EXAMPLE 4")
currentpair <- 4
compare("[[4,4],4,4]","[[4,4],4,4,4]")
message("EXAMPLE 5")
currentpair <- 5
compare("[7,7,7,7]","[7,7,7]")
message("EXAMPLE 6")
currentpair <- 6
compare("[]","[3]")
message("EXAMPLE 7")
currentpair <- 7
compare("[[[]]]","[[]]")
message("EXAMPLE 8")
currentpair <- 8
compare("[1,[2,[3,[4,[5,6,7]]]],8,9]","[1,[2,[3,[4,[5,6,0]]]],8,9]")

knownorder <- rep(0,length(q13a))
currentpair <- 1

for (i in 1:150) {
  currentpair <- i
  message(currentpair,"\n")
  compare(q13a[i],q13b[i])
}

a13 <- sum(1:150 * (knownorder == -1))
