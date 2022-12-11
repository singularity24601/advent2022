
rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q1 <- read.csv("q1.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...


q11items <- list()
q11items[["0"]] <- (c(99, 67, 92, 61, 83, 64, 98))
q11items[["1"]] <- (c(78, 74, 88, 89, 50))
q11items[["2"]] <- (c(98, 91))
q11items[["3"]] <- (c(59, 72, 94, 91, 79, 88, 94, 51))
q11items[["4"]] <- (c(95, 72, 78))
q11items[["5"]] <- (c(76))
q11items[["6"]] <- (c(69, 60, 53, 89, 71, 88))
q11items[["7"]] <- (c(72, 54, 63, 80))
# I reference entries in the list by strings because R lists have weird behaviour if I reference them by number
# 1. I can't have an entry with index 0
# 2. If one monkey runs out of items, it disappears completely and the remaining monkeys get renumbered

q11checks <- list()
for (i in 0:7) {
  q11checks[[toString(i)]] <- 0  
}

q11functions <- list()
q11functions[["0"]] <- function(x) {return(x*17)}
q11functions[["1"]] <- function(x) {return(x*11)}
q11functions[["2"]] <- function(x) {return(x+4)}
q11functions[["3"]] <- function(x) {return(x*x)}
q11functions[["4"]] <- function(x) {return(x+7)}
q11functions[["5"]] <- function(x) {return(x+8)}
q11functions[["6"]] <- function(x) {return(x+5)}
q11functions[["7"]] <- function(x) {return(x+3)}

q11tests <- list()
q11tests[["0"]] <- function(x) {return(x %% 3 == 0)}
q11tests[["1"]] <- function(x) {return(x %% 5 == 0)}
q11tests[["2"]] <- function(x) {return(x %% 2 == 0)}
q11tests[["3"]] <- function(x) {return(x %% 13 == 0)}
q11tests[["4"]] <- function(x) {return(x %% 11 == 0)}
q11tests[["5"]] <- function(x) {return(x %% 17 == 0)}
q11tests[["6"]] <- function(x) {return(x %% 19 == 0)}
q11tests[["7"]] <- function(x) {return(x %% 7 == 0)}


q11m <- function(self="0",option1="4",option2="2") {
  if (length(q11items[[self]]) == 0) {return()}
  for (i in 1:length(q11items[[self]])) { #inspect each item
    q11checks[[self]] <<- q11checks[[self]] + 1  
    q11items[[self]][i] <<- q11functions[[self]](q11items[[self]][i]) 
    q11items[[self]][i] <<- floor(q11items[[self]][i]/3)
    if (q11tests[[self]](q11items[[self]][i])) {
      q11items[[option1]] <<- c(q11items[[option1]],q11items[[self]][i])
    } else {
      q11items[[option2]] <<- c(q11items[[option2]],q11items[[self]][i])
    }
  }
  q11items[[self]] <<- c() #no items left
}

for (i in 1:20) {
  q11m("0","4","2")
  q11m("1","3","5")
  q11m("2","6","4")
  q11m("3","0","5")
  q11m("4","7","6")
  q11m("5","0","2")
  q11m("6","7","1")
  q11m("7","1","3")
}

a11 <- rev(sort(unlist(q11checks)))
a11 <- a11[1]*a11[2]

q11m2 <- function(self="0",option1="4",option2="2") {
  if (length(q11items[[self]]) == 0) {return()}
  for (i in 1:length(q11items[[self]])) { #inspect each item
    q11checks[[self]] <<- q11checks[[self]] + 1  
    q11items[[self]][i] <<- q11functions[[self]](q11items[[self]][i]) 
    q11items[[self]][i] <<- q11items[[self]][i] %% 9699690
    if (q11tests[[self]](q11items[[self]][i])) {
      q11items[[option1]] <<- c(q11items[[option1]],q11items[[self]][i])
    } else {
      q11items[[option2]] <<- c(q11items[[option2]],q11items[[self]][i])
    }
  }
  q11items[[self]] <<- c() #no items left
}

for (i in 0:7) {
  q11checks[[toString(i)]] <- 0  
}

q11items[["0"]] <- (c(99, 67, 92, 61, 83, 64, 98))
q11items[["1"]] <- (c(78, 74, 88, 89, 50))
q11items[["2"]] <- (c(98, 91))
q11items[["3"]] <- (c(59, 72, 94, 91, 79, 88, 94, 51))
q11items[["4"]] <- (c(95, 72, 78))
q11items[["5"]] <- (c(76))
q11items[["6"]] <- (c(69, 60, 53, 89, 71, 88))
q11items[["7"]] <- (c(72, 54, 63, 80))

for (i in 1:10000) {
  q11m2("0","4","2")
  q11m2("1","3","5")
  q11m2("2","6","4")
  q11m2("3","0","5")
  q11m2("4","7","6")
  q11m2("5","0","2")
  q11m2("6","7","1")
  q11m2("7","1","3")
}

a11b <- rev(sort(unlist(q11checks)))
a11b <- a11b[1]*a11b[2]
