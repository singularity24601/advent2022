
rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q1 <- read.csv("q1.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...


q11items <- list()
q11items[["zero"]] <- (c(99, 67, 92, 61, 83, 64, 98))
q11items[["one"]] <- (c(78, 74, 88, 89, 50))
q11items[["two"]] <- (c(98, 91))
q11items[["three"]] <- (c(59, 72, 94, 91, 79, 88, 94, 51))
q11items[["four"]] <- (c(95, 72, 78))
q11items[["five"]] <- (c(76))
q11items[["six"]] <- (c(69, 60, 53, 89, 71, 88))
q11items[["seven"]] <- (c(72, 54, 63, 80))

q11checks <- list()
q11checks[["zero"]] <- 0
q11checks[["one"]] <- 0
q11checks[["two"]] <- 0
q11checks[["three"]] <- 0
q11checks[["four"]] <- 0
q11checks[["five"]] <- 0
q11checks[["six"]] <- 0
q11checks[["seven"]] <- 0

q11functions <- list()
q11functions[["zero"]] <- function(x) {return(x*17)}
q11functions[["one"]] <- function(x) {return(x*11)}
q11functions[["two"]] <- function(x) {return(x+4)}
q11functions[["three"]] <- function(x) {return(x*x)}
q11functions[["four"]] <- function(x) {return(x+7)}
q11functions[["five"]] <- function(x) {return(x+8)}
q11functions[["six"]] <- function(x) {return(x+5)}
q11functions[["seven"]] <- function(x) {return(x+3)}

q11tests <- list()
q11tests[["zero"]] <- function(x) {return(x %% 3 == 0)}
q11tests[["one"]] <- function(x) {return(x %% 5 == 0)}
q11tests[["two"]] <- function(x) {return(x %% 2 == 0)}
q11tests[["three"]] <- function(x) {return(x %% 13 == 0)}
q11tests[["four"]] <- function(x) {return(x %% 11 == 0)}
q11tests[["five"]] <- function(x) {return(x %% 17 == 0)}
q11tests[["six"]] <- function(x) {return(x %% 19 == 0)}
q11tests[["seven"]] <- function(x) {return(x %% 7 == 0)}

q11mods <- list()
q11mods[["zero"]] <- 3
q11mods[["one"]] <- 5
q11mods[["two"]] <- 2
q11mods[["three"]] <- 13
q11mods[["four"]] <- 11
q11mods[["five"]] <- 17
q11mods[["six"]] <- 19
q11mods[["seven"]] <- 7


q11m <- function(self="zero",option1="four",option2="two") {
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
  q11m("zero","four","two")
  q11m("one","three","five")
  q11m("two","six","four")
  q11m("three","zero","five")
  q11m("four","seven","six")
  q11m("five","zero","two")
  q11m("six","seven","one")
  q11m("seven","one","three")
}

a11 <- sort(unlist(q11checks))

q11items[["zero"]] <- (c(99, 67, 92, 61, 83, 64, 98))
q11items[["one"]] <- (c(78, 74, 88, 89, 50))
q11items[["two"]] <- (c(98, 91))
q11items[["three"]] <- (c(59, 72, 94, 91, 79, 88, 94, 51))
q11items[["four"]] <- (c(95, 72, 78))
q11items[["five"]] <- (c(76))
q11items[["six"]] <- (c(69, 60, 53, 89, 71, 88))
q11items[["seven"]] <- (c(72, 54, 63, 80))

q11m2 <- function(self="zero",option1="four",option2="two") {
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

q11checks[["zero"]] <- 0
q11checks[["one"]] <- 0
q11checks[["two"]] <- 0
q11checks[["three"]] <- 0
q11checks[["four"]] <- 0
q11checks[["five"]] <- 0
q11checks[["six"]] <- 0
q11checks[["seven"]] <- 0

cat("s")

for (i in 1:10000) {
  q11m2("zero","four","two")
  q11m2("one","three","five")
  q11m2("two","six","four")
  q11m2("three","zero","five")
  q11m2("four","seven","six")
  q11m2("five","zero","two")
  q11m2("six","seven","one")
  q11m2("seven","one","three")
  #print(q11items)
}

a11b <- sort(unlist(q11checks))
