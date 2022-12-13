
rm(list=ls(all = TRUE))
#setwd("/mnt/c/Users/singu/OneDrive/WEHI projects etc/Advent22")
setwd("C:/Users/singu/OneDrive/WEHI projects etc/Advent22")
q1 <- read.csv("q1.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
t <- list() #a list of all my temporary variables because I don't want the workspace too cluttered...
t$total <- 0
a1 <- 0
for (i in 1:nrow(q1)) {
  if (is.na(q1[i,])) {
    a1 <- max(t$total,a1)
    t$total <- 0
  } else {
    t$total <- t$total + q1[i,]    
  }
}
t$total <- 0
t$firstplace <- a1
a1b <- 0
for (i in 1:nrow(q1)) {
  if (is.na(q1[i,])) {
    if (t$total == t$firstplace) {
      t$total <- 0
      t$firstplace <- 0
    } else {
      a1b <- max(t$total,a1b)
      t$total <- 0
    }
  } else {
    t$total <- t$total + q1[i,]    
  }
}
t$total <- 0
t$firstplace <- a1
t$secondplace <- a1b
a1c <- 0
for (i in 1:nrow(q1)) {
  if (is.na(q1[i,])) {
    if (t$total == t$firstplace) {
      t$total <- 0
      t$firstplace <- 0
    } else if (t$total == t$secondplace) {
      t$total <- 0
      t$secondplace <- 0
    } else {
      a1c <- max(t$total,a1c)
      t$total <- 0
    }
  } else {
    t$total <- t$total + q1[i,]    
  }
}
a1b <- a1+a1b+a1c
rm(a1c)
q2 <- read.csv("q2.txt",sep=" ",header=F,stringsAsFactors=F)
#golf
#S=function(v,x=read.csv("2",F," "))sum(diag(matrix(v,3)[x[,1],x[,2]]))
#S(c(4,1,7,8,5,2,3,9,6))
#S(c(3,1,2,4,5,6,8,9,7))
#/golf
a2 <- sum(q2$V2 == "X") + sum(q2$V2 == "Y") * 2 + sum(q2$V2 == "Z") * 3 +
  sum(q2$V1 == "A" & q2$V2 == "X") * 3 +
  sum(q2$V1 == "B" & q2$V2 == "Y") * 3 +
  sum(q2$V1 == "C" & q2$V2 == "Z") * 3 +
  sum(q2$V1 == "A" & q2$V2 == "Y") * 6 +
  sum(q2$V1 == "B" & q2$V2 == "Z") * 6 +
  sum(q2$V1 == "C" & q2$V2 == "X") * 6
a2b <-sum(q2$V2 == "Y") * 3 + sum(q2$V2 == "Z") * 6 +
  sum(q2$V1 == "A" & q2$V2 == "X") * 3 +
  sum(q2$V1 == "A" & q2$V2 == "Y") * 1 +
  sum(q2$V1 == "A" & q2$V2 == "Z") * 2 +
  sum(q2$V1 == "B" & q2$V2 == "X") * 1 +
  sum(q2$V1 == "B" & q2$V2 == "Y") * 2 +
  sum(q2$V1 == "B" & q2$V2 == "Z") * 3 +
  sum(q2$V1 == "C" & q2$V2 == "X") * 2 +
  sum(q2$V1 == "C" & q2$V2 == "Y") * 3 +
  sum(q2$V1 == "C" & q2$V2 == "Z") * 1 
q3 <- read.csv("q3.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
a3 <- c()
for (i in 1:nrow(q3)) {
  t$currentstring <- q3[i,]
  t$length <- nchar(t$currentstring)
  t$a <- substr(t$currentstring,1,t$length/2)
  t$b <- substr(t$currentstring,t$length/2+1,t$length)
  t$intersect <- utf8ToInt(intersect(unlist(strsplit(t$a,"")),unlist(strsplit(t$b,"")))[1])
  if (t$intersect >= 97 & t$intersect <= 122) {
    a3 <- c(a3,t$intersect-96)      
  } else {
    a3 <- c(a3,t$intersect-38) 
  }
}
a3b <- c()
t$counter <- 0
for (i in 1:nrow(q3)) {
  t$counter <- t$counter + 1
  t$currentstring <- q3[i,]
  if (t$counter == 1) t$temp1 <- unlist(strsplit(t$currentstring,""))
  if (t$counter == 2) t$temp2 <- unlist(strsplit(t$currentstring,""))
  if (t$counter == 3) t$temp3 <- unlist(strsplit(t$currentstring,""))
  if (t$counter==3) {
    t$counter<-0
    t$intersect <- utf8ToInt(intersect(intersect(t$temp1,t$temp2),t$temp3)) 
    if (t$intersect >= 97 & t$intersect <= 122) {
      a3b <- c(a3b,t$intersect-96)      
    } else {
      a3b <- c(a3b,t$intersect-38) 
    }
  }
}
a3 <- sum(a3)
a3b <- sum(a3b)
q4 <- read.csv("q4.txt",sep=",",header=F,stringsAsFactors=F,blank.lines.skip=F)
a4 <- 0
a4b <- 0
for (i in 1:nrow(q4)) {
  t$a <- as.numeric(unlist(strsplit(q4$V1[i],split="-")))
  t$b <- as.numeric(unlist(strsplit(q4$V2[i],split="-")))
  if ((t$a[1] >= t$b[1] &  t$a[2] <= t$b[2]) | (t$b[1] >= t$a[1] &  t$b[2] <= t$a[2])) {
    a4 <- a4 + 1
  }
  if ((t$a[2] >= t$b[1] & t$a[2] <= t$b[2]) | (t$a[1] >= t$b[1] & t$a[1] <= t$b[2]) |
      (t$b[2] >= t$a[1] & t$b[2] <= t$a[2]) | (t$b[1] >= t$a[1] & t$b[1] <= t$a[2])) {
    a4b <- a4b + 1
  }
}
q5s <- read.csv("q5.txt",nrow=8,stringsAsFactors=F,header=F)
q5d <- list()
q5d[[1]] <- substr(q5s[1:8,],2,2)[substr(q5s[1:8,],2,2)!=" "]
q5d[[2]] <- substr(q5s[1:8,],6,6)[substr(q5s[1:8,],6,6)!=" "]
q5d[[3]] <- substr(q5s[1:8,],10,10)[substr(q5s[1:8,],10,10)!=" "]
q5d[[4]] <- substr(q5s[1:8,],14,14)[substr(q5s[1:8,],14,14)!=" "]
q5d[[5]] <- substr(q5s[1:8,],18,18)[substr(q5s[1:8,],18,18)!=" "]
q5d[[6]] <- substr(q5s[1:8,],22,22)[substr(q5s[1:8,],22,22)!=" "]
q5d[[7]] <- substr(q5s[1:8,],26,26)[substr(q5s[1:8,],26,26)!=" "]
q5d[[8]] <- substr(q5s[1:8,],30,30)[substr(q5s[1:8,],30,30)!=" "]
q5d[[9]] <- substr(q5s[1:8,],34,34)[substr(q5s[1:8,],34,34)!=" "]
q5d2 <- q5d
q5i <- read.csv("q5.txt",sep=" ",skip=8,stringsAsFactors=F)[c(2,4,6)]
for (i in 1:nrow(q5i)) {
  t$n <- q5i[i,1]
  t$from <- q5i[i,2]
  t$to <- q5i[i,3]
  for (j in 1:t$n) {
    q5d[[t$to]] <- c(q5d[[t$from]][1],q5d[[t$to]])    #top box
    q5d[[t$from]] <- q5d[[t$from]][2:length(q5d[[t$from]])]
    for (k in 1:9) {q5d[[k]] <- q5d[[k]][!is.na(q5d[[k]])]    } #cleanup
  }
}
a5 <- c(q5d[[1]][1],q5d[[2]][1],q5d[[3]][1],q5d[[4]][1],q5d[[5]][1],q5d[[6]][1],q5d[[7]][1],q5d[[8]][1],q5d[[9]][1])
q5d <- q5d2
rm(q5d2)
for (i in 1:nrow(q5i)) {
  t$n <- q5i[i,1]
  t$from <- q5i[i,2]
  t$to <- q5i[i,3]
  t$movee <- q5d[[t$from]][1:t$n]
  q5d[[t$to]] <- c(t$movee,q5d[[t$to]])
  q5d[[t$from]] <- q5d[[t$from]][(1+length(t$movee)):length(q5d[[t$from]])]
  for (k in 1:9) {q5d[[k]] <- q5d[[k]][!is.na(q5d[[k]])]    } #cleanup
}
a5b <- c(q5d[[1]][1],q5d[[2]][1],q5d[[3]][1],q5d[[4]][1],q5d[[5]][1],q5d[[6]][1],q5d[[7]][1],q5d[[8]][1],q5d[[9]][1])
q6 <- unlist(strsplit(read.csv("q6.txt",stringsAsFactors=F,header=F)[1,1],""))
a6 <- 0
for (i in 1:(length(q6)-3)) {
  t$window <- q6[i:(i+3)]
  a6 <- i+3
  if (length(t$window) == length(unique(t$window))) {
    break()
  }
}
a6b <- 0
for (i in 1:(length(q6)-13)) {
  t$window <- q6[i:(i+13)]
  a6b <- i+13
  if (length(t$window) == length(unique(t$window))) {
    break()
  }
}
q7 <- read.csv("q7.txt",stringsAsFactors=F,header=F)
for (i in 1:nrow(q7)) {
  t$currentline <- q7[i,]
  if (substr(t$currentline,1,4) == "$ ls" | substr(t$currentline,1,6) == "$ cd /") {
    q7[i,] <- ""
  } else if (substr(t$currentline,1,1) == "$") {
    t$newline <- substr(t$currentline,3,nchar(t$currentline))
    q7[i,] <- t$newline
  } else if (substr(t$currentline,1,3) == "dir") {
    t$input <- unlist(strsplit(t$currentline," "))
    t$newline <- paste0("mkdir ",t$input[2])
    q7[i,] <- t$newline
  } else {
    t$input <- unlist(strsplit(t$currentline," "))
    t$filename <- t$input[2]
    t$filesize <- t$input[1]
    t$newline <- paste0("echo '",t$filesize,"' > ",t$filename)
    q7[i,] <- t$newline
  }
}
q7 <- data.frame("V1"=q7$V1[q7$V1 != ""])
write.table(q7,"q7.sh",col.names=F,row.names=F,quote=F)
# dos2unix q7.sh
# . q7.sh
t$dirlist <- list.dirs(path="q7")
a7 <- c()
for (i in t$dirlist) {
  t$filelist <- list.files(path=i,recursive=T,include.dirs=F,full.names=T)
  t$total <- 0
  for (j in t$filelist) {
    t$total <- t$total + as.numeric(read.csv(j,header=F))
  }
  a7 <- c(a7,t$total)
}
a7 <- sort(a7,decreasing=T)
a7b <- a7
a7 <- sum(a7[a7 <= 100000])
t$target <- 7e7-max(a7b) #free space
t$target <- 3e7 - t$target
a7b <- min(a7b[a7b > t$target])

q8 <- read.csv("q8.txt",header=F,numerals="no.loss")
q8m <- list()
for (i in 1:nrow(q8)) {
  q8m[[i]] <- as.numeric(unlist(strsplit(q8[i,],"")  ))
}
q8m <- matrix(unlist(q8m),length(q8m[[1]]))
a8 <- matrix(0,nrow=nrow(q8m),ncol=ncol(q8m))
for ( i in 1:ncol(a8)) {
  a8[1,i] <- 1
  a8[nrow(a8),i] <- 1
}
for ( i in 1:nrow(a8)) {
  a8[i,1] <- 1
  a8[i,ncol(a8)] <- 1
}
for (i in 2:(ncol(a8)-1)) { #downward rays
  t$maxheight <- q8m[1,i]
  for (j in 2:(nrow(a8-1))) {
    if (q8m[j,i] > t$maxheight) {
      a8[j,i] <- 1
      t$maxheight <- q8m[j,i]
    } 
  } 
} 
cat(sum(a8),"")
for (i in 2:(ncol(a8)-1)) { #upward rays
  t$maxheight <- q8m[nrow(a8),i]
  for (j in rev(2:(nrow(a8-1)))  ) {
    if (q8m[j,i] > t$maxheight) {
      a8[j,i] <- 1
      t$maxheight <- q8m[j,i]
    } 
  } 
} 
cat(sum(a8),"")
for (i in 2:(nrow(a8)-1)) { #fromleft rays
  t$maxheight <- q8m[i,1]
  for (j in 2:(ncol(a8-1))  ) {
    if (q8m[i,j] > t$maxheight) {
      a8[i,j] <- 1
      t$maxheight <- q8m[i,j]
    } 
  } 
} 
cat(sum(a8),"")
for (i in 2:(nrow(a8)-1)) { #fromright rays
  t$maxheight <- q8m[i,ncol(a8)]
  for (j in rev(2:(ncol(a8-1)))  ) {
    if (q8m[i,j] > t$maxheight) {
      a8[i,j] <- 1
      t$maxheight <- q8m[i,j]
    } 
  } 
} 
cat(sum(a8),"")
f8 <- function(data,i,j) {
 initialheight <- data[i,j]
 #lookleft
 left <- 0
 if (i == 1) {} else {
   for (k in 1:(i-1)) { #lookdistance
     left <- left + 1
     if ((data[i-k,j]) >= initialheight) {break()}
   }
 }
 #lookright
 right <- 0
 if (i == ncol(data)) {} else {
   for (k in 1:(ncol(data)-i)) { #lookdistance
     right <- right + 1
     if ((data[i+k,j]) >= initialheight) {break()}
   }
 }
 #lookup
 up <- 0
 if (j == 1) {} else {
   for (k in 1:(j-1)) { #lookdistance
     up <- up + 1
     if ((data[i,j-k]) >= initialheight) {break()}
   }
 }
 #lookdown
 down <- 0
 if (j == nrow(data)) {} else {
   for (k in 1:(nrow(data)-j)) { #lookdistance
     down <- down + 1
     if ((data[i,j+k]) >= initialheight) {break()}
   }
 }
 return(left*right*up*down)
}
a8b<-0
for (i in 1:nrow(a8)) {
  for (j in 1:ncol(a8)) {
    a8b <- max(a8b,f8(q8m,i,j))
  } 
}
q9 <- read.csv("q9.txt",header=F,sep=" ")
q9a <- c()
for (i in 1:nrow(q9)) {
  q9a <- c(q9a,rep(q9[i,1],q9[i,2]))
}
a9 <- c()
t$headx <- 0
t$heady <- 0
t$tailx <- 0
t$taily <- 0
f9 <- function(dir) {
  if (dir == "U") {
    t$heady <<- t$heady + 1
  } else if (dir == "D") {
    t$heady <<- t$heady - 1
  } else if (dir == "L") {
    t$headx <<- t$headx - 1
  } else if (dir == "R") {
    t$headx <<- t$headx + 1
  }
  if (abs(t$heady - t$taily) >= 2 | abs(t$headx - t$tailx) >= 2) {
    if (t$tailx > t$headx) {t$tailx <<- t$tailx - 1}
    if (t$tailx < t$headx) {t$tailx <<- t$tailx + 1}
    if (t$taily > t$heady) {t$taily <<- t$taily - 1}
    if (t$taily < t$heady) {t$taily <<- t$taily + 1}
  }
  a9 <<- c(a9,paste0(t$tailx,",",t$taily))
}
for (i in 1:length(q9a)) {
  f9(q9a[i])
}
a9<-length(unique(a9))
a9b <- c()
t$tailx <- list()
t$taily <- list()
for (i in 1:10) {
  t$tailx[[i]] <- 0
  t$taily[[i]] <- 0
}
f9b <- function(dir) {
  if (dir == "U") {
    t$taily[[1]] <<- t$taily[[1]] + 1
  } else if (dir == "D") {
    t$taily[[1]] <<- t$taily[[1]] - 1
  } else if (dir == "L") {
    t$tailx[[1]] <<- t$tailx[[1]] - 1
  } else if (dir == "R") {
    t$tailx[[1]] <<- t$tailx[[1]] + 1
  }
  for (i in 2:10) {
    if (abs(t$taily[[i-1]] - t$taily[[i]]) >= 2 | abs(t$tailx[[i-1]] - t$tailx[[i]]) >= 2) {
      if (t$tailx[[i]] > t$tailx[[i-1]]) {t$tailx[[i]] <<- t$tailx[[i]] - 1}
      if (t$tailx[[i]] < t$tailx[[i-1]]) {t$tailx[[i]] <<- t$tailx[[i]] + 1}
      if (t$taily[[i]] > t$taily[[i-1]]) {t$taily[[i]] <<- t$taily[[i]] - 1}
      if (t$taily[[i]] < t$taily[[i-1]]) {t$taily[[i]] <<- t$taily[[i]] + 1}
    }
  }
  a9b <<- c(a9b,paste0(t$tailx[[10]],",",t$taily[[10]]))
}
for (i in 1:length(q9a)) {
  f9b(q9a[i])
}
a9b<-length(unique(a9b))
q10<- read.csv("q10.txt",header=F,sep=" ")
t$max <- (2*sum(q10$V1=="addx")+sum(q10$V1=="noop"))
a10 <- data.frame(t=1:t$max,"x"=NA)
t$x <- 1
t$pointer <- 1
t$adding <- 0
for (i in 1:t$max) {
  if (is.na(q10[t$pointer,2])) {
    a10$x[i] <- t$x
    t$pointer <- t$pointer + 1
  } else if (t$adding) {
    a10$x[i] <- t$x
    t$x <- t$x + q10[t$pointer,2]

    t$pointer <- t$pointer + 1
    t$adding <- 0
  } else {
    t$adding <- 1  
    a10$x[i] <- t$x
  }
}
a10$str <- a10$t * a10$x
a10$t2 <- a10$t %% 40
a10$t2 <- (a10$t-1) %% 40 + 1
a10$spr <- abs((a10$x+1)-a10$t2)
a10$scr <- a10$spr <= 1
a10$scr[a10$scr==1] <- "#"
a10$scr[a10$scr=="FALSE"] <- ""
cat("\n")
print(t(matrix(a10$scr,nrow=40)))