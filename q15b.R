rm(list=ls(all = TRUE))

q15 <- read.csv("q15.txt",sep=" ",header=F,stringsAsFactors=F,blank.lines.skip=F)
q15$sx <- as.numeric(substr(q15$V3,3,nchar(q15$V3)-1))
q15$sy <- as.numeric(substr(q15$V4,3,nchar(q15$V4)-1))
q15$bx <- as.numeric(substr(q15$V9,3,nchar(q15$V9)-1))
q15$by <- as.numeric(substr(q15$V10,3,nchar(q15$V10)))
q15 <- q15[c("sx","sy","bx","by")]
q15$dist <- abs(q15$bx - q15$sx) + abs(q15$by - q15$sy)

bx <- q15$bx
by <- q15$by
sx <- q15$sx
sy <- q15$sy
dist <- q15$dist

checkline <- function(sx,sy,bx,by,dist,targety) {
  ydiff <- abs(sy - targety)
  targetdist <- dist - ydiff
  
  newx <- sx[targetdist>0]
  newdist <- targetdist[targetdist>0]
  
  startx <- newx - newdist
  startx[startx < 0] <- 0
  endx <- newx + newdist
  endx[endx > 4000000] <- 4000000
  startxo <- startx[order(startx)]
  endxo <- endx[order(startx)]
  left <- -1
  for (i in 1:length(startxo)) {
    if (startxo[i] > (left+1)) {
      cat("y =",targety,"x =",left+1)
      return(c(targety,left+1))
    } else {
      left <- max(left,endxo[i])
    }
  }
}

#for (i in 1:4000000) {
#  checkline(sx,sy,bx,by,dist,i)
#}

ans<-checkline(sx,sy,bx,by,dist,3349056)
ans <- ans[1]+ans[2]*4e6
