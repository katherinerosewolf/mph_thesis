# global_indices_rev1.r                            January 2014

# examples taken from Local Models for Spatial Analysis by C. D. Lloyd

rm(list=ls())

cat(" Results from global_indices.r --- ", format(Sys.time(),
    "%A, %d %B %Y"), "\n",
 "==================================================================\n" )

 # from p. 62 of 1st edition
 
 z.mat <- matrix(nrow=3, ncol=3, byrow=T,
             data=c(47, 48, 44, 42, 40, 38, 43, 37, 35) )
 z.mat
 
 z <- c(47, 48, 44, 42, 40, 38, 43, 37, 35)
 N <- length(z)
 
 z.ave <- mean(z)
 z.ssq <- sum(z^2) - N*z.ave^2
 
 rc <- matrix(nrow=9, ncol=3, byrow=F, 
              data=c(seq(1:9), 
              rep(1,3), rep(2,3), rep(3,3), rep(seq(1:3),3) ) )
 rc
 
 Z.ij <- matrix(nrow=9, ncol=9, byrow=T, data=rep(0, 81) )
 for(i in 1:9) {
   for(j in 1:9) Z.ij[i,j] <- (z[i] - z.ave) * (z[j] - z.ave)
  } 
round(Z.ij ,2)

Dist <- matrix(nrow=9, ncol=9, byrow=T, data=rep(0, 81) )
 for(i in 1:9) {
   ip1 <- i + 1
   for(j in ip1:9) {
     Dist[i,j] <- sqrt( (rc[i,2] - rc[j,2])^2 + (rc[i,3] - rc[j,3])^2 )
     Dist[j,i] <- Dist[i,j] 
  }  }
round(Dist, 2)

#---------------------------------------------------------------------------
W.rook <- matrix(nrow=9, ncol=9, byrow=T, data=rep(0, 81) )
 for(i in 1:9) {
   for(j in 1:9) W.rook[i,j] <- 
                        ifelse(Dist[i,j] > 0 & Dist[i,j] < 1.001, 1, 0)  } 
round(W.rook ,2)

sum(W.rook)

( I.rook <- sum(N * W.rook * Z.ij) / (sum(W.rook) * z.ssq) )

#-------------------------------------------------------------------------
W.bishop <- matrix(nrow=9, ncol=9, byrow=T, data=rep(0, 81) )
 for(i in 1:9) {
   for(j in 1:9) W.bishop[i,j] <- 
                          ifelse(Dist[i,j] > 1 & Dist[i,j] < 1.42, 1, 0)  } 

W.bishop
sum(W.bishop)

( I.bishop <- sum(N * W.bishop * Z.ij) / (sum(W.bishop) * z.ssq) )

#-------------------------------------------------------------------------

W.queen <- matrix(nrow=9, ncol=9, byrow=T, data=rep(0, 81) )
 for(i in 1:9) {
   for(j in 1:9) W.queen[i,j] <- 
                           ifelse(Dist[i,j] > 0 & Dist[i,j] < 1.5, 1, 0)  } 

W.queen
sum(W.queen)

( I.queen <- sum(N * W.queen * Z.ij) / (sum(W.queen) * z.ssq) )

#-------------------------------------------------------------------------