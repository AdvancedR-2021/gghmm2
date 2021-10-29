#' @title Title
#'
#' @param Y 
#' @param D 
#' @param L 
#'
#' @return
#' @export
#'
#' @examples
EM_mix_2_poisson <- function(Y,D,L) {
  n = length(Y)
  condP <- function(Y,L,D) {
    (D*dpois(Y,L[1]))/(D* dpois(Y,L[1])+(1-D)*dpois(Y,L[2]))
  }
  
  for (i in c(1:10000)) {
    D = sum(condP(Y,L,D))/n
    L[1] = (sum(Y * condP(Y,L,D)))/(sum(condP(Y,L,D)))
    L[2] = (sum(Y * (1-condP(Y,L,D))))/(sum(1-condP(Y,L,D)))
  }
  print(paste("Delta is equal to ",round(D,2)))
  print(paste("Lambda is ",round(L[1],2)," and ",round(L[2],2)))
}


#A little test
#Y <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")[,2]
#D = 0.5
#L = c(200,10)
#n = length(Y)
#EM_mix_2_poisson(Y,D,L)