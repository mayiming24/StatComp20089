#' @title Rcpp functions.
#' @name test
#' @description just for test Rcpp function
#' @importFrom Rcpp evalCpp
#' @useDynLib StatComp20089
#' @examples
#' \dontrun{
#' cla(1,2,12)
#' }
NULL
#'
#'
#'
#' This is some descriptio of this function.
#' @title simple kenerl estimation function.
#' @description caculate the density of the specified bandwith
#'
#' @details the function can caculate the density of the specified bandwith
#'
#' @param y is sample data
#' @param h bandwidth
#' @param x the point you need estimate
#' @return density of x
#' @examples  \dontrun{
#' nkes(faithful$eruptions,0.5,3)
#' }
#' @export
nkes<- function(y,h,x){
  n <- length(y)
  ye <- 0
  for (i in 1:n){
    ye <- ye + as.numeric((x >= y[i] - h) & (x < y[i] + h))
  }
  ye <- ye / (2*h*n)
  return(ye)
}
#' This is some descriptio of this function.
#' @title plot naive kenel estimate
#'
#' @description plot the pic of naive kenel estimate
#'
#' @details plot the pic of naive kenel estimate with specified bandwith and selected number of estimated points.
#'
#' @param y is sample data
#'
#' @param N the number of estimate points
#'
#' @param h bandwith
#' @return a pic
#'
#' @examples  \dontrun{
#' nkesplot(faithful$eruptions,200,1)
#' }
#' @export
nkesplot<- function(y,N,h){
  y<- sort(y)
  x<- seq(y[1],y[length(y)],length.out = N)
  m <- length(x)
  n <- length(y)
  ye <- rep(0, m)
  for (i in 1:n){
    ye <- ye + as.numeric((x >= y[i] - h) & (x < y[i] + h))
  }
  ye <- ye / (2*h*n)
  plot(x,ye,type = 'l',ylab = 'density')
}
#' This is some descriptio of this function.
#' @title plot naive two demesions product kenel estimate
#'
#' @description plot naive two demesions product kenel estimate
#'
#' @details plot the pic of two demesions product kenel estimate with specified bandwith and selected number of estimated points.
#'
#' @param x is sample data and x must be a n rows 2colums matrix
#' @param h1 the bandwith of col1
#'
#' @param h2 the bandwith of col2
#' @param N1 the width of estimate of col1
#'
#' @param N2 the width of estimate of col2
#' @return a pic
#'
#' @examples  \dontrun{
#' n2kesplot(as.matrix(faithful),0.5,3,200,200)
#' }
#' @export
n2kesplot<- function(x,h1,h2,N1,N2){
  n <- nrow(x)
  x1<-sort(x[,1])
  x2<-sort(x[,2])
  xe<- numeric(N1)
  ye<- numeric(N2)
  xranges<- seq(x1[1],x1[length(x1)],length.out = N1)
  yranges<- seq(x2[1],x2[length(x2)],length.out = N2)
  for (i in 1:N1) {
    xe[i]<- sum(abs(x[,1]-rep(xranges[i],n))/h1 <1)/(n*h1)
  }
  for (i in 1:N2) {
    ye[i]<- sum(abs(x[,2]-rep(yranges[i],n))/h2 <1)/(n*h2)
  }
  esti_mult<- xe%*%t(ye)
  persp(xranges,yranges,esti_mult,phi = 20,theta = 30, col = "gray",border = 1)
}

