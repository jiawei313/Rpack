#' histogram of sample means
#'
#' @param n number of binomial distribution
#' @param iter number of iterations
#' @param a min
#' @param b max
#'
#' @return histogram of sample means
#' @export
#'
#' @examples
#'
#' w=myclt(n=10,iter=10000)
myclt=function(n,iter,a=0,b=5){
  ## r-random sample from the uniform and set to y
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix and set to data
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix and set to sm
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}


