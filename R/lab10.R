#' 95% confidence interval
#'
#' @param x sample
#'
#' @return 95% confidence interval for U from a single sample x
#' @export
#'
#' @examples
#' set.seed(23); x = rnorm(30,mean=10,sd=12)
#' myci(x)
myci = function(x){
  t=qt(0.95,length(x)-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(25)
  ci[2]=mean(x)+t*sd(x)/sqrt(25)
  ci
}
