#' Title
#'
#' @param mu mu
#' @param sigma sigma
#' @param a a
#'
#'@importFrom graphics polygon
#'
#' @return Returns the curve
#' @export
#'
#' @examples myncurve(2,2,2)
myncurve = function(mu, sigma, a){
  x = NULL
  curve(dnorm(x,mean=mu,sd=sigma),xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma, mu + 3*sigma,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,mu + 3*sigma),c(0,ycurve,0),col="Red")

  pnorm(a, mu, sigma)
}
