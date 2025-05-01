#' Title
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a the probability
#'
#' @returns graphs the curve givem the params
#' @export
#'
#' @examples
#' mycurve(5, 2, 7)
mycurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), ylab = "Normal Density",
        main = paste("Mu = ", mu, "sigma = ", sigma, "P(Y <= a, a = ", a), xlab = "Y")

  xcurve <- seq(mu-3*sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  prob <- round(pnorm(a, mean = mu, sd = sigma), 4)

  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col ="Grey", border = NA)
  text(mu, max(ycurve)/2, paste("P(X <=", a, ") =", prob), cex=1.2, col="black")

}

