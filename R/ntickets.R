#' N Tickets Function for maximizing ticket sales
#'
#' @param N the number of seats available
#' @param gamma desired probability of overbooking
#' @param p probability that a passenger who purchased a ticket will show up
#'
#' @returns A discrete plot and a continuous plot, a list printing the discrete and continuous optimal values, the probability, and the gamma
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
#'
ntickets <- function(N, gamma, p){
  # Discrete calculation using qbinom
  n_vals <- N:(N + 50)
  qvals <- qbinom(1 - gamma, n_vals, p)
  nd <- n_vals[which(qvals == N)[1]]  # First value where qbinom returns N

  # Normal approximation
  #z <- qnorm(1 - gamma)
  #nc <- round((N - z * sqrt(p * (1 - p))) / p)

  # Solve N = np + z * sqrt(np(1 - p)) for n using numerical root finding
  z <- qnorm(1 - gamma)

  objective_continuous <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    return(mu + z * sigma - N)
  }

  # Use uniroot to solve objective == 0
  nc <- uniroot(objective_continuous, lower = N, upper = N + 50)$root

  #plotting the objective functions
  n_plot <- (N):(N + 30)
  discrete_obj <- 1 - gamma - pbinom(N, n_plot, p)
  continuous_obj <- 1 - gamma - pnorm(N, mean = n_plot * p, sd = sqrt(n_plot * p * (1 - p)))

  plot(n_plot, discrete_obj, type = "l", ylim=c(0,1), col = "blue", lwd = 2,
       ylab = "Objective Function", xlab = "Number of Tickets Sold (n)",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    nd, ") gamma =", gamma, "N =", N, "discrete"))
  points(n_plot, discrete_obj, col = "blue", pch = 20, cex = 1.5)
  abline(v = nd, col = "red", lwd = 2)
  abline(h = 0, col = "red", lwd = 2)

  plot(n_plot, continuous_obj, type = "l", col = "red", lwd = 2, lty = 2,
       ylab = "Objective Function", xlab = "Number of Tickets Sold (n)",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    round(nc,2), ") gamma =", gamma, "N =", N, "continuous"))
  abline(v = nd, col = "blue", lwd = 2)
  abline(h = 0, col = "blue", lwd = 3)

  # Return named list
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

ntickets(N=400,gamma = 0.02, p = 0.95)
