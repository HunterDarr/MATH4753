#' @title Number of Tickets
#'
#' @param N The number of seats on a plane
#' @param gamma The probability of overbooking
#' @param p The probability that someone will show up for their flight
#'
#' @return 2 plots and a named list of nd, nc, N, and p
#' @export
#'
#'@importFrom graphics abline curve title
#'@importFrom stats pbinom qbinom pnorm qnorm
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets = function(N,gamma,p) {
  #Discrete calculation
  #Add N/10 to N to use in binom
  plus = N + N/10
  #Find the results of possible n's
  nds = qbinom(1-gamma,N:plus, p)
  #Find the gammas of these n's
  ndg = 1 - pbinom(N,N:plus,p) - gamma
  #Find the index of the objective closest to 0
  index = which.min(abs(ndg-0))
  ##Based on the index closest to N find the n associated with it
  nd = N + index -1
  ##Plot our results
  plot(N:plus,ndg, type='b', xlab='n', ylab='objective', pch=21, bg='steel blue')
  abline(v= nd, h= 1 - pbinom(N,nd,p)-gamma, col="red")
  title(main = paste0("Objective Vs n to find optimal tickets sold (", nd, ") \ngamma=", gamma, " N=", N, " discrete"))

  #Continuous calculation
  #Create a sequence from N to N+N/10
  sequenceNPlus = seq(N, plus, by=0.001)
  #Use this sequence in qnorm to find n's
  ncs = qnorm(1-gamma, sequenceNPlus*p, sqrt((sequenceNPlus*p)*(1-p))) -0.5
  # Find the index of the n closest to N
  closestTo = which.min(abs(ncs-N))
  # Find the n closest to N. This will return within 3 decimal places
  nc = sequenceNPlus[closestTo]

  #Use pnorm to plot our data
  objectiveFun = function(x) {1- pnorm(N+0.5,(x)*p,sqrt((x)*p*(1-p)) ) - gamma}
  dataTesting2 = curve(objectiveFun, from=N, to=plus, xlab='n', ylab='objective')
  #Use abline to point to our spot on the curve
  abline(v= nc, h= 1- pnorm(N+0.5,(nc)*p,sqrt((nc)*p*(1-p)) ) - gamma, col="red")
  title(main = paste0("Objective Vs n to find optimal tickets sold (", nc, ") \ngamma=", gamma, " N=", N, " continuous"))

  #Create our named list
  namedList = list(nd = nd, nc = nc, N = N, p = p)
  print(namedList)
}
