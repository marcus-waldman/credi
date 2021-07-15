#' Gradient of posterior density
#'
#' This calculates the analytic gradient of the posterior density function.
#' @param THETAi Size K vector of scaled scores for the i-th individual.
#' @param Yi Size Y vector of item responses for the i-th individual.
#' @param MUi Size K vector of prior's mean conditional on MONTHS.
#' @param invS Size K-by-K matrix of prior's inverse of var/cov matrix.
#' @param TAU Size J vector of item intercepts.
#' @param LAMBDA Size J-by-K matrix of item loadings.
#' @param J Number of items.
#' @param K Number of latent dimensions.
#' @keywords CREDI
#' @export
#' @examples
#' lf_grad_posterior_density(THETAi, Yi, MUi, invS, TAU, LAMBDA,J,K)

lf_grad_posterior_density<-function(THETAi, Yi, MUi, invS, TAU, LAMBDA,J,K){

  #inputs
  #Yi - J (vector)
  #MUi - K (vector)
  #invS - KxK (vector)
  #THETAi - K (vector)
  #TAU- J (vector)
  #LAMBDA - JxK (matrix)
  # J (integer)
  # K (integer)

  # Defined variables
  # PYi - J (vector)
  # LAMBDAj - K (vector)
  # dMUi -  K (vector)
  # dll - K (vector)
  # dprior - (scalar)

  # Computations

  PYi = as.vector(1/(1 + exp(TAU - LAMBDA%*%THETAi))) # J (vector)
  dll = rep(0,K) # K (vector)
  for (j in 1:J){
    if (Yi[j]==0L | Yi[j]==1L){
      LAMBDAj = as.vector(LAMBDA[j,]) # K (vector)
      dll = dll + (Yi[j]-PYi[j])*LAMBDAj
    }
  }

  dMUi = (THETAi - MUi) # K (vector)
  dprior = invS%*%dMUi

  return(as.numeric(dprior - dll))

}
