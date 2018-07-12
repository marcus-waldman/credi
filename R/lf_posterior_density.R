#' Posterior density for CREDI subscales
#'
#' This calculates the posterior density function.
#' @param THETAi Size K vector of scaled scores for the i-th individual.
#' @param Yi Size Y vector of item responses for the i-th individual.
#' @param MUi Size K vector of prior's mean conditional on MONTHS.
#' @param invS Size K-by-K matrix of prior's inverse of var/cov matrix.
#' @param TAU Size J vector of item intercepts.
#' @param LAMBDA Size J-by-K matrix of item loadings.
#' @param J Number of items.
#' @param K Number of latent dimensions.
#' @keywords CREDI
#' @examples
#' lf_posterior_density(THETAi, Yi, MUi, invS, TAU, LAMBDA,J,K)


lf_posterior_density<-function(THETAi, Yi, MUi, invS, TAU, LAMBDA,J,K){

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
  # ll - (scalar)
  # dMUi -  K (vector)
  # prior - (scalar)

  # Computations

  PYi = as.vector(1/(1 + exp(TAU - LAMBDA%*%THETAi))) # J (vector)

  # likelihood component
  ll = as.numeric(0) #(scalar)
  for (j in 1:J){
    if (Yi[j] == 1L){ll = ll + log(PYi[j])}
    if (Yi[j] == 0L){ll = ll + log(1.0-PYi[j])}
  }

  # prior distribution component
  dMUi = (THETAi - MUi) # K (vector)
  prior = as.numeric(-0.5*(dMUi%*%invS%*%dMUi)) #(scalar)

  # Return
  return(-ll - prior)

}
