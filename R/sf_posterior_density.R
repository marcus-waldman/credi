#' Posterior density for short form CREDI scale.
#'
#' This calculates the posterior density function.
#' @param THETAi Scalar representing scaled score for the i-th individual.
#' @param Yi Size N vector of item responses for the i-th individual.
#' @param MUi Scalar representing prior's mean conditional on AGE.
#' @param SIGMA_SQi Scalar represeting prior variance conditional on AGE.
#' @param DELTA Size J vector of item difficulties.
#' @param ALPHA Size J vector of item discrimination values.
#' @param J Number of items.
#' @param weight Size J vector of weights for Bayesian Bootstrapping.
#' @keywords CREDI
#' @examples
#' sf_posterior_density(THETAi, Yi, MUi, SIGMA_SQi, DELTA, ALPHA,J)


sf_posterior_density<-function(THETAi, Yi, MUi, SIGMA_SQi, DELTA, ALPHA,J, weight = NULL){

  #inputs
  #Yi - J (vector)
  #MUi - (scalar)
  #sigma_sq - (scalar)
  #THETAi - (scalar)
  #DELTA - J (vector)
  #ALPHA - J (vector)
  # J (integer)
  # W (vector)

  if(is.null(weight)){weight = rep(1,J)}

  # Defined variables
  # PYi - J (vector)
  # ll - (scalar)
  # dMUi - (scalar)
  # prior - (scalar)

  # Computations

  PYi = as.vector(1.0/(1.0 + exp( -1.0*ALPHA*(THETAi-DELTA) ) )) # J (vector)

  # likelihood component
  ll = as.numeric(0) #(scalar)
  for (j in 1:J){
    if (Yi[j] == 1L){ll = ll + weight[j]*log(PYi[j])}
    if (Yi[j] == 0L){ll = ll + weight[j]*log(1.0-PYi[j])}
  }

  # prior distribution component
  dMUi = (THETAi - MUi) # (scalar)
  prior = as.numeric(-0.5*(dMUi^2/SIGMA_SQi)) #(scalar)

  # Return
  return(-ll - prior)

}
