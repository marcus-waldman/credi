#' Weighted likelihood estimating equation for the CREDI SF/OVERALL ability model
#'
#' Warm's (1989) weighted likelihood estimator for a single-dimension 2PL-style
#' item response model -- the same item model used by \code{sf_posterior_density}
#' for SF and OVERALL scoring, but without a Bayesian population prior. Root-finding
#' this equation (e.g. via \code{uniroot()}) removes the shrinkage-toward-the-mean
#' bias that MAP estimation carries for short item sets, at the cost of higher
#' variance for any individual estimate. Domain (MOT/COG/LANG/SEM) scores are not
#' estimated this way -- that would require generalizing this bias correction to the
#' package's multidimensional item model, which has not been validated.
#'
#' @param theta Scalar ability value at which to evaluate the estimating equation.
#' @param Yi Vector of item responses (0/1) for the items in alpha_i/delta_i.
#' @param alpha_i Vector of item discriminations for the same items.
#' @param delta_i Vector of item difficulties for the same items.
#' @return A scalar; \code{uniroot()} finds the theta where this is zero.
#' @keywords internal
wle_equation <- function(theta, Yi, alpha_i, delta_i) {
  P <- 1 / (1 + exp(-alpha_i * (theta - delta_i)))
  U <- sum(alpha_i * (Yi - P))
  I_test <- sum(alpha_i^2 * P * (1 - P))
  Jprime <- sum(alpha_i^3 * P * (1 - P) * (1 - 2 * P))
  U + Jprime / (2 * I_test)
}

#' Weighted likelihood ability estimate and standard error
#'
#' @param Yi Vector of item responses (0/1, NA for not administered) for a single respondent.
#' @param alpha_i Vector of item discriminations, same length/order as Yi.
#' @param delta_i Vector of item difficulties, same length/order as Yi.
#' @param min_items Minimum non-missing items required to attempt an estimate.
#' @return A list with `theta` (NA if too few items or root-finding fails) and `se`.
#' @keywords internal
wle_theta <- function(Yi, alpha_i, delta_i, min_items = 5) {
  ok <- !is.na(Yi)
  Yi <- Yi[ok]; alpha_i <- alpha_i[ok]; delta_i <- delta_i[ok]

  if (length(Yi) < min_items) return(list(theta = NA_real_, se = NA_real_))

  theta <- tryCatch(
    stats::uniroot(wle_equation, interval = c(-20, 20), Yi = Yi, alpha_i = alpha_i, delta_i = delta_i,
                   extendInt = "no")$root,
    error = function(e) NA_real_)

  if (is.na(theta)) return(list(theta = NA_real_, se = NA_real_))

  P <- 1 / (1 + exp(-alpha_i * (theta - delta_i)))
  I_test <- sum(alpha_i^2 * P * (1 - P))
  list(theta = theta, se = 1 / sqrt(I_test))
}
