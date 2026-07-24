#' Predict age-conditional mu and sigma from a stored natural-spline reference model
#'
#' Reconstructs the fitted mu(age)/sigma(age) curves from a natural cubic spline
#' (`splines::ns()`) basis -- stored as knots, boundary knots, and linear coefficients
#' in `conditional.mu.sigma` (see `R/sysdata.rda`) -- and evaluates them at the
#' respondent's exact (continuous) age, rather than a fixed integer-age grid.
#'
#' Ages outside the boundary knots (the norming sample's observed age range) return
#' NA, matching the package's existing behavior of not producing Z-scores for
#' children older than the norming sample supports.
#'
#' @param age Numeric vector of ages in months.
#' @param model A list with mu_knots, mu_boundary_knots, mu_coef, sigma_knots,
#'   sigma_boundary_knots, sigma_coef -- one entry of `conditional.mu.sigma`.
#' @return A data.frame with columns mu and sigma, one row per element of `age`.
#' @keywords internal
predict_conditional_mu_sigma <- function(age, model) {

  in_range <- !is.na(age) & age >= model$mu_boundary_knots[1] & age <= model$mu_boundary_knots[2]

  mu <- rep(NA_real_, length(age))
  sigma <- rep(NA_real_, length(age))

  if (any(in_range)) {
    Xmu <- splines::ns(age[in_range], knots = model$mu_knots, Boundary.knots = model$mu_boundary_knots)
    mu[in_range] <- as.vector(cbind(1, Xmu) %*% model$mu_coef)

    Xsigma <- splines::ns(age[in_range], knots = model$sigma_knots, Boundary.knots = model$sigma_boundary_knots)
    sigma[in_range] <- exp(as.vector(cbind(1, Xsigma) %*% model$sigma_coef))
  }

  data.frame(mu = mu, sigma = sigma)
}
