#' Compute per-respondent conditional mu/sigma under a chosen Z-score reference model
#'
#' Dispatches on `ref_model$method`:
#' \itemize{
#'  \item{"grid": the original (2021-03-26) reference -- age is floored to the nearest
#'    whole month and joined against a fixed integer-age lookup table.}
#'  \item{"ns_spline": the 2026-07-24 reference -- mu(age)/sigma(age) are evaluated at
#'    the respondent's exact continuous age from stored natural-spline coefficients.}
#' }
#' Both branches return NA for ages outside the reference's supported range, so
#' downstream Z-scores are NA rather than silently extrapolated.
#'
#' @param AGE A data.frame with columns ID and AGE (unfloored).
#' @param ref_model One entry of `zscore_refs` (see R/sysdata.rda).
#' @return A data.frame with ID and *_mu/*_sigma columns for OVERALL, COG, LANG, SEM, MOT.
#' @keywords internal
get_conditional_mu_sigma <- function(AGE, ref_model) {

  domains <- c("OVERALL", "COG", "LANG", "SEM", "MOT")

  if (identical(ref_model$method, "grid")) {

    out <- AGE %>%
      mutate(AGE = floor(AGE)) %>%
      left_join(ref_model$table, by = "AGE") %>%
      select(ID, ends_with("_mu"), ends_with("_sigma"))

  } else if (identical(ref_model$method, "ns_spline")) {

    out <- data.frame(ID = AGE$ID)
    for (d in domains) {
      ms <- predict_conditional_mu_sigma(AGE$AGE, ref_model$models[[d]])
      out[[paste0(d, "_mu")]] <- ms$mu
      out[[paste0(d, "_sigma")]] <- ms$sigma
    }

  } else {
    stop("Unknown Z-score reference model method: ", ref_model$method)
  }

  out
}
