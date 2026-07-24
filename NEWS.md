# credi 1.2.0

## Breaking change

* The default age-conditional reference (norming) model used to compute
  Z-scores has changed from the original 2021-03-26 model to a refit
  2026-07-24 model. The new model uses a continuous-age reference curve (no
  integer-age flooring) and revised, BIC-selected age-smoothing (with the mu
  and sigma smoothness search deliberately capped at mu df <= 8 and sigma
  df <= 3 to keep the curves visually consistent with smooth developmental
  patterns, rather than using whatever an unrestricted BIC search preferred).
  `score()` calls that do not specify `ref` will return different Z-scores
  than previous package versions as a result.

  The new model also extends the supported age range from 0-36 months to
  0-42 months, fit on an updated and expanded reference sample (same
  "advantaged" filter criteria as the original 2021 sample -- caregiver
  education and household asset index -- but without the age <= 36 cutoff).
  Children aged 36-42 months, who previously received no Z-score at all,
  now do under the default reference model.

  To exactly reproduce Z-scores from package versions prior to 1.2.0, call
  `score(..., ref = "20210326")` -- note this reproduces the original
  behavior exactly, including the 0-36 month age limit.

## New features

* `score()` gains an `est` argument selecting the estimator used to compute
  the OVERALL/SF ability score: `est = "MAP"` (default; the original Bayesian
  maximum a posteriori estimator, unchanged) or `est = "WLE"` (Warm's (1989)
  weighted likelihood estimator).

  MAP shrinks ability estimates toward the age-conditional mean, an effect
  that grows more pronounced the fewer items are administered -- most visible
  in Short Form scoring, where response patterns near the extremes (e.g. all
  items answered "no") come out noticeably less extreme than the same
  response pattern would if scored from a longer administration. WLE removes
  this shrinkage, at the cost of higher variance for any individual estimate.

  WLE is only supported for OVERALL/SF scores. Domain scores (MOT/COG/LANG/
  SEM) are always estimated via MAP; WLE's bias correction has not been
  validated for that multidimensional item model. Calling
  `score(..., est = "WLE")` on Long Form data issues a warning and returns
  only `ID`/`OVERALL`/`OVERALL_SE`/`Z_OVERALL`/`NOTES`, matching how Short
  Form data has always been reported.

* `score()` gains a `ref` argument selecting which age-conditional reference
  model to standardize against. Each `est` has its own reference curve(s);
  `ref` values are not interchangeable across `est` (e.g. `ref = "20210326"`
  is only available under `est = "MAP"`).

## Internal

* Continuous-age reference curves (`ref = "20260724"`, for either `est`) are
  stored as natural cubic spline (`splines::ns()`) knots and linear
  coefficients rather than a fixed integer-age lookup table, so mu(age) and
  sigma(age) are evaluated at a respondent's exact age rather than
  `floor(AGE)`.
