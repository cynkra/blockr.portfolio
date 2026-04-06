#' Derive risk and horizon from demographics
#'
#' Maps age and dependents to continuous 0-100 values for risk appetite
#' and investment horizon.
#'
#' @param age Numeric: investor age (18-100)
#' @param has_dependents Logical: has kids or other dependents
#' @return List with risk_slider (0-100), horizon_slider (0-100)
#' @noRd
pf_derive_profile <- function(age, has_dependents = FALSE, ...) {
  age <- max(18, min(100, as.numeric(age)))

  # Risk: continuous linear decline with age
  # Age 20 → 90, Age 45 → 55, Age 65 → 20, Age 80 → 10
  risk_slider <- as.integer(round(max(5, min(95,
    100 - (age - 18) * 1.1
  ))))

  # Dependents nudge risk down by ~15 points
  if (isTRUE(has_dependents)) {
    risk_slider <- as.integer(max(5, risk_slider - 15L))
  }


  # Horizon: gradual decline with age, but never below ~30 (5-10 years)
  # Age 20 → 90, Age 40 → 70, Age 55 → 55, Age 65 → 42, Age 75 → 32, Age 85 → 28
  # Even elderly investors have 5-15 year horizons (life expectancy, legacy)
  horizon_slider <- as.integer(round(max(25, min(95,
    95 - (age - 18) * 0.85
  ))))

  list(
    risk_slider = risk_slider,
    horizon_slider = horizon_slider
  )
}

#' Map risk slider to descriptive label
#' @param value Numeric 0-100
#' @return Character label
#' @noRd
pf_risk_label <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return("Moderate")
  value <- as.numeric(value)
  if (value < 20) "Very Conservative"
  else if (value < 40) "Conservative"
  else if (value < 60) "Moderate"
  else if (value < 80) "Aggressive"
  else "Very Aggressive"
}

#' Map horizon slider to descriptive label
#' @param value Numeric 0-100
#' @return Character label
#' @noRd
pf_horizon_label <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return("5-10 years")
  value <- as.numeric(value)
  if (value < 15) "1-3 years"
  else if (value < 30) "3-5 years"
  else if (value < 50) "5-10 years"
  else if (value < 70) "10-20 years"
  else if (value < 85) "20-30 years"
  else "30+ years"
}
