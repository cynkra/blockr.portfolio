#' Derive financial parameters from investor profile
#'
#' Maps personal details (age, family status, investment amount) to
#' continuous slider values for risk appetite and investment horizon.
#'
#' @param age Integer: investor age (18-100)
#' @param family Character: "single", "married", "married_kids", or "retired"
#' @param amount Numeric: investment amount
#' @return List with risk_slider (0-100), horizon_slider (0-100)
#' @noRd
pf_derive_profile <- function(age, family, amount) {
  # Risk slider: 0 = most conservative, 100 = most aggressive
  risk_slider <- if (age < 30) {
    85L
  } else if (age < 45) {
    if (family %in% c("married_kids", "retired")) 50L else 70L
  } else if (age < 55) {
    if (family %in% c("married_kids", "retired")) 40L else 50L
  } else if (age < 65) {
    if (family == "single") 40L else 25L
  } else {
    20L
  }

  # Horizon slider: 0 = shortest (1yr), 100 = longest (30+yr)
  # Rough mapping: horizon ≈ years until retirement (65) or drawdown
  years_left <- max(5, 65 - age)
  horizon_slider <- as.integer(min(100, max(0,
    if (years_left >= 30) 90L
    else if (years_left >= 20) 75L
    else if (years_left >= 10) 55L
    else if (years_left >= 5) 35L
    else 10L
  )))

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
