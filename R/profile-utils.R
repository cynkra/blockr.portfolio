#' Derive financial parameters from investor profile
#'
#' Maps personal details (age, family status, investment amount) to
#' portfolio optimization parameters.
#'
#' @param age Integer: investor age (18-100)
#' @param family Character: "single", "married", "married_kids", or "retired"
#' @param amount Numeric: investment amount in dollars
#' @return List with risk, horizon, strategy, min_positions
#' @noRd
pf_derive_profile <- function(age, family, amount) {
  risk <- if (age < 30) {
    "aggressive"
  } else if (age < 45) {
    if (family %in% c("married_kids", "retired")) "moderate" else "aggressive"
  } else if (age < 55) {
    "moderate"
  } else if (age < 65) {
    if (family == "single") "moderate" else "conservative"
  } else {
    "conservative"
  }

  horizon <- if (age < 35) {
    "long"
  } else if (age < 55) {
    "medium"
  } else {
    "short"
  }

  strategy <- if (amount < 10000) {
    "equal_weight"
  } else {
    "mean_variance"
  }

  min_positions <- if (amount < 10000) 3L
  else if (amount < 50000) 5L
  else 8L

  list(
    risk = risk,
    horizon = horizon,
    strategy = strategy,
    min_positions = min_positions
  )
}
