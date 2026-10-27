#' Create a design matrix and check for variance problems before ridge regression
#'
#' @param formula A formula of the form y ~ var1 + var2 ...
#' @param data A data frame
#'
#' @returns A list with:
#' X = a design matrix
#' is_intercept = true if intercept in the design
#' x_sd = sd for the predictors in X (not the intercept)
#' variance_alert = true if not all predictors have a finite, positive sd
#' (if variance_alert i true, a warning message is also printed)
#' @export
ridge_design_and_check <- function(formula, data) {

  stopifnot("formula argument must be formula object" =
              inherits(formula, "formula"),
            "data argument must be a data frame (or tibble)" =
              is.data.frame(data))

  X <- stats::model.matrix(formula, data)
  is_intercept <- colnames(X) == "(Intercept)"
  pred_names <- colnames(X)[!is_intercept]

  # x_sd <- apply(X[, !is_intercept, drop = FALSE], 2, sd)
  x_sd <- purrr::map_dbl(
    as.data.frame(X[, !is_intercept, drop = FALSE]),
    sd
  )

  variance_alert <- names(x_sd)[!(is.finite(x_sd) & x_sd > 0)]

  if (length(variance_alert) > 0) {
    warning(
      "Invalid predictor standard deviations: ",
      paste(variance_alert, collapse = ", ")
    )
  }
  return(list(X = X,
              is_intercept = is_intercept,
              x_sd = x_sd,
              variance_alert = variance_alert))
}
