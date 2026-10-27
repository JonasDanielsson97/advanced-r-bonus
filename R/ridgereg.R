#' ridgereg(formula, data, lambda)
#'
#'
#' @param formula A ridge regression in the form  y ~ var1 + var2 ...
#' @param data A data frame on which the regression will be done
#' @param lambda The ridge penalty, a single non-negative number
#'
#' @returns A ridgereg object
#'
#' @importFrom stats model.matrix sd
#' @export
ridgereg <- function(formula, data, lambda){

  stopifnot("formula argument must be formula object" =
              inherits(formula, "formula"),
            "data argument must be a data frame (or tibble)" =
              is.data.frame(data),
            "lambda argument must be a single non-negative number" =
              is.numeric(lambda) & length(lambda) == 1 & lambda >= 0)

  ### extract model
  # create a model/design matrix from the formula and data
  X <- model.matrix(formula, data) # independent variables

  # extract the depedent variable from data using formula names
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  # keep the call
  call_arg <- match.call()

  ### normalize covariates (intercept column left as is)
  is_intercept <- colnames(X) == "(Intercept)"
  x_mean <- colMeans(X[, !is_intercept, drop = FALSE])
  x_sd <- apply(X[, !is_intercept, drop = FALSE], 2, sd)

  ### check for zero or undefined variance before scaling
  # a covariate with sd 0 (e.g. an empty factor level) would give 0/0 = NaN
  variance_alert <- names(x_sd)[!(is.finite(x_sd) & x_sd > 0)]

  if (length(variance_alert) > 0) {
    stop(
      "Invalid predictor standard deviations: ",
      paste(variance_alert, collapse = ", ")
    )
  }

  X[, !is_intercept] <-
    scale(X[, !is_intercept, drop = FALSE], center = x_mean, scale = x_sd)

  ### calculate ridge regression statistics (least squares)
  # penalty matrix, lambda * I with no penalty on the intercept
  penalty <- diag(lambda, ncol(X))
  penalty[is_intercept, is_intercept] <- 0

  # beta_hat = (X^T X + lambda I)^-1 X^T y (ridge specific)
  beta_hat <- solve(crossprod(X) + penalty, crossprod(X, y))
  rownames(beta_hat) <- colnames(X)

  # y_hat = fitted values
  y_hat <- X %*% beta_hat

  ### create a ridgereg object
  ridgereg_obj <-
    structure(
      list(
        formula = formula,
        call_arg = call_arg,
        lambda = lambda,
        X = X,
        y = y,
        y_name = y_name,
        x_mean = x_mean,
        x_sd = x_sd,
        beta_hat = beta_hat,
        y_hat = y_hat),
      class = "ridgereg")

  return(ridgereg_obj)
}
