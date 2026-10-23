# ridgereg methods gathered

#' Print for a ridgereg object
#'
#' Prints the ridge regression coefficients of a ridgereg object
#'
#' @param x An object of class ridgereg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns Invisibly the ridgereg object
#' @export
#'
print.ridgereg <- function(x, ...){
  print(x$call_arg)

  # create a coefficients table
  coeffs_table <-
    rbind(
      Estimate = as.vector(x$beta_hat)
    )
  colnames(coeffs_table) <- rownames(x$beta_hat)
  cat("\nCoefficients:\n")
  print(coeffs_table)
  invisible(x)
}


#' Predicted values for a ridgereg object
#'
#' @param object An object of class ridgereg
#' @param newdata An optional data frame to predict on. If omitted, the fitted
#'   values of the model are returned
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns ridgereg object predicted values
#'
#' @importFrom stats predict model.matrix terms delete.response
#' @export
#'
predict.ridgereg <-  function(object, newdata = NULL, ...){
  if (is.null(newdata)) {
    return(as.vector(object$y_hat))
  }

  stopifnot("newdata argument must be a data frame (or tibble)" =
              is.data.frame(newdata))

  # create a model/design matrix from newdata, the response is not needed
  X_new <- model.matrix(delete.response(terms(object$formula)), newdata)

  # normalize covariates with the mean and sd of the training data
  is_intercept <- colnames(X_new) == "(Intercept)"
  X_new[, !is_intercept] <-
    scale(X_new[, !is_intercept, drop = FALSE],
          center = object$x_mean, scale = object$x_sd)

  # y_hat = X_new beta_hat
  y_hat <- X_new %*% object$beta_hat
  return(as.vector(y_hat))
}


#' Ridge regression coefficients for a ridgereg object
#'
#' @param object An object of class ridgereg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns ridgereg object ridge regression coefficients
#'
#' @importFrom stats coef
#' @export
#'
coef.ridgereg <-  function(object, ...){
  coeffs_table <-
    rbind(
      Estimate = as.vector(object$beta_hat)
    )
  colnames(coeffs_table) <- rownames(object$beta_hat)
  return(coeffs_table)
}
