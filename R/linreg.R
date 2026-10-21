
#' linreg(formula, data)
#'
#' @param formula A linear regression in the form  y ~ var1 + var2 ...
#' @param data A data frame on which the regression will be done
#'
#' @returns A linreg object
#'
#' @importFrom stats model.matrix pt
#' @export
linreg <- function(formula, data){

  stopifnot("formula argument must be formula object" =
              inherits(formula, "formula"),
            "data argument must be a data frame (or tibble)" =
              is.data.frame(data))

  ### extract model
  # create a model/design matrix from the formula and data
  X <- model.matrix(formula, data) # independent variables

  # extract the depedent variable from data using formula names
  y_name <- all.vars(formula)[1]
  y <- data[[y_name]]

  # keep the call
  call_arg <- match.call()

  ### calculate multiple linear regression statistics (QR decomposition)
  qr_results <- linreg_qr(X, y)
  beta_hat <- qr_results$beta_hat

  # y_hat = fitted values
  y_hat <- qr_results$y_hat

  # e_hat = residuals
  e_hat <- qr_results$e_hat

  # degrees of freedom
  df <- qr_results$df

  # sigma_hat_squared = residual variance
  sigma_hat_squared <- qr_results$sigma_hat_squared

  # beta_hat_var = variance of regression coefficients (QR decomposition)
    beta_hat_var <- qr_results$beta_hat_var


  # t_values = t values for the regression coefficients
  t_values <-
    (beta_hat / sqrt(beta_hat_var))|>
    as.vector()

  # p_values = p values for the regression coefficients
  p_values <- 2 * pt(-abs(t_values), df)

  ### create a linreg object
  linreg_obj <-
    structure(
      list(
        formula = formula,
        call_arg = call_arg,
        X = X,
        y = y,
        y_name = y_name,
        beta_hat = beta_hat,
        y_hat = y_hat,
        e_hat = e_hat,
        df = df,
        sigma_hat_squared = sigma_hat_squared,
        beta_hat_var = beta_hat_var,
        t_values = t_values,
        p_values = p_values),
      class = "linreg")

  return(linreg_obj)
}

