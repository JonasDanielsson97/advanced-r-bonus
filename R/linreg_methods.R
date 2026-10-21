# linreg methods gathered

#' Print for a linreg object
#'
#' Prints the regression coefficients of a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns Invisibly the linreg object
#' @export
#'
print.linreg <- function(x, ...){
  # cat("Linear regression call:\n")
  # print(x$formula)

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


#' Residuals for a linreg object
#'
#' @param object An object of class linreg.
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns linreg object residuals as the difference between predicted and actual response values
#'
#' @export
#'
residuals.linreg <-  function(object, ...){ # stat::resid() calls residuals()
  return(object$e_hat)
}


#' Predicted values generic function
#'
#' @param object An object of class linreg.
#' @param ... Additional arguments for compatibility with generic method
#'
#' @export
pred <- function(object, ...) {
  UseMethod("pred") # use linreg method
}


#' Predicted values for a linreg object
#'
#' @param object An object of class linreg.
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns linreg object predicted values
#' @export
#'
pred.linreg <-  function(object, ...){
  return(object$y_hat)
}


#' Regression coefficients for a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns linreg object regression coefficients
#'
#' @importFrom stats coef
#' @export
#'
coef.linreg <-  function(object, ...){
  coeffs_table <-
    rbind(
      Estimate = as.vector(object$beta_hat)
    )
  colnames(coeffs_table) <- rownames(object$beta_hat)
  return(coeffs_table)
}


#' Summary of regression results for a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns Invisibly the linreg object
#'
#' @importFrom stats printCoefmat
#' @export
#'
summary.linreg <-  function(object, ...){
  # call
  print(object$call_arg)

  # results table
  coeffs_table_full <-
    cbind(
      Estimate = as.vector(object$beta_hat),
      SE = sqrt(object$beta_hat_var),
      t_value = object$t_values,
      p_values = object$p_values
    )
  colnames(coeffs_table_full) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(coeffs_table_full) <- rownames(object$beta_hat)
  cat("\nCoefficients:\n")
  printCoefmat(coeffs_table_full, signif.stars = TRUE)

  # Residual standard error and degrees of freedom
  cat("\nResidual standard error:",
      round(sqrt(object$sigma_hat_squared), digits = 4),
      "on",
      object$df,
      "degrees of freedom")

  invisible(object)
}


#' Diagnostic plots for a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments for compatibility with generic method
#'
#' @returns Invisibly a list with two ggplot2 objects
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_summary geom_hline labs .data geom_text theme_bw theme element_text
#' @export
plot.linreg <- function(x, ...){

  # dataframe for plotting
  plot_df <- data.frame(
    fitted = x$y_hat |> as.vector(),
    resid = x$e_hat |> as.vector(),
    obs = x$e_hat |> as.vector() |> seq_along()
  )


  # Add standardized residuals and sqrt of absolute standardized residuals to df
  plot_df$std_resid <- plot_df$resid / sqrt(x$sigma_hat_squared) # might want to add leverage adjustment here
  plot_df$sqrt_abs_std_resid <- sqrt(abs(plot_df$std_resid))

  # Top 3 residuals in each plot
  top_resid <- plot_df[order(abs(plot_df$resid), decreasing = TRUE)[1:3],]
  top_std_resid <- plot_df[order(abs(plot_df$std_resid), decreasing = TRUE)[1:3],]

  # x axis label, deparse() takes 'formula' and turns it into a string
  x_label <- paste0("Fitted values\nlinreg(", deparse(x$formula), ")")

  # Create the plots
  p1 <- ggplot(plot_df, aes(x = .data$fitted, y = .data$resid)) +
    geom_hline(yintercept = 0, color = "grey50", linetype = "dotted") +
    geom_point(shape = 1, size = 2) +
    stat_summary(fun = stats::median, geom = "line", color = "red") +
    geom_text(data = top_resid, aes(label = .data$obs), hjust = 1.5) +
    labs(title = "Residuals vs Fitted", x = x_label, y = "Residuals") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) # center title


  p2 <- ggplot(plot_df, aes(x = .data$fitted, y = .data$sqrt_abs_std_resid)) +
    geom_point(shape = 1, size = 2) +
    stat_summary(fun = stats::median, geom = "line", color = "red") +
    geom_text(data = top_std_resid, aes(label = .data$obs), hjust = 1.5) +
    labs(title = "Scale-Location", x = x_label, y = expression(sqrt("|Standardized residuals|"))) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) # center title

  # Print the plots
  print(p1)
  print(p2)

  invisible(list(p1, p2))
}
