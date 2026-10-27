data("iris")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=1))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=irfsfdis, lambda=1))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=-1))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=c(1, 2)))
})

test_that("ridgereg rejects predictors with zero variance", {
  # add a factor level without any observations, its dummy column is all zeros
  iris_empty <- iris
  iris_empty$Species <- factor(iris_empty$Species, levels = c(levels(iris$Species), "empty"))

  expect_error(ridgereg(Petal.Length~Species, data=iris_empty, lambda=1),
               "Invalid predictor standard deviations: Speciesempty")
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)

  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("print() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)

  expect_output(print(ridgereg_mod), "ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length")
  expect_output(print(ridgereg_mod), "( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("coef() is similar to MASS::lm.ridge()", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  lm_ridge_mod <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)

  # lm.ridge()$coef are the coefficients for the scaled covariates (no intercept).
  # lm.ridge() sand ridgereg() is calculated somwhat differently thus a tolerance is needed
  expect_equal(coef(ridgereg_mod)[1, -1], lm_ridge_mod$coef, tolerance = 0.01)
})

test_that("predict() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  lm_mod <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  # lambda = 0 is ordinary linear regression
  expect_equal(predict(ridgereg_mod), unname(fitted(lm_mod)))

  # predicting on the training data gives the fitted values
  expect_equal(predict(ridgereg_mod, newdata = iris), predict(ridgereg_mod))
})
