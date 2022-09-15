##Create class RC linreg to perform linear regression
#' linreg RC class
#' The class linreg is a class of type RC that performs a linear regression given a
#' data frame and a formula. The class has several methods. print(), summary(), coef(), resid(), plot()
#' and pred().
#' @field X matrix.
#' @field y numeric.
#' @field b_hat matrix.
#' @field y_hat numeric.
#' @field e_hat numeric.
#' @field n numeric.
#' @field p numeric.
#' @field df numeric.
#' @field v_hat numeric.
#' @field v_b_hat_hat matrix.
#' @field t_values numeric.
#' @field p_values numeric.
#' @field data data.frame.
#' @field formula formula.
#' @field d character.
#'
#' @return an object of class linreg
#' @export linreg
#' @exportClass linreg
#' @importFrom methods new
#'
#' @examples
#' data(iris)
#' lin <- linreg$new(Petal.Length~Species, iris)
linreg <- setRefClass(
  "linreg",

  fields = list(
    X = "matrix",
    y = "numeric",
    b_hat = "matrix",
    y_hat = "numeric",
    e_hat = "numeric",
    n = "numeric",
    p = "numeric",
    df = "numeric",
    v_hat = "numeric",
    v_b_hat_hat = "matrix",
    t_values = "numeric",
    p_values = "numeric",
    data = "data.frame",
    formula = "formula",
    d = "character"

  ),

  methods = list(
    initialize = function(formula, data) {
      #Check input values
      stopifnot(is.data.frame(data))
      stopifnot(class(formula) == "formula")

      .self$d <-
        as.character(substitute(data)) #useful for the print method
      .self$data <- data
      .self$formula <- formula
      .self$X <- model.matrix(formula, data)
      #Create y vector
      .self$y <- data[[all.vars(formula)[[1]]]]

      #Compute regression coefficients (vector)
      .self$b_hat <-
        solve(t(X) %*% X) %*% t(X) %*% y ##NB we keep this as a matrix
      #Compute fitted values
      .self$y_hat <- as.numeric(X %*% b_hat)
      #Compute the residuals
      .self$e_hat <- y - y_hat
      #Compute the degrees of freedom
      ##number of observations
      .self$n <- nrow(X)
      ##number of parameters
      .self$p <- ncol(X)
      .self$df <- n - p
      #Compute residual variance
      .self$v_hat <-
        as.numeric((t(e_hat) %*% e_hat) / df)
      #Compute variance of regression coefficients
      .self$v_b_hat_hat <-
        v_hat * solve(t(X) %*% X)

      #Compute t values for each coefficient
      .self$t_values <- rep(0, p)
      for (i in seq(.self$p)) {
        .self$t_values[i] <- b_hat[i] / sqrt(v_b_hat_hat[i, i])
      }
      #Compute p values for each coefficient
      .self$p_values <-
        sapply(t_values, pt, df, lower.tail = FALSE)

    },

    print = function() {
      # b_hat_v <- as.vector(b_hat)
      # names(b_hat_v) <- row.names(b_hat)
      # cat(
      #   "Call:",
      #   "\n",
      #   "linreg(formula = ",
      #   paste0(formula)[2],
      #   "~",
      #   paste0(formula)[3],
      #   ", data = ",
      #   deparse(substitute(data)),
      #   ")",
      #   "\n\n",
      #   "Coefficients: ",
      #   "\n",
      #   sep = ""
      # )
      # b_hat_v
      #base::print("linreg(formula = paste0(formula)[2] ~ paste0(formula)[3], data = deparse(substitute(data))")
      cat("linreg(formula = ",
          paste0(formula)[2],
          " ~ ",
          paste0(formula)[3],
          ", data = ",
          d,
          ")",
          sep = "")
      cat("\n", row.names(b_hat))
      cat("\n", b_hat)

      #base::print("linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)"
      #base::print("(Intercept) Sepal.Width Sepal.Length")
    },

    plot = function() {
      #Plot residuals vs fitted values
      x1 <- y_hat
      y1 <- e_hat
      data1 <- data.frame(x = x1, y = y1)
      plot1 <-
        ggplot2::ggplot(data = data1, mapping = ggplot2::aes(x = x1, y = y1)) +
        ggplot2::geom_point() + ggplot2::stat_summary(fun = median, geom = "line") +
        ggplot2::labs(title = "Residuals vs fitted") +
        ggplot2::labs(x = "Fitted values", y = "Residuals") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(.5, .5 , .5 , .5, "cm"),
          plot.title = ggplot2::element_text(size = 18),
          axis.title = ggplot2::element_text(size = 18),
          axis.text = ggplot2::element_text(size = 15),
          axis.title.y =
            ggplot2::element_text(margin =
                                    ggplot2::margin(
                                      t = 0,
                                      r = 20,
                                      b = 0,
                                      l = 0
                                    ))
        )
      base::print(plot1)

      #Plot scale-location
      x2 <- y_hat
      y2 <-
        sqrt(abs((e_hat - mean(e_hat)) / sd(e_hat)))
      data2 <- data.frame(x = x2, y = y2)
      plot2 <-
        ggplot2::ggplot(data = data2, mapping = ggplot2::aes(x = x2, y = y2)) +
        ggplot2::geom_point() + ggplot2::stat_summary(fun = mean, geom = "line") +
        ggplot2::labs(title = "Scale-Location") +
        ggplot2::labs(x = "Fitted values", y = expression(sqrt(abs(
          Standardized ~ residuals
        )))) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(.5, .5 , .5 , .5, "cm"),
          plot.title = ggplot2::element_text(size = 18),
          axis.title = ggplot2::element_text(size = 18),
          axis.text = ggplot2::element_text(size = 15),
          axis.title.y =
            ggplot2::element_text(margin =
                                    ggplot2::margin(
                                      t = 0,
                                      r = 20,
                                      b = 0,
                                      l = 0
                                    ))
        )
      base::print(plot2)
    },

    resid = function() {
      #Return the vector of residuals
      e_hat
    },

    pred = function() {
      #Return the predicted values
      y_hat
    },

    coef = function() {
      #Return the coefficients
      b_hat_v <- as.vector(b_hat)
      names(b_hat_v) <- row.names(b_hat)
      b_hat_v
    },

    summary = function() {
      # cat("Coefficients:", "\n")
      # m <-
      #   matrix(c(b_hat, diag(v_b_hat_hat), t_values, p_values), ncol = 4)
      # row.names(m) <- row.names(b_hat)
      # colnames(m) <-
      #   c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      # base::print(m)
      # cat("\n",
      #     "Residual standard error:",
      #     v_hat,
      #     "on",
      #     df,
      #     "degrees of freedom")
      m <-
        matrix(c(b_hat, sqrt(diag(v_b_hat_hat)), t_values, p_values), ncol = 4)
      for (i in seq(p)) {
        if (m[i, 4] <= 0.001) {
          end <- "***"
        } else if (0.001 < m[i, 4] & m[i, 4] <= 0.01) {
          end <- "**"
        } else if (0.01 < m[i, 4] & m[i, 4] <= 0.05) {
          end <- "*"
        } else{
          end <- ""
        }
        cat(row.names(b_hat)[i], m[i, 1:3], end, "\n", sep = " ")
      }
      cat("Residual standard error: ",
          sqrt(v_hat),
          " on ",
          df,
          " degrees of freedom",
          sep = "")
    }

  )
)
