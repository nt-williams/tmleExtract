
#' Extract TSM and ATE from a tmle object
#'
#' @param fit a TMLE model from the \code{tmle} package
#' @param obs_a A vector of the observed binary exposures
#' @param obs_y A vector of the observed outcomes
#'
#' @return A list of class \code{tmleExtract} with elements \code{tsm} (the extract estimates) and \code{IF} a dataframe with the elements used to extract such estimates.
#' @export
#'
#' @examples
#' library(tmleExtract)
#' library(tmle)
#'
#' set.seed(1)
#' n <- 250
#' W <- matrix(rnorm(n*3), ncol=3)
#' A <- rbinom(n,1, 1/(1+exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
#' Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
#' tmle_fit <- tmle(Y,A,W, Q.SL.library = "SL.glm", g.SL.library = "SL.glm")
#'
#' tmle_extract(tmle_fit, A, Y)
tmle_extract <- function(fit, obs_a, obs_y) {

  tmle_fit <- fit
  g1w <- tmle_fit$g$g1W
  g0w <- 1 - g1w
  Q1W <- tmle_fit$Qstar[, 2]
  Q0W <- tmle_fit$Qstar[, 1]
  use <- data.frame(A = obs_a,
                    Y = obs_y,
                    g1w = g1w,
                    g0w = g0w,
                    Q1W = Q1W,
                    Q0W = Q0W,
                    ATE = Q1W - Q0W)

  if (!is.null(tmle_fit$estimates$RR)) {
    oob <- purrr::map_dfr(tmle_fit$estimates[c("ATT", "ATC", "RR", "OR")], out_of_box, .id = "parameter")
  } else {
    oob <- purrr::map_dfr(tmle_fit$estimates[c("ATT", "ATC")], out_of_box, .id = "parameter")
  }

  IF <- get_IF(use)
  infer <- tmle_inference(IF)

  out <- list(estimates = rbind(infer, oob),
              IF = IF)

  class(out) <- "tmleExtract"
  return(out)
}

get_IF <- function(data) {
  a <- data[["A"]]
  y <- data[["Y"]]
  g <- as.list(data[, c("g1w", "g0w")])
  q <- as.list(data[, c("Q1W", "Q0W")])
  i <- c(1, 0)

  build_IF <- function(i., g., q., a. = a, y. = y) {
    theta <- q.
    theta[a. == i.] <- theta[a. == i.] + (y.[a. == i.] - q.[a. == i.])/g.[a. == i.]
    theta <- theta - mean(theta)
    return(theta)
  }

  IF <- purrr::pmap(list(i. = i, g. = g, q. = q), build_IF)
  ate_IF <- IF[[1]] - IF[[2]]
  out <- data.frame(a1_IF = IF[[1]],
                    a0_IF = IF[[2]],
                    ate_IF = ate_IF)
  cbind(data, out)
}

out_of_box <- function(est) {
  mu <- est$psi
  variance <- if (is.null(est$var.psi)) est$var.log.psi else est$var.psi
  se <- sqrt(variance)
  z <- if (is.null(est$log.psi)) mu / se else log(mu) / se
  p <- est$pvalue
  conf.low <- est$CI[1]
  conf.high <- est$CI[2]

  data.frame(estimate = mu,
             variance = variance,
             standard_error = se,
             z = z,
             p = p,
             conf.low = conf.low,
             conf.high = conf.high)
}

inference <- function(qstar, IF) {
  mu <- mean(qstar)
  variance <- var(IF) / length(IF)
  se <- sqrt(variance)
  z <- mu / se
  er <- abs(qnorm(0.025)) * se
  ci <- c(mu - er, mu + er)

  data.frame(estimate = mu,
             variance = variance,
             standard_error = se,
             z = z,
             p = 2 * pnorm(abs(z), lower.tail = F),
             conf.low = ci[1],
             conf.high = ci[2])
}

tmle_inference <- function(data) {
  subs <- as.list(data[, c("Q1W", "Q0W", "ATE")])
  ifs <- as.list(data[, c("a1_IF", "a0_IF", "ate_IF")])

  purrr::map2_dfr(subs, ifs, inference, .id = "parameter")
}

#' Print an object of class \code{tmleExtract}
#'
#' @param x an object of class \code{tmleExtract}
#' @export
print.tmleExtract <- function(x) {
  print(x$estimates)
}
