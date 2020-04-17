#' Title
#'
#' @param uv
#' @param H
#' @param n
#'
#' @return
#' @export
#'
#' @examples
TRIPLEF <- function(uv, H, n) {
    coef1 <- (-1)^(uv[2] - 1) / (uv[1]^2 * uv[3])
    coef2 <- choose((H - 1), (uv[1] - 1))
    coef3 <- choose((uv[1] - 1), (uv[2] - 1))
    coef4 <- choose(n, uv[3])
    coef5 <- (uv[1] - uv[2])^(n - uv[3])
    ret <- coef1 * coef2 * coef3 * coef4 * coef5
    return(ret)
}
