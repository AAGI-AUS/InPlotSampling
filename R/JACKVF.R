#' Title
#'
#' @param u
#'
#' @return
#' @export
#'
#' @examples
JACKVF <- function(u) {
    n <- length(u)-1
    # jackrep=n*u[1]-(n-1)*u[-1]
    #  ret=var(jackrep)
    ret <- (n - 1) * var(u) * ((n - 1) / n)^2
}
