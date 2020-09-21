#' Alternative version of expand.grid to enable faster evaluation
#'
#' @param seq1 A sequence of integers to expand along first
#' @param seq2 A sequence of integers to expand along second
#'
#' @return
#' @export
#'
#' @examples
expand.grid.alt <- function(seq1, seq2) {
  cbind(
    Var1 = rep.int(seq1, length(seq2)),
    Var2 = rep.int(seq2, rep.int(length(seq1), length(seq2)))
  )
}
