#' Coombe Vineyard Data from 2019 Season.
#'
#' Measurements taken on the Coombe Research Vineyard,
#' University of Adelaide Waite Campus after the 2019 season.
#'
#' @format A data frame with 352 rows and 11 variables:
#' \describe{
#'   \item{vine_id}{Identifier for the individual vines (1-352)}
#'   \item{rootstock}{The rootstock that the vine is growing on (8 levels)}
#'   \item{row}{The row in the vineyard (10-20)}
#'   \item{panel}{The panel of the vines. A pair of vines is grouped into a panel (1-16)}
#'   \item{trunk_circ_18}{The trunk circumference of the vine in 2018, measured in cm at watering height (~20 cm above the ground).}
#'   \item{trunk_circ_19}{The trunk circumference of the vine in 2019, measured in cm at watering height (~20 cm above the ground).}
#'   \item{count_shoots}{Check this?}
#'   \item{non_count_shoots}{Check this?}
#'   \item{total_shoots}{Sum of `count_shoot` and `non_count_shoot`.}
#'   \item{pruning_weight}{Weight of the material removed during pruning in Kg. Check this?}
#'   \item{cordon_length}{The length of the cordon in cm}
#' }
"coombe2019"


#' Seed Emergence Population.
#'
#' The entire population of actual and estimated seed emergence.
#'
#' @format A data frame with 2640 rows of 2 variables: \code{actual_seed_emergence} and
#' \code{estimated_seed_emergence} giving the actual and estimated number of seeds emerged.
"population"

#' Ranks for Seed Emergence.
#'
#' Contains the ranks given by 5 rankers of the number of seeds emerged in a sample of 15 plots.
#'
#' @format A data frame with 6 variables: \code{seed_emergence}, \code{ranker1}, \code{ranker2},
#' \code{ranker3}, \code{ranker4} and \code{ranker5}.
"emergence_ranks"


