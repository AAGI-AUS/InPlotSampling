#' Coombe Vineyard data from 2019 season
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
#'   \item{trunk_circ_18}{The trunk circumference of the vine in 2018, measured in cm at watering height (\~20 cm above the ground).}
#'   \item{trunk_circ_19}{The trunk circumference of the vine in 2019, measured in cm at watering height (\~20 cm above the ground).}
#'   \item{count_shoots}{Check this?}
#'   \item{non_count_shoots}{Check this?}
#'   \item{total_shoots}{Sum of `count_shoot` and `non_count_shoot`.}
#'   \item{pruning_weight}{Weight of the material removed during pruning in Kg. Check this?}
#'   \item{cordon_length}{The length of the cordon in cm}
#' }
"Coombe2019"
