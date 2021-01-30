#' Title
#'
#' @param data The data for `JPS` or `RSS` sampling
#' @param set_size The set size of the ranks.
#' @param method Takes values `JPS` (the default) for Judgment Post Stratification, or `RSS` for Ranked Set Sample.
#' @param confidence The confidence level to use.
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model
#' @param pop_size The population size. Must be provided if sampling without replacement, or if `model` is set to 'superpopulation'.
#'
#' @return A `data.frame` with the rankings
#' @export
#'
#' @examples
OneSample <- function(data, set_size, method = c("JPS", "RSS"), confidence = 0.95,
                      replace = TRUE, model = 0, pop_size = NULL) {
    # Check valid values of set_size >= 1
    # Check model is 0 or 1 - Change to text? What are the types of model?
    # If model is 0, it's design based inference, if model = 1, it is model based inference using super population model
    # pop_size: nrow(data)*set_size <= pop_size, > 0, only relevant if replace = FALSE


    if(!isTRUE(replace) & !isFALSE(replace)) {
        stop("replace must take TRUE or FALSE")
    }

    method <- match.arg(toupper(method), c("JPS", "RSS"))

    alpha <- 1 - confidence

    ## Check if the sample is Judgment-post stratified sample (JPS)
    if (method == "JPS") {
        if (!replace & missing(pop_size)) {
            stop("The population size pop_size must be provided when sampling without replacement.")
        }
        if (model == 1 & missing(pop_size)) {
            stop("The population size pop_size must be provided for superpopulation model")
        }

        results <- JPSEF(data, set_size, replace, model, pop_size, alpha)

        if (model == 1) {
            colnames(results) <- c("Predictor", "Prediction", "Pred. Error", paste0(confidence * 100, "% Prediction intervals"))
        }
    }

    #################################################################
    ### Ranked set sample ###########################################
    #################################################################

    else if(method == "RSS") {
        if (!replace & missing(pop_size)) {
            stop("The population size pop_size must be provided when sampling without replacement")
        }
        if (model == 1 & missing(pop_size)) {
            stop("The population size pop_size must be provided for superpopulation model")
        }
        RV <- data[, 2]
        GSV <- aggregate(RV, list(RV), length)$x

        if (length(GSV) != set_size | min(GSV) <= 1) {
            stop("In Ranked set sampling design, first ranking method should not have less than two observations in any judgment ranking group")
        }

        results <- RSSEF(data, set_size, replace, model, pop_size, alpha)

        if (model == 1) {
            colnames(results) <- c("Predictor", "Prediction", "Pred. Error", paste0(confidence * 100, "% Prediction intervals"))
        }
    }
    return(results)
}
