#' Title
#'
#' @param data The data for `JPS` or `RSS` sampling
#' @param set_size The set size of the ranks.
#' @param method Takes values `JPS` (the default) for Judgment Post Stratification, or `RSS` for Ranked Set Sample.
#' @param confidence The confidence level to use. Defaults to 0.95
#' @param replace Logical (default `TRUE`). Sample with replacement?
#' @param model
#' @param N The population size. Must be provided if sampling without replacement, or if `model` is set to 'superpopulation'.
#'
#' @return
#' @export
#'
#' @examples
OneSample <- function(data, set_size, method = c("JPS", "RSS"), confidence = 0.95, replace = TRUE, model = 0, N = NULL) {
    # Check valid values of set_size
    # Check model is 0 or 1 - Change to text?

    if(!isTRUE(replace) & !isFALSE(replace)) {
        stop("replace must take TRUE or FALSE")
    }

    method <- match.arg(toupper(method), c("JPS", "RSS"))

    ## Check if the sample is Judgment-post stratified sample (JPS)
    if (method == "JPS") {
        if (!replace && is.null(N)) {
            stop("The population size N must be provided when sampling without replacement.")
        }
        if (model == 1 && is.null(N)) {
            stop("The population size N must be provided for superpopulation model")
        }

        alpha <- 1 - confidence
        Results <- JPSEF(data, set_size, replace, model, N, alpha)
        if (model == 1) {
            colnames(Results) <- c("Predictor", "Prediction", "Pred. Error", paste0((1 - alpha) * 100, "% Prediction intervals"))
            # colnames(Results) <- COLname
        }
        return(Results)
    }

    #################################################################
    ### Ranked set sample ###########################################
    #################################################################

    if (method == "RSS") {
        if (!replace & (missing(N) | is.null(N))) {
            stop("The population size N must be provided when sampling without replacement")
        }
        if (model == 1 && (missing(N) | is.null(N))) {
            stop("The population size N must be provided for superpopulation model")
        }
        RV <- data[, 2]
        GSV <- aggregate(RV, list(RV), length)$x
        #   if(length(unique(GSV))!=1 && !replace) {print("First ranking method should be balanced for wthout replaclement sampling");
        #                          return()  }
        if (length(GSV) != set_size | min(GSV) <= 1) {
            print("In Ranked set sampling design,first ranking method should not have less than two observations in any judgment ranking group")
            return()
        }

        alpha <- 1 - confidence
        RSS.Return <- RSSEF(data, set_size, replace, model, N, alpha)
        if (model == 1) {
            COLname <- c("Predictor", "Prediction", "Pred. Error", paste((1 - alpha) * 100, "% Prediction intervals", sep = ""))
            colnames(RSS.Return) <- COLname
        }
        return(RSS.Return)
    }
}
