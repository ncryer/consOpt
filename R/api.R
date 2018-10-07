#' Optimize over a range of budgets and survival thresholds
#'
#' @param benefits.matrix A [strategies]x[species] dataframe with named rows and columns
#' @param cost.vector A list of strategy costs
#' @param budgets A list of budgets over which to optimize. If NULL, a sensible range of budgets will be automatically generated
#' @param thresholds A list of survival thresholds over which to optimize, default = (50, 60, 70)
#' @param combo.strategies A combination object specifying which strategies are combinations of which other strategies
#' @param weights A named list of species weights
#'
#' @return A dataframe of optimization results
#' @export
#'
#' @examples
#' COMBO_info <- get(data("COMBO_info"))
#' benefits.matrix <- get(data("Bij_fre_01"))
#' cost.vector <- get(data("cost_fre_01"))$Cost
#' opt.results <- Optimize(benefits.matrix, cost.vector, combo.strategies=COMBO_info)
#' PlotResults(opt.results)
Optimize <- function(benefits.matrix, cost.vector,
                     budgets = NULL,
                     thresholds = c(50.01, 60.01, 70.01),
                     combo.strategies = NULL,
                     weights = NULL) {

  combos <- NULL
  constraints <- NULL

  if (is.null(combo.strategies) ){
    warning("No combination or constraint information supplied.")
  } else {
    if (!is.null(combo.strategies)) {
      # All index was supplied through a combo matrix
      non.empty <- apply(combo.strategies, 2, function(x) sum(x != ''))
      if (!any(grepl("baseline", colnames(combo.strategies), ignore.case = TRUE))) {
        warning("Didn't find a baseline in the strategy combination matrix, assuming baseline strategy has index 1")
      }
      combos <- parse.combination.matrix(combo.strategies)
      constraints <- get.constraint.list(combo.strategies, benefits.matrix)
    }
  }


  opt.results <- do.optimize.range(B = benefits.matrix,
                                   cost.vector = cost.vector,
                                   thresholds = thresholds,
                                   compound.strategies = combos,
                                   combo.constraints = constraints,
                                   weights = weights)
  opt.results
}


#' Loads sample data for the optimization function.

#' @return None
#' @export
#'
#' @examples
#' FREdata()
FREdata <- function(){
  assign("COMBO_info", get(data("COMBO_info")), envir = .GlobalEnv)
  assign("benefits.matrix", get(data("Bij_fre_01")), envir = .GlobalEnv)
  assign("strategy.cost.df", get(data("cost_fre_01")), envir= .GlobalEnv)
  assign("cost.vector", get(data("cost_fre_01"))$Cost, envir= .GlobalEnv)
}
