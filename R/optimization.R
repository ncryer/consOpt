# ------------------------------
# Container object
# ------------------------------

optStruct <- R6Class("optStruct",
                     public = list(
                       B = NULL,
                       cost.vector = NULL,
                       all.index = NULL,
                       t = NULL,
                       budget.max = NULL,
                       weights = NULL,
                       combo.constraints = NULL,

                       initialize = function(B, cost.vector, t, weights=NULL){
                         self$B <- B
                         self$cost.vector <- cost.vector
                         self$t <- t
                         names(self$cost.vector) <- rownames(self$B)
                         # Check for names and do the rounding of B
                         private$prepare()
                         # Threshold B
                         private$threshold(self$t)
                         # Weight species groups (optional)
                         if (!is.null(weights)) {
                           self$weight.species(weights)
                         }
                         # Count the baseline results and remove etc.
                         private$baseline.prep()
                         # Set the zeroed out species to -1
                         self$B[self$B==0] <- -1
                         # We are now ready to do optimization
                       },


                       add.combo.constraints = function(combo.constraints){
                         #' Title
                         #'
                         #' @return
                         #' @export
                         #'
                         #' @examples
                         #'

                         self$combo.constraints <- combo.constraints
                       },

                       add.compound = function(input.strategies=NULL, combined.strategies){
                         #' The benefits matrix might contain strategies which are combinations of several strategies (called compound strategies). The joint selection of these strategies
                         #' will be artificially expensive if two compound strategies contain one or more of the same individual strategy, as the cost will be doubled
                         #' E.g.: Strategy S12 is a combination of strategies S3, S7, and S10.
                         #'       Strategy S13 is a combination of strategies S6, S9, and S10
                         #' Selecting strategies S12 and S13 simultaneously will erronously count the cost of S10 twice, making this combination less favorable to the objective function.
                         #'
                         #' TODO This function is a goddamn travesty and should be refactored...
                         #'
                         #'
                         #' @param input.strategies A named list denoting strategy names, e.g. list(strategy1="S1", strategy2="S2", ...)
                         #' @param combined.strategies A list of lists denoting strategy names that are combined, e.g. list(strategy1=c("S5", "S6", "S7"), strategy2=c("S6", "S7", "S8"))
                         #'
                         #' @return Silently updates the benefits matrix and the cost vector
                         #'
                         #' @example
                         #' TODO

                         if ( length(names(input.strategies)) < 1){
                           stop("Error: input.strategies must be a named list")
                         }

                         if ( length(names(combined.strategies)) < 1){
                           stop("Error: combined.strategies must be a named list")
                         }


                         input.strategy.names <- unlist(input.strategies, use.names=F)
                         combined.strategy.names <- unlist(combined.strategies, use.names=F)

                         # Check if the user supplied two (or more) existing strategies to combine
                         if (length(input.strategy.names) > 1){
                           # Strategies must be in the benefits matrix to be combined
                           if (!all(input.strategy.names %in% rownames(self$B))){
                             stop("User supplied multiple strategies to combine, but they were not found in the benefits matrix")
                           }

                           # Both strategies are present, compute the cost vector correctly
                           combined.strategy.names <- unlist(combined.strategies, use.names=F)
                           applied.strategies <- union(combined.strategy.names, combined.strategy.names)
                           # Make sure strategies are in the cost vector
                           if (!all(applied.strategies %in% names(self$cost.vector))){
                             stop("Some strategies to be combined were not in the cost vector")
                           }
                           total.cost <- sum(self$cost.vector[applied.strategies])
                           # Add cost to cost vector
                           strategy.name <- paste(input.strategy.names, collapse=" + ")
                           self$cost.vector <- c(self$cost.vector, total.cost)
                           names(self$cost.vector)[length(self$cost.vector)] <- strategy.name
                           # Add to benefits matrix - new species benefit vector is the logical OR of the benefit vectors of each input strategy
                           old.benefits <- self$B[input.strategy.names,]
                           new.row <- apply(old.benefits, 2, max) # take the max (1) of each column - same as (x['S2',] | x['S1',] )*1 for two rows
                           self$B <- rbind(self$B, new.row)
                           l <- length(rownames(self$B))
                           rownames(self$B)[l] <- strategy.name

                           # Done
                           invisible(self)
                         } else {
                           # User supplied ONE strategy name as input, adding a novel strategy to the mix
                           union.strategy.names <- union(combined.strategy.names, combined.strategy.names)
                           if (!all(union.strategy.names %in% rownames(self$B))){
                             stop("Error: User attempted to combine strategies that were not in the benefits matrix")
                           }

                           if (!all(union.strategy.names %in% names(self$cost.vector))){
                             stop("Error: User attempted to combine strategies that were not in the cost vector")
                           }
                           if (is.null(input.strategy.names)) {
                             warning("No strategy name supplied, setting default name")
                             default.strategy.name <- paste(union.strategy.names, collapse=" + ")
                           } else {
                             default.strategy.name <- input.strategy.names
                           }

                           # Compute cost
                           total.cost <- sum(self$cost.vector[union.strategy.names])
                           self$cost.vector <- c(self$cost.vector, total.cost)
                           names(self$cost.vector)[length(self$cost.vector)] <- default.strategy.name
                           # Compute benefits
                           old.benefits <- self$B[union.strategy.names,]
                           new.row <- apply(old.benefits, 2, max)
                           self$B <- rbind(self$B, new.row)
                           l <- length(rownames(self$B))
                           rownames(self$B)[l] <- default.strategy.name
                           invisible(self)
                         }
                       },

                       weight.species = function(weights){
                         #' Replace each species that survived the threshold with a species weight
                         #'
                         #' @param weights A list of integers. Must have a number for each species in the benefits matrix.
                         #' @returns Updates the benefits matrix in place

                         if ( ncol(self$B) != length(weights) ){
                           stop("Mismatch between species matrix and weights")
                         }

                         for(i in 1:nrow(self$B)){
                           self$B[i,] <- self$B[i,] * weights
                         }
                         invisible(self)
                       },

                       solve = function(budget, debug=FALSE){
                         #' Solve the ILP for this optStruct and a supplied budget
                         #'
                         #' @param budget A number
                         #' @return A result container

                         if (private$baseline.solved) {
                           return(private$get.baseline.results())
                         }

                         if (budget == 0){
                           return(private$get.baseline.results())
                         }
                         res <- private$solve.ilp(budget)
                         parsed <- private$parse.results(res)
                         if(debug){
                           return(res)
                         }
                         parsed
                       }


                     ),
                     private = list(
                       current.budget = NULL,
                       baseline.solved = FALSE,
                       baseline.idx = 1,
                       baseline.results = NULL,
                       species.buffer = list(),
                       state = list(
                         weighted = FALSE
                       ),

                       get.baseline.results = function(){
                         #' Returns the "results" from only running the baseline strategy
                         #'
                         #' @return A list holding the results
                         return( private$baseline.results )
                       },

                       prepare = function(){
                         #' Rounds the B matrix, check if B is labelled
                         #'
                         #' @return Updates self$B
                         self$B <- round(self$B, digits=2)

                         strategy.names <- rownames(self$B)
                         species.names <- colnames(self$B)

                         if (length(strategy.names) < nrow(self$B) || length(species.names) < ncol(self$B))
                           warning("Warning: Missing strategy or species label information, results will not be meaningful")
                         names(self$cost.vector) <- strategy.names
                         invisible(self)
                       },

                       threshold = function(t){
                         #' Thresholds the B matrix, binarizing the entries
                         #'
                         #' @param t A number
                         #' @return Modifies the B matrix in place
                         self$t <- t
                         self$B <- as.data.frame( (self$B >= t)*1 )
                         invisible(self)
                       },

                       baseline.prep = function(){
                         #' Count up the species saved by the baseline strategy, then remove it;
                         #' These species are buffered and are added freely to nontrivial strategies at results time
                         #' B is mutated by removing the baseline strategy, and the all_index is decremented
                         #'
                         #' @return Updates private$baseline.results

                         baseline.species.idx <- which(self$B[private$baseline.idx,] > 0)

                         # If ALL species are saved by the baseline, the B matrix will be useless
                         if (length(baseline.species.idx) == ncol(self$B)){
                           private$baseline.solved = TRUE
                         }

                         baseline.species.names <- colnames(self$B)[baseline.species.idx]
                         species.names.string <- paste(baseline.species.names, sep=" | ")

                         # Store in the species buffer
                         private$species.buffer <- c(private$species.buffer, baseline.species.names)

                         if (length(baseline.species.idx > 0)) {
                           # Remove baseline species from B, costs, and the all_index
                           self$B <- self$B[-private$baseline.idx, -baseline.species.idx]
                           self$cost.vector <- self$cost.vector[-private$baseline.idx]
                           self$all.index <- self$all.index - 1
                         }

                         # Update baseline results
                         baseline.num.species <- length(baseline.species.idx)
                         baseline.cost <- 0
                         baseline.threshold <- self$t

                         private$baseline.results <- list(numspecies = baseline.num.species,
                                                          totalcost = baseline.cost,
                                                          threshold = baseline.threshold,
                                                          species.groups = species.names.string,
                                                          strategies="Baseline",
                                                          budget = baseline.cost)
                         invisible(self)
                       },

                       parse.results = function(results){
                         #' Convert the optimization results into something human readable
                         #'
                         #' @param results An OMPR solution object
                         #' @return A list compiling the results of the optimization

                         assignments <- get_solution(results, X[i,j])
                         # Get entries of the assignment matrix
                         assignments <- assignments[assignments$value==1,]

                         # Get strategy names
                         strategy.idx <- sort(unique(assignments$i))
                         strategy.names <- rownames(self$B)[strategy.idx]

                         # Get strategy cost
                         total.cost <- sum(self$cost.vector[strategy.idx])

                         # Get species names
                         species.idx <- sort(unique(assignments$j))
                         species.names <- colnames(self$B)[species.idx]

                         # Add in the baseline species
                         species.names <- c(species.names, private$get.baseline.results()$species.groups)

                         species.total <- length(species.names)
                         threshold <- self$t

                         # Return
                         list(numspecies = species.total,
                              totalcost = total.cost,
                              threshold = threshold,
                              species.groups = species.names,
                              strategies = strategy.names,
                              assignments = assignments,
                              budget = private$current.budget)
                       },

                       solve.ilp = function(budget){
                         #' Solves the ILP given a budget
                         #'
                         #' @param budget A number
                         #' @return A list of results
                         private$current.budget <- budget
                         B <- self$B
                         strategy.cost <- self$cost.vector
                         budget.max <- budget
                         all_idx <- self$all.index

                         # Number of strategies
                         n <- nrow(B)
                         # Number of species
                         m <- ncol(B)

                         others <- which(1:n != all_idx)

                         # Set up the ILP
                         # --------------

                         model <- MIPModel() %>%

                           # Decision variables
                           # ------------------

                           # X[i,j] binary selection matrix
                           add_variable(X[i,j], i = 1:n, j = 1:m, type="binary") %>%
                           # y[i] Strategy selection vector
                           add_variable(y[i], i = 1:n, type="binary") %>%

                           # Objective function
                           # ------------------
                           set_objective(sum_expr(B[i,j] * X[i,j], i = 1:n, j = 1:m)) %>%

                           # Constraints
                           # -----------

                           # Constraint (1):
                           # Ensure only one strategy applies to a target species
                           add_constraint(sum_expr(X[i,j], i = 1:n) <= 1, j = 1:m, .show_progress_bar = FALSE) %>%

                           # Constraint (2)
                           # Force contributions of management strategy i to every target species j to be null if strategy i is not selected
                           # forall(i in strategies, j in target) xij[i][j] <= yi[i];
                           add_constraint(X[i,j] <= y[i], i = 1:n, j = 1:m, .show_progress_bar = FALSE) %>%

                           # Constraint (3)
                           # Budget constraint
                           add_constraint(sum_expr(y[i]*strategy.cost[i], i = 1:n) <= budget.max, i = 1:n, .show_progress_bar = FALSE)

                         # Constraint (4)
                         # Optional constraints: certain strategies are combinations of certain others; therefore, picking one forbids picking the other
                         print("Debug:")
                         print("Adding constraints")

                         if (!is.null(self$combo.constraints)) {
                           for(i in 1:length(self$combo.constraints)){
                             this.combo <- self$combo.constraints[[i]]
                             model <- add_constraint(model, y[this.combo$strat.idx] + y[i] <= 1, i = this.combo$combined.idx, .show_progress_bar = FALSE)
                           }
                         }
                         print("Successfully added constraints")

                         # Solve the model
                         result <- solve_model(model, with_ROI(solver="lpsolve", verbose=FALSE))

                         result
                       }
                     )
)


# ------------------------------
# Function to optimize over a range of thresholds
# ------------------------------

#' Optimize over a range of budgets and survival thresholds
#'
#' @param B A [strategies]x[species] dataframe with named rows and columns
#' @param cost.vector A list of strategy costs
#' @param all.index An integer signifying the index of the strategy that combines all strategies
#' @param budgets A list of budgets over which to optimize. If NULL, a sensible range of budgets will be automatically generated
#' @param thresholds A list of survival thresholds over which to optimize, default = (50, 60, 70)
#' @param combo.strategies A combination object specifying which strategies are combinations of which other strategies
#' @param weights A named list of species weights
#'
#' @return A dataframe of optimization results
#'
#' @examples
#'
do.optimize.range <- function(B, cost.vector, budgets = NULL, thresholds = c(50.01, 60.01, 70.01), compound.strategies=NULL, combo.constraints=NULL, weights=NULL){
  # Set up the progress bar
  progress.bar <- txtProgressBar(min=1, max=100, initial=1)
  step <- 1

  # Collect results of the optimization here
  out <- data.frame()

  for (threshold in thresholds) {

    # Initialize a new optimization run with an opStruct
    this.opt <- optStruct$new(B=B, cost.vector=cost.vector, t=threshold, weights=weights)

    # Check if constraints need to be added to the ILP
    if (!is.null(combo.constraints)){
      this.opt$add.combo.constraints(combo.constraints)
    }

    # Check if combo information needs to be supplied
    if (!is.null(compound.strategies)){
      compounds <- compound.strategies$get.combos()

      for(i in 1:length(compounds)){
        input <-compounds[[i]]$input
        output <- compounds[[i]]$output
        this.opt$add.compound(input, output)
      }

    }

    if ( is.null(budgets) ){
      # No budget range supplied. Use the costs of individual strategies
      budgets <- make.budget(this.opt$cost.vector)
    }


    for (budget in budgets){
      # Run over the budgets and compile the results

      optimization.result <- this.opt$solve(budget)

      out <- rbind(out, opt.result.to.df(optimization.result))

      # Update progress bar
      step <- step + 1
      setTxtProgressBar(progress.bar, step)
    }
  }


  # Remove duplicate entries from the result
  remove.duplicates(out)
}

remove.duplicates <- function(range.result.df){
  # Remove runs that didn't contribute new species groups for the same number of species saved
  tmp <- range.result.df
  # Remove expensive strategies that don't improve on the number of species saved
  tmp$duplicated_species <- FALSE
  tmp$duplicated_numspecies <- FALSE
  for(threshold in unique(tmp$threshold)){
    th.idx <- which(tmp$threshold==threshold)
    this.df <- tmp[th.idx,]
    tmp[th.idx,]$duplicated_species <- duplicated(this.df$species_groups)
    tmp[th.idx,]$duplicated_numspecies <- duplicated(this.df$number_of_species)
  }
  out <- tmp[!tmp$duplicated_species,]
  out <- out[!out$duplicated_numspecies,]
  out$duplicated_species <- NULL
  out$duplicated_numspecies <- NULL
  out
}

opt.result.to.df <- function(opt.result){
  #' TODO: Documentation
  #'
  #' @param opt.result A list
  #' @return slkdfl


  # Concatenate species groups
  species.groups.flat <- paste(opt.result$species.groups, collapse = " | ")
  # Concatenate strategies
  strategies.flat <- paste(opt.result$strategies, collapse = " + ")

  out <- data.frame(total_cost = opt.result$totalcost,
                    strategies = strategies.flat,
                    species_groups = species.groups.flat,
                    threshold = opt.result$threshold,
                    number_of_species = opt.result$numspecies,
                    budget.max = opt.result$budget)
  out$duplicated <- NULL
  out
}

make.budget <- function(cost.vector){
  #' Generate a list of budgets that adequately tests out different combinations of strategies
  #' Currently computes the prefix sum of the strategy cost vector and mixes it with the strategy costs
  #'
  #' @param cost.vector A list of numbers
  #' @return A sorted list of new budget levels
  sc <- sort(unlist(cost.vector)) / (10^6)
  newbudget <- seq(min(sc), max(sc), 30)
  newbudget <- newbudget * (10^6)
  out <- c(cost.vector, newbudget)
  return(c(0, unique(sort(out))))
}

