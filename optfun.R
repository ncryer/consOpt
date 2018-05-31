library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)

get.results<- function(result, strategy.names, species.names, strategy.costs){
  # Retrieve which strategies were assigned to which species
  assignments <- get_solution(result, X[i,j])
  assignments <- assignments[assignments$value==1,]
  assignments$strategies <- strategy.names[assignments$i]
  assignments$species <- species.names[assignments$j]
  
  # Get total cost of implementing this set of strategies
  strategies.idx <- unique(assignments$i)
  total.cost <- sum(strategy.costs[strategies.idx])
  
  
  # Get species conserved
  conserved <- unique(assignments$species)
  
  # Get strategies
  strategies <- strategy.names[strategies.idx]
  
  list(
    total.cost=total.cost,
    assignments=assignments,
    species=conserved,
    strategies=strategies
  )
}


solve.ilp <- function(benefits, strategy.cost, budget.max, all_idx, threshold=FALSE){
  if(threshold){
    B <- (benefits > threshold)*1
  } else {
    B <- benefits
  }
  
  # Number of strategies 
  n <- nrow(B)
  # Number of species
  m <- ncol(B)
  
  others <- 2:(all_idx-1) 
  
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
  add_constraint(sum_expr(X[i,j], i = 1:n) <= 1, j = 1:m) %>%
  
  # Constraint (2)
  # Force contributions of management strategy i to every target species j to be null if strategy i is not selected
  # forall(i in strategies, j in target) xij[i][j] <= yi[i];
  add_constraint(X[i,j] <= y[i], i = 1:n, j = 1:m) %>%
  
  # Constraint (3)
  # "All" strategy constraint - if the "all" strategy is active, all others must be deselected
  add_constraint(y[all_idx] + y[i] <= 1, i = others) %>%
      
  # Constraint (4)
  # Budget constraint
  add_constraint(sum_expr(y[i]*strategy.cost[i], i = 1:n) <= budget.max, i = 1:n)
  
  # Solve the model
  result <- solve_model(opt, with_ROI(solver="lpsolve", verbose=FALSE))
  
  result
}

optimize.range <- function(benefits, strategy.costs, all_idx, budget.max=FALSE, budget.increment.size=FALSE, thresholds = c(90, 75, 50)){
  
  # Declare a maximum budget for the constrained optimization
  if(!budget.max){
    # Assume budget is cost of all strategies
    budget.max <- sum(strategy.costs)
  }
  
  if(!budget.increment.size ){
    budget.increment.size <- min(strategy.costs[strategy.costs > 0])
  }
  
  strategy.names <- rownames(benefits)
  species.names <- colnames(benefits)
  
  # Create a range of budget values over which to run the optimization
  budgets <- seq(budget.increment.size, budget.max, by=budget.increment.size)
  
  # Progress bar
  iters <- length(budgets)*length(thresholds)
  pb <- txtProgressBar(min=1, max=iters, initial = 1)
  step <- 1
  
  # Collect results in a funky list
  out <- c()
  # Set up the optimization problem for each threshold
  for(k in 1:length(thresholds)){
    this.threshold <- thresholds[k]
    
    for(j in 1:length(budgets)){
      this.budget.max <- budgets[j]
      
      # Solve model for this budget and threshold
      result <- solve.ilp(benefits, strategy.cost = strategy.costs, budget.max = this.budget.max, all_idx = all_idx, threshold = this.threshold)
      
      # Parse results
      parsed <- get.results(result, strategy.names = strategy.names, species.names = species.names, strategy.costs = strategy.costs)
      parsed$threshold <- this.threshold
      parsed$budget.max <- this.budget.max
      
      out <- c(out, parsed)
      
      # Update the progress bar
      step <- step + 1
      setTxtProgressBar(pb, step)
    }
    step <- step + 1
    setTxtProgressBar(pb, step)
  }
  out
}
