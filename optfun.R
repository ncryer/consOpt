library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)


symdiff <- function(x, y){
  # Symmetric set difference 
  # Returns elements of either x or y that are not in the other
  setdiff(union(x, y), intersect(x, y))
}

get.results <- function(result, strategy.names, species.names, strategy.costs){

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
  if(length(strategies)==0){
    strategies <- strategies.idx
  }
  
  # Remove redundant columns from assignments
  assignments$variable <- NULL 
  assignments$i <- NULL
  assignments$j <- NULL
  
  list(
    total.cost=total.cost,
    assignments=assignments,
    species=conserved,
    strategies=strategies
  )
}

summarize.result <- function(results, threshold){
  this.run <- results[[as.character(threshold)]]
  res.names <- names(this.run)
  print(paste("Ensuring at least", threshold, "persistance probability:"))
  print("--------------------------------------------")
  for(res in res.names){
    this.res <- this.run[[res]]
    
    cost <- this.res$total.cost
    species <- this.res$species
    strategies <- this.res$strategies
  
    print("Strategies:")
    print(strategies)
    print("Species:")
    print(species)
    print(paste("Total cost:", cost))
    print("--------------")
  }
}

summary.results.df <- function(results){
  # Return a dataframe of optimization results, filtering out redundant budget settings:
  # if a budget setting doesn't yields a set of strategies already discovered by a cheaper budget setting, discard it
  # and if a budget setting doesn't save additional/different species, discard it
  res.df <- data.frame()
  
  for(threshold in names(results)){
    this.threshold <- results[[threshold]]
    for(budget.name in names(this.threshold)){
      this.run <- this.threshold[[budget.name]]
      
      # If no strategies were selected, we're dealing essentially with the baseline strategy
      if(length(this.run$strategies) == 0){
        next
      }
      
      this.totalcost <- this.run$total.cost
      this.strategies <- paste(sort(this.run$strategies), collapse=" | ")
      this.species <- paste(sort(this.run$species), collapse=" | ")
      this.numspecies <- length(this.run$species)
      
     
      
      tmp.df <- data.frame(total_cost=this.totalcost, strategies=this.strategies, species=this.species, threshold=threshold, number_of_species=this.numspecies)
      res.df <- rbind(res.df, tmp.df)
    }
  }
  
  unique(res.df)
}


solve.ilp <- function(benefits, strategy.cost, budget.max, all_idx, threshold=FALSE){
  if(threshold){
    B <- (benefits >= threshold)*1
  } else {
    B <- benefits
  }
  
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
  result <- solve_model(model, with_ROI(solver="lpsolve", verbose=FALSE))
  
  result
}

optimize.range <- function(benefits, strategy.costs, all_idx, budget.max=FALSE, budget.increment.size=FALSE, thresholds = c(50, 60, 70), budget.length=10, budget.range=FALSE){
  
  # Declare a maximum budget for the constrained optimization
  if(!budget.max){
    # Assume budget is cost of all strategies
    budget.max <- sum(strategy.costs)
  }
  
  if(!budget.increment.size ){
    budget.increment.size <- min(strategy.costs[strategy.costs > 0])
  }
  
  if(!budget.range){
    # Create a range of budget values over which to run the optimization
    budgets <- sort(strategy.costs)
  } else {
    budgets <- budget.range
  }
  
  # Round benefits to nearest whole number before optimization
  benefits <- round(benefits, digits=0)
  
  strategy.names <- rownames(benefits)
  species.names <- colnames(benefits)
  
  
  # Progress bar
  iters <- length(budgets)*length(thresholds)
  pb <- txtProgressBar(min=1, max=iters, initial = 1)
  step <- 1
  
  # Collect results in a funky list
  out <- c()
  # Set up the optimization problem for each threshold
  for(k in 1:length(thresholds)){
    this.threshold <- thresholds[k]
    
    # Store results for this threshold
    threshold.container <- c()
    
    # Before any optimization run, count a "baseline" step which amounts to:
    # 1) Only selecting the baseline strategy, at a cost of 0
    # 2) Counting the number of species affected, and which species
    # 3) Removing the baseline strategy and the species only affected by the baseline
    # This bit must be included in the threshold.container before output
    
    
    # @Laura: "there always needs to be a baseline, and we may as well make it the first row" 
    baseline.idx <- 1
    baseline.strategy <- benefits[baseline.idx,]
    # Threshold and count species
    
    baseline <- (baseline.strategy >= this.threshold)*1
    baseline.species.idx <- which(baseline>0)
    # If the baseline doesn't save any species, don't remove it since it won't matter
    if(length(baseline.species.idx) > 0){
      # Append results to the container and remove
      total.cost <- strategy.costs[baseline.idx]
      baseline.conserved <- species.names[baseline.species.idx]
      strategies <- strategy.names[baseline.idx]
      threshold.container[["baseline"]] <- list(total.cost = total.cost,
                                                species=baseline.conserved,
                                                strategies=strategies,
                                                budget.max = total.cost)
      
      # Edge case: if ALL species are saved by the baseline, return the baseline result and stop here
      if(length(baseline.species.idx) == ncol(benefits)){
        out[[as.character(this.threshold)]] <- threshold.container
        next
      }
      
      
      # Remove the baseline strategy AND the baseline-affected species from the subsequent optimization runs
      # Baseline-affected species are assumed to automatically count towards all strategy combinations, and are thus added in later
      this.benefits <- cbind(benefits)
      this.benefits <- this.benefits[-baseline.idx, -baseline.species.idx]
      this.species.names <- species.names[-baseline.species.idx]
      this.strategy.costs <- strategy.costs[-baseline.idx]
      this.strategy.names <- strategy.names[-baseline.idx]
      if(baseline.idx < all_idx){
        this.all_idx <- all_idx - 1  
      } 
    } else {
      # No species were affected by the baseline, keep things as they are
      this.benefits <- cbind(benefits)
      this.species.names <- species.names
      this.strategy.costs <- strategy.costs
      this.strategy.names <- strategy.names
      this.all_idx <- all_idx
    }
    
  
    for(j in 1:length(budgets)){
      this.budget.max <- budgets[j]
      
      # Solve model for this budget and threshold
      result <- solve.ilp(this.benefits, strategy.cost = this.strategy.costs, budget.max = this.budget.max, all_idx = this.all_idx, threshold = this.threshold)
      
      # Parse results
      parsed <- get.results(result, strategy.names = this.strategy.names, species.names = this.species.names, strategy.costs = this.strategy.costs)
      parsed$threshold <- this.threshold
      parsed$budget.max <- this.budget.max
      
      # Add the baseline species to the results
      parsed$species <- c(parsed$species, baseline.conserved)
      
      # Label the results for later
      budget.name <- as.character(j)
      threshold.container[[budget.name]] <- parsed
      
      # Update the progress bar
      step <- step + 1
      setTxtProgressBar(pb, step)
    }
    threshold.name <- as.character(this.threshold)
    out[[threshold.name]] <- threshold.container
    step <- step + 1
    setTxtProgressBar(pb, step)
  }
  out
}
