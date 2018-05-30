library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)


optimize <- function(benefits, strategy.costs, budget.max=FALSE, thresholds = c(90, 75, 50)){
  
  # Declare a maximum budget for the constrained optimization
  if(!budget.max){
    # No max, assume more money than all the strategies combined
    budget.max <- sum(strategy.costs)
  }
  
  # Set up the optimization problem for each threshold
  for(k in 1:length(thresholds)){
    this.threshold <- thresholds[k]
    # Binarize the benefits matrix according to the current threshold
    B <- (benefits >= this.threshold)*1
    
    # Number of strategies 
    n <- nrow(B)
    # Number of species
    m <- ncol(B)
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
      add_constraint(sum_expr(y[i]*strategy_cost[i], i = 1:n) <= budget_max, i = 1:n)
      
    # Solve the model
    result <- solve_model(opt, with_ROI(solver="lpsolve", verbose=FALSE))
    
    
  }
}