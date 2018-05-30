library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)

# Load in and prepare the data
# ----------------------------

# Benefits per strategy (rows) for each species (columns)
benefits <- read.csv("Bij.csv")
rownames(benefits) <- benefits$X
benefits <- benefits[,-1]
# Cost of implementing each strategy
costs <- read.csv("cost.csv")
strategy_cost <- costs$Cost
# Set up the optimization problem
# -------------------------------

# Number of strategies
n <- nrow(benefits)
# Number of species
m <- ncol(benefits)
# Index of the "all" strategy, which deselects every other strategy
all_idx <- 19
budget_max <- max(strategy_cost)

# [Index variables]
strategies <- 1:n
targets <- 1:m
others <- 2:(all_idx-1) 

# Set up the model
opt <- MIPModel() %>%
  
    # Decision variables 
    # ------------------

    # X[i,j] binary selection matrix 
    add_variable(X[i,j], i = 1:n, j = 1:m, type="binary") %>%
    # y[i] Strategy selection vector
    add_variable(y[i], i = 1:n, type="binary") %>%
  
    # Objective function
    # ------------------
    set_objective(sum_expr(benefits[i,j] * X[i,j], i = 1:n, j = 1:m)) %>%
  
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

# Solve

result <- solve_model(opt, with_ROI(solver="lpsolve", verbose=FALSE))

# Get the resulting assignment matrix - which strategies apply to which species
assignments <- get_solution(result, X[i,j])$value %>% matrix(., nrow=n, ncol=m)
rownames(assignments) <- rownames(benefits); colnames(assignments) <- colnames(benefits)
# Get the resulting strategies to employ
strategies <- get_solution(result, y[i])
strategies$variable <- rownames(benefits)
# Print selected strategies
strategies[strategies$value==1,]
# Get the cost of implementing these strategies
total_cost <- sum(strategies$value * strategy_cost)
total_cost
