library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)
source('plotutil.R')
source('optfun.R')

# Load in and prepare the data
# ----------------------------

# Benefits per strategy (rows) for each species (columns)
benefits <- read.csv("testdata2/BijFRE.csv")
rownames(benefits) <- benefits$X
benefits <- benefits[,-1]
# Cost of implementing each strategy
costs <- read.csv("testdata2/costFRE.csv")
strategy_cost <- costs$Cost


# Run the optimization over a default number of budget steps, over a default number of threshold

results <- optimize.range(benefits, strategy_cost, budget.max = max(strategy_cost), all_idx=15)

# Plot the results
plt <- do.plot(results)

# Results can be queried by indexing into the results object, organized by threshold and then by budget

thresholds <- names(results)

# View all runs for the first threshold

runs <- results[[thresholds[1]]]

budgets <- names(runs)

# View results for the first budget

runs[[budgets[1]]]

# Results can also be condensed into a non-redundant data table, showing unique budget and strategy combinations for each threshold

neat.results <- summary.results.df(results)


# Additional tests
# ----------------

benefits <- read.csv("testdata2/opt_t3.csv")
rownames(benefits) <- benefits$X
benefits <- benefits[,-1]
# Cost of implementing each strategy
costs <- read.csv("testdata2/costFRE.csv")
strategy_cost <- costs$Cost

results <- optimize.range(benefits, strategy_cost, budget.max = max(strategy_cost), all_idx=15)
neat.results <- summary.results.df(results)
