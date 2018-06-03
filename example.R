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
