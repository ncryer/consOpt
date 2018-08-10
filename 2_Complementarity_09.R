library(ompr)
library(ompr.roi)
library(ROI.plugin.lpsolve)
library(magrittr)
source('plotutil.R')
source('debugging.R')

# ------------------------------
# SOD 
# ------------------------------

Bij_fre_01 <- read.csv("debug09072018/Bij_sod.csv")
cost_fre_01 <- read.csv("debug09072018/cost_sod.csv")

# Prep benefits 
rownames(Bij_fre_01) <- Bij_fre_01$X
Bij_fre_01$X <- NULL

# Prep costs 
cost.vector <- cost_fre_01$Cost

# ------------------------------
# One-off optimization runs
# ------------------------------

# Initialize the optimization object for threshold=60.01
testOpt70 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=19,
                           t=70.01)


# Initialize the optimization object for threshold=60.01
testOpt60 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=19,
                           t=60.01)

#' The creation of the optStruct takes care of thresholding and various manipulations to do with the baseline,
#' as well as preparing the benefits matrix for computation (rounding etc.)

# Initialize the optimization object for threshold=50.01
testOpt50 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=19,
                           t=50.01)


input <- list(strat1="S15", strat2="S16", strat3="S17")
output <- list(strat1=c("S9", "S14"), strat2=c("S3", "S8", "S9","S10", "S14"), strat3=c("S8", "S9","S10", "S14"))

testOpt60$add.combo(input.strategies = input, combined.strategies = output)



# ------------------------------
# Optimizing over a range of budgets and thresholds
# ------------------------------

combo <- combination$new()
# And adds (any number of) combinations by calling add.combo()
combo$add.combo(input, output) # Add the S12+S13 

# The combination object is then passed to the optimize.range() function as an argument. If this argument is NULL (or just not supplied),
# no combo information is added. 
test.range <- optimize.range(Bij_fre_01, cost.vector, all.index = 19)

# Plotting the results is straightforward, as much of the work is done 
neat.plot(test.range)

# ------------------------------
# FRE 
# ------------------------------


Bij_fre_01 <- read.csv("testdata2/Bij_fre_opt.csv")
cost_fre_01 <- read.csv("testdata2/cost_fre_01.csv")

# Prep benefits 
rownames(Bij_fre_01) <- Bij_fre_01$X
Bij_fre_01$X <- NULL

# Prep costs 
cost.vector <- cost_fre_01$Cost

# ------------------------------
# One-off optimization runs
# ------------------------------

# Initialize the optimization object for threshold=60.01
testOpt70 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=15,
                           t=70.01)


# Initialize the optimization object for threshold=60.01
testOpt60 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=15,
                           t=60.01)

#' The creation of the optStruct takes care of thresholding and various manipulations to do with the baseline,
#' as well as preparing the benefits matrix for computation (rounding etc.)

# Initialize the optimization object for threshold=50.01
testOpt50 <- optStruct$new(B=Bij_fre_01,
                           cost.vector=cost.vector,
                           all.index=15,
                           t=50.01)





# ------------------------------
# Optimizing over a range of budgets and thresholds
# ------------------------------

# The combination object is then passed to the optimize.range() function as an argument. If this argument is NULL (or just not supplied),
# no combo information is added. 
test.range <- optimize.range(Bij_fre_01, cost.vector, all.index = 15)
# Plotting the results is straightforward, as much of the work is done 
neat.plot(test.range)
