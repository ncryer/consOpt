# Load in and prepare the data
# ----------------------------

Bij_fre_01 <- read.csv("Bij_fre_01.csv")
cost_fre_01 <- read.csv("cost_fre_01.csv")

# Prep benefits
rownames(Bij_fre_01) <- Bij_fre_01$X
Bij_fre_01$X <- NULL

# Prep costs
cost.vector <- cost_fre_01$Cost
Species_weight <- read.csv("Species_weight.csv")

COMBO_info <- read.csv("testdata2/COMBO_info.csv")
