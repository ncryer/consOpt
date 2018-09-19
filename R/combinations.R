# ------------------------------
# Struct for combinations (for optimize.range())
# ------------------------------

combination <- R6Class("combination",
                       public = list(
                         add.combo = function(input, output){
                           #' Add a combination
                           #'
                           #' @param input A named list of the form: list(strat1="<some name>")
                           #' @param output A named list of the form list(strat1=c("strategy1", "strategy2", "..."))
                           #' @return void

                           combo.idx <- private$combo.counter + 1
                           private$combo.counter <- combo.idx

                           private$combos[[combo.idx]] <- list(input=input, output=output)
                           invisible(self)
                         },

                         get.combos = function(){
                           private$combos
                         }
                       ),
                       private = list(
                         combo.counter = 0,
                         combos = list()
                       ))



parse.combination.matrix <- function(combo.mat){
  # Find combination strategies by identifying columns containing nontrivial combinations
  strategy.combination.size <- apply(combo.mat, 2, function(x) length(which(x != '')))
  combinations.idx <- which(strategy.combination.size > 1 & strategy.combination.size < length(strategy.combination.size))
  combinations <- combo.mat[,combinations.idx]

  # Find strategies that are implemented by several combination strategies
  combo.table <- table(unlist(combinations))
  combo.table <- combo.table[2:length(combo.table)]
  overlaps <- names(combo.table[which(combo.table > 1)])

  # Each strategy containing each overlap must be combined
  combo.container <- combination$new()

  for (overlap in overlaps){
    # Find all strategies containing this overlapping strategy
    to.combine <- c()
    for (i in 1:ncol(combinations)){
      if (overlap %in% combinations[,i]) {
        to.combine <- c(to.combine, colnames(combinations)[i])
      }
    }
    # Combine the found strategies
    input <- list()
    for (i in 1:length(to.combine)){
      input[i] <- to.combine[i]
      names(input)[i] <- paste("strat", i, sep="")
    }
    output <- list()
    for (i in 1:length(to.combine)){
      strat <- list(remove.empty(combinations[,to.combine[i]]))
      output[i] <- strat
      names(output)[i] <- paste("strat", i, sep="")
    }

    combo.container$add.combo(input, output)
  }
  combo.container
}


remove.empty <- function(factorlist){
  out <- as.character(factorlist[factorlist != ""])
  gsub(" ", "", out)
}

