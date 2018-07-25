library(ggplot2)


neat.plot <- function(summary.results){
  # Create a plot object from the neat results table
  tmp <- summary.results
  # scale
  tmp$total_cost <- (tmp$total_cost / 10^6)
  
  
  # Create plot object
  this.plot <- ggplot(tmp, aes(x=total_cost, y=number_of_species, group=threshold, shape=factor(threshold))) +
    geom_step(aes(color=factor(threshold))) + 
    geom_point(aes(color=factor(threshold), size=5), show.legend=FALSE) +
    scale_y_continuous(labels = function (x) floor(x), breaks=min(tmp$number_of_species):max(tmp$number_of_species)) +
    labs(x="Total cost (millions)", y = "No. of species groups conserved", color="Persistence threshold")
  
  plot(this.plot)
  this.plot
}



range.plot <- function(range.result){
  # This function is deprecated, see neat.plot
  plot.df <- data.frame()
  
  for(threshold.name in names(range.result)){
    this.threshold <- range.result[[threshold.name]]
    
    total_species <- c()
    total_cost <- c()
    
    for(budget.name in names(this.threshold)){
      this.budget <- this.threshold[[budget.name]]
      
      this.cost <- this.budget$total.cost
      this.totalspecies <- length(this.budget$species)
      
      total_species <- c(total_species, this.totalspecies)
      total_cost <- c(total_cost, this.cost)
    }
    
    tau <- rep(threshold.name, length(total_species))
    #return(list(tau=tau, species=total_species, cost=total_cost))
    tmp.df <- data.frame(totalcost=total_cost, numspecies=total_species, threshold=tau)
    plot.df <- rbind(plot.df, tmp.df)
  }
  
  # Sanitize the plotting dataframe:
  # 1) scale budget values to millions
  # 2) Remove data points for budgets that failed to save additional species
  plot.df <- scale.costs.plot.df(plot.df)
  plot.df <- sanitize.plot.df(plot.df)
  
  this.plot <- ggplot(plot.df, aes(x=totalcost, y=numspecies, group=threshold, shape=factor(threshold))) + 
    geom_step(aes(color=factor(threshold))) + 
    geom_point(aes(color=factor(threshold), size=5), show.legend=FALSE) +
    scale_y_continuous(labels = function (x) floor(x), breaks=min(plot.df$numspecies):max(plot.df$numspecies)) +
    labs(x="Total cost (millions)", y = "No. of species groups conserved", color="Persistence threshold")
  
  plot(this.plot)
  this.plot
}





sanitize.plot.df <- function(plot.df){
  # Remove budget runs that don't add additional species for the cost
  tmp.df <- unique(plot.df)
  # Remove rows with duplicated species and threshold
  tmp.df <- tmp.df[!duplicated(tmp.df[,c("numspecies", "threshold")]),]
  tmp.df
}

scale.costs.plot.df <- function(plot.df){
  # Scale costs to millions
  plot.df$totalcost <- plot.df$totalcost / (10^6)
  plot.df
}
