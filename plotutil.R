library(ggplot2)


do.plot <- function(range.result){
  
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
    
    # Do plotting stuff here -- add 
  }
  
  this.plot <- ggplot(plot.df, aes(x=totalcost, y=numspecies, group=threshold, shape=factor(threshold))) + 
    geom_step(aes(color=factor(threshold))) + 
    geom_point(aes(color=factor(threshold), size=5), show.legend=FALSE) +
    scale_y_continuous(labels = function (x) floor(x)) +
    labs(x="Total cost", y = "No. of species groups conserved", color="Persistence threshold")
  
  plot(this.plot)
  this.plot
}

