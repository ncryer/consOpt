#' Plot results of running optimize.range
#'
#' asdasd
#'
#' @param summary.results A dataframe containing the results of the complementarity optimization
#' @param draw.labels Boolean, indicating whether to draw strategy names on the plot
#'
#' @return A ggplot object
#' @export
#'
#' @examples
plot.results <- function(summary.results, draw.labels=TRUE){
  # Create a plot object from the neat results table
  tmp <- summary.results
  # scale
  tmp$total_cost <- (tmp$total_cost / 10^6)

  # Create plot object
  this.plot <- ggplot(tmp, aes(x=total_cost, y=number_of_species, group=threshold, shape=factor(threshold), label=strategies)) +
    geom_step(aes(color=factor(threshold))) +
    geom_point(aes(color=factor(threshold), size=5), show.legend=FALSE) +
    scale_y_continuous(labels = function (x) floor(x), breaks=min(tmp$number_of_species):max(tmp$number_of_species)) +
    labs(x="Total cost (millions)", y = "No. of species groups conserved", color="Persistence threshold")

  if(draw.labels){
    this.plot <- this.plot + geom_text_repel()
  }

  plot(this.plot)
  this.plot
}

