# plot for count data of deviations from analysis plan
plot_deviations = function(data, labels, xlabel, positions = 'topright'){
  cols <- RColorBrewer::brewer.pal(3, "Dark2")[1:2]
  # create plots
  data <- t(with(data, table(deviation_count,condition)))
  #get value rounded to 10 for y limits
  ymax <- .roundUp(max(data))
  colnames(data) <- c("None",1,2,3)
  barplot(data, col = alpha(cols,0.6),
          beside = T,
          ylim = c(0,ymax), axis.lty = 1,
          ylab = "Count", xlab=xlabel)
  if(is.character(positions)){
    legend(positions, legend=labels,
           fill = alpha(cols, 0.6),
           box.lty = 0)
  } else {
    legend(x = positions[1], y = positions[2], legend=labels,
           fill = alpha(cols, 0.6),
           box.lty = 0,
           cex = 1.5)
  }

}

#function to round up to 10
.roundUp <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}


