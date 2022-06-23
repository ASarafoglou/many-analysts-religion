# function to make butterfly plot with variable inclusion rates
butterfly_plot = function(dat1, dat2, xrange=c(0,1)){
  incl_prop1 <- rowMeans(dat1) #left side 
  incl_prop2 <- rowMeans(dat2) #right side
  
  # if there are more items for RQ2, add that row to RQ1
  if(length(incl_prop2)>length(incl_prop1)) {
    mis <- names(incl_prop2[!(names(incl_prop2) %in% names(incl_prop1))])
    incl_prop1 <- c(incl_prop1, "new" = 0)
    incl_prop1 <- plyr::rename(incl_prop1, replace = c("new"=mis))
  }
    
  # make sure the variables are in the same order between RQs  
  incl_prop2 <- incl_prop2[order(factor(names(incl_prop2), levels = names(incl_prop1)))]

  # reorder based on proportion included RQ1
  ord <- order(incl_prop1,decreasing = FALSE)
  incl_prop1 <- incl_prop1[ord]
  incl_prop2 <- incl_prop2[ord]
  
  names(incl_prop1) <- gsub("_"," ", names(incl_prop1))
  names(incl_prop2) <- gsub("_"," ", names(incl_prop2))
  names(incl_prop1) <- .capwords(names(incl_prop1))
  names(incl_prop2) <- .capwords(names(incl_prop2))
  
  layout(matrix(c(1,1,2,3,3), nrow = 1, ncol = 5))
  par(mar=c(4,0.4,2,0)+0.2, cex.main = 1.4)
  pos <- barplot(height = incl_prop1, axes = FALSE, xlim = rev(xrange), width = 1, cex.names = 1, las = 2, 
                 horiz = TRUE, names.arg = FALSE)
  axis(1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)* 100, lwd = 1, cex.axis = 1)
  title(main = "Research Question 1", adj=1)
  mtext("Percentage", side = 1, line = 3.1, cex = 1)
  plot(-10, xlim=c(0,1),ylim=c(0.4,max(pos)+0.4), axes = FALSE, bty = 'n', xlab="")
  text(x=0.5, y=pos, labels = names(incl_prop1), cex=1.1)
  par(mar=c(4,0.4,2,0.5)+0.2, cex.main = 1.4)
  barplot(height = incl_prop2, axes = FALSE, xlim = xrange, width = 1, cex.names = 1, las = 2, 
          horiz = TRUE, names.arg = FALSE)
  axis(1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)* 100, lwd = 1, cex.axis = 1)
  title(main = "Research Question 2", adj=0)
  mtext("Percentage", side = 1, line = 3.1, cex = 1)
}
