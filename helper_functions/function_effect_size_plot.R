# effect size plot
plotter <- function(effect, upper, lower, main, ylab, yrange, add_subjective=T, subj_evidence){
  par(mar=c(3, 4, 3, 1)+0.1, font.main=1)
  
  #Effect plot
  ord <- order(effect)
  J <- length(effect)
  col_base <- ifelse(add_subjective, "white","black")
  
  plot(1:J, effect[ord]
       , pch = 3
       , ylab = ""
       , xlab = ""
       , col = "darkgray"
       , axes = F
       , ylim = yrange
       , cex = 1.2
       , cex.lab = 1.2
       , type = "n")
  mtext(side = 1, line = 1.5, "Analysis Team", cex = 1)
  mtext(side = 2, line = 2.5, ylab, cex = 1)
  arrows(1:J, lower[ord], 1:J, upper[ord], code=3, angle = 90, length = .05)
  points(1:J
         , effect[ord]
         , pch = 19
         , col = col_base
         , cex = 1.2)
  if(add_subjective) {
    points(1:J
           , effect[ord]
           , pch = 19
           , col = cols[subj_evidence[ord]]
           , cex = 1.2)
    legend("topleft", pch = 19, col = cols, box.lty = 0, title = "Subjective evidence evaluation",
           legend = c("Evidence in favor of the hypothesis", "Ambiguous evidence","Evidence against the hypothesis"), cex=0.9)
  }
  axis(side = 1, c(1, J))
  axis(side = 2, las = 1)
  lines(c(1,J),c(0,0),lty=1)
  if(add_subjective) {title(main = main, font=1, line=0.5)}
  if(!add_subjective) {
    par(mar=c(3, 4, 5, 1)+0.1, font.main=1)
    title(main = main, font=1, line=0.5, cex.main=1.5)
  }
}

