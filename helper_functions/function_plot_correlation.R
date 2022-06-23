# function to plot the correlation between an ordinal and a continuous measure 

correlation_plot = function(x, y, subj_eval, yrange=c(-0.2,0.5), xlab, ylab,
                            xlabels = c("Very\nunlikely","Very\nlikely"), title, 
                            xlevels = 7, cols = c('#03A89E', 'grey70', '#CD7F32')){
  
  data <- data.frame(x=x, y=y, subj_eval=subj_eval)
  data$eval_fact <- factor(data$subj_eval, levels = c("1","2","3"), 
                           labels = c("Evidence in favor of H1","Ambiguous evidence","Evidence against H1"))
  data <- na.omit(data)
  
  blankPlot <- ggplot() +
    geom_blank(aes(1,1)) +
    theme(line = element_blank(),
          text  = element_blank(),
          title = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  yDense <- ggplot(data, aes(x=y, fill=eval_fact)) + 
    geom_density(aes(y= ..count..),trim=F,alpha=.8) + 
    xlab("") + ylab("") + xlim(yrange) +
    scale_fill_manual(values=cols) +
    theme_void() +
    coord_flip() + 
    theme(legend.position = "none", plot.margin = unit(c(0,0,5,0), "lines")) 
  
  xHist <- ggplot(data, aes(x=x, fill=eval_fact)) +
    geom_histogram(aes(y= ..count..), binwidth = 1, alpha=.8, color="black") +
    xlab("") + ylab("") + xlim(0.5,xlevels+0.5) +
    scale_fill_manual(values=cols) +
    theme_void() +
    theme(legend.position = "none", plot.margin = unit(c(0,0,0,6), "lines")) 
  
  scatterP <- ggplot(data, aes(x=jitter(x), y=y)) +
    geom_rug(aes(color=eval_fact),size=1,sides="b",alpha=.8) + 
    geom_rug(aes(color=eval_fact),,size=1,sides="l",alpha=.8) + 
    geom_hline(aes(yintercept=0),linetype=2) +
    geom_point(aes(fill=eval_fact), shape=21, alpha=0.8, size=4) +
    theme_classic() +
    scale_x_continuous(name = xlab, limits= c(0.5,xlevels+0.5), breaks = 1:xlevels, labels = c(xlabels[1],2:(xlevels-1),xlabels[2])) +
    scale_y_continuous(name = ylab, limits= yrange) +
    scale_fill_manual(values=cols, name="Subjective evaluation") +
    scale_color_manual(values=cols, name="Subjective evaluation") +
    geom_segment(aes(x=1,xend=xlevels,y=-Inf,yend=-Inf)) +
    geom_segment(aes(y=yrange[1],yend=yrange[2],x=-Inf,xend=-Inf)) +
    theme(axis.line=element_blank()) +
    theme(legend.position=c(x=1.2,y=1.2), legend.text=element_text(size=10), 
          legend.title=element_text(size=12), plot.margin = unit(c(0,0,2,2), "lines"), 
          axis.text.x=element_text(size=14),  axis.text.y=element_text(size=14), 
          axis.title.x=element_text(size=14, vjust=-1.6), axis.title.y=element_text(size=14, vjust=2.6)) 
    
  p <- arrangeGrob(xHist, blankPlot, scatterP, yDense, ncol=2, nrow=2, widths=c(4, 1.3), heights=c(1, 4),
               top = textGrob(paste0(title,"\n"),gp=gpar(fontsize=18,font=1),x=.15, hjust=0))
  return(p)
}
