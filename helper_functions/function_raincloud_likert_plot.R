#' Horizontal stacked barplot for Likert-type data + raincloudplot 
#' 
#' The function creates a horizontal stacked barpolot for visualizing
#' Likert-type data with an odd number of response levels. The function can
#' take multiple scales and present them in one figure.
#' 
#' @section Source: The function is based on the code from \url{https://gist.github.com/trinker/0260a9dfdd9531f9b90d9fad2f7b4b12}
#' 
#' @param dat dataframe containing the data for the plot
#' @param labs labels for two time points
#' @param title title of the plot
#' @param levels levels of the likert scale (vector)
#' @param var1 name of the variable for time point 1 
#' @param var2 name of the variable for time point 2
#' @param positive_side if `"lower"` lower levels than the middle are the positive side of the scale, while if `"upper"` levels bigger than the middle are the positive side
#' @param levels_to_drop character vector containing the levels that should not be included e.g. "NA" or "I do not know" 
#' @param limits the limits of the x axis
#' @param text_push the distance between the x axis limits and the text along the y axis
#' @param cols colors for the raincloud and likert bars (vector of 4)
#' @return The function returns a ggplot2 object.
raincloud_likert_plot <- function(dat, title, labs, levels, var1, var2, positive_side = c("lower", "upper"), 
                                  limits = c(-1, 1.2), text_push = 0.1, textsize = 16, 
                                  cols = c("grey36","grey36","#03A89E","#CD7F32")) {
  
  `%ni%` <- Negate(`%in%`)
  nlevels <- length(levels)
  
  # Counts per level of Likert scale
  n1 <- table(factor(dat[,var1], levels=1:nlevels))
  n2 <- table(factor(dat[,var2], levels=1:nlevels))
  
  # Create dataframe with counts
  data <- data.frame(question = rep(factor(labs, levels = labs), each = nlevels),
                     levels = rep(factor(levels, levels = levels), 2), 
                     n = c(n1,n2))
  
  # Filter levels
  likert_plot_data <-
    data %>%
    # Drop unwanted levels
    #filter(.data[[levels]] %ni% levels_to_drop) %>% 
    # Get levels as integer
    mutate(levels_int = as.integer(levels))
  
  # The number of levels should be odd
  if (length(unique(likert_plot_data$levels_int)) %ni% c(3, 5, 7)) {
    stop("The number of levels should 3, 5, or 7!")
  }
  
  # Get middle level
  middle_level <- median(likert_plot_data$levels_int)
  
  # Save color palette 
  pal <- colorRampPalette(c(cols[4],"grey90",cols[3]))
  color_palette <- data.frame(
    colors = pal(max(likert_plot_data$levels_int)),
    levels_int = 1:max(likert_plot_data$levels_int)
  )
  
  # Modify data for plotting
  likert_plot_data <-
    likert_plot_data %>% 
    group_by(question) %>%
    mutate(prop = n / sum(n),
           question = as.factor(question),
           support = case_when(levels_int < middle_level ~ "negative",
                               levels_int ==  middle_level ~ "neutral",
                               levels_int > middle_level ~ "positive",
                               TRUE ~ NA_character_)) %>%
    ungroup() %>% 
    left_join(., color_palette, by = "levels_int") %>% 
    mutate(colors = forcats::as_factor(colors))
  
  # Split the data to positive and negative part
  likert_plot_data_pos <-
    likert_plot_data %>%
    filter(support %in% c("positive", "neutral")) %>%
    mutate(
      colors = factor(colors, levels = rev(levels(colors))),
      prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop))
  
  likert_plot_data_neg <-
    likert_plot_data %>%
    filter(support %in% c("negative", "neutral")) %>%
    mutate(
      prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop),
      prop = -1 * prop)
  
  # Calculate the text labels for the plot
  label_data <-
    likert_plot_data %>%
    group_by(question, support) %>%
    summarise(n_support = sum(n)) %>%
    group_by(question) %>%
    mutate(prop_support = n_support / sum(n_support),
           label = paste0(round_percent(prop_support), "%")) %>%
    ungroup()
  
  divisor <-
    label_data %>%
    filter(support == "neutral") %>%
    mutate(divisor = prop_support / 2) %>%
    dplyr::select(-c(support, label, n_support, prop_support))
  
  label_data <-
    label_data %>%
    left_join(., divisor, by = "question") %>%
    mutate(prop_support = case_when(support %in% c("positive", "negative") ~ prop_support + divisor,
                                    support == "neutral" ~ prop_support),
           proploc = case_when(support == "negative" ~ -1 * prop_support,
                               TRUE ~ prop_support)) %>%
    group_by(support) %>%
    mutate(textloc = case_when(support == "positive" ~ limits[2] - text_push,
                               support == "negative" ~ limits[1] + text_push,
                               support == "neutral" ~ 0)) %>%
    split(.$support)
  
  breaks <- round(seq(limits[1], limits[2], by = 0.5), 1)
  
  labels <- paste0(as.character(abs(breaks) * 100), "%")
  
  # Create the plot
  likert_plot <-
    ggplot() +
    geom_hline(yintercept = 0, color = 'black', size = 0.6) +
    geom_bar(
      data = likert_plot_data_pos,
      aes(x = question, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    )  +
    geom_bar(
      data = likert_plot_data_neg,
      aes(x = question, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    ) +
    labs(y = "Percentage") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_identity(
      labels = unique(likert_plot_data$levels),
      breaks = unique(likert_plot_data$colors),
      guide = "legend",
      name = '')
  
  likert_plot <-
    likert_plot +
    geom_text(
      data = label_data$negative,
      aes(label = label, x = question,  y = textloc),
      color = "black", size = textsize*1/4) +
    geom_text(
      data = label_data$positive,
      aes(label = label, x = question,  y = textloc),
      color = "black", size = textsize*1/4)  +
    geom_text(
      data = label_data$neutral,
      aes(label = label, x = question,  y = textloc),
     color = "black", size = textsize*1/4) +
    theme_classic() +
    scale_y_continuous(limits = limits, breaks = breaks, labels = labels, position="right") +
    xlab("") +
    geom_segment(aes(x=1,xend=2,y=-Inf,yend=-Inf)) +
    geom_segment(aes(y=-1,yend=1,x=Inf,xend=Inf)) +
    theme(
      axis.text = element_text(size = textsize-2),
      axis.title = element_text(size = textsize-2),
      axis.line=element_blank(),
      strip.text = element_text(
        hjust = 0, face = 'bold', 
        size = textsize
      ),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      #legend.key.size = unit(0.8, 'cm'),
      legend.text = element_text(size = textsize*4/5)
    )
  
  dat <- dat %>% 
    dplyr::select(var1,var2) %>%
    na.omit()
  
  # make sure rounded percentages add up to 100%
  text_change <- round_percent(c(
    sum(dat[,var1]<dat[,var2])/nrow(dat),
    sum(dat[,var1]==dat[,var2])/nrow(dat),
    sum(dat[,var1]>dat[,var2])/nrow(dat)
  )) %>%
    paste0(.,"%")
  
  # create dataframe for rainclouds   
  df <- raincloudplots::data_1x1(
    array_1 = jitter(dat[,var1]),
    array_2 = jitter(dat[,var2])
  ) 
   
  raincloud_plot <- df %>% 
    mutate(group = as.factor(x_axis)) %>%
    ggplot(aes(y=y_axis, x=jit)) +    
    geom_line(aes(group=id), color="grey", alpha=.5) +
    geom_point(aes(color=group, fill=group), alpha=0.5, size=2) +
    #geom_point(alpha=0.6, size=2, color="grey36") +
    scale_color_manual(values=cols[c(1,2)]) +
    scale_x_continuous(breaks=c(1,2), labels=labs, limits=c(0.8, 2.2)) +
    scale_y_continuous(breaks = 1:nlevels, limits = c(0.8,nlevels*1.1+.25), labels = levels) +
    xlab("") + 
    ylab("") +
    theme_classic() +
    theme(text = element_text(size=textsize-2),
          axis.text = element_text(size = textsize-2),
          axis.title = element_text(size = textsize-2)) +
    geom_segment(aes(x=1,xend=2,y=-Inf,yend=-Inf))+
    geom_segment(aes(y=1,yend=nlevels,x=-Inf,xend=-Inf))+
    theme(axis.line=element_blank()) +
    theme(legend.position = "none") +
    annotate("segment", x=1.3, xend=1.3, y=nlevels*1.1-0.15, yend = nlevels*1.1+0.15, arrow=arrow(length = unit(.2,"cm"))) +
    annotate(geom="text", y=nlevels*1.1, x=1.22, label = text_change[1], size=textsize*1/4) +
    annotate("segment", x=1.515, xend=1.575, y=nlevels*1.1, yend = nlevels*1.1, arrow=arrow(ends="both", length = unit(.2,"cm"))) +
    annotate(geom="text", y=nlevels*1.1, x=1.45, label = text_change[2], size=textsize*1/4) +
    annotate("segment", x=1.8, xend=1.8, yend=nlevels*1.1-0.15, y = nlevels*1.1+0.15, arrow=arrow(length = unit(.2,"cm"))) +
    annotate(geom="text", y=nlevels*1.1, x=1.72, label = text_change[3], size=textsize*1/4)
  
  # combine raincloud and likert bars   
  combined_plot <- arrangeGrob(
    arrangeGrob(
      raincloud_plot,
      likert_plot,
      ncol=2, widths = c(3,2),
      top = textGrob(title,
                     gp=gpar(fontsize=textsize*1.2), x=0.12, hjust=0)
    )
  )
  
    # Return output
  return(combined_plot)
}

round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}

round_percent_decimal <- function(x) { 
  x <- x/sum(x)*1000  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<1000) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(1000-rsum)]] <- res[o[1:(1000-rsum)]]+1
  } 
  res/10
}
