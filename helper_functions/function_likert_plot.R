likert_plot <- function(likert_plot_data, limits, text_push, title){
  likert_plot_data <- as_tibble(likert_plot_data)
  # Split the data to positive and negative part
  likert_plot_data_pos <-
    likert_plot_data %>% 
    filter(support %in% c("positive", "neutral")) %>% 
    mutate(colors = factor(colors, levels = rev(levels(colors))),
           prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop)) 
  
  likert_plot_data_neg <-
    likert_plot_data %>% 
    filter(support %in% c("negative", "neutral")) %>% 
    mutate(prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop),
           prop = -1 * prop)
  
  # Calculate the text labels for the plot
  label_data <- 
    likert_plot_data %>%
    group_by(vars, groups, support) %>%
    summarise(n_support = sum(n)) %>% 
    group_by(vars,groups) %>%
    mutate(prop_support = n_support / sum(n_support),
           label = paste0(round(prop_support * 100, 0), "%")) %>% 
    ungroup()
  
  divisor <- 
    label_data %>%
    filter(support == "neutral") %>% 
    mutate(divisor = prop_support / 2) %>% 
    select(-c(support, label, n_support, prop_support))
  
  label_data <- 
    label_data %>% 
    left_join(., divisor, by = c("vars","groups")) %>% 
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
    geom_bar(
      data = likert_plot_data_pos, 
      aes(x = groups, y = prop, fill = colors),
      position = "stack", 
      stat = "identity"
    )  + 
    geom_bar(
      data = likert_plot_data_neg, 
      aes(x = groups, y = prop, fill = colors),
      position = "stack", 
      stat = "identity"
    ) +
    coord_flip()  +
    geom_hline(yintercept = 0, color = 'white', size = 1) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_identity(
      labels = unique(likert_plot_data$levels), 
      breaks = unique(likert_plot_data$colors), 
      guide = "legend", 
      name = '')
  likert_plot <- 
    likert_plot +
    labs(title = title)
  likert_plot <- 
    likert_plot +
    geom_text(
      data = label_data$negative, 
      aes(label = label, x = groups,  y = textloc), 
      hjust = 0, color = "grey50", size = 6) +
    geom_text(
      data = label_data$positive,
      aes(label = label, x = groups,  y = textloc), 
      hjust = 1, color = "grey50", size = 6)  +
    geom_text(
      data = label_data$neutral,
      aes(label = label, x = groups,  y = textloc), 
      hjust = .5, color = "grey50", size = 6) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = limits,
      breaks = breaks,
      labels = labels) +
    theme(
      title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = 'bold', size = 25),
      panel.border = element_rect(color = 'gray90', linetype = "dashed", fill = NA),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 15))
  
  return(likert_plot)
}
