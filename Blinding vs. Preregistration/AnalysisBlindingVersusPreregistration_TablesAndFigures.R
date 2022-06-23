## Plots and Tables for Manuscript ##


#setwd('...') set working directory
rm(list=ls())
library(papaja)
library(ggplot2)
library(purrr)
library(stringr)
library(dplyr)
library(rlang)
library(RColorBrewer)
source('../helper_functions/function_variable_table.R')
source('../helper_functions/function_deviations_plot.R')
source('../helper_functions/function_capitalize_words.R')
source('../helper_functions/function_likert_plot.R')
source('../helper_functions/function_likert_plot_odd.R')
source('../helper_functions/function_likert_plot_new.R')

datafile <- 'data/data_prereg_blinding_unblinded.csv' # specify correct path
dat <- read.csv(file = datafile, header=T)

dat$Positions <- gsub(",",";", dat$Positions)
mat_positions <- create_variable_matrix(dat$Positions, colnames = dat$Team)
# reorder mat based on position
mat_positions <- mat_positions[c("Doctoral student","Post-doc","Assistant professor","Associate professor","Full professor"),]

mat_domains <- create_variable_matrix(dat$Domain, colnames = dat$Team)
# reorder mat based on position
mat_domains <- mat_domains[c("Religion and Culture", "Methodology and Statistics","Health",
                             "Social Psychology","Cognition","Psychology (Other)"),]

mat_positions_domains <- rbind(mat_positions, mat_domains)

# Positions:
# preregistration
x_prereg      <- dat$Positions[dat$condition == 'preregistration']
item_list     <- strsplit(x_prereg, split = "; ")
tab_positions <- table(.capwords(trimws(unlist(item_list)))) 
tab_df       <- as.data.frame(round(sort(tab_positions/length(x_prereg) * 100, decreasing = TRUE), 2))
tab_df[,2]    <- paste0(sort(tab_positions, decreasing = TRUE), '/',length(x_prereg), ' (', tab_df[,2], ' %)')
# blinding
x_blind       <- dat$Positions[dat$condition == 'blinding']
item_list     <- strsplit(x_blind, split = "; ")
tab_positions <- table(.capwords(trimws(unlist(item_list)))) 
tab_df0        <- as.data.frame(round(sort(tab_positions/length(x_blind) * 100, decreasing = TRUE), 2))
tab_df[,3]    <- paste0(sort(tab_positions, decreasing = TRUE), '/',length(x_blind), ' (', tab_df0[,2], ' %)')

# Domain:
# preregistration
x_prereg     <-  dat$Domain[dat$condition == 'preregistration']
item_list    <- strsplit(x_prereg, split = "; ")
tab_domains  <- table(trimws(unlist(item_list))) 
tab_df_2     <- as.data.frame(round(sort(tab_domains/length(x_prereg) * 100, decreasing = TRUE), 2))
tab_df_2[,2] <- paste0(sort(tab_domains, decreasing = TRUE), '/',length(x_prereg), ' (', tab_df_2[,2], ' %)')
# blinding
x_blind      <-  dat$Domain[dat$condition == 'blinding']
item_list    <- strsplit(x_blind, split = "; ")
tab_domains  <- table(trimws(unlist(item_list))) 
tab_df_20     <- as.data.frame(round(sort(tab_domains/length(x_blind) * 100, decreasing = TRUE), 2))
tab_df_2[,3] <- paste0(sort(tab_domains, decreasing = TRUE), '/',length(x_blind), ' (', tab_df_20[,2], ' %)')

tab_df           <- rbind(tab_df, tab_df_2)
colnames(tab_df) <- c('', 'Preregistration', 'Analysis Blinding')

apa_table(tab_df, 
          stub_indents = list(`Positions` = 1:5, `Domains` = 6:11), 
          caption = "Positions and domains featured in the analysis teams per condition.",
          note = "Teams may include multiple members of the same position and in the same domain.", 
          landscape = FALSE,
          font_size = "small")

op <- par(cex.main = 2, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
          font.lab = 2, cex.axis = 1.7, bty = "n", las = 1)
# Plot number of deviations per condition
plot_deviations(dat, labels = c("Blinding","Preregistration"),
                position = c(8, 40),
                xlabel = "Deviations")

# Deviations:
# preregistration
dat_sub <- dat[dat$condition=='preregistration', ]
n_teams     <- nrow(dat_sub)
tab_domains <- c('Nr. of Teams Reporting Deviations',
                 'Exclusion Criteria',
                 'Included Variables',
                 'Operationalization of IV',
                 'Statistical Model',
                 'Statistical Test',
                 'Operationalization of DV',
                 'Hypothesis',
                 'Direction of Effect'
                 )
x           <- c(sum(dat_sub$deviation_count > 0),
                sum(dat_sub$DeviateExclusions),
                sum(dat_sub$DeviateVariables),
                sum(dat_sub$DeviateIV),
                sum(dat_sub$DeviateModel),
                sum(dat_sub$DeviateTest),
                sum(dat_sub$DeviateDV),
                sum(dat_sub$DeviateHypotheses),
                sum(dat_sub$DeviateDirection)
                )
tab_df     <- data.frame(domains = tab_domains, prereg_counts = x)
tab_df[,2] <- paste0(x, '/', n_teams, ' (', round((x/n_teams)*100, 2), ' %)')
# blinding
dat_sub <- dat[dat$condition=='blinding', ]
n_teams     <- nrow(dat_sub)
x           <- c(sum(dat_sub$deviation_count > 0),
                 sum(dat_sub$DeviateExclusions),
                 sum(dat_sub$DeviateVariables),
                 sum(dat_sub$DeviateIV),
                 sum(dat_sub$DeviateModel),
                 sum(dat_sub$DeviateTest),
                 sum(dat_sub$DeviateDV),
                 sum(dat_sub$DeviateHypotheses),
                 sum(dat_sub$DeviateDirection)
)
tab_df$blinding_counts <- paste0(x, '/', n_teams, ' (', round((x/n_teams)*100, 2), ' %)')
colnames(tab_df) <- c('', 'Preregistration', 'Analysis Blinding')
apa_table(tab_df, 
          stub_indents = list(`Domains` = 2:9), 
          caption = "Reported deviations form planned analysis per condition.",
          note = "Teams may report multiple deviations.", 
          landscape = FALSE,
          font_size = "small")

# Knowledge Questions: Religion and Well-Being
knowledge_data_theory<- data.frame(question = rep(factor(c('Preregistration', 'Blinding')), each = 5),
                             vars = 'Expertise',
                             levels = rep(factor(c('No knowledge', 2:4, 'Expert'), levels = c('No knowledge', 2:4, 'Expert')), 2),
                             n = c(as.numeric(table(dat[dat$condition=='preregistration',]$TheoreticalKnowledge)), 
                                   as.numeric(table(dat[dat$condition=='blinding',]$TheoreticalKnowledge)))
                             )
# plot
likert_plot_odd(knowledge_data_theory, limits = c(-1,1.2), text_push = 0.1,
                questions = "question", levels = "levels", levels_to_drop = NA, n = "n", textsize = 18, 
                title='"Please rate your knowledge on the topic of\n religion and well-being"')


knowledge_data_methods <- data.frame(question = rep(factor(c('               ', '        ')), each = 5),
                                    vars = 'Expertise',
                                    levels = rep(factor(c('No knowledge', 2:4, 'Expert'), levels = c('No knowledge', 2:4, 'Expert')), 2),
                                    n = c(0, as.numeric(table(dat[dat$condition=='preregistration',]$MethodsKnowledge)), 
                                        c(0, as.numeric(table(dat[dat$condition=='blinding',]$MethodsKnowledge))))
                                    )
# plot
likert_plot_odd(knowledge_data_methods, limits = c(-1,1.2), text_push = 0.1,
                questions = "question", levels = "levels", levels_to_drop = NA, n = "n", textsize = 18, 
                title='"Please rate your knowledge on the topic of\n methodology and statistics"')
