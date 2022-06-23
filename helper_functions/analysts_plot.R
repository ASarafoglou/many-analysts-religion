plot_analysts_descriptives = function(data, colors, labelsx, main){
  ### Plot settings
  cols <- colors
  x <- 1:nrow(data)
  labelsy <- c("Psychology (other)","Cognition","Social Psycholgy","Health","Methods & Statistics","Religion & Culture",
               "Doctural student","Post-doc","Assistant professor","Associate professor","Full professor")
  I <- nrow(data)
  ys <- c(4,4.5,5,5.5,6,6.5, 8.5,9,9.5,10,10.5) - 3
  ### format data
  # A. Domains 
  religion  <- as.numeric(grepl("Religion", data$Domain, fixed = TRUE)) * ys[6]
  methods   <- as.numeric(grepl("Methodology", data$Domain, fixed = TRUE)) * ys[5]
  health    <- as.numeric(grepl("Health", data$Domain, fixed = TRUE)) * ys[4]
  social    <- as.numeric(grepl("Social Psychology", data$Domain, fixed = TRUE)) * ys[3]
  cognition <- as.numeric(grepl("Cognition", data$Domain, fixed = TRUE)) * ys[2]
  psych     <- as.numeric(grepl("Other", data$Domain, fixed = TRUE)) * ys[1]
  counts_domains <- c(sum(psych)/ys[1], sum(cognition)/ys[2], sum(social)/ys[3],
              sum(health)/ys[4], sum(methods)/ys[5], sum(religion)/ys[6])
  # B. Positions
  prof  <- as.numeric(grepl("Full", data$Positions, fixed = TRUE)) * ys[11]
  assoc <- as.numeric(grepl("Associate", data$Positions, fixed = TRUE)) * ys[10]
  assis <- as.numeric(grepl("Assistant", data$Positions, fixed = TRUE)) * ys[9]
  post  <- as.numeric(grepl("Post-doc", data$Positions, fixed = TRUE)) * ys[8]
  phd   <- as.numeric(grepl("Doctoral", data$Positions, fixed = TRUE)) * ys[7]
  counts_positions <- c(sum(phd)/ys[7], sum(post)/ys[8], sum(assis)/ys[9],
              sum(assoc)/ys[10], sum(prof)/ys[11])
  
  counts <- c(counts_domains, counts_positions)
  # plot A 
  par(mar = c(4,9,2,4))
  plot(x = x, y = rep(ys[1],I),
       pch = 19, col = cols[1],
       ylim = c(1,8), axes = FALSE,
       xlab = "", ylab = "", type = "n")
  points(religion,  pch = 15, cex = 1.3, col = cols[1])
  points(methods,   pch = 15, cex = 1.3, col = cols[1])
  points(health,    pch = 15, cex = 1.3, col = cols[1])
  points(social,    pch = 15, cex = 1.3, col = cols[1])
  points(cognition, pch = 15, cex = 1.3, col = cols[1])
  points(psych,     pch = 15, cex = 1.3, col = cols[1])
  # plot B
  points(prof,  pch = 18, cex = 1.6, col = cols[2])
  points(assoc, pch = 18, cex = 1.6, col = cols[2])
  points(assis, pch = 18, cex = 1.6, col = cols[2])
  points(post,  pch = 18, cex = 1.6, col = cols[2])
  points(phd,   pch = 18, cex = 1.6, col = cols[2])
  
  axis(1, at = c(1,I), labels = labelsx)
  axis(2, at = ys, labels = labelsy, las = 1)
  axis(4, at = ys, labels = counts, las = 1)
  mtext("Teams", line = 1.5, side = 1)
  p <- par('usr')
  text(p[2]+5, mean(p[3:4]), labels = 'Frequency', xpd = NA, srt = -90)
  lines(x = c(1,I), y = c(4.5,4.5), lty=2)
  title(main = main, font.main = 1, cex.main = 2.5)
}

plot_variables = function(data, colors, labelsx, main){
  ### Plot settings
  cols <- colors
  x <- 1:nrow(data)
  labelsy <- c("Psychology (other)","Cognition","Social Psycholgy","Health","Methods & Statistics","Religion & Culture",
               "Doctural student","Post-doc","Assistant professor","Associate professor","Full professor")
  I <- nrow(data)
  ys <- c(4,4.5,5,5.5,6,6.5, 8.5,9,9.5,10,10.5) - 3
  ### format data
  
  
  counts <- c(counts_domains, counts_positions)
  # plot A 
  par(mar = c(4,9,2,4))
  plot(x = x, y = rep(ys[1],I),
       pch = 19, col = cols[1],
       ylim = c(1,8), axes = FALSE,
       xlab = "", ylab = "", type = "n")
  points(religion,  pch = 15, cex = 1.3, col = cols[1])
  points(methods,   pch = 15, cex = 1.3, col = cols[1])
  points(health,    pch = 15, cex = 1.3, col = cols[1])
  points(social,    pch = 15, cex = 1.3, col = cols[1])
  points(cognition, pch = 15, cex = 1.3, col = cols[1])
  points(psych,     pch = 15, cex = 1.3, col = cols[1])
  # plot B
  points(prof,  pch = 18, cex = 1.6, col = cols[2])
  points(assoc, pch = 18, cex = 1.6, col = cols[2])
  points(assis, pch = 18, cex = 1.6, col = cols[2])
  points(post,  pch = 18, cex = 1.6, col = cols[2])
  points(phd,   pch = 18, cex = 1.6, col = cols[2])
  
  axis(1, at = c(1,I), labels = labelsx)
  axis(2, at = ys, labels = labelsy, las = 1)
  axis(4, at = ys, labels = counts, las = 1)
  mtext("Teams", line = 1.5, side = 1)
  p <- par('usr')
  text(p[2]+5, mean(p[3:4]), labels = 'Frequency', xpd = NA, srt = -90)
  lines(x = c(1,I), y = c(4.5,4.5), lty=2)
  title(main = main, font.main = 1, cex.main = 2.5)
}
