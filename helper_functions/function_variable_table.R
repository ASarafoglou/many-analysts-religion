# functions for variable tables

str_arrange <- function(x){
  x %>%
    stringr::str_split("; ") %>% # Split string into letters
    purrr::map(~sort(trimws(.)) %>% paste(collapse = ";")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

create_variable_matrix <- function(x, colnames, order_percentage = FALSE){
  x <- str_arrange(x)
  item_list <- strsplit(x, split = ";")
  item_names <- unique(unlist(item_list))
  mat <- sapply(item_list, function(x) item_names %in% x)
  rownames(mat) <- item_names
  colnames(mat) <- colnames
  if(order_percentage == TRUE ){
    order_rows <- order(round(rowMeans(mat)*100,0), decreasing = TRUE)
    mat <- mat[order_rows, ]
  }
  return(mat)
}
create_variable_table <- function(x, note, caption, include_counts=TRUE){
  Percentage <- paste0(round(rowMeans(x)*100,0),"%")
  Counts <- c(as.character(colSums(x)),"")
  mat <- ifelse(x==TRUE, "X","")
  percol <- NULL
  if(include_counts){
    mat <- rbind(cbind(mat,Percentage), Counts)
    percol <- which(colnames(mat) == 'Percentage')}
  
  return(list(
    apa_table(mat[, c(1:30, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(1:30, percol)]))), 
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(31:60, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(31:60, percol)]))), 
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(61:90, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(61:90, percol)]))), 
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(91:ncol(mat))], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(91:ncol(mat), percol)]))), 
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny")
  )
  )
}

create_variable_table_long <- function(x, note, caption, include_counts=TRUE){
  Percentage <- paste0(round(rowMeans(x)*100,0),"%")
  Counts <- c(as.character(colSums(x)),"")
  mat <- ifelse(x==TRUE, "X","")
  percol <- NULL
  if(include_counts){
    mat <- rbind(cbind(mat,Percentage), Counts)
    percol <- which(colnames(mat) == 'Percentage')}
  
  row_names <- c(rownames(mat),"",rownames(mat))
  
  return(list(
    apa_table(cbind(row_names, rbind(mat[, c(1:30, percol)], colnames(mat)[c(31:60, percol)], mat[, c(31:60, percol)])), 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(1:30, percol)]))), 
              midrules = c(nrow(mat), nrow(mat)+1),
              row.names = FALSE, 
              col.names = c("", colnames(mat)[c(1:30, percol)]),
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(rbind(mat[, c(61:90, percol)], colnames(mat)[c(91:ncol(mat), percol)], mat[, c(91:ncol(mat), percol)]) ,
              col_spanners = list(`Team` = c(2,ncol(mat[, c(61:90, percol)]))), 
              midrules = c(nrow(mat), nrow(mat)+1),
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny")
  )
  )
}

create_variable_table_combined <- function(x, note, caption, include_counts=TRUE){
  Percentage <- paste0(round(rowMeans(x)*100,0),"%")
  Counts <- c(as.character(colSums(x)),"")
  mat <- ifelse(x==TRUE, "X","")
  percol <- NULL
  if(include_counts){
    mat <- rbind(cbind(mat,Percentage), Counts)
    percol <- which(colnames(mat) == 'Percentage')}
  
  return(list(
    apa_table(mat[, c(1:30, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(1:30, percol)]))), 
              midrules = 5,
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(31:60, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(31:60, percol)]))), 
              midrules = 5,
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(61:90, percol)], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(61:90, percol)]))), 
              midrules = 5,
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny"),
    apa_table(mat[, c(91:ncol(mat))], 
              col_spanners = list(`Team` = c(2,ncol(mat[, c(91:ncol(mat), percol)]))), 
              midrules = 5,
              caption = caption,
              note = note, 
              landscape = TRUE,
              font_size = "tiny")
  )
  )
}

create_long_table <- function(x, note, caption, include_counts=TRUE){
  Percentage <- paste0(round(rowMeans(x)*100,0),"%")
  Counts <- c(as.character(colSums(x)),"")
  mat <- ifelse(x==TRUE, "X","")
  percol <- NULL
  if(include_counts){
    mat <- rbind(cbind(mat,Percentage), Counts)
    percol <- which(colnames(mat) == 'Percentage')}
  
  mat <- t(mat)
  
  return(list(
    apa_table(mat, 
              caption = caption,
              note = note,
              longtable = TRUE,
              font_size = "tiny")
  ))
}
