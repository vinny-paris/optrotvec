#' Used to build tabluated incidents
#' 
#' @param x two column data frame for which the indicents are to be counted
#' @param table_values The character values for which the incidents of x are to be counted for
#' 
#' @importFrom reshape2 melt
#' @importFrom stringr str_c

cat_and_count <- function(x, table_values){
  tab <- table(x[,1], x[,2])
  tab_melt <- melt(tab)
  tab_melt[,4] <- apply(tab_melt[,1:2], 1, str_c, collapse = "")
  tab_melt <- tab_melt[,3:4]
  colnames(tab_melt)[2] <- "pairs"
  t_melt <- merge(tab_melt, table_values, all = TRUE)
  t_melt[is.na(t_melt)] <- 0
  return(t_melt)
}
  
