#' Used to build tabluated incidents allowing for partial rotations
#' 
#' @param x two column data frame for which the indicents are to be counted
#' @param table_values The character values for which the incidents of x are to be counted for
#' @param t2 Characters for 2/2 level incident tables
#' @param t3 Characters for 2/3 level incident tables
#' 
#' 
#' @importFrom reshape2 melt
#' @importFrom stringr str_c



cat_and_count_partial <- function(x, table_values, t2, t3){
  if(sum(apply(x[,1:2], 2, function(z) length(unique(z))) == c(3,3)) == 2){
  tab <- table(x[,1], x[,2])
  tab_melt <- melt(tab)
  tab_melt[,4] <- apply(tab_melt[,1:2], 1, str_c, collapse = "")
  tab_melt <- tab_melt[,3:4]
  colnames(tab_melt)[2] <- "pairs"
  t_melt <- merge(tab_melt, table_values, all = TRUE)
  t_melt[is.na(t_melt)] <- 0}
  
  if(sum(apply(x[,1:2], 2, function(z) length(unique(z))) == c(2,2)) == 2){
  tab <- table(x[,1], x[,2])
  tab_melt <- melt(tab)
  tab_melt[,4] <- apply(tab_melt[,1:2], 1, str_c, collapse = "")
  tab_melt <- tab_melt[,3:4]
  colnames(tab_melt)[2] <- "pairs"
  t_melt <- merge(tab_melt, t2, all = TRUE)
  t_melt[is.na(t_melt)] <- 0}
  
  if(sum(apply(x[,1:2], 2, function(z) length(unique(z))) == c(2,3)) == 2){
    x[,1:2] <- x[,2:1]}
    
  if(sum(apply(x[,1:2], 2, function(z) length(unique(z))) == c(3,2)) == 2) {
  tab <- table(x[,1], x[,2])
  tab_melt <- melt(tab)
  tab_melt[,4] <- apply(tab_melt[,1:2], 1, str_c, collapse = "")
  tab_melt <- tab_melt[,3:4]
  colnames(tab_melt)[2] <- "pairs"
  t_melt <- merge(tab_melt, t3, all = TRUE)
  t_melt[is.na(t_melt)] <- 0}
  
  return(t_melt)
}
  
