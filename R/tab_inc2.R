#' Tabulates Incidents
#' 
#' Twin of tab_inc except this is for a 0/1 design only.
#' 
#' @param d the design to be tabulated
#' 
#' @importFrom utils combn


tab_inc2 <-  function(d){
  f <- dim(d)[2]
  


  table_values <- c('00', '10', 
                  '01', '11')

   tt <- data.frame(table_values)
   colnames(tt) <- 'pairs'

  col_pairs <- combn(c(1:f), 2)
  
  e <- col_copier(d, col_pairs)
  
  g <- full_incident(e, tt)
  
  val <- table(g$value)
  
  return(val)
}