#'Internal function for tabulating incidents
#'
#'Generally useful to compare minimum incidents
#'@param d The design to have it's incidnets tabulated



tab_inc <- function(d){
  f <- dim(d)[2]
  


  table_values <- c('00', '10', '20',
                  '01', '11', '21',
                  '02', '12', '22')

   tt <- data.frame(table_values)
   colnames(tt) <- 'pairs'

  col_pairs <- combn(c(1:f), 2)
  
  e <- col_copier(d, col_pairs)
  
  g <- full_incident(e)
  
  val <- table(g$value)
  
  return(val)
}
  