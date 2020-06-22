#'Internal function for tabulating incidents for partial foldovers
#'
#'Generally useful to compare minimum incidents
#'@param d The design to have it's incidnets tabulated
#'
#'@importFrom utils combn




tab_inc2_partial <-  function(d){
  f <- dim(d)[2]
  


  table_values1 <- c('00', '10', 
                  '01', '11')

   tt1 <- data.frame(table_values1)
   colnames(tt1) <- 'pairs'
   
   
  table_values2 <- c('00', '10', '20',
                  '01', '11', '21',
                  '02', '12', '22')

   tt2<- data.frame(table_values2)
   colnames(tt2) <- 'pairs'
   
   table_values3 <- c('00', '10', '20',
                  '01', '11', '21')
   tt3<- data.frame(table_values3)
   colnames(tt3) <- 'pairs'

  col_pairs <- combn(c(1:f), 2)
  
  e <- col_copier(d, col_pairs)
  
  g <- full_incident_partial(e, tt1, tt2, tt3)
  
  val <- table(g$value)
  
  return(val)
}