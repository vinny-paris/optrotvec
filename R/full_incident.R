


full_incident <- function(e){
  f <- dim(e)[2]/2
  holding <- list(NA)
  for(x in 1:f){
    holding <- cat_and_count(e[,c(x, x+1)], tt)
    k <- rbind(holding, k)
  }
  return(k)
}