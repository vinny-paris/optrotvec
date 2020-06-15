#' Encodes 2nd order interactions
#' 
#' For a given 3-level design this function creates and encodes all second order interactions 
#' 
#' @param  D an encoded design matrix 
#' 
#' @return This will return a data frame with the first f columns being that of D and the remaining being the encoded second order interactions. All facotrs are assigned a latin value (A, B, C, etc..).
#' 




oa_check <- function(D){
  X <- D
  final <- NULL
  
  for(i in 1:(dim(X)[2] - 1)){
    for(j in i:dim(X)[2]){
      
      if(i == j) next
      
      k <- as.matrix((X[,i] + X[,j]) %% 3)
      l <- as.matrix((X[,i] + 2*X[,j]) %% 3)
      colnames(k) <- paste(latin[c(i,j), 2], sep = "", collapse = "")
      colnames(l) <- paste(latin[i,2], latin[j,3], sep = "", collapse = "")
      
      final <- cbind(final, k, l)
    }
  }
  
  colnames(X) <- latin[1:dim(X)[2], 2]
  
  final <- cbind(X, final)
  colnames(final) <- gsub('_$', "", colnames(final))
  
  return(final)
}
