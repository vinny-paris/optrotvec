#' Generating a New Design Matrix
#' 
#' Create the new design matrix from a triple foldover techinque using a rotation vector
#'
#' @export
#' 
#' @param D The original design matrix that you would like expanded upon, must come in coded with 0,1,2's
#' @param rotation_vector The rotation vector with the same length as the column width of the design matirx. Entries may be 0, 1, or 2.
#' @param unique Should rows that are repeated be removed? Default is TRUE
#' 
#' @return  This will return a data frame such that the triple foldover has been applied to the original design D.
#' 
#' @examples
#' \dontrun{
#' design <- matrix(c(1,2,1,0,1,1,1,2,2), byrow = TRUE, nrow = 3)
#' x <- c(1, 2, 0)
#' rot(design, x)
#' }


rot <- function(D, rotation_vector, unique = TRUE){
  
  if(sum(class(D) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", call. = FALSE)}
  if(dim(D)[2] != length(rotation_vector)) {stop("Please make the rotation vector have the same number of factors as the design matrix!")}
  if(sum(class(rotation_vector) == c("numeric", "vector")) == 0) {stop("Please make the rotation vector a numeric vector!")}

  #create matrix (rows are rotation vector) to add to the original design  
  rot_matrix <- matrix(rep(rotation_vector, dim(D)[1]), ncol = dim(D)[2], byrow = TRUE)
  
  #first and second rotations 
  DD <- (D + rot_matrix) %% 3
  DDD <- (D + 2*rot_matrix) %% 3
  
  #new design 
  new_des <- rbind(D, DD, DDD)
  
  if(unique ==TRUE) new_des <- unique.matrix(new_des, order = FALSE)
  
  return(new_des)
}