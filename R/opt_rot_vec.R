#' Finds the optimal foldover plans for 3 level designs
#' 
#' For a given design matrix encoded with 0,1,2 this function will return the values of Omega, the determinant of the inverse of X_1'X_1, the ratio of the determinants of the the rotation vector design vs a null rotation design. Duplicated designs are removed.
#' 
#' @export
#' 
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' 
#' @param design The encoded design matrix using 0,1,2 notation that is to be expanded
#' @param return_n How many of the top rotation vectors (with regards to Omega) should be returned? Default is 5.
#' @param opt What optimality criterion should be used? Options is Omega (default), Det, Run_Size, Min_Inc (see below for details).
#' @return Returns a data frame that is return_n by f + 2.  
#' Omega-value. This will be the first column
#' Determenant of the information matrix
#' Det. Ratio. This is the ratio of the deteminant column over the the detemenant of the information matrix for a design that is just a tripling of the original.
#' Rotation vectors
#' 
#' @details There are four unique optimality criterion's that the foldover design can be chosen for. They are Omega, which returns the highest Omega-values (ties broken by Det.), Det which returns designs ranked by the determinant value, Run_Size, which ranks designs in the minimum number of runs to compelte a foldover without replication and finally Min_Incident which ranks designs by the maximum of the minimum of the number of pairs of any column at any level coincide with each other.  
#' 
#' @examples
#' \dontrun{
#' d <- ran_D(25, 4)
#' opt_rot_vec(d, return_n) 
#' }
#' 


opt_rot_vec <- function(design, return_n = 5, opt = "Omega"){
  inv <- FALSE
  
  
  #warnings 
  if(return_n %% 1 != 0){stop("Please make the return_n parameter a natural number!", immediate. = TRUE)}
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", immediate. = TRUE)}
  if(sum(opt == c("Min_Incident", "Omega", "Det", "Run_Size")) == 0 ){stop("Unknown Optimality Choice, please correct!", immediate. = TRUE)}
  
  d <- as.matrix(design)
  
  f <- dim(d)[2]
 
  all_rot_vec <- alias_design(f)
  s <- sample(dim(all_rot_vec)[1])
  all_rot_vec <- all_rot_vec[s,]
  
  omegas1 <- apply(all_rot_vec, 1, tester, design = d)
  omegas2 <- apply(all_rot_vec, 1, tester, design = d, unique = FALSE)
  omegas  <- c(omegas1, omegas2)
  o_tf    <- c(rep(TRUE, length(omegas1)), rep(FALSE, length(omegas2)))
               
  alts     <- rbind(all_rot_vec, all_rot_vec)
  candidate_vecs <- alts[order(omegas, decreasing = TRUE),]
  unqieness <- o_tf[order(omegas, decreasing = TRUE)]
  
  if(length(dim(candidate_vecs)) == 0){candidate_vecs <- as.matrix(t(candidate_vecs))}
  hope <- cbind(candidate_vecs, unqieness)

  
  dets <- apply(hope, 1, tester_d, design = d, inv = inv)
  det_org <- tester_d(design = d, rotv = c(rep(0,f), FALSE), inv = inv)
  
  inc_min_list <- apply(hope[,-c(dim(hope)[2])], 1, tester_inc, design = d)
  inc_min <- do.call(rbind.data.frame, inc_min_list)
  inc_min <- apply(inc_min, 2, as.numeric)
                
  best_omegas <- omegas[order(omegas, decreasing = TRUE)]

  finals <- cbind(best_omegas, dets, dets/det_org, inc_min, hope[,(f+1)], matrix(hope[,-(f+1)], ncol = f))
  
  n_runs <- apply(finals[,-c(1:5)], 1, n_counter, design = d)
  
  finals <- cbind(best_omegas, dets^(1/(2*f + 1)), dets/det_org, n_runs, inc_min, hope[,(f+1)], matrix(hope[,-(f+1)], ncol = f))
  
  finals <- finals[!duplicated(finals[,-7]),] 
  
    colnames(finals)[1:8] <- c('Omega value', 'Det', 'Det Ratio', 'Run Size', 'Min. Incident', 'Frequency', 'Run Reduced?', 'Rotation Vectors')
  if(opt == 'Omega') {finals <- arrange(as.data.frame(finals), desc('Omega value'), desc('Det'))[c(1:return_n),]}
  if(opt == 'Det') {finals <- arrange(as.data.frame(finals), desc('Det'))[c(1:return_n),]}
  if(opt == 'Run_Size') {finals <- arrange(as.data.frame(finals), 'Run Size', desc('Det'))[c(1:return_n),]}
  if(opt == 'Min_Incident') {finals <- arrange(as.data.frame(finals), desc('Min. Incident'), ('Frequency'), desc('Det'))[c(1:return_n),]}
  

  return(finals)
}

