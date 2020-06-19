opt_2 <- function(design, return_n = 5){
  inv <- FALSE
  
  #warnings 
  if(return_n %% 1 != 0){stop("Please make the return_n parameter a natural number!", immediate. = TRUE)}
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", immediate. = TRUE)}

  
  d <- as.matrix(design)
  
  f <- dim(d)[2]
 
  all_rot_vec <- alias_design(f)
  s <- sample(dim(all_rot_vec)[1])
  all_rot_vec <- all_rot_vec[s,]

  all_rot_vec <- all_rot_vec[apply(all_rot_vec, 1, function(r) !any(r %in% 0)),]
  
  
               
  alts     <- rbind(all_rot_vec)
  
  hope <- cbind(alts)

  
  dets <- apply(hope, 1, tester_d2, design = d, inv = inv)
  inc_min_list <- apply(hope, 1, tester_inc, design = d)
  inc_min <- do.call(rbind.data.frame, inc_min_list)
  

  finals <- cbind(dets^(1/(2*f + 1)), inc_min, matrix(hope, ncol = f))
  
  finals <- data.frame(finals)
  colnames(finals)[c(1,2,3)] <- c('Det.',  'Min. Incidents', 'Rotation Vectors')
  finals <- arrange(as.data.frame(finals), desc(inc_min), desc(dets))[c(1:return_n),]
  return(finals)
}



