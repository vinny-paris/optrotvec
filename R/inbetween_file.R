rot2 <- function(roti, D, unique = FALSE){
  j <- list()
  j[[1]] <- data.table(D)
  j[[2]] <- data.table(t(apply(D, 1, function(x) (x + roti) %% 3)))
  j[[3]] <- data.table(t(apply(D, 1, function(x) (x + 2*roti) %% 3)))
  return(rbindlist(j, use.names = FALSE))
}

rot_det_calc <- function(all_rot_vec, D_star){
lapply(D_star, det_calc, roti = all_rot_vec)
}

 my_det_f <- function(x){
      det(t(x) %*% x/(dim(x)[1]))^(1/(dim(x)[2]))
 }
 
 det_calc <- function(D, roti){
   cand_des <- rot2(roti, D, unique = FALSE)
   cand_des_enc <- cbind(1, optrotvec:::g_matrix_convert(cand_des))
   d <- det(t(cand_des_enc) %*% cand_des_enc/(dim(cand_des_enc)[1]))^(1/(dim(cand_des_enc)[2])) 
   return(d)
}
 
 library(Rfast)
 library(optrotvec)
library(AlgDesign)
library(BGData)
library(data.table)
library(foreach)
library(doParallel)
library(parallel)



n <- 4
cand_trt <- expand.grid(replicate(n, 0:2, simplify = FALSE))
all_rot_vec <- optrotvec:::alias_design(n) 
p <- 3
cand_list <- split(cand_trt, seq(nrow(cand_trt)))
cand_list2 <- split(all_rot_vec, seq(nrow(all_rot_vec)))

cand_list_rot2 <- mclapply(cand_list2, function(x){
  lapply(cand_list, FUN = rot2, roti = as.matrix(x), unique = FALSE)}, mc.cores = 3
)


cand_list_rot2_enc <- mclapply(cand_list_rot2, function(x){
  lapply(x, function(y) data.frame(1, optrotvec:::g_matrix_convert(y)))}, mc.cores = 3)
cand_list_rot2_enc2 <- unlist(cand_list_rot2_enc, recursive = FALSE)


for(p in 3:5){
rowrep4_rotated40_candidate81 <- rep(list(cand_list_rot2_enc2), p)


#cand_list <- split(cand_trt, seq(nrow(cand_trt)))
#cand_list_rot <- lapply(cand_list, function(x){
#  apply(all_rot_vec, 1, FUN = rot2, D = x, unique = FALSE)}
#)
#cand_list_enc <- lapply(cand_list_rot, function(x){
#  lapply(x, function(y) data.frame(1, optrotvec:::g_matrix_convert(y)))})
#cand_list_enc <- cand_list_enc5 





D <- ran_D(40, n)[1:p,]
e <- list()
e_det <- list()



for(l in 1:4){
  
D <- ran_D(40, n)[1:p,]


for(k in 1:3){
d_lit <- apply(all_rot_vec, 1, FUN = rot2, D = D, unique = FALSE)
nr <- dim(D)[1]
my_list <- list()
for(j in 1:nr){
  my_list[[j]] <- lapply(d_lit, function(x) x[-c(j, j+nr, j+(2*nr)), ])
}



my_list_enc <- lapply(my_list, function(x) lapply(x, function(y) data.frame(1, optrotvec:::g_matrix_convert(y))))

rowomit_rotated_rep81 <- lapply(my_list_enc, rep, each = dim(cand_trt)[1])



      
      
      
   w <- list()
   
#for(r in 1:dim(D)[1]){
#  w[[r]] <- list()
#for(s in 1:dim(all_rot_vec)[1]){
#  w[[r]][[s]] <- list()
#for(t in 1:dim(cand_trt)[1]){
#  w[[r]][[s]][[t]] <- as.matrix(rbindlist(list(my_list_enc[[r]][[s]], cand_list_enc[[t]][[s]]), use.names = FALSE))
#}}}
   
   
kook <- mclapply(seq_along(rowrep4_rotated40_candidate81), function(g) {lapply(seq_along(rowrep4_rotated40_candidate81[[g]]), 
       function(x) data.frame.to_matrix(rbindlist(list(rowrep4_rotated40_candidate81[[g]][[x]], rowomit_rotated_rep81[[g]][[x]]))))}, mc.cores = 3)   
   

 
w2_det_unlist <-(unlist(lapply(kook, function(x) lapply(x,my_det_f))))

my_choice_det <- sample(which(w2_det_unlist == max(w2_det_unlist, na.rm = TRUE)), 1)

my_choice_cand <- cand_trt[ifelse(my_choice_det %% dim(cand_trt)[1] == 0, dim(cand_trt)[1], my_choice_det %% dim(cand_trt)[1]),]
my_choice_rot  <- all_rot_vec[(floor(my_choice_det/dim(cand_trt)[1])+1) %% dim(all_rot_vec)[1], ]
my_choice_row  <- floor(my_choice_det/(dim(cand_trt)[1] * dim(all_rot_vec)[1])) + 1
               

D[my_choice_row,] <- my_choice_cand
D <- matrix(unlist(D), nrow = nr, byrow = FALSE)
e[[l]] <- D
}              
e_det[[l]] <- (max(w2_det_unlist, na.rm = TRUE))              
               
} 

ome_lists <- list()  
      
for(u in which(unlist(e_det) == max(unlist(e_det)))){        
y <- apply(all_rot_vec, 1, det_calc, D = e[[u]])  
which_y <- which(y == max(y, na.rm = TRUE))
  roo <- all_rot_vec[which_y,]
  if(dim(as.data.frame(roo))[2] == 1){roo <- t(as.data.frame(roo))}
ome_lists[[u]] <- max(apply(roo, 1, function(x) {omega(rot(e[[u]], x, unique = FALSE))/(n*n*(n-1))}))
}
               
cat('\n', p, '   ', max(unlist(ome_lists)), '   ', max(unlist(e_det)), '\n')               


}