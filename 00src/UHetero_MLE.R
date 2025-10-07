
source("Result/Default_function.r")
library(fastmatch)
library(data.table)
MLE_GLM <- function(data,Ini,design_mat){
  f_g <- function(theta){
    L <- Likelihood_wider_GLM(theta,data,design_mat)
    return(L)
  }
  g_g <- function(theta){
    G <- score_function_GLM(theta,data,design_mat)
    return(G)
  }
  h_g <- function(theta){
    
    H <- Hessian_function_GLM(theta,data,design_mat)
    return(H)
    
  }
  M_f <- maxLik(f_g,g_g,h_g,Ini)
  M_f <- maxLik(f_g,g_g,h_g,M_f$estimate,tol=.Machine$double.eps)
  return(M_f)
}
MLE <- function(data,Ini,Lambda=0){
  f <- function(theta){
    L <- Likelihood_wider(theta,data,Lambda)
    return(L)
  }
  g <- function(theta){
    G <- score_function(theta,data,Lambda)
    return(G)
  }
  h <- function(theta){
    
    H <- Hessian_function(theta,data,Lambda)
    return(H)
    
  }
  M_f <- maxLik(f,g,h,Ini)
  M_f <- maxLik(f,g,h,M_f$estimate,tol=.Machine$double.eps)
  return(M_f)
}
Choice_matrix <- function(data,subset){
  option <- select(data,alt1,alt2,alt3,alt4,
                   alt5,alt6,alt7,alt8,alt9,alt10,alt11,alt12)
  choice <- data$choice
  matrix <- matrix(0,nrow=dim(option)[1],ncol =dim(option)[2] )
  choice_vec <- matrix(0,nrow=dim(option)[1],1)
  for (i in 1:dim(option)[2]) {
    matrix[,i] <- match(t(option[,i]),subset)
  }
  choice_vec <- match(choice,subset)
  req1 <-  apply(matrix,1,function(x){sum(is.na(x)==0)})>=2
  req2 <- is.na(choice_vec)==0
  Req <- req1&req2
  matrix_with_id <- cbind(data$id,choice_vec,matrix)
  Sub_stu <- matrix_with_id[Req,]
  matrix2 <- cbind(number=c(1:dim(Sub_stu)[1]),Sub_stu[,-c(1,2)])
  matrix2 <-  data.frame(matrix2)
  colnames(matrix2) <- c("number","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12")
  matrix2 <- pivot_longer(matrix2,c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12"),
                          names_to = "C",values_to = "opt_index")
  matrix3 <- na.omit(matrix2)
  matrix3$number <- as.numeric(matrix3$number)
  matrix3$opt_index <- as.numeric(matrix3$opt_index)
  
  sub_alt_mat <- sparseMatrix(i=matrix3$number,j=matrix3$opt_index,x=1)
  sub_choice_data <- cbind(as.numeric(Sub_stu[,c(1,2)]),sub_alt_mat)
  return(sub_choice_data)
}
EST_fun <- function(datas,DIR="aaa"){
  #name <- paste("/home/d11207103/Desktop/R_project/College Preference and Choice/",DIR,".csv",sep="")
  #print(name)
  
  MAT0 <- read.csv(file=DIR,fileEncoding = "UTF8")
  MMAT0 <- as.matrix(MAT0[-1,])
  Bmat0 <- MMAT0[,-c(1,2,3,4)]
  
  mat_num <- matrix(as.numeric(Bmat0),    # Convert to numeric matrix
                    ncol = ncol(Bmat0))
  sparse_mat <- as(mat_num, "sparseMatrix")
  Ini22 <- rep(0,dim(Bmat0)[2])
  
  Subsets= as.vector(MAT0$S_Dep)
  CD <- Choice_matrix(datas,Subsets)
  
  Ini1 <- rep(0,(length(Subsets)-1))
  (EST <- MLE(CD,Ini1))
  #Likelihood_wider_GLM(Ini2,choice_data_all,sparse_mat)
  print("EST")
  print(EST$message)
  (EST_glm <- MLE_GLM(CD,Ini22,sparse_mat))
  Var0 <- vcov(EST)
  print("EST_glm")
  print(EST_glm$message)
  se0 <- diag(Var0) %>% sqrt()
  Estimate0 <- cbind(S_Dep =Subsets,effect = c(0,EST$estimate),se=c(0,se0)) %>% data.frame()
  Estimate0$effect <- as.numeric(Estimate0$effect)
  
  Var00 <- vcov(EST_glm)
  se00 <- diag(Var00) %>% sqrt()
  Estimate_glm0 <- cbind(S_Dep =colnames(Bmat0),effect = c(EST_glm$estimate),se=c(se00)) %>% data.frame()
  Estimate_glm0$effect <- as.numeric(Estimate_glm0$effect)
  RES <- list(Estimate0,Estimate_glm0)
  return(RES)
}


MLE_GLM_wrap <- function(ldt){
  
  cat("list of data should be provided: CM, DM")
  
  data <- ldt$CM
  Design <- ldt$DM
  DM <- Design[,-c(1)] %>% as.matrix()
  DM <- DM[-1,]
  Ini <- rep(0,(dim(DM)[2]))
  
  Start_time <- Sys.time()
  GLM_result <- MLE_GLM(data, Ini, DM)
  print(Sys.time()-Start_time)
  
  return( GLM_result)
}

