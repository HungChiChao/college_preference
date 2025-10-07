
library(EnvStats)
library(nnet)
library(Matrix)
library("caret")
library(readr)
library(maxLik)
library(qgraph)
library(pracma)
library(tidyverse)
library(data.table)
library(fastmatch)
library(collapse)

Data_gen2 <- function(n,j,effect,gamma=c(0.1,0.5)){
  K <- length(gamma)
  
  jm <- j-1
  #effect <- seq(0,jm*0.1,0.1)
  #effect <-  sample(effect,length(effect))
  U <- matrix(-Inf,nrow=n,ncol=j)
  
  for(i in 1:j ){
    
    if(i!=1 & i!=j){
      
      U[c(((i-2)*n/jm+1):(i*n/jm)),i] <- effect[i] +rgevd((2*n/jm))
      
    }
    if(i==1){
      U[c(1:(n/jm)),i] <- effect[i]+rgevd((1*n/jm))
    }
    if (i==j){
      
      U[c((n*(jm-1)/jm+1):n),i] <- effect[i]+rgevd((1*n/jm))
    }
  }
  Distance_tab <- list()
  for (k in 1:K) {
    Distance_mat <- rchisq(n*j,df=1)
    outMatrix <- matrix(data = Distance_mat, nrow = n, ncol = j, byrow = FALSE)
    Distance_tab[[k]] <- outMatrix
    U <- U+outMatrix*gamma[k]
  }
  
  
  #Distance_mat <- matrix(replicate(K,rchisq(n*j,df=1)),ncol = K,byrow = TRUE)
  #Distance <- rowSums(t(t(Distance_mat) * gamma))
  
  
  # outMatrix <- matrix(data = Distance, nrow = n, ncol = j, byrow = FALSE)
  
  
  
  U <- data.frame(U)
  
  choices <- apply(U, 1, which.max)
  
  Indicator<- matrix(0,nrow=n,ncol=j)
  
  for(i in 1:j ){
    if(i!=1 &i!=j){
      Indicator[c(((i-2)*n/jm+1):(i*n/jm)),i] <- 1
      
    }
    if(i==1){
      Indicator[c(1:(n/jm)),i] <-1
    }
    if (i==j){
      Indicator[c((n*(jm-1)/jm+1):n),i] <-1
    }
  }
  
  student_id <- 1:n
  
  
  
  Distnace_table <- list()
  for (k in 1:K) {
    c_distance <- rep(0,n)
    
    for (i in 1:length(choices)) {
      
      c_distance[i] <- Distance_tab[[k]][i,choices[i]]
    }
    
    
    
    Distnace_table[[k]] <-  data.frame(student_id =student_id,
                                       choices =c_distance ,Distance_tab[[k]])
    
  }
  
  
  choice_data <- data.frame(student_id =student_id,
                            choices = choices,Indicator)
  #distance_table <- data.frame(student_id =student_id,
  #                          choices =c_distance ,outMatrix)
  return(list(choice_data,Distnace_table))
}


Likelihood_wider_heter <- function(theta,gamma,data,distance_tab,lambda=0){
  choice_matrix<- (data[,-c(1,2)])
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    distance_matrix<- distance_tab[,-c(1,2)]
    choice_dis <- distance_tab[,2] 
    CG_sum <- choice_dis*gamma
    DG_sum <-   distance_matrix*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    choice_dis <- distance_1[,2] 
    CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      choice_dis <- distance_sub[,2] 
      CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  
  
  
  exp_theta <- exp(c(0,theta))
  #exp_theta <- exp(theta-theta[1])
  
  
  
  
  weight_mat <- choice_matrix*exp(DG_sum)
  #  print(dim(weight_mat))
  denominator <- weight_mat%*%exp_theta
  numerator <- exp_theta[data[,2]]
  
  Lik <- log(numerator)-log(denominator)
  if(lambda!=0){
    l <- sum(Lik)+sum(CG_sum)-lambda*sum(theta^2)
  } else {
    l <- sum(Lik)+sum(CG_sum)
  }
  
  return(l)
}


score_function_heter=function(theta,gamma,X,distance_tab,lambda=0){
  Theta2=exp(c(0,theta)) #j
  
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    alt_dis<- distance_tab[,-c(1,2)]
    ch_dis <- distance_tab[,2] 
    CG_sum <- ch_dis*gamma
    DG_sum <-   alt_dis*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    choice_dis <- distance_1[,2] 
    CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      choice_dis <- distance_sub[,2] 
      CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  
  
  # g=rep(0,length(theta)) # j-1
  
  alt_vec <- (X[,-c(1,2)]) #N by p
  #alt_dis<- X_distance[,-c(1,2)] #N by p
  #ch_dis <- X_distance[,2] # n by 1
  #alt_wet <- as.matrix( alt_vec*exp(alt_dis*gamma) ) # n by p
  alt_wet <-  alt_vec*exp(DG_sum)  # n by p
  ch <- X[,2]#N by 1
  
  
  #numerator <- as.matrix(exp(ch_dis*gamma))   # n by 1
  # 要算的不是和ch 的距離適合微分 elenmnt 距離
  #print(numerator)
  #numerator <- Theta2[ch]
  deno <- (alt_wet%*%(Theta2))  # n by p time p by 1
  g_i <- -(1/deno)
  g <- t(alt_wet)%*%(g_i)
  g=g*Theta2
  
  
  
  if(length(gamma)==1){
    wei2 <- (alt_wet*alt_dis)%*%(Theta2) # n by p time p by 1
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    gamma_s <-sum(ch_dis)- sum(wei2/deno)
  }else{
    gamma_s <- rep(0,length(gamma))
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    for (i in 1:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      choice_dis <- distance_sub[,2] 
      wei2 <- (alt_wet*distance_matrix)%*%(Theta2) # n by p time p by 1
      gamma_s[i] <-sum(choice_dis)- sum(wei2/deno)
    }
    
  }
  
  
  
  
  
  C_count <- rle(sort(X[,2]))
  g_0 <- rep(0,length(Theta2))
  g_0[C_count$values] <- C_count$lengths
  if(lambda!=0){
    g=g[-1]+g_0[-1]-2*lambda*c(theta)
  } else{
    g=g[-1]+g_0[-1]
  }
  g <- c(gamma_s,g)
  
  return(g)
}
## transpose transpose + diag 在合併 vector
Hessian_function_heter <- function(theta,gamma,X,distance_tab,lambda=0){
  # Theta2=exp(theta-theta[1])
  Theta2=exp(c(0,theta)) #j
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    alt_dis<- distance_tab[,-c(1,2)]
    #  ch_dis <- distance_tab[,2] 
    #  CG_sum <- ch_dis*gamma
    DG_sum <-   alt_dis*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    #   choice_dis <- distance_1[,2] 
    #   CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      #         choice_dis <- distance_sub[,2] 
      #         CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  alt_vec <- (X[,-c(1,2)]) #N by j
  
  #  alt_dis<- X_distance[,-c(1,2)] #N by p
  #ch_dis <- X_distance[,2] # n by 1
  #alt_wet <- as.matrix( alt_vec*exp(alt_dis*gamma) ) # n by p
  alt_wet <- ( alt_vec*exp(DG_sum) ) # n by p
  #  ch <- X[,2]
  
  #numerator <- as.matrix(exp(ch_dis*gamma))   # n by 1
  # 要算的不是和ch 的距離適合微分 elenmnt 距離
  #print(numerator)
  #numerator <- Theta2[ch]
  deno <- (alt_wet%*%(Theta2))  # n by p time p by 1
  g_i <- -(1/deno) #n by 1
  g_1 <- alt_wet*as.vector(g_i) # n by p
  # g <- t(alt_wet)%*%(g_i)
  G=t(g_1)*Theta2 # p by n
  a <- -G[-1,] # p-1 by n
  
  L_1 <- a%*%t(a)
  
  sum1 <- matrix(-1,dim(X)[1],1)
  a2 <- as.numeric(a %*% sum1) # p-1 by 1
  
  if(lambda!=0){
    H <- diag(a2)+L_1-2*diag(lambda,nrow =length(theta),ncol=length(theta) )
  } else{
    H <- diag(a2)+L_1
  }
  
  if(length(gamma)==1){
    wei2 <- (alt_wet*alt_dis)%*%(Theta2) # n by p time p by 1
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    side  <- Theta2*t(alt_wet*alt_dis)%*%(g_i)+Theta2*t(alt_wet)%*%(wei2/deno^2)
    side <- side[-1]
    Side <- c(side)
    wei3 <- (as.matrix(alt_wet*(alt_dis^2))%*%(Theta2)) 
    gamma_H <- sum( (wei2/deno)^2)-sum(wei3/deno)
  }else{
    Side <- matrix(0,nrow=length(theta),ncol=length(gamma))
    gamma_h <- rep(0,length(gamma))
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    Wei2 <- list()
    for (i in 1:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      #choice_dis <- distance_sub[,2] 
      wei2 <- (alt_wet*distance_matrix)%*%(Theta2) # n by p time p by 1
      Wei2[[i]] <- wei2
      side  <- Theta2*t(alt_wet*distance_matrix)%*%(g_i)+Theta2*t(alt_wet)%*%(wei2/deno^2)
      side <- side[-1]
      Side[,i] <- side
      gamma_h[i] <- sum( (wei2/deno)^2)-sum((as.matrix(alt_wet*(distance_matrix^2))%*%(Theta2)) /deno)
    }
    gamma_tri <- matrix(0,nrow=length(gamma),ncol=length(gamma))
    for(i in 2:(length(gamma))){
      for(j in 1:(i-1)){
        distance_sub1 <- as.matrix(distance_tab[[i]])
        alt_dis1<- distance_sub1[,-c(1,2)]
        distance_sub2 <- as.matrix(distance_tab[[j]])
        alt_dis2<- distance_sub2[,-c(1,2)]
        wei3 <- (as.matrix(alt_wet*(alt_dis1)*alt_dis2)%*%(Theta2)) 
        gamma_tri[i,j] <- sum(Wei2[[i]]*Wei2[[j]]/deno^2)-sum(wei3/deno)
      }
    }
    gamma_H <- gamma_tri+ t(gamma_tri)+diag(gamma_h)
    
  }
  #print(Side)
  #print(gamma_H)
  
  
  #wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
  
  
  
  
  H_sub <- cbind(Side,H)
  H_g <- rbind(cbind(gamma_H,t(Side)),H_sub)
  return(H_g)
}

MLE_heter <- function(data,Ini,distance){
  g_num <- length(distance)
  f <- function(theta){
    L <- Likelihood_wider_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],data=data,distance_tab=distance,lambda=0)
    return(L)
  }
  g <- function(theta){
    # G <- score_function(theta,data)
    G <- score_function_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],X=data,distance_tab=distance,lambda=0)
    return(G)
  }
  h <- function(theta){
    
    #H <- Hessian_function(theta,data)
    H <- Hessian_function_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],X=data,distance_tab=distance,lambda=0)
    return(H)
    
  }
  M_f <- maxLik(f,g,h,Ini)
  M_f <- maxLik(f,g,h,M_f$estimate,tol=.Machine$double.eps)
  return(M_f)
}



Likelihood_wider_GLM_heter <- function(theta,gamma,data,distance_tab,design_mat,lambda=0){
  Theta <- design_mat %*% as.matrix(c(gamma,theta))
  Theta <- as.numeric(Theta[-(1:length(gamma))])
  choice_matrix<- (data[,-c(1,2)])
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    distance_matrix<- distance_tab[,-c(1,2)]
    choice_dis <- distance_tab[,2] 
    CG_sum <- choice_dis*gamma
    DG_sum <-   distance_matrix*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    choice_dis <- distance_1[,2] 
    CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      choice_dis <- distance_sub[,2] 
      CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  
  
  
  exp_theta <- exp(c(0,Theta))
  #exp_theta <- exp(theta-theta[1])
  
  
  
  
  weight_mat <- choice_matrix*exp(DG_sum)
  #  print(dim(weight_mat))
  denominator <- weight_mat%*%exp_theta
  numerator <- exp_theta[data[,2]]
  
  Lik <- log(numerator)-log(denominator)
  if(lambda!=0){
    l <- sum(Lik)+sum(CG_sum)-lambda*sum(theta^2)
  } else {
    l <- sum(Lik)+sum(CG_sum)
  }
  
  return(l)
}

score_function_GLM_heter=function(theta,gamma,X,distance_tab,design_mat,lambda=0){
  #Theta2=exp(theta-theta[1])
  # Distance_mat <- eg_dta[[2]]
  # X_distance <- Distance_mat
  
  #Choice_mat <- eg_dta[[1]]
  #X <- Choice_mat
  #theta=Ini[-1]
  #gamma=Ini[1]
  Theta <- design_mat %*% as.matrix(c(gamma,theta))
  Theta <- as.numeric(Theta[-c(1:length(gamma))])
  Theta2=exp(c(0,Theta)) #j
  # g=rep(0,length(theta)) # j-1
  
  
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    alt_dis<- distance_tab[,-c(1,2)]
    ch_dis <- distance_tab[,2] 
    CG_sum <- ch_dis*gamma
    DG_sum <-   alt_dis*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    choice_dis <- distance_1[,2] 
    CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      choice_dis <- distance_sub[,2] 
      CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  
  
  # g=rep(0,length(theta)) # j-1
  
  alt_vec <- (X[,-c(1,2)]) #N by p
  #alt_dis<- X_distance[,-c(1,2)] #N by p
  #ch_dis <- X_distance[,2] # n by 1
  #alt_wet <- as.matrix( alt_vec*exp(alt_dis*gamma) ) # n by p
  alt_wet <-  alt_vec*exp(DG_sum)  # n by p
  ch <- X[,2]#N by 1
  
  
  #numerator <- as.matrix(exp(ch_dis*gamma))   # n by 1
  # 要算的不是和ch 的距離適合微分 elenmnt 距離
  #print(numerator)
  #numerator <- Theta2[ch]
  deno <- (alt_wet%*%(Theta2))  # n by p time p by 1
  g_i <- -(1/deno)
  g <- t(alt_wet)%*%(g_i)
  g=g*Theta2
  
  
  
  if(length(gamma)==1){
    wei2 <- (alt_wet*alt_dis)%*%(Theta2) # n by p time p by 1
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    gamma_s <-sum(ch_dis)- sum(wei2/deno)
  }else{
    gamma_s <- rep(0,length(gamma))
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    for (i in 1:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      choice_dis <- distance_sub[,2] 
      wei2 <- (alt_wet*distance_matrix)%*%(Theta2) # n by p time p by 1
      gamma_s[i] <-sum(choice_dis)- sum(wei2/deno)
    }
    
  }
  
  
  
  
  
  C_count <- rle(sort(X[,2]))
  g_0 <- rep(0,length(Theta2))
  g_0[C_count$values] <- C_count$lengths
  if(lambda!=0){
    g=g[-1]+g_0[-1]-2*lambda*c(theta)
  } else{
    g=g[-1]+g_0[-1]
  }
  g <- c(gamma_s,g)
  
  g_2 <-as.matrix(g %*%design_mat)
  return(g_2)
}



Hessian_function_GLM_heter <- function(theta,gamma,X,distance_tab,design_mat,lambda=0){
  # Theta2=exp(theta-theta[1])
  Theta <- design_mat %*% as.matrix(c(gamma,theta))
  Theta <- as.numeric(Theta[-c(1:length(gamma))])
  Theta2=exp(c(0,Theta)) #j
  
  
  
  if(length(gamma)==1){
    distance_tab <- as.matrix(distance_tab)
    alt_dis<- distance_tab[,-c(1,2)]
    #  ch_dis <- distance_tab[,2] 
    #  CG_sum <- ch_dis*gamma
    DG_sum <-   alt_dis*gamma
  }else{
    distance_1 <-as.matrix(distance_tab[[1]])
    distance_matrix<- distance_1[,-c(1,2)]
    #   choice_dis <- distance_1[,2] 
    #   CG_sum <- choice_dis*gamma[1]
    DG_sum <-   distance_matrix*gamma[1]
    for (i in 2:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      DG_sum <-   DG_sum+distance_matrix*gamma[i]
      
      #         choice_dis <- distance_sub[,2] 
      #         CG_sum <- CG_sum+choice_dis*gamma[i]
      
    }
  }
  alt_vec <- (X[,-c(1,2)]) #N by j
  
  #  alt_dis<- X_distance[,-c(1,2)] #N by p
  #ch_dis <- X_distance[,2] # n by 1
  #alt_wet <- as.matrix( alt_vec*exp(alt_dis*gamma) ) # n by p
  alt_wet <- ( alt_vec*exp(DG_sum) ) # n by p
  #  ch <- X[,2]
  
  #numerator <- as.matrix(exp(ch_dis*gamma))   # n by 1
  # 要算的不是和ch 的距離適合微分 elenmnt 距離
  #print(numerator)
  #numerator <- Theta2[ch]
  deno <- (alt_wet%*%(Theta2))  # n by p time p by 1
  g_i <- -(1/deno) #n by 1
  g_1 <- alt_wet*as.vector(g_i) # n by p
  # g <- t(alt_wet)%*%(g_i)
  G=t(g_1)*Theta2 # p by n
  a <- -G[-1,] # p-1 by n
  
  L_1 <- a%*%t(a)
  
  sum1 <- matrix(-1,dim(X)[1],1)
  a2 <- as.numeric(a %*% sum1) # p-1 by 1
  
  if(lambda!=0){
    H <- diag(a2)+L_1-2*diag(lambda,nrow =length(theta),ncol=length(theta) )
  } else{
    H <- diag(a2)+L_1
  }
  
  if(length(gamma)==1){
    wei2 <- (alt_wet*alt_dis)%*%(Theta2) # n by p time p by 1
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    side  <- Theta2*t(alt_wet*alt_dis)%*%(g_i)+Theta2*t(alt_wet)%*%(wei2/deno^2)
    side <- side[-1]
    Side <- c(side)
    wei3 <- (as.matrix(alt_wet*(alt_dis^2))%*%(Theta2)) 
    gamma_H <- sum( (wei2/deno)^2)-sum(wei3/deno)
  }else{
    Side <- matrix(0,nrow=length(Theta),ncol=length(gamma))
    gamma_h <- rep(0,length(gamma))
    #  wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
    Wei2 <- list()
    for (i in 1:length(gamma)) {
      distance_sub <- as.matrix(distance_tab[[i]])
      distance_matrix<- distance_sub[,-c(1,2)]
      #choice_dis <- distance_sub[,2] 
      wei2 <- (alt_wet*distance_matrix)%*%(Theta2) # n by p time p by 1
      Wei2[[i]] <- wei2
      side  <- Theta2*t(alt_wet*distance_matrix)%*%(g_i)+Theta2*t(alt_wet)%*%(wei2/deno^2)
      side <- side[-1]
      print(length(side))
      print(dim(Side))
      Side[,i] <- side
      gamma_h[i] <- sum( (wei2/deno)^2)-sum((as.matrix(alt_wet*(distance_matrix^2))%*%(Theta2)) /deno)
    }
    gamma_tri <- matrix(0,nrow=length(gamma),ncol=length(gamma))
    for(i in 2:(length(gamma))){
      for(j in 1:(i-1)){
        distance_sub1 <- as.matrix(distance_tab[[i]])
        alt_dis1<- distance_sub1[,-c(1,2)]
        distance_sub2 <- as.matrix(distance_tab[[j]])
        alt_dis2<- distance_sub2[,-c(1,2)]
        wei3 <- (as.matrix(alt_wet*(alt_dis1)*alt_dis2)%*%(Theta2)) 
        gamma_tri[i,j] <- sum(Wei2[[i]]*Wei2[[j]]/deno^2)-sum(wei3/deno)
      }
    }
    gamma_H <- gamma_tri+ t(gamma_tri)+diag(gamma_h)
    
  }
  #print(Side)
  #print(gamma_H)
  
  
  #wei2 <- (as.matrix(alt_wet*alt_dis)%*%(Theta2))  # n by p time p by 1
  
  
  
  
  H_sub <- cbind(Side,H)
  H_g <- rbind(cbind(gamma_H,t(Side)),H_sub)
  H2 <- t(design_mat)%*%H_g%*%design_mat
  return(H2)
}




# MLE_GLM_heter <- function(data,Ini,distance,design_mat){
#   f <- function(theta){
#     L <- Likelihood_wider_GLM_heter(theta=theta[-1],gamma=theta[1],data=data,distance=distance,design_mat=design_mat,lambda=0)
#     return(L)
#   }
#   g <- function(theta){
#     # G <- score_function(theta,data)
#     G <- score_function_GLM_heter(theta=theta[-1],gamma=theta[1],X=data,X_distance=distance,design_mat=design_mat,lambda=0)
#     return(G)
#   }
#   h <- function(theta){
#     
#     #H <- Hessian_function(theta,data)
#     H <- Hessian_function_GLM_heter(theta=theta[-1],gamma=theta[1],X=data,X_distance=distance,design_mat=design_mat,lambda=0)
#     return(H)
#     
#   }
#   M_f <- maxLik(f,g,h,Ini)
#   M_f <- maxLik(f,g,h,M_f$estimate,tol=.Machine$double.eps)
#   return(M_f)
# }
# 


MLE_GLM_heter <- function(data,Ini,distance,design_mat){
  g_num <- length(distance)
  if(g_num==1){
    distance_input <- distance[[1]]
  }else{
    distance_input <- distance
  }
  f <- function(theta){
    L <- Likelihood_wider_GLM_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],data=data,distance_tab=distance_input,design_mat,lambda=0)
    return(L)
  }
  g <- function(theta){
    # G <- score_function(theta,data)
    G <- score_function_GLM_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],X=data,distance_tab=distance_input,design_mat,lambda=0)
    return(G)
  }
  h <- function(theta){
    
    #H <- Hessian_function(theta,data)
    H <- Hessian_function_GLM_heter(theta=theta[-c(1:g_num)],gamma=theta[c(1:g_num)],X=data,distance_tab=distance_input,design_mat,lambda=0)
    return(H)
    
  }
  M_f <- maxLik(f,g,h,Ini)
  M_f <- maxLik(f,g,h,M_f$estimate,tol=.Machine$double.eps)
  return(M_f)
}

MLE_GLM_heter_wrap <- function(ldt, use_D, use_W){
  cat("list of data including cM, desgin_mat, DistM, WM", "/n",
      "ldt, use_D, use_W")
  
  data <- ldt$CM
  
  if (use_D == 1 & use_W == 1){
    dist_mat <- list(ldt$DistM, ldt$WM) # !!!!!
  }else if (use_D == 0 & use_W == 1){
    dist_mat <- list(ldt$WM)
  }
  
  design_mat <- ldt$design_mat
  
  print(dim(design_mat))
  len <- dim(design_mat)[2]
  
  Ini <- rep(0, len ) # !!! DEPENDS ON HOW MANY PARAMETERS
  Start_time <- Sys.time()
  system.time( GLM_result <- MLE_GLM_heter(data=data ,Ini=Ini,
                            distance=dist_mat,design_mat = design_mat))
  print(Sys.time()-Start_time)
  
  return(GLM_result)
}
