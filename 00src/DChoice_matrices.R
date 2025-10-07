
cat("Hello_world","/n","This scource file provides create_DM() and create_matrices().")


verfify_choice <- function(df, S_Dep_type, ref_point){
  
  # Reference Point
  ref_sch <- ref_point[1]
  ref_maj <- ref_point[2]
  ref_sd  <- ref_point[3]
  
  # 建立選項，確認choice不是NA以及有讀入S_Dep_Vec(Largest Conpoent)
  Obs_vec <- c()
  
  cht_data <- copy(df)
  if ( sum(cht_data$choice == "") == 0 & length(S_Dep_type) > 0 ) {
    
    cht_data[, choice := paste0(choice,"_K",cluster_id )]
    cht_data[, Option := paste0(Option,"_K",cluster_id )]
    
    # 確認有在S_Dep_Vec
    cht_data <- cht_data[choice %in% S_Dep_type]
    cht_data <- cht_data[Option %in% S_Dep_type]
    # Report
    Obs_vec <- c(Obs_vec,nrow(cht_data))
    #print(Obs_vec)
    
    # 將只有一個選項的樣本刪除
    cht_count <- cht_data[,.N, by = y_id]
    count_vec <- cht_count[N == 1]$y_id
    cht_data <- cht_data[! y_id %in% count_vec]
    rm(cht_count,count_vec)
    # Report
    Obs_vec <- c(Obs_vec,nrow(cht_data))
    #print(Obs_vec)
    
    program_list <- unique(cht_data$choice)
  }else{
    print("error")
    break
  }
  
  # 當Largest Componet改變就有可能會遺失樣本
  #當有樣本刪除(Largest Compoent)就有改變的可能
  
  dt <- copy(cht_data)
  
  for (i in 1:10){
    # 新的Largest Component
    dt <- dt[Option %in% program_list]
    
    #將只有一個樣本的選項刪除
    dt_count <- dt[,.N, by = y_id]
    count_vec <- dt_count[N == 1]$y_id
    dt_1 <-  dt[! y_id %in% count_vec]
    rm(dt_count,count_vec)
    
    #更新Largest Compoent
    program_list <- unique(dt_1$choice)
    print(nrow(dt_1))
    if (nrow(dt_1) == nrow(dt)){
      cht_data <- dt_1
      # Report
      Obs_vec <- c(Obs_vec,nrow(cht_data))
      cat("iter",i)
      # 
      rm(dt,dt_1)
      break
    }else{
      dt <- dt_1
    }
  }
  
  #  --- 4.3 產出：cht_set, program_list(== unique(dt_1$choice)) 
  program_list <-  unique( cht_data$choice )
  
  print(ref_sd)
  
  indx <- match( paste0( ref_sd,"_K1" ),  program_list)
  
  # FIXME
  if( is.na(indx) ){
    indx <- match( paste0( ref_sd,"_K3" ),  program_list)
  }
  
  print(indx)
  program_list <- replace( program_list, c(1,indx), program_list[ c(indx, 1) ])
  
  print(Obs_vec)
  return( list("final_dt" = cht_data, "p_list" = program_list) )
}

#

create_DM <- function(cht_data, program_list, ref_point , least=0){
  
  # Reference Point
  ref_sch <- ref_point[1]
  ref_maj <- ref_point[2]
  ref_sd  <- ref_point[3]
  maj <- ref_point[4]
  
  
  require(Matrix)
  
  cht_data[, major := get(maj)]
  DM <- unique(cht_data[,.(choice, school, major, cluster_id)])
  DM[, school:= paste0(school,"_K",cluster_id )]
  DM[, major := paste0(major,"_K",cluster_id )]
  
  DM <- fastDummies::dummy_cols(DM, select_columns = "school")
  new_name <- sapply(colnames(DM), function (x) gsub(pattern = "school_",replacement = "",x), USE.NAMES = FALSE)
  colnames(DM) <- new_name
  #new_name <- sapply(colnames(DM), function(x) gsub(pattern = "school_", replacement = "", x), USE.NAMES = FALSE)
  DM <- fastDummies::dummy_cols(DM, select_columns = "major")
  new_name <- sapply(colnames(DM),function (x) gsub(pattern = "major_",replacement = "", x), USE.NAMES = FALSE)
  colnames(DM) <- new_name
  
  # Rearrange the data.table row based on the desired order
  setkeyv(DM, c("choice"))
  DM <- DM[program_list]
  
  # 確認每個parameter都是有被選過 （這裏可能會有不同的設定）
  check_vec <- colSums(DM[,-c("choice","school","major","cluster_id")]) # Column Sums
  #print(check_vec)
  idtfied <- c("choice",names(check_vec[check_vec > least]))
  
  idtfied <- idtfied[!grepl(ref_sch, idtfied) & !grepl(ref_maj, idtfied)]
  full_DM<- DM[,..idtfied]
  
  #Print 參考科系出現的Type(理論上每組都要有)
  cat("Ref point","\n",grep(ref_sd, full_DM$choice ,value = TRUE),"\n")
  
  cat("Dimension","\n", (dim(full_DM)),"\n" )
  r <- Matrix::rankMatrix(full_DM[,-c(1)])
  print(r)
  
  return(list("full_DM"= full_DM, "idtfied" = idtfied))
}


create_matrices <- function(data, p_list){
  dt <- copy(data)
  
  Pythagorean <- function(a1,a2,b1,b2){
    round(sqrt((a1-b1)**2 + (a2-b2)**2))}
  
  dt[, dist := Pythagorean(UX, UY, SX, SY)/100000]
  dt[, c_dist := Pythagorean(cUX, cUY, SX, SY)/100000]
  dt[, alt_wage := alt_wage/100000]
  dt[, c_wage := c_wage/100000]
  
  # 編碼
  dt[, opt_index :=  match(t(dt$Option),p_list)]
  dt[, c_index :=  match(t(dt$choice),p_list)]
  num_list <- unique(dt$y_id)
  dt[, number :=  match(t(dt$y_id), num_list)]
  rm(num_list)
  
  m <- as.matrix(unique(dt[,.(number, c_index, c_wage, c_dist)]))
  colnames(m) <- NULL
  
  # Choice Matrix
  sub_alt_mat <- sparseMatrix(i=dt$number,j=dt$opt_index,x=1)
  sub_choice_data <- cbind(as.numeric(m[,1:2]),sub_alt_mat)
  
  # Distance Matrix
  sub_alt_mat_dist <- sparseMatrix(i=dt$number,j=dt$opt_index,x=dt$dist)
  sub_choice_data_dist <- cbind(as.numeric(m[,c(1,4)]),sub_alt_mat_dist)
  
  # Wage Matrix
  sub_alt_mat_wage <- sparseMatrix(i=dt$number,j=dt$opt_index,x=dt$alt_wage)
  sub_choice_data_wage <- cbind(as.numeric(m[,c(1,3)]),sub_alt_mat_wage)
  
  return(list("CM" = sub_choice_data,"DistM" = sub_choice_data_dist,
                                  "WM" = sub_choice_data_wage))
}


Pre_estimation <- function( cov_num, cht_data,  program_list, ref_point ,least = 0){
  
  # Reference Point
  ref_sch <- ref_point[1]
  ref_maj <- ref_point[2]
  ref_sd  <- ref_point[3]
  maj <- ref_point[4]
  
  ## --- 5.1 Simple Restricted Logit -
  # Step 1: 從頭設定Design Matrix
  list_dm <- create_DM(cht_data, program_list, ref_point = ref_point, least = 1)
  
  # Step 2 : Matrices
  list_data <- create_matrices(cht_data, program_list)
  
  # Step 3 
  list_data[["DM"]] <- list_dm$full_DM
  list_data[["Par"]] <- list_dm$idtfied
  #list_data[["Obs"]] <- Obs_vec
  list_data[["Prog"]] <- program_list
  
  
  if(  cov_num != 0){
    dm <- list_dm$full_DM[,-c(1)] %>% as.matrix()
    dm <- dm[-1,]
    
    add_var <- function(D){ 
      dmd <- rbind(
        cbind(matrix(1, nrow = 1, ncol = 1), matrix(0, nrow = 1, ncol = dim(D)[2])),
        cbind(matrix(0, nrow = dim(D)[1], ncol = 1), D)
      )
      return (dmd)
    }
    
    # !!!!!! Depends on how many covariates
    if (cov_num == 1){
      list_data$design_mat <- add_var(dm)
    }else if(cov_num == 2){
      list_data$design_mat <- add_var(add_var(dm)) }
    
  }
  return(list_data)
}

verfify <- function(df, S_Dep_type){
  
  cht_data <- copy(df)
  if ( sum(cht_data$choice == "") == 0 & length(S_Dep_type) > 0 ) {
    
    # 確認有在S_Dep_Vec
    cht_data <- cht_data[choice %in% S_Dep_type]
    cht_data <- cht_data[Option %in% S_Dep_type]
    
    # 將只有一個選項的樣本刪除
    cht_count <- cht_data[,.N, by = y_id]
    count_vec <- cht_count[N == 1]$y_id
    cht_data <- cht_data[! y_id %in% count_vec]
    rm(cht_count,count_vec)

    program_list <- unique(cht_data$choice)
  }else{
    print("error")
    break
  }
  
  # 當Largest Componet改變就有可能會遺失樣本
  #當有樣本刪除(Largest Compoent)就有改變的可能
  
  dt <- copy(cht_data)
  
  for (i in 1:10){
    # 新的Largest Component
    dt <- dt[Option %in% program_list]
    
    #將只有一個樣本的選項刪除
    dt_count <- dt[,.N, by = y_id]
    count_vec <- dt_count[N == 1]$y_id
    dt_1 <-  dt[! y_id %in% count_vec]
    rm(dt_count,count_vec)
    
    #更新Largest Compoent
    program_list <- unique(dt_1$choice)
    print(nrow(dt_1))
    if (nrow(dt_1) == nrow(dt)){
      cht_data <- dt_1
      # Report
      cat("iter",i)
      # 
      rm(dt,dt_1)
      break
    }else{
      dt <- dt_1
    }
  }
  
  return( cht_data )
}





