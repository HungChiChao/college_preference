
library(igraph)

get_largest_component <- function(g){
  com <- components(g)
  max_s <- which( com$csize == max(com$csize))
  v <- (com$membership == max_s)
  largest_component <- data.table("school" = names(v[v == TRUE]))
  return(largest_component)
}

drop_dominate <- function(dt){
  dt <- dt[, .N, by=list(w, l)]
  inv <-  dt[,.(l,w)]
  colnames(inv) <- c("w","l")
  inv <- inv[dt,on=c("w","l"), N := i.N]
  dt[,re := inv[,N]][,re := ifelse(is.na(re),0,re)]
  dt[,rate := N*re]
  edge_ND <- dt[rate != 0]
  edge_ND <- edge_ND[w != l][,.(w,l)]
  return(edge_ND)
}


drop_dominate_weak <- function(df){
  dt <- df[, .N, by=list(w, l)]
  inv <-  dt[,.(l,w)]
  colnames(inv) <- c("w","l")
  inv <- inv[dt,on=c("w","l"), N := i.N]
  dt[,re := inv[,N]][,re := ifelse(is.na(re),0,re)]
  
  # out == 0, in = 0 
  excld <- c( setdiff(dt$l, dt$w), setdiff(dt$w, dt$l) )
  
  edge_ND <- df[! w %in% excld &!l %in% excld]
  
  cat("exlude: ",length(excld),"\n")
  
  return( list(
    edge = edge_ND,
    N = length(excld) )
    )
}


verify_new <- function(cht_data){
  print("Now Verifying")
  df <- copy(cht_data)
  
  # Create a table for edges
  edge1 <- data.table(
    w = df$choice,
    l = df$Option )
  edge1 <- edge1[ w != l]
  
  # Drop end nodes
  dt <- copy(edge1)
  for (t in 1:20){
    nd <- drop_dominate_weak(dt)
    if( nd$N == 0){
      break
    }
    dt <- copy(nd$edge)
  }
  
  #head(dt)
  # Find the largest component
  g <- graph.data.frame(dt, directed= T)
  lg_new <- get_largest_component(g)
  
  # Updat cht_data 
  # filter (in largest component)
  df <- df[choice %in% lg_new$school]
  df <- df[Option %in% lg_new$school]
  
  #Check (delete cs == 1)
  cht_count <- df[,.N, by = y_id]
  count_vec <- cht_count[N == 1]$y_id
  #cat( length(count_vec) )
  df <- df[! y_id %in% count_vec]
  
  return(df)
}


verify_temp <- function(dt, ref_point){
  v_dt <- lapply( split(dt, by = "cluster_id"), verify_new)
  cht_data <- rbindlist( v_dt)
  return( cht_data )
}


verify_combine <- function(dt, ref_point){
  
  # Reference Point
  ref_sch <- ref_point[1]
  ref_maj <- ref_point[2]
  ref_sd  <- ref_point[3]
  
  v_dt <- lapply( split(dt, by = "cluster_id"), verify_new)
  cht_data <- rbindlist( v_dt)
  
  cht_data[, choice := paste0(choice,"_K",cluster_id )]
  cht_data[, Option := paste0(Option,"_K",cluster_id )]
  
  # 
  program_list <-  unique( cht_data$choice )
  
  indx <- match( paste0( ref_sd,"_K1" ),  program_list)
  # FIXME
  if( is.na(indx) ){
    indx <- match( paste0( ref_sd,"_K3" ),  program_list)
  }
  #print(indx)
  
  program_list <- replace( program_list, c(1,indx), program_list[ c(indx, 1) ])

  return( list("final_dt" = cht_data, "p_list" = program_list) )
}



