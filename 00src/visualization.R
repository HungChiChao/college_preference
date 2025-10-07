# Header =====================================================
# Title:        Create Tables and Visualization 
# Description:   
# Date:          
# Author:        Chi-Chao Hung
# Version:       v2.0

# Change Log:
# - 2024-11-25(v1.0):
# - 2025-01-05(v2.0):
# - 2025-01-20(v2.1): Remove _ in filed names, Updata diag_plot legend =
#.                    "p > 0.05", remove numbers on heatmap.

# Inputs:        
# Outputs:       
# Replication:   [Instructions for reproducing the results, e.g., "Run scripts in the following order..."]
# Notes:         [Additional notes about assumptions, data sources, or ethical considerations]
# =====================================================

cat("Last Update 2024-11-25","\n",
    "This source file contains res_lst(),avg_applicant, bar_plot,","\n",
    ",g_heatmap, diag_plot, distr_plot.")

# 0925: Remove text on heatmap. 
# 1125: Change make_n_table using quantile instead of IQR

# 基本物件：係數表
res_lst <- function(list_data, ref_point, k, cov_lst = c() ){
  require(data.table)
  
  ref_sch <- ref_point[1]
  ref_maj <- ref_point[2]
  
  # re 併入 list_data
  re <- list_data$RES
  result_list <- list()
  
  cov_num <- length(cov_lst)
  
  # FIXME 改成用use wage, use dist
  if (cov_num == 2){
    par <- c("Dist", "Wage", colnames(list_data$DM)[-1])
  }else if(cov_num == 1){
    par <- c("Wage", colnames(list_data$DM)[-1])
  }else{
    par <- c(colnames(list_data$DM)[-1])
  }

  # Coefficients !!!!!!
  coef <- data.frame(parameter=c(par), Effect=as.numeric(re$estimate))
  coef <- as.data.table(coef)
  
  # Standard Error
  var <- vcov(re)
  rownames(var) <- par
  colnames(var) <- par
  SE <- diag(var)**(0.5)
  coef[, SE := SE]
  
  # Reference Points
  for( i in 1:k ){
    coef <- rbind(coef,list(paste0(ref_maj,"_K",i),0,0))
  }
  for( i in 1:k ){
    coef <- rbind(coef,list(paste0(ref_sch,"_K",i),0,0))
  }
  result_list$coef_tab <- coef
  result_list$vcov <- var
  
  # Field 
  f_effect<- coef[grepl("^[0-9]", parameter)] # start with 0-9
  f_effect[, Type := tstrsplit(parameter, "_K")[[2]]]
  f_effect[, Par := tstrsplit(parameter, "_K")[[1]]]
  
  f_effect[, Par := gsub("^[0-9]+", "", Par)] # remove number 
  f_effect[, Par := gsub("_+", " ", Par)]
  result_list$f_effect  <- f_effect
  
  # School
  # FIXME
  s_effect<- coef[grep("^U",parameter)]  # start with U
  s_effect[, Type := tstrsplit(parameter, "_K")[[2]] ]
  s_effect[, Par := tstrsplit(parameter, "_K")[[1]] ]
  
  s_effect[, Par := gsub("^.", "", Par)]
  result_list$s_effect  <- s_effect
  
  # Num_Obs
  result_list$Num_Obs <- dim(list_data$CM)[1]
  
  # 
  result_list$cov_list <- cov_lst
  
  #
  result_list$num_clus <- k
  
  return(result_list)
  # Output: coef_tab, vcov, f_effect, s_effect, Num_Obs, Cov_lst
}


complete_tab <- function(re, latex){
  
  require(data.table)
  require(kableExtra)
  
  f_tab <- re$f_effect
  s_tab <- re$s_effect
  
  k <- re$num_clus
  
  # dcast f_effect
  dt1 <- dcast(f_tab, Par ~ Type, value.var =  c("Effect", "SE"))
  # dcast seffect
  dt2 <- dcast(s_tab, Par ~ Type, value.var =  c("Effect", "SE"))
  # rbindlist()
  big_tab <- rbindlist(list(dt1, dt2),use.names = FALSE)
  
  nom <- c("Effect_", "SE_")
  # Generate the combinations
  comb <- expand.grid(Letter = nom, Number = 1:k)
  colnam <- c("Par",paste0(comb$Letter, comb$Number))
  
  # reorder
  big_tab <- big_tab[, ..colnam]
  
  # round
  big_tab[, (colnam[-1]) := lapply(.SD, round, digits = 2), .SDcols = colnam[-1]]
  
  # Append dynamic model labels (e.g., "(I)", "(II)", "(III)", etc.)
  header_labels <- c(" " = 1, setNames(rep(2, k), 
                                             paste0("(", seq_along(1:k), ")")))
  fx <- nrow(dt1)
  
  # Generate the table using kableExtra
  kb <- kbl(big_tab, format = if(latex == TRUE){"latex"} else {"html"},
            caption = paste0("Coefficients of Model ..."), booktabs = T, longtable = T) %>%
    kable_styling(latex_options = c("repeat_header")) %>%
    add_header_above(header_labels) %>%
    pack_rows("Panel A: ", 1, fx, italic = T) %>%
    pack_rows("Panel B: ", (fx+1), nrow(big_tab), italic = T)
  
  return(kb)
}

make_n_table <- function(Model_list, latex = FALSE){
  
  n = length(Model_list)
  
  # Helper function to calculate statistics for each model
  # Input: RES
  col_stats <- function(re){
    
    cal_statistics <- function(data, ...){
      vec <- round(
        c(max(data$Effect), min(data$Effect),
          median(data$Effect),  quantile(data$Effect, 0.25),
          quantile(data$Effect, 0.75))
        , 2)
      return(vec)
    }
    vec <- c(cal_statistics(re$f_effect),
             cal_statistics(re$s_effect))
    return(vec)
  }
  
  # Does the model contain necessary info?
  
  cal_cov <- function(re, ...){
    
    cov_num <- length(re$cov_list)
    coef <- re$coef
    
    if (cov_num == 0){
      dt <- data.table( Var =  c("Earnings","se1","Distance","se2"),
                        rep("",4))
    }else if( cov_num == 2){
      
      temp <- coef[1:2, .(parameter, Effect, SE)]
      dt_temp <- melt(temp, id.vars = "parameter", value.name = "value" )
      dt_temp[, Var := c("Distance", "Earnings","se2","se1")]
      
      dt <- data.table(Var = c("Earnings","se1","Distance","se2"))
      dt[ dt_temp, on ="Var" , value := i.value ]
      dt[, value := as.character(round(value, 2))]
      
      print(dt)
    }else{
      dt <- data.table( Var = c("Earnings","se1","Distance","se2"),
                        rep("",4) ) #FIXME
    }
  }
  
  cal_info <- function(re){
    vec <- c(re$num_clus, re$Num_Obs, nrow(re$coef) )
   return(vec) 
  }
  
  
  # Initialize the data.table with the row labels for statistics
  row_labels <- c("Max", "Min", "Median", "25-th percentile","75-th percentile")
  
  # Compute statistics for each model dynamically based on Model_list length
  col_stats_list <- lapply(Model_list, col_stats)
  
  distribute_dt <- data.table(c("Max","Min","Median","25-th percentile","75-th percentile"),
                       do.call(cbind, col_stats_list))  # Combine columns dynamically
  
  cov_stats_list <- lapply(Model_list, cal_cov)
  # Create dt1 (dummy table)
  cov_dt <- Reduce(function(x, y) merge(x, y, by = "Var", all.x = TRUE, sort = FALSE, suffixes = c("", paste0("_", length(x) + 1))), 
                   cov_stats_list )
  
  # Create dt2 (dummy table)
  # FIXME
  info_list <- lapply(Model_list, cal_info)
  
  info_dt <- data.frame(
    c("Num.clusters","Num. Observation","Num.Parameters"),
    do.call( cbind, info_list )
  )
  
  # Combine dt1, dst_dt, and dt2
  dt_comb <- rbindlist(list(cov_dt, distribute_dt, info_dt), use.names=FALSE)
  
  # Dynamically set the column names based on the number of models
  model_labels <- paste0("Model", 1:n)
  colnames(dt_comb) <- c("", model_labels)
  
  # Dynamically create header labels for kableExtra
  header_labels <- c(" " = 1)  # First column for the row labels
  # Append dynamic model labels (e.g., "(I)", "(II)", "(III)", etc.)
  header_labels <- c(header_labels, setNames(rep(1, n), 
                                             paste0("(", seq_along(Model_list), ")")))
  
  # Generate the table using kableExtra
  kb <- kbl(dt_comb, format = if(latex == TRUE){"latex"} else {"html"},
            caption = paste0("Cluster Table K = "), booktabs = T) %>%
    kable_styling() %>%
    add_header_above(header_labels) %>%
    pack_rows("Panel A: ", 5, 9, italic = T) %>%
    pack_rows("Panel B: ", 10, 14, italic = T) %>%
    footnote(number = c("Footnote 1; ", "Footnote 2; "),
             general = "This table is generated by kableExtra()", 
             threeparttable = T)
  
  return(kb)
}

avg_applicant <- function(dt){
  app_num <- cht_data[, .N, by = cluster_id]
  dt[app_num, on = c(Type="cluster_id"), num := i.N ]
  dt1 <- dt[SE < 50, weighted.mean(Effect, w= num, na.rm= TRUE) , 
            by = Par]
  colnames(dt1)[2] <- "Effect"
  setorder(dt1, Effect)
  dt1[, rank := 1:nrow(dt1)]
  return(dt1)
}


bar_plot <- function(res_lst,t){
  
  public <- c(
    "NTHU","NIU", "NTUE","NCCU","utaipei", "NYMU",  "NCKU",   "NTOU", "NTNU", "NCU",  "NDHU",   
    "NCTU",   "NCYU" ,"NTCU", "NTPU" , "NTTU" ,  "NCHU", "NCNU" ,"CCU"  ,   "NSYSU", "NTU" , "NUK"  ,  
    "NUU",    "NTUA" ,  "NCUE", "NQU" , "NTUS" , "TNNUA","NTSU" , "NKNU" ,  "NUTN"  ,  "UTNUA"  ,  "UNPTU"  , 
    "NDMC"
  )
  
  ins_effect <- rbind(res_lst$s_effect[Par %in% public ,.(Effect,SE,Type,Par)][, par:= "Public"],
                 res_lst$s_effect[! Par %in% public ,.(Effect,SE,Type,Par)][, par:= "Private"])
                
  all_effects <- rbind(res_lst$f_effect[,.(Effect,SE,Type,Par)][order(Effect)][, par:= "Major"],
                       ins_effect[order( Effect)])
  
  custom_colors <- c("Public" = "#D55E00",  # Blue
                     "Private" = "#009E73", # Green
                     "Major" = "#56B4E9")   # Red
  
  p <- all_effects[Type == t][, Par := factor(Par, levels = Par)][SE < 100] %>%
    ggplot(aes(y = Par, x= Effect, color = par )) + # FIXME
    geom_point(size = 1,position = position_dodge(width = 0.5) )+
    geom_errorbar(aes(xmin=Effect-1.96*SE, xmax=Effect+1.96*SE), width=0.3, 
                  position = position_dodge(width = 0.5))+
    scale_color_manual(values = custom_colors) + 
    ylab("")+
    xlab("")+
    theme_bw()+
    theme( axis.text = element_text(size = 5),
           legend.position = "none")
  
  return(p)
}



g_heatmap <- function(result, t = 0, ...){
  if(t != 0 ){
    dt <- result$f_effect[Type == t & SE < 50,.(Par, Effect)]
    setorder(dt, Effect)
    dt[, rank := 1:nrow(dt)]
    
    dt1 <- result$s_effect[Type == t & SE < 50,.(Par, Effect)]
    setorder(dt1, Effect)
    dt1[, rank := 1:nrow(dt1)] 
  }else{
    dt <- avg_f
    dt1 <- avg_s
  }
  expanded <- dt1[ ,.( Par= dt$Par), by = .(Par, Effect, rank)]
  colnames(expanded)[1] <- "Par_x"
  expand_tab <- merge(expanded, dt, by = c("Par"), 
                      allow.cartesian = TRUE)
  expand_tab[, predicted_effect := Effect.x + Effect.y]
  
  expand_tab$Par_y <- factor(expand_tab$Par, levels = dt$Par )
  expand_tab$Par_x <- factor(expand_tab$Par_x, levels = dt1$Par )
  
  p <- ggplot(expand_tab, aes(x = factor(Par_x), y = factor(Par_y),
                              fill = predicted_effect)) +
    geom_tile() +
    scale_fill_gradient2(name = "Predicted Effect",
                         low = "#075AFF",
                         mid = "#FFFFCC",
                         high = "#FF0000")+
    labs(x = "", y = "") +
    #geom_text(aes(label = round(predicted_effect,2)), color = "black", size = 1.5) +
    theme_minimal()+
    theme(text=element_text(family = "黑體-繁 中黑") ,
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  return(p)
}



Type_tab <-  function(res, par, data, ref_lst){
  
  ref_sch <- ref_lst[1]
  ref_maj  <- ref_lst[2]

  
  # coeficients 
  coef <- res$estimate
  names(coef) <- par
  
  # Variance_Covariance Matrix
  var <- vcov(res)
  rownames(var) <- par
  colnames(var) <- par
  
  # Redefine
  data[, Par :=  tstrsplit(parameter, "_K")[[1]] ]
  
  # A wide table
  dt <- dcast(data[! Par %in% c(ref_maj,ref_sch)], 
              Type ~ Par, value.var = c("Effect", "SE"))
  return(dt)
}


# Remove if not further issues
diag_plot_old <-  function(dlist, type, MoS, ref_lst, cov_num, cluster_name_list){
  
  res <- dlist$RES
  
  # FIXME 改成用use wage, use dist
  if (cov_num == 2){
    par <- c("Dist", "Wage", dlist$Par[-1])
  }else if(cov_num == 1){
    par <- c("Wage", dlist$Par[-1])
  }else{
    par <- dlist$Par[-1]
  }
  
  if(MoS == "M"){
    data <- copy(dlist$tab$f_effect)
  }else if(MoS == "S"){
    data <- copy(dlist$tab$s_effect)
  }
  
  
  ref_sch <- ref_lst[1]
  ref_maj  <- ref_lst[2]
  # coeficients 
  coef <- res$estimate
  names(coef) <- par
  
  # Variance_Covariance Matrix
  var <- vcov(res)
  rownames(var) <- par
  colnames(var) <- par
  
  # Redefine
  data[, Var :=  tstrsplit(parameter, "_K")[[1]] ]
  
  # A wide table
  dt <- dcast(data[! Var %in% c(ref_maj,ref_sch)], 
              Var+Par ~ Type, value.var = c("Effect", "SE"))

  # Create a Table with only the two type of interest
  vec <- c("Var", "Par", paste0("Effect_",type[1]), paste0("Effect_",type[2]))
  dt1 <- dt[, ..vec]
  print(dt1)
  colnames(dt1) <- c("Var","Par", "X", "Y")
  dt1 <- na.omit(dt1)
  
  diff_wald_test <- function(x, t, co = coef, v = var ){
    
    # hypothesis
    x1 <- paste0(x, "_K", t[1])
    x2 <- paste0(x, "_K", t[2])
    #print(x1)
    # test statistics
    diff_coef <- co[x1] - co[x2]
    se_diff <- sqrt(v[x1, x1] + v[x2, x2] - 2 * v[x1, x2])
    wald_stat <- (diff_coef / se_diff)^2
    
    # p-value
    p_value <- 1 - pchisq(wald_stat, df = 1)
    names(p_value) <- NULL
    return(p_value)
  }
  
  # 
  dt1[, p_val := sapply(dt1[, Var], diff_wald_test , t = type)]
  dt1[, `p < 0.05` := p_val < 0.05]
  
  
  name1 <- cluster_name_list[ as.numeric(type[1]) ]
  name2 <- cluster_name_list[ as.numeric(type[2]) ]
  
  if(MoS == "M"){
    scp = 1
    
    # plot range
    axis_range <- range(c(dt1$X, dt1$Y))
    if(-scp >= axis_range[1] | scp <= axis_range[2]){
      warning("Some value out of range")
    }
    
    #
    p <- ggplot(dt1, aes(x = X, y = Y, shape = `p < 0.05`, label = Par)) +
      geom_point(size = 2) +      # Scatter plot
      geom_abline(slope = 1, intercept = 0,     # Diagonal line
                  color = "black", linetype = "dashed") +
      coord_equal() +               # Equal scaling on both axes
      theme_minimal() +
      geom_text(vjust=-1, hjust= 0, size= 3)+
      scale_x_continuous(limits = c(-scp,scp)) +# Set the same range for x-axis
      scale_y_continuous(limits = c(-scp,scp)) +  
      labs(x = paste0(name1),
           y = paste0(name2))+
      scale_shape_manual(values = c(1,16))  
  }else{
    scp = 7
    
    # plot range
    axis_range <- range(c(dt1$X, dt1$Y))
    if(-scp >= axis_range[1] | scp <= axis_range[2]){
      warning("Some value out of range")
    }
    
    #
    p <- ggplot(dt1, aes(x = X, y = Y, shape = `p < 0.05`)) +
      geom_point(size = 2) +      # Scatter plot
      geom_abline(slope = 1, intercept = 0,     # Diagonal line
                  color = "black", linetype = "dashed") +
      coord_equal() +               # Equal scaling on both axes
      theme_minimal() +
      #geom_text(vjust=-1, hjust= 0, size= 2)+
      scale_x_continuous(limits = c(-scp,scp)) +# Set the same range for x-axis
      scale_y_continuous(limits = c(-scp,scp)) +  
      labs(x = paste0(name1),
           y = paste0(name2))+
      scale_shape_manual(values = c(1,16))  
  }
  
  
  return(p)
  
}



diag_plot <-  function(dlist, type, MoS, ref_lst, cov_num, cluster_name_list){
  
  res <- dlist$RES
  
  # FIXME 改成用use wage, use dist
  if (cov_num == 2){
    par <- c("Dist", "Wage", dlist$Par[-1])
  }else if(cov_num == 1){
    par <- c("Wage", dlist$Par[-1])
  }else{
    par <- dlist$Par[-1]
  }
  
  if(MoS == "M"){
    data <- copy(dlist$tab$f_effect)
  }else if(MoS == "S"){
    data <- copy(dlist$tab$s_effect)
  }
  
  
  ref_sch <- ref_lst[1]
  ref_maj  <- ref_lst[2]
  
  # coeficients 
  coef <- res$estimate
  names(coef) <- par
  
  # Variance_Covariance Matrix
  var <- vcov(res)
  rownames(var) <- par
  colnames(var) <- par
  
  # Redefine
  data[, Var :=  tstrsplit(parameter, "_K")[[1]] ]
  
  # A wide table
  dt <- dcast(data[! Var %in% c(ref_maj,ref_sch)], 
              Var+Par ~ Type, value.var = c("Effect", "SE"))
  
  # Create a Table with only the two type of interest
  vec <- c("Var", "Par", paste0("Effect_",type[1]), paste0("Effect_",type[2]))
  dt1 <- dt[, ..vec]
  #print(dt1)
  colnames(dt1) <- c("Var","Par", "X", "Y")
  dt1 <- na.omit(dt1)
  
  diff_wald_test <- function(x, t, co = coef, v = var ){
    
    # hypothesis
    x1 <- paste0(x, "_K", t[1])
    x2 <- paste0(x, "_K", t[2])
    #print(x1)
    # test statistics
    diff_coef <- co[x1] - co[x2]
    se_diff <- sqrt(v[x1, x1] + v[x2, x2] - 2 * v[x1, x2])
    wald_stat <- (diff_coef / se_diff)^2
    
    # p-value
    p_value <- 1 - pchisq(wald_stat, df = 1)
    names(p_value) <- NULL
    return(p_value)
  }
  
  # 
  dt1[, p_val := sapply(dt1[, Var], diff_wald_test , t = type)]
  dt1[, `p < 0.05` := p_val < 0.05]
  
  
  name1 <- cluster_name_list[ as.numeric(type[1]) ]
  name2 <- cluster_name_list[ as.numeric(type[2]) ]
  
  if(MoS == "M"){
    
    par_name <- dlist$tab$f_effect[ parameter == paste0(ref[2], "_K1"),]$Par
    dt1 <- rbind( dt1,
                      list( ref_maj, par_name, 0,0,0, FALSE))
    
    print(dt1)
    
    scp = 1
    
    # plot range
    axis_range <- range(c(dt1$X, dt1$Y))
    if(-scp >= axis_range[1] | scp <= axis_range[2]){
      warning("Some value out of range")
    }
    
    #
    p <- ggplot(dt1, aes(x = X, y = Y, shape = `p < 0.05`, label = Par)) +
      geom_point(size = 2) +      # Scatter plot
      geom_abline(slope = 1, intercept = 0,     # Diagonal line
                  color = "black", linetype = "dashed") +
      coord_equal() +               # Equal scaling on both axes
      theme_minimal() +
      geom_text_repel(size = 2.5, max.overlaps = 15) +
      scale_x_continuous(limits = c(-scp,scp)) +# Set the same range for x-axis
      scale_y_continuous(limits = c(-scp,scp)) +  
      labs(x = paste0(name1),
           y = paste0(name2))+
      scale_shape_manual(values = c(1,16))  
  }else{
    par_name <- dlist$tab$s_effect[ parameter == paste0(ref[1], "_K1"),]$Par
    dt1 <- rbind( dt1,
                  list( ref_sch, par_name, 0,0,0, FALSE))
    
    print(dt1)
    scp = 7
    
    # plot range
    axis_range <- range(c(dt1$X, dt1$Y))
    if(-scp >= axis_range[1] | scp <= axis_range[2]){
      warning("Some value out of range")
    }
    
    #
    p <- ggplot(dt1, aes(x = X, y = Y, shape = `p < 0.05`)) +
      geom_point(size = 2) +      # Scatter plot
      geom_abline(slope = 1, intercept = 0,     # Diagonal line
                  color = "black", linetype = "dashed") +
      coord_equal() +               # Equal scaling on both axes
      theme_minimal() +
      #geom_text(vjust=-1, hjust= 0, size= 2)+
      scale_x_continuous(limits = c(-scp,scp)) +# Set the same range for x-axis
      scale_y_continuous(limits = c(-scp,scp)) +  
      labs(x = paste0(name1),
           y = paste0(name2))+
      scale_shape_manual(values = c(1,16))  
  }
  
  
  return(p)
  
}

#ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 
#cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
#diag_plot( datalist_0, c("1","2"), "M" , ref, cov_num = 0, cn_lst)

# indiff <- function(){
#   ggplot(expand_tab, aes(x = factor(Par_x), y = factor(Par_y),
#                               fill = predicted_effect))+
#     geom_contour_filled()+
#     scale_fill_brewer(palette = "Spectral",drop=FALSE)+
#     guides(fill = guide_colorsteps(direction = "horizontal",
#                                    barwidth = unit(par("pin")[1], "in")))+
#     scale_x_continuous( expand = c(0, 0)) +
#     scale_y_continuous( expand = c(0, 0))+
#     theme(legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=1))+
#     xlab("")+
#     ylab("")+
#     theme(legend.position = "bottom")
# }

distr_plot <- function( dt ,a){
  if (a == 1){
    p <- dt[SE < 50]%>%
      ggplot( aes(y=Effect, x=Type, label = Par)) +
      geom_jitter(width = 0.1, height = 0, size =1)+
      theme_bw()+
      geom_text(vjust=0.5, hjust=1.2, size= 2.5)+
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))+
      xlab("")
  }else{
    p <- dt[SE < 50]%>%
      ggplot( aes(y=Effect, x=Type)) +
      geom_jitter(width = 0.1, height = 0, size =1)+
      theme_bw()+
      theme(axis.title = element_text(size = 20), axis.text = element_text(size = 15))+
      xlab("")
  }
  return(p)
}







library(kableExtra)

make_5table <- function(Model_list, latex = FALSE){
  
  col_stats <- function(re){
    cal_statistics <- function(data,...){
      vec <- round(
        c(max(data$Effect), min(data$Effect),
          median(data$Effect), quantile(data$Effect, 0.25),
          quantile(data$Effect, 0.75))
        , 2)
      return(vec)
    }
    vec <- c(cal_statistics(re$f_effect),
             cal_statistics(re$s_effect))
    return(vec)
  }
  
  try_dt <- data.table(
    c("Max","Min","Median","IQR"),
    col_stats(Model_list[[1]]),
    col_stats(Model_list[[2]]),
    col_stats(Model_list[[3]]),
    col_stats(Model_list[[4]]),
    col_stats(Model_list[[5]])
  )
  
  dt1 <- data.frame(
    c("Earings","Distance"),
    c("",""),
    c("",""),
    c("",""),
    c("",""),
    c("","")
  )
  
  dt2 <- data.frame(
    c("Num.clusters","Num. Observation","Num.Parameters"),
    c("","",""),
    c("","",""),
    c("","",""),
    c("","",""),
    c("","","")
  )
  
  dt_comb <- rbindlist(list(dt1, try_dt, dt2), use.names=FALSE)
  colnames(dt_comb) <- c("","Model1","Model2","Model3","Model4","Model5")
  
  kb <- kbl( dt_comb , format = if(latex == TRUE){"latex"}, 
             caption = paste0("Cluster Table K ="), booktabs = T) %>% 
    kable_styling() %>%
    add_header_above(c(" " = 1, "(I)" = 1, "(II)" = 1, "(III)" = 1, "(IV)" = 1,"(V)" = 1 )) %>%
    pack_rows("Panel A: Field", 3, 7) %>%
    pack_rows("Panel B: Institution", 8, 12) %>%
    footnote(number = c("Footnote 1; ", "Footnote 2; "),
             general = "Here is a very very very very very very very very very very long",
             threeparttable = T)
  
  return(kb)
}


# ---- New Tab --------------------

make_table_new <- function( tab, k, latex = FALSE){
  
  f_list <- list()
  for (i in 1:k){
    f_list[[i]] <- tab$f_effect[Type == as.character(i)]
  } 
  
  s_list <- list()
  for (i in 1:k){
    s_list[[i]] <- tab$s_effect[Type == as.character(i)]
  } 
  n = k
  
  # Helper function to calculate statistics for each model
  # Input: RES
  col_stats <- function(re){
    
    cal_statistics <- function(data, ...){
      vec <- round(
        c(max(data$Effect), quantile(data$Effect, 0.75),
          median(data$Effect),  quantile(data$Effect, 0.25), 
          min(data$Effect))
        , 2)
      vec <- c(
        vec, data[order(-Effect)]$Par[1:3], nrow(data)-1
      )
      return(vec)
    }
    vec <- cal_statistics(re)
    return(vec)
  }
  
  # Initialize the data.table with the row labels for statistics
  row_labels <- c("Max","75-th percentile", "Median", "25-th percentile","Min",
                  "1st","2nd","3rd","Parmeters")
  
  # Compute statistics for each model dynamically based on Model_list length
  col_stats_list1 <- lapply( f_list , col_stats)
  
  distribute_dt1 <- data.table( row_labels,
                              do.call(cbind, col_stats_list1))  # Combine columns dynamically
  
  
  col_stats_list2 <- lapply( s_list , col_stats)
  
  distribute_dt2 <- data.table( row_labels,
                               do.call(cbind, col_stats_list2))  # Combine columns dynamically


  # Combine dt1, dst_dt, and dt2
  dt_comb <- rbindlist(list( distribute_dt1, distribute_dt2), use.names=FALSE)
  
  # Dynamically set the column names based on the number of models
  model_labels <- paste0("Type", 1:n)
  colnames(dt_comb) <- c("", model_labels)
  
  # Dynamically create header labels for kableExtra
  header_labels <- c(" " = 1)  # First column for the row labels
  # Append dynamic model labels (e.g., "(I)", "(II)", "(III)", etc.)
  header_labels <- c(header_labels, setNames(rep(1, n), 
                                             paste0("(", seq_along(s_list), ")")))
  
  # Generate the table using kableExtra
  kb <- kbl(dt_comb, format = if(latex == TRUE){"latex"} else {"html"},
            caption = paste0("Cluster Table K = "), booktabs = T) %>%
    kable_styling() %>%
    add_header_above(header_labels) %>%
    pack_rows("Panel A: ", 1, 9, italic = T) %>%
    pack_rows("Panel B: ", 10, 17, italic = T) %>%
    footnote(number = c("Footnote 1; ", "Footnote 2; "),
             general = "This table is generated by kableExtra()", 
             threeparttable = T)
  
  return(kb)
}

make_table_type1 <- function( tab_lst, n, latex = FALSE){
  
  f_list <- list()
  for (i in 1:n){
    f_list[[i]] <- tab_lst[[i]]$f_effect[Type == as.character(1)]
  } 
  
  s_list <- list()
  for (i in 1:n){
    s_list[[i]] <- tab_lst[[i]]$s_effect[Type == as.character(1)]
  } 
  
  # Helper function to calculate statistics for each model
  # Input: RES
  col_stats <- function(re){
    
    cal_statistics <- function(data, ...){
      vec <- round(
        c(max(data$Effect), quantile(data$Effect, 0.75),
          median(data$Effect),  quantile(data$Effect, 0.25), 
          min(data$Effect))
        , 2)
      vec <- c(
        vec, data[order(-Effect)]$Par[1:3]
      )
      return(vec)
    }
    vec <- cal_statistics(re)
    return(vec)
  }
  
  # Initialize the data.table with the row labels for statistics
  row_labels <- c("Max","75-th percentile", "Median", "25-th percentile","Min",
                  "1st","2nd","3rd")
  
  # Compute statistics for each model dynamically based on Model_list length
  col_stats_list1 <- lapply( f_list , col_stats)
  
  distribute_dt1 <- data.table( row_labels,
                                do.call(cbind, col_stats_list1))  # Combine columns dynamically
  
  
  col_stats_list2 <- lapply( s_list , col_stats)
  
  distribute_dt2 <- data.table( row_labels,
                                do.call(cbind, col_stats_list2))  # Combine columns dynamically
  
  
  # Combine dt1, dst_dt, and dt2
  dt_comb <- rbindlist(list( distribute_dt1, distribute_dt2), use.names=FALSE)
  
  # Dynamically set the column names based on the number of models
  model_labels <- paste0("Type", 1:n)
  colnames(dt_comb) <- c("", model_labels)
  
  # Dynamically create header labels for kableExtra
  header_labels <- c(" " = 1)  # First column for the row labels
  # Append dynamic model labels (e.g., "(I)", "(II)", "(III)", etc.)
  header_labels <- c(header_labels, setNames(rep(1, n), 
                                             paste0("(", seq_along(s_list), ")")))
  
  # Generate the table using kableExtra
  kb <- kbl(dt_comb, format = if(latex == TRUE){"latex"} else {"html"},
            caption = paste0("Cluster Table K = "), booktabs = T) %>%
    kable_styling() %>%
    add_header_above(header_labels) %>%
    pack_rows("Panel A: ", 1, 8, italic = T) %>%
    pack_rows("Panel B: ", 9, 16, italic = T) %>%
    footnote(number = c("Footnote 1; ", "Footnote 2; "),
             general = "This table is generated by kableExtra()", 
             threeparttable = T)
  
  return(kb)
}


save_plot <- function(t, func , sz, res_lst = result,  output_dir, p_name) {
  cat("Save Plot() ouput directory is:", output_dir)
  # Create the plot for the current value of t
  
  p <- func(res_lst, t = t)
  
  # Define the filename based on the current value of t
  file_name <- paste0(p_name,"_type", t, ".png")
  
  # Save the plot with customized size
  ggsave(filename = file_name, plot = p, path = output_dir,
         width = sz[1], height = sz[2], units = "in")
}


save_plot_diag <- function( t_pair, sz, res_lst = result, MoS, ref, cov_num, output_dir,
                            p_name, cn_lst = c("T1","T2","T3","T4")){
  # Create the plot for the current value of t
  p <- diag_plot( res_lst, t_pair, MoS , ref, cov_num, cn_lst)
  # Define the filename based on the current value of t
  file_name <- paste0(p_name,"_type", paste0(t_pair, collapse="") , ".png")
  # Save the plot with customized size
  ggsave(filename = file_name, plot = p, path = output_dir,
         width = sz[1], height = sz[2], units = "in")
  print("OK")
}


basic_figures <- function( model_name, g, dtlst, out_dir, cn_lst, cov_num = 0 ){
  
  m_name <- model_name
  # Define the values of t
  t_values <- 1:g
  
  # --- Heat map ---
  p_name <- paste0(m_name,"_hm")
  # Use purrr::walk() to apply the save_plot function to each value of t
  purrr::walk(t_values, function(x)  save_plot(x, func = g_heatmap, 
                     sz = c(10,8), res_lst = dtlst$tab,  out_dir, p_name))
  
  # --- Bar map ---
  p_name <- paste0(m_name,"_bar")
  # Use purrr::walk() to apply the save_plot function to each value of t
  purrr::walk(t_values, function(x)  save_plot(x, func = bar_plot, 
                     sz = c(8,10), res_lst = dtlst$tab,  out_dir, p_name))
  
  # --- Diag Plot ---
  
  t_pair <- lapply(2:g, function(k) c("1", as.character(k)))
  p_name <- paste0(m_name,"_fld_diag_")
  
  purrr::walk(1:(g-1) , function(x)  save_plot_diag( t_pair[[x]], MoS= "M", 
            ref= ref, sz = c(5,5), res_lst = dtlst, cov_num, out_dir ,p_name, cn_lst))
  
  p_name <- paste0(m_name,"_sch_diag_")
  purrr::walk(1:(g-1) , function(x)  save_plot_diag( t_pair[[x]], MoS= "S", 
            ref= ref, sz = c(5,5), res_lst = dtlst, cov_num, out_dir, p_name, cn_lst))
  
}
