# No Hetero Ver. 0228

# --- 1. Initialize Environment ----
library(fastmatch)
library(data.table)
library(plotly)

ref_sd <- "輔仁大學企業管理學系"
ref_tab <- fread("01Data/complete_reference_table_250302.csv")
choice_set_long <- fread("01Data/pre_cluster_sample0219.csv")

# Create Choice Matrix
create_matrices_sep <- function(data, p_list){
  dt <- copy(data)
  
  # 編碼
  dt[, opt_index :=  match(t(dt$Option),p_list)]
  dt[, c_index :=  match(t(dt$choice),p_list)]
  num_list <- unique(dt$y_id)
  dt[, number :=  match(t(dt$y_id), num_list)]
  rm(num_list)
  
  m <- as.matrix(unique(dt[,.(number, c_index)]))
  colnames(m) <- NULL
  
  # Choice Matrix
  sub_alt_mat <- sparseMatrix(i=dt$number, j=dt$opt_index, x=1)
  sub_choice_data <- cbind(as.numeric(m[,1:2]),sub_alt_mat)
  
  return(sub_choice_data)
}



# rename
choice_set_long[, loc := location ]
choice_set_long[, gender := Gender ]

g = 4 # Input: g and cht_set_choice_long
data_used <- choice_set_long[,.N , by = .(y_id,Ch,EN,Math,Social,Nature,
                                          D1,D2,D3,D4,D5,
                                          g1,g2,g3,
                                          gender,loc,year )]
set.seed(123)
source("00src/cluster_table.R")
clustered_dt <- first_step(data_used, g=4, col = 
                             c( "Ch","EN","Math","Social","Nature","D1","D2","D3","D4","D5"))

choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '2',
                               '2' = '1',
                               '3' = '4',
                               '4' = '3'))

# ----------------------------------------------------------
source("00src/new_verify.R")

# Create Choice Matrix
create_matrices_sep <- function(data, p_list){
  dt <- copy(data)
  
  # 編碼
  dt[, opt_index :=  match(t(dt$Option),p_list)]
  dt[, c_index :=  match(t(dt$choice),p_list)]
  num_list <- unique(dt$y_id)
  dt[, number :=  match(t(dt$y_id), num_list)]
  rm(num_list)
  
  m <- as.matrix(unique(dt[,.(number, c_index)]))
  colnames(m) <- NULL
  
  # Choice Matrix
  sub_alt_mat <- sparseMatrix(i=dt$number, j=dt$opt_index, x=1)
  sub_choice_data <- cbind(as.numeric(m[,1:2]),sub_alt_mat)
  
  return(sub_choice_data)
}

source("Result/Default_function.r")

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


# ---------------------------------------------------------
K = 4

cht_data <- choice_set_long[cluster_id == K]
cleaned_dt <- verify_new(cht_data)

program_list <-  unique(cleaned_dt$choice)
indx <- match(ref_sd,program_list)
program_list <- replace(program_list, c(1,indx), program_list[c(indx, 1)])


CM <-  create_matrices_sep(cleaned_dt,  program_list)


# -----5. Econometrics Model ---------------------------------

Ini <- rep(0,(length(program_list)-1))
system.time(EST <- MLE(CM,Ini,Lambda=0))
print(EST$message)

Var0 <- vcov(EST)
se0 <- diag(Var0) %>% sqrt()
Estimate0 <- cbind(S_Dep = program_list,effect = c(0,EST$estimate),se=c(0,se0)) %>% data.frame()
Estimate0$effect <- as.numeric(Estimate0$effect)


# -------- Visualization -----------------------------------

## --- Maj = Category -----
{
ref <- c("UFJU", "041Business", "輔仁大學企業管理學系","category") 
m_name<- c("mcatk4_all")

# For all g =4, sample  = conditional on Wage and Dist
c_dt <- copy(choice_set_long)

# Model 1 (cov_num=0, gen = 0, reg =0)
subsmp <- verify_combine( c_dt, ref)
datalist_cat1 <- Pre_estimation(cov_num = 0, subsmp$final_dt, subsmp$p_list, ref)

datalist_cat1$RES <- MLE_GLM_wrap(datalist_cat1) #Optional: summary(m1_result)
datalist_cat1$tab <- res_lst( datalist_cat1, ref, k =4 )
}



# Comparison 
tab <- datalist_cat1$tab
type <- tab$f_effect[Type == K ][order(Effect)]
type[, rank := 1: nrow(type)]
type[, parameter := tstrsplit(parameter, "_K")[[1]] ]

sch <- tab$s_effect[Type == K ][order(Effect)]
sch[, rank := 1: nrow(sch)]

dt <- as.data.table(Estimate0)
dt[ref_tab, on = c(S_Dep= "V1"), school := i.eng_school ]
dt[ref_tab, on = c(S_Dep= "V1"), cat := i.Eng_Category ]

dt[sch, on=c(school = "Par"), c("sch_rank","sch_effect") := .(i.rank,i.Effect)]
dt[type,on=c(cat = "parameter"), c("type_rank","type_effect") := .(i.rank, i.Effect)]

#dt[,sd_effect := sch_effect+type_effect]

dt[,effect_level := cut(effect,breaks=c(-10,-4,-3,-2,-1,0,1,2,3,5,7,10,15))]
ggplot(dt, aes(x= sch_rank, y= type_rank, color = effect_level))+
  geom_jitter()


dt1 <- dt[! is.na(sch_rank) & ! is.na(type_rank)]

fig <- plot_ly(
  x = dt1$sch_rank,
  y = dt1$type_rank,
  z = dt1$effect,
  type = "contour",
  colorscale = 'BrRd',
  contours = list(
    start = 15,
    end = -5,
    size = 1
    #showlabels = TRUE
  ),
  line=list(width=0)
)

fig

rm( Ini, EST, Estimate0, Var0, se0)
rm(tab, type, sch, dt, dt1)
