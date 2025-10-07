# =========
# Title:  Main Results
# Steps: Select K


# ==========


# ----------------------- 1. Initialize Environment -----------------------
library(data.table)
library(tidyverse)
library(maxLik)

source("00src/cluster_table.R")
source("00src/new_verify.R")
source("00src/DChoice_matrices.R")
source("00src/UHetero_MLE.r")
source("00src/visualization.R")

# ----------------------- 3. Load Data ------------------------------------

# choice_set_long <- fread()
choice_set_long <- fread("01Data/pre_cluster_sample0219.csv")

# FIXME
choice_set_long[, loc := location ]
choice_set_long[, gender := Gender ]

# ----------------------- 4. Clustering ------------------------------------

g = 3 # Input: g and cht_set_choice_long
data_used <- choice_set_long[,.N , by = .(y_id,Ch,EN,Math,Social,Nature,
                                          D1,D2,D3,D4,D5,
                                          g1,g2,g3,
                                          gender,loc,year
)]

set.seed(123)

clustered_dt <- first_step(data_used, g = g, 
                           col = c( "Ch","EN","Math","Social","Nature","D1","D2","D3","D4","D5"))

# Take a look
cluster_table_n(clustered_dt , c(1,2,3), latex = FALSE, ks = 3) # 暫時取消製作header的功能

#
latex_code <- cluster_table_n(clustered_dt , c(1,2,3), latex = TRUE, ks = 3)
filename <- paste0("04Output/Tables/Appendix/cluster_tableK3_before.tex")
writeLines(latex_code, con = filename)

# FIXME
choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '1',
                               '2' = '2',
                               '3' = '3'))
rm(data_used)

# ------------------------ 5. Main Result ---------------------------------

m_name <- c("mk3_all")
c_dt <- copy(choice_set_long)

## ---------------------- 5.1 Baseline -------------------------------------

# Model 1 (cov_num=0, gen = 0, reg =0)

ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 

subsmp0 <- verify_combine( choice_set_long, ref) 
datalist_0 <- Pre_estimation(cov_num = 0, subsmp0$final_dt, subsmp0$p_list, ref , least = 0)

datalist_0$RES <- MLE_GLM_wrap(datalist_0) #Optional: summary(m1_result)
datalist_0$tab <- res_lst( datalist_0, ref, k=3 )

out_dir <- paste0("04Output/Figure/Main/All")


# Tables
com_tab_0 <- make_table_new(datalist_0$tab, k = 3, latex = TRUE)
filename <- paste0("04Output/Tables/Appendix/coef_list_mk3_all.tex")
writeLines(com_tab_0, con = filename)


# -------------- K = 5 -----------------------------------------

# choice_set_long <- fread()
choice_set_long <- fread("01Data/pre_cluster_sample0219.csv")

# FIXME
choice_set_long[, loc := location ]
choice_set_long[, gender := Gender ]

g = 5 # Input: g and cht_set_choice_long
data_used <- choice_set_long[,.N , by = .(y_id,Ch,EN,Math,Social,Nature,
                                          D1,D2,D3,D4,D5,
                                          g1,g2,g3,
                                          gender,loc,year
)]

set.seed(123)

clustered_dt <- first_step(data_used, g = g, 
                           col = c( "Ch","EN","Math","Social","Nature","D1","D2","D3","D4","D5"))

# Take a look
cluster_table_n(clustered_dt , c(1,2,3,4,5), latex = FALSE, ks = 5) # 暫時取消製作header的功能

#
latex_code <- cluster_table_n(clustered_dt , c(1,2,3,4,5), latex = TRUE, ks = 5)
filename <- paste0("04Output/Tables/Appendix/cluster_tableK5_before.tex")
writeLines(latex_code, con = filename)

# FIXME
choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '1',
                               '2' = '2',
                               '3' = '3',
                               '4' = '4',
                               '5' = '5'))
rm(data_used)

# ------------------------ 5. Main Result ---------------------------------

m_name <- c("mk5_all")
c_dt <- copy(choice_set_long)

## ---------------------- 5.1 Baseline -------------------------------------

# Model 1 (cov_num=0, gen = 0, reg =0)

ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 

subsmp0 <- verify_combine( choice_set_long, ref) 
datalist_0 <- Pre_estimation(cov_num = 0, subsmp0$final_dt, subsmp0$p_list, ref , least = 0)

datalist_0$RES <- MLE_GLM_wrap(datalist_0) #Optional: summary(m1_result)
datalist_0$tab <- res_lst( datalist_0, ref, k=5 )


# Tables
com_tab_0 <- make_table_new(datalist_0$tab, k = 5, latex = TRUE)
filename <- paste0("04Output/Tables/Appendix/coef_list_mk5_all.tex")
writeLines(com_tab_0, con = filename)

