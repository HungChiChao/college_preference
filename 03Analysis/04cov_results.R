# =========
# Title: Covariates Results
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

out_dir <- paste0("04Output/Figure/Main")

# ----------------------- 4. Clustering ------------------------------------

g = 4 # Input: g and cht_set_choice_long
data_used <- choice_set_long[,.N , by = .(y_id,Ch,EN,Math,Social,Nature,
                                          D1,D2,D3,D4,D5,
                                          g1,g2,g3,
                                          gender,loc,year
)]

set.seed(123)

clustered_dt <- first_step(data_used, g=4, col = c( "Ch","EN","Math","Social","Nature","D1","D2","D3","D4","D5"))

# Take a look
cluster_table_temp(clustered_dt , c(2,1,4,3), latex = FALSE, ks = 4) # 暫時取消製作header的功能


choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '2',
                               '2' = '1',
                               '3' = '4',
                               '4' = '3'))
rm(data_used)

# ----------------------- 5. Results ------------------------------------

c_dt <- copy(choice_set_long)
c_dt <- c_dt[ ! is.na(c_wage) & ! is.na(alt_wage) & ! is.na(UX) & !is.na(SX)]


## ----- Model 1 (cov_num=0, gen = 0, reg =0) -------
ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 

subsmp_1 <- verify_combine( c_dt, ref)
datalist_1 <- Pre_estimation(cov_num = 0, subsmp_1$final_dt, subsmp_1$p_list, ref)

datalist_1$RES <- MLE_GLM_wrap(datalist_1) #Optional: summary(m1_result)
datalist_1$tab <- res_lst( datalist_1, ref, k=4 )


## ----- Model 2 (cov_num=1, use_wage, use_dist, gen = 0, reg =0) -----
subsamp <- verify_combine(c_dt, ref)
datalist_2 <- Pre_estimation(1, subsamp$final_dt, subsamp$p_list, ref)

datalist_2$RES  <- MLE_GLM_heter_wrap(datalist_2, 0, 1) #summary(m3_result)
datalist_2$tab  <- res_lst( datalist_2, ref, k=4, cov_lst = c("earning") )

saveRDS(datalist_2, paste0("04Output/Result2/",m_name[2],"_",dat,".rds"))


## ----- Model 3 (cov_num=2, use_wage, use_dist, gen = 0, reg =0) ------
subsamp_3 <- verify_combine(c_dt, ref)
datalist_3 <- Pre_estimation(2, subsamp_3$final_dt, subsamp_3$p_list, ref)

datalist_3$RES  <- MLE_GLM_heter_wrap(datalist_3, 1, 1) #summary(m3_result)
datalist_3$tab  <- res_lst( datalist_3, ref, k=4, cov_lst = c("earning", "dist") )

# Save 
saveRDS(datalist_3, paste0("04Output/Result2/",m_name[3],"_",dat,".rds"))

m_name <- c("mk4-3_covs")
# Figures
out_dir <- paste0("04Output/Figure/Main/Cov") # FIXME
cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
basic_figures( m_name[1], g = 4, dtlst = `mk4-3_0221`, out_dir, cn_lst)
# FIXME

# Table 

# Tables
com_tab_0 <- make_table_new(datalist_0$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/0221/coef_list_mk4_all",dat ,".tex")
writeLines(com_tab_0, con = filename)


# ---------- 6. Comparison: Tab and diag -----------------


sub_lst <- list(datalist_1$tab, datalist_2$tab,
                datalist_3$tab) 

make_table_type1(sub_lst, n = 3, latex = FALSE)

com_tab_cov <- make_table_type1(sub_lst, n = 3, latex = TRUE)
filename <- paste0("04Output/Tables/",dat,"/coef_list_mk4_covariates",dat ,".tex")
writeLines(com_tab_cov, con = filename)




save_plot <- function(t, func , sz, res_lst = result) {
  # Create the plot for the current value of t
  
  p <- func(res_lst, t = t)
  
  # Define the filename based on the current value of t
  file_name <- paste0(p_name,"_type", t,"_",dat, ".png")
  
  # Save the plot with customized size
  ggsave(filename = file_name, plot = p, path = output_dir,
         width = sz[1], height = sz[2], units = "in")
}

exp_coef <- merge(datalist_1$tab$coef_tab, datalist_3$tab$coef_tab[-c(1:2),], by = "parameter", all = TRUE)
exp_coef[, Type := tstrsplit(parameter, "K")[[2]] ]

lim <- 7
p <- ggplot( exp_coef[Type == 1], aes(x = Effect.x, y = Effect.y))+
  geom_point(size = 1.5) +      # Scatter plot
  geom_abline(slope = 1, intercept = 0,     # Diagonal line
              color = "black", linetype = "dashed") +
  coord_equal() +               # Equal scaling on both axes
  theme_minimal() +
  scale_x_continuous(limits = c(-3,lim)) +# Set the same range for x-axis
  scale_y_continuous(limits = c(-3,lim))+
  labs(x = "Baseline (Earning Sample)",
       y = "Ctrl. for Earning & Distance")
p

p_name <- paste0("mk4_13_diag")
file_name <- paste0(p_name,"_",dat, ".png")
sz <- c(5,5)

ggsave(filename = file_name, plot = p, path = output_dir,
       width = sz[1], height = sz[2], units = "in")




# ------------------------- Garbage ----------------------------------
{
choice_set_long <-  copy(rv_prf)
dat <- format(Sys.Date(), "%m%d")
output_dir <- paste0("04Output/Figure/", dat)
# Sample and Estimation

g = 4 # Input: g and cht_set_choice_long
data_used <- choice_set_long[,.N , by = .(y_id,Ch,EN,Math,Social,Nature,
                                          D1,D2,D3,D4,D5,
                                          g1,g2,g3,
                                          gender,loc,year
)]

set.seed(123)
source("00src/cluster_table.R")
clustered_dt <- first_step(data_used, g=4, col = c( "Ch","EN","Math","Social","Nature","D1","D2","D3","D4","D5"))

# Take a look
cluster_table_temp(clustered_dt , c(2,1,4,3), latex = FALSE, ks = 4) # 暫時取消製作header的功能


choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '2',
                               '2' = '1',
                               '3' = '4',
                               '4' = '3'))

# hm

# Define the values of t
t_values <- 1:4
p_name <- paste0(m_name[3],"_hm")

# Use purrr::walk() to apply the save_plot function to each value of t
purrr::walk(t_values, function(x)  save_plot(x, func = g_heatmap, 
                                             sz = c(10,8), res_lst = datalist_3$tab))

}