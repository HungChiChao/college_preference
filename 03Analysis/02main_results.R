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

# ------------------------ 5. Main Result ---------------------------------

m_name <- c("mk4_all","mk4_male","mk4_female","mk4_urban","mk4_rural")
c_dt <- copy(choice_set_long)

## ---------------------- 5.1 Baseline -------------------------------------

# Model 1 (cov_num=0, gen = 0, reg =0)

ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 

subsmp0 <- verify_combine( choice_set_long, ref) 
datalist_0 <- Pre_estimation(cov_num = 0, subsmp0$final_dt, subsmp0$p_list, ref , least = 0)

datalist_0$RES <- MLE_GLM_wrap(datalist_0) #Optional: summary(m1_result)
datalist_0$tab <- res_lst( datalist_0, ref, k=4 )

out_dir <- paste0("04Output/Figure/Main/All")

# Figures
cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
basic_figures( m_name[1], g = 4, dtlst = datalist_0, out_dir, cn_lst)

# Tables
com_tab_0.0 <- make_table_new(datalist_0$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_", m_name[1] ,".tex")
writeLines(com_tab_0.0, con = filename)

com_tab_0.1 <- complete_tab(datalist_0$tab, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_mk4_all_complete.tex")
writeLines(com_tab_0.1, con = filename)


## ---------------------- 5.2 Gender-----------------------------------------
#Model 4 (cov_num=0, gen = 0, reg =0)
subsmp <- verify_combine( c_dt[Gender == "Male"] ,ref)
datalist_m <- Pre_estimation(cov_num = 0, subsmp$final_dt, subsmp$p_list, ref)

datalist_m$RES <- MLE_GLM_wrap(datalist_m) #Optional: summary(m1_result)
datalist_m$tab <- res_lst( datalist_m, ref, k=4 )

out_dir <- paste0("04Output/Figure/Main/Male")
cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
basic_figures( m_name[2], g = 4, dtlst = datalist_m, out_dir, cn_lst)

# Tables
com_tab_m <- make_table_new(datalist_m$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_", m_name[2] ,".tex")
writeLines(com_tab_m, con = filename)



#Model 5 (cov_num=0, gen = 0, reg =0)
subsmp <- verify_combine( c_dt[Gender == "Female"], ref)
datalist_f <- Pre_estimation(cov_num = 0, subsmp$final_dt, subsmp$p_list, ref)

datalist_f$RES <- MLE_GLM_wrap(datalist_f) #Optional: summary(m1_result)
datalist_f$tab <- res_lst( datalist_f, ref, k=4 )

out_dir <- paste0("04Output/Figure/Main/Female")
cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
basic_figures( m_name[3], g = 4, dtlst = datalist_f, out_dir, cn_lst)

# Tables
com_tab_f <- make_table_new(datalist_f$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_", m_name[3] ,".tex")
writeLines(com_tab_f, con = filename)

## ---------------------- 5.3 Location-----------------------------------------

#Model 6
city <-  c("台北","台中","台南","高雄","桃園","新北")
c_dt6 <- c_dt[ location %in% city, ]

subsmp <- verify_combine(c_dt6, ref)
datalist_6 <- Pre_estimation(0, subsmp$final_dt, subsmp$p_list, ref)

datalist_6$RES  <- MLE_GLM_wrap(datalist_6) #summary(m4_result)
datalist_6$tab  <- res_lst( datalist_6, ref, k=4, cov_lst = c() )

#Model 7
c_dt7 <- c_dt[! location %in% city, ]

subsmp <- verify_combine(c_dt7, ref)
datalist_7 <- Pre_estimation(0, subsmp$final_dt, subsmp$p_list, ref)

datalist_7$RES  <- MLE_GLM_wrap(datalist_7) #summary(m4_result)
datalist_7$tab  <- res_lst( datalist_7, ref, k=4, cov_lst = c() )

## -------------------- 5.4 Maj = Category ----------------------

ref <- c("UFJU", "041Business", "輔仁大學企業管理學系","category") 
m_name<- c("mcatk4_all")

# For all g =4, sample  = conditional on Wage and Dist
c_dt <- copy(choice_set_long)
c_dt[ category == "999Field_unknown", category:= "999Others"]

# Model 1 (cov_num=0, gen = 0, reg =0)
subsmp <- verify_combine( c_dt, ref)
datalist_cat1 <- Pre_estimation(cov_num = 0, subsmp$final_dt, subsmp$p_list, ref)

datalist_cat1$RES <- MLE_GLM_wrap(datalist_cat1) #Optional: summary(m1_result)
datalist_cat1$tab <- res_lst( datalist_cat1, ref, k =4 )

# Figures
out_dir <- paste0("04Output/Figure/Main/27Fields")
cn_lst <- c("All-Rounders", "STEM-H", "STEM-L", "Humanities")
basic_figures( m_name[1], g = 4, dtlst = datalist_cat1, out_dir, cn_lst)

# Tables
com_tab <- make_table_new(datalist_cat1$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_", m_name[1] ,".tex")
writeLines(com_tab, con = filename)

com_tab <- complete_tab(datalist_cat1$tab, latex = TRUE)
filename <- paste0("04Output/Tables/Main/coef_list_", m_name[1] ,"_complete.tex")
writeLines(com_tab, con = filename)


# ---------------------- 6 Joint Table -----------------------------------------
# Sub-Sample Baseline Table

sub_lst <- list(datalist_m$tab, datalist_f$tab,
                datalist_6$tab, datalist_7$tab) 

make_table_type1(sub_lst, n = 4, latex = FALSE)

com_tab_subset <- make_table_type1(sub_lst, n = 4, latex = TRUE)
filename <- paste0("04Output/Tables/",dat,"/coef_list_mk4_subsample",dat ,".tex")
writeLines(com_tab_subset, con = filename)




# ---- Gargabe ------

# --- 8.1 Diaganal  Plot -----------------



source("00src/visualization.R")

m_name <- c("mk4_mf")
gender_coef <- merge(datalist_m$tab$coef_tab, datalist_f$tab$coef_tab, 
                     by = "parameter", all = TRUE)
gender_coef[, type := tstrsplit(parameter, "K")[[2]] ]



output_dir <- paste0("04Output/Figure/", dat)


lim <- 7
p <- ggplot( gender_coef[type ==1], aes(x = Effect.x, y = Effect.y))+
  geom_point(size = 1.5) +      # Scatter plot
  geom_abline(slope = 1, intercept = 0,     # Diagonal line
              color = "black", linetype = "dashed") +
  coord_equal() +               # Equal scaling on both axes
  theme_minimal() +
  scale_x_continuous(limits = c(-3,lim)) +# Set the same range for x-axis
  scale_y_continuous(limits = c(-3,lim))+
  labs(x = "Male",
       y = "Female")

p_name <- paste0(m_name,"_gen_diag")
file_name <- paste0(p_name,"_",dat, ".png")
sz <- c(5,5)

ggsave(filename = file_name, plot = p, path = output_dir,
       width = sz[1], height = sz[2], units = "in")




# Define the values of t
t_values <- 1:4
p_name <- paste0(m_name,"_hm")

# Use purrr::walk() to apply the save_plot function to each value of t
purrr::walk(t_values, function(x)  save_plot(x, func = g_heatmap, 
                                             sz = c(10,8), res_lst = datalist_cat1$tab))
