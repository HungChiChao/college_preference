# Header =====================================================
# Title:        
# Description:   
# Date:          
# Author:        Chi-Chao Hung
# Version:       v2.0

# Change Log:
#

# Inputs:        
# Outputs:       
# Replication:   [Instructions for reproducing the results, e.g., "Run scripts in the following order..."]
# Notes:         [Additional notes about assumptions, data sources, or ethical considerations]
# =====================================================

source("00src/visualization.R")
dat <- format(Sys.Date(), "%m%d")
output_dir <- "04Output/Figure/"

library(data.table)
library(tidyverse)
library(ggrepel)
library(maxLik)




#  Working Area 0110 ......................................................


m_name <- c("mk4_all")
m_name <- c("mk4-1","mk4-2","mk4-3","mk4-4","mk4-5","mk4-6","mk4-7","mk4-8")
m_name_cat <- c("mcatk4-1","mcatk4-2","mcatk4-3")
m_name_gen <- c("mk4_male","mk4_female")

datalist_0 <- readRDS()

datalist_1 <- readRDS("04Output/Result2/mCk4-1_0108.rds")
  
datalist_cat1 <- readRDS("04Output/Result2/mCk4-1_0108.rds")

datalist_cat3 <- readRDS("04Output/Result2/mcatk4-3_0108.rds")

datalist_m <- mk4_male_0211
datalist_f <- mk4_female_0211

# 之前的result會帶有_
ref <- c("UFJU", "041Business", "輔仁大學企業管理學系","category") 
datalist_cat3$tab <- res_lst( datalist_cat3, ref, k =4 , cov_lst = c("earning", "dist"))
  



# ----- 1. Baseline --------------------------------

save_plot <- function(t, func , sz, res_lst = result) {
  # Create the plot for the current value of t
  
  p <- func(res_lst, t = t)
  
  # Define the filename based on the current value of t
  file_name <- paste0(p_name,"_type", t,"_",dat, ".png")
  
  # Save the plot with customized size
  ggsave(filename = file_name, plot = p, path = output_dir,
         width = sz[1], height = sz[2], units = "in")
}



## ---- 2.1 Bar plot ------------------------------------
bar_plot( datalist_0$tab, t = 3) 


# Define the values of t
t_values <- 1:4
p_name <- paste0(m_name[3],"_bar")

# Use purrr::walk() to apply the save_plot function to each value of t
purrr::walk(t_values, function(x)  save_plot(x, func = bar_plot, 
                                      sz = c(8,10), res_lst = datalist_3$tab))


## --- 2.2 Heatmap -----------------------------------------
g_heatmap( datalist_f$tab, t = 1)

# Define the values of t
t_values <- 1:4
p_name <- paste0(m_name_gen[2],"_hm")

# Use purrr::walk() to apply the save_plot function to each value of t
purrr::walk(t_values, function(x)  save_plot(x, func = g_heatmap, 
                                sz = c(10,8), res_lst = datalist_f$tab))


## --- 2.3 Diagonal ----------------------------------------

save_plot_diag <- function( t_pair, sz, res_lst = result, MoS, ref, cov_num){
  # Create the plot for the current value of t
  p <- diag_plot( res_lst, t_pair, MoS , ref, cov_num)
  # Define the filename based on the current value of t
  file_name <- paste0(p_name,"_type", paste0(t_pair, collapse="") ,"_",dat, ".png")
  # Save the plot with customized size
  ggsave(filename = file_name, plot = p, path = output_dir,
         width = sz[1], height = sz[2], units = "in")
  print("OK")
}

t_pair <- list(c("1","2"),c("1","3"), c("1","4"))
p_name <- paste0(m_name[1],"_fld_diag_")
sz <- c(5,5)

purrr::walk(1:3 , function(x)  save_plot_diag( t_pair[[x]], MoS= "M", 
                ref= ref, sz = sz, res_lst = datalist_0, cov_num = 0))

# --- 
test <- Type_tab(datalist_1$RES,datalist_1$Par[-1], datalist_1$tab$f_effect, ref)














# --- 2. Gender ---------------------------------------------

m_name <- c("mk4_mf")
#list_data_m <- read_rds("04Output/Result2/Model4_0809.rds")
#list_data_f <- read_rds("04Output/Result2/Model5_0809.rds")
#result_m <- res_lst(list_data_m$RES ,list_data_m  , d = 0)
#result_f <- res_lst(list_data_f$RES ,list_data_f  , d = 0)

gender_coef <- merge(datalist_m$tab$coef_tab, datalist_f$tab$coef_tab, 
              by = "parameter", all = TRUE)

gender_coef[, type := tstrsplit(parameter, "K")[[2]] ]

#gender_coef <- gender_coef %>%
#  mutate(Type = recode(type,
#                       "1" = "Social_L",
#                       "2" = "Natural_L",
#                       "3" = "Natural_H", "4" = "Social_H"))

## --- 8.1 Diaganal  Plot -----------------
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

# ---- 9. Explanatory ==================================

list_data_1 <- read_rds("04Output/Result2/mk4-1_0212.rds")
list_data_3 <- read_rds("04Output/Result2/mk4-3_0212.rds")

#result_1 <- res_lst(list_data_1$RES ,list_data  , d = 0, ref_sch, ref_maj) # 請把g改掉或加入參數
#result_3 <- res_lst(list_data_3$RES ,list_data  , d = 2, ref_sch, ref_maj) # 請把g改掉或加入參數

exp_coef <- merge(`mk4-1_0212`$tab$coef_tab, datalist_3$tab$coef_tab[-c(1:2),], by = "parameter", all = TRUE)
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



# --- 10. Regional ------------------------------------------

## Plot

# List of data frames to be merged
#coef_list <- list(Model1$coef_tab, Model2$coef_tab, Model3$coef_tab)
# Use Reduce to merge multiple data frames by "parameter"

coef_list <- list(mk4_urban_0212$tab$coef_tab, mk4_rural_0212$tab$coef_tab)
reg_coef <- Reduce(function(x, y) merge(x, y, by = "parameter", all = TRUE), coef_list)

#column name

#colnames(reg_coef) <- c("parameter","Effect.N","SE.N","Effect.C","SE.C","Effect.S","SE.S")
colnames(reg_coef) <- c("parameter","Effect.U","SE.U","Effect.R","SE.R")

#
reg_coef[, type := tstrsplit(parameter, "K")[[2]] ]
reg_coef <- reg_coef %>%
  mutate(Type = recode(type,
                       "1" = "Type I",
                       "2" = "Type II",
                       "3" = "Type III", "4" = "Type IV"))

lim <- 1
p_13 <- ggplot( reg_coef[type == "1" &! grepl("U", parameter) ], aes(x = Effect.U, y = Effect.R))+
  geom_point(size = 1.5) +      # Scatter plot
  geom_abline(slope = 1, intercept = 0,     # Diagonal line
              color = "black", linetype = "dashed") +
  coord_equal() +               # Equal scaling on both axes
  theme_minimal() +
  scale_x_continuous(limits = c(-1,lim)) +# Set the same range for x-axis
  scale_y_continuous(limits = c(-1,lim))+
  labs(x = "Urban",
       y = "Rural")+
  scale_shape_manual(values = c(1,2, 15, 3))

# Only major

ggplot( reg_coef[1:40], aes(x = Effect.N, y = Effect.C, shape= Type, label = parameter))+
  geom_point(size = 1.5) +      # Scatter plot
  geom_abline(slope = 1, intercept = 0,     # Diagonal line
              color = "black", linetype = "dashed") +
  coord_equal() +               # Equal scaling on both axes
  theme_minimal() +
  geom_text(vjust=-1, hjust= 0, size= 1)+
  scale_x_continuous(limits = c(-1,2)) +# Set the same range for x-axis
  scale_y_continuous(limits = c(-1,2))+
  labs(x = "North",
       y = "Central")+
  scale_shape_manual(values = c(1,2, 15, 3))

## --- 10.2 Save------------

p_name <- paste0("mk4_UR_",c("ins","major"),"_diag")
file_name <- paste0(p_name,"_",dat, ".png")
sz <- c(5,5)

ggsave(filename = file_name[1], plot = p_12, path = output_dir,
       width = sz[1], height = sz[2], units = "in")

ggsave(filename = file_name[2], plot = p_13, path = output_dir,
       width = sz[1], height = sz[2], units = "in")

# --------------------------

p_name <- paste0(c("M89","M810","M910"),"_reg_diag")
file_name <- paste0(p_name,"_",dat, ".png")
sz <- c(5,5)

ggsave(filename = file_name[1], plot = p_12, path = output_dir,
       width = sz[1], height = sz[2], units = "in")

ggsave(filename = file_name[2], plot = p_13, path = output_dir,
       width = sz[1], height = sz[2], units = "in")

ggsave(filename = file_name[3], plot = p_23, path = output_dir,
       width = sz[1], height = sz[2], units = "in")






