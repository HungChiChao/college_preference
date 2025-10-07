# =========
# Title:  Raw Data


# ==========

# --- 1. Initialize Environment ----------
# Load required libraries
library(knitr)
library(data.table)
library(ggplot2)
library(tidyverse)
require(kableExtra)
# Set random seed for reproducibility
set.seed(1234)

# --- 2. Set Working Directory -----
# Specify file paths for data, output, and script storage
setwd("/Users/hungchichao/Documents/GitHub/College_Department_Choice")

# --- 3. Load Data -------
# Load raw data sets
# Name the data.tables `X+year`
for (i in c(104:109)){
  assign(paste0("X", i), fread(paste0("01Data/00Raw_Data/",i, ".csv")))
}

# ------ 4. Data Cleaning and Preparation ----------


# Exercise 2: 
# Turn into long format
#  Our Goal here is to construct a sample that does not remove any sample.
#  The variables names each year is slightly different and the following code would 
#  combine them into a single large data set
# Variables Required
# Gender: 104-107 Gender_full_name, 108-109 Gender
# Location: 
# Status_i : The status of each applicants: 正 備 no
# Final Decision

# Initialize a list to store processed data for each year
yearly_data_list <- list()

# 104 - 107
for (year in 104:107) {
  
  # Copy the dataset for the given year
  year_data <- copy(get(paste0("X", year)))
  
  # Reshape the dataset from wide to long format
  long_format_data <- melt(
    year_data,
    id.vars = c(
      "registration_num", "id",
      "name_1", "name_2", "name_3",
      "Gender_full_name", "exam_area", "location",
      "school_0", "department_0", "status_0","rank_0"
    ),
    measure = patterns("^school_", "^department_", "^status_", "^rank_"),
    variable.name = "order",
    value.name = c("school", "department", "status", "rank")
  )
  
  # Order the data by applicant ID
  long_format_data <- setorder(long_format_data, id)
  
  # Add a column indicating the year of the data
  long_format_data <- long_format_data[school != "", ]
  
  # Remove duplicated Programs
  # Note: 因為...
  long_format_data <- long_format_data[ !( (school_0 == school) &
                                             (department_0 == department) &
                                             (order != 1) ) , ]
  long_format_data[, Year := year]
  
  # Store the processed dataset in the list
  yearly_data_list[[year - 103]] <- long_format_data
}

# 108 
for (year in 108:108) {
  
  # Copy the dataset for the given year
  year_data <- copy(get(paste0("X", year)))
  
  # Reshape the dataset from wide to long format
  long_format_data <- melt(
    year_data,
    id.vars = c(
      "Sid", "id",
      "name_1", "name_2", "name_3",
      "Gender", "exam_area",
      "school_0", "department_0", "status_0","rank_0"
    ),
    measure = patterns("^school_", "^department_", "^status_", "^rank_"),
    variable.name = "order",
    value.name = c("school", "department", "status", "rank")
  )
  
  # Order the data by applicant ID
  long_format_data <- setorder(long_format_data, id)
  
  # Add a column indicating the year of the data
  long_format_data <- long_format_data[school != "", ]
  
  # Remove duplicated Programs
  # Note: 因為...
  long_format_data <- long_format_data[ !( (school_0 == school) &
                                             (department_0 == department) &
                                             (order != 1) ) , ]
  long_format_data[, Year := year]
  
  long_format_data[, registration_num := Sid]
  long_format_data[, Sid := NULL]
  long_format_data[, Gender_full_name := Gender]
  
  # Store the processed dataset in the list
  yearly_data_list[[year - 103]] <- long_format_data
}

# 109
for (year in 109:109) {
  
  # Copy the dataset for the given year
  year_data <- copy(get(paste0("X", year)))
  
  # Reshape the dataset from wide to long format
  long_format_data <- melt(
    year_data,
    id.vars = c(
      "registration_num", "id",
      "name_1", "name_2", "name_3",
      "Gender", "exam_area", "location",
      "school_0", "department_0", "status_0","rank_0"
    ),
    measure = patterns("^school_", "^department_", "^status_", "^rank_"),
    variable.name = "order",
    value.name = c("school", "department", "status", "rank")
  )
  
  # Order the data by applicant ID
  long_format_data <- setorder(long_format_data, id)
  
  # Add a column indicating the year of the data
  long_format_data <- long_format_data[school != "", ]
  
  # Remove duplicated Programs
  # Note: 因為...
  long_format_data <- long_format_data[ !( (school_0 == school) &
                                             (department_0 == department) &
                                             (order != 1) ) , ]
  long_format_data[, Year := year]
  long_format_data[, Gender_full_name := Gender]
  
  # Store the processed dataset in the list
  yearly_data_list[[year - 103]] <- long_format_data
}

# Combine all yearly datasets into a single dataset
combined_data <- rbindlist(yearly_data_list, fill = TRUE) # fill = TRUE fills missing columns with NAs. By default FALSE. When TRUE, use.names is set to TRUE.

# Clean up by removing intermediate variables to free memory
rm(yearly_data_list, long_format_data)

# Sample selection 1 (Mild)
# Remove non-academic oriented college
gen_college <- unique(combined_data$school_0)
combined_data <- combined_data[ school %in% gen_college]

# FIXME: Delete column "gender_full_name"


## --- 4.1 Extra Procedure -----

rm(w_dt)

# FIXME: 108 has no location
w_dt <- rbind( X104[,.(exam_area,location)],X105[,.(exam_area,location)],
               X106[,.(exam_area,location)],X107[,.(exam_area,location)],
               X109[,.(exam_area,location)] )

# test_area to location
location_table <- w_dt[,.N,by=.(exam_area,location)][order(-N)]

#
location_table <- location_table %>% 
  rename(mapped_location = location)
# !!!!!
location_table <- location_table[! duplicated(exam_area)]


# FIXME: there are still warnings on this

# Match
combined_data <-  combined_data %>%
  left_join( location_table[,.(exam_area, mapped_location)] , by = "exam_area") %>%
  mutate( location = ifelse( Year == 108, mapped_location, location)) %>%
  select(-mapped_location)

# unique exam_area in X108
combined_data[exam_area == "臺北市立内湖高中", location := "台北"]
rm(location_table)

#
enrolled_dt <- combined_data[ school_0 != "" & department_0 != ""]


# ------------------- 5. Supplementary Data Set  --------------------------

enr_all <- copy(enrolled_dt)
rm(enrolled_dt)

univ_eng <- fread("01data/uni_engcode.csv")
wage_tab <- fread("01Data/schoolWage.csv") 
univ_loc <- fread("01Data/univ_loc.csv")
exam_loc <- fread("01Data/exam_loc.csv")
cdata <- fread("01Data/clustering_data250107.csv")

# year id
enr_all[, y_id := sprintf("%05d",id)]
enr_all[, y_id := as.numeric( paste0(as.character(Year), y_id))]


# -------------------- 6.  Create Long Data ----------------------------------

summary(enr_all)

# 改名、改column
enr_all[, choice := paste0(school_0 , department_0)]
enr_all[, Option := paste0(school, department)]
enr_all[, number := order]


enr_all <- enr_all[, .(
  y_id , ids = id, Gender = Gender_full_name, exam_area, year = Year, 
  choice, number, Option, location, status_0, status, rank_0, rank
)]

setorder(enr_all , y_id)

# --- 7 Add additional Variables -------------

ref_tab <- fread("01Data/complete_reference_table_250219.csv") # 科系分類資料

# School, Field
enr_all[ref_tab, on = c(choice = "V1"), ':='(school_chinese = i.school, field = i.Eng_Field)]
# School, Field
enr_all[ref_tab, on = c(Option = "V1"), ':='(alt_school = i.school, alt_field = i.Eng_Field)]

enr_all[ref_tab, on = c( choice = "V1"), category := i.Eng_Category]
enr_all[ref_tab, on = c(choice = "V1"), school := i.U_school ]

# FIXME Here's a check for ref-tab (Do not run)
{
  sd_left <- base::union(unique(enr_all[is.na(school)]$choice), 
                         unique(enr_all[is.na(alt_school)]$Option))
  left1 <- sd_left[! grepl("旋坤揚帆|向日葵|璞玉|晨光|飛鳶|薪火|興翼|旭日|成星|政星|西灣南星|嘉星|揚鷹|展翅|翔飛|新芽|希望"
                           ,sd_left)]
  left2 <- left1[! grepl("國立新竹教育大學", left1)]
  left3 <- left2[! grepl("音樂", left2)]
}


## ---5.0 wage ----


# Wage table names
wage_tab[,SD := paste0(schl_name,col_name)]
wage_tab[ref_tab, on = "SD", `:=`(BF = i.fld, school = i.school, V1 = i.V1) ]

# Wage Table wide to long
wage_long <- melt(wage_tab[! is.na(V1)], id.vars = "V1", measure.vars = patterns("^wage"), value.name = "wage" )
wage_long[,year := as.numeric(substring(variable, 5,7))]

# Wage of Final Choice
enr_all[ wage_long, on = c(year = "year", choice = "V1"), `:=` (c_wage = i.wage)]
# Wage of alternatives
enr_all[ wage_long, on = c(year = "year", Option= "V1"), `:=` (alt_wage = i.wage)]

rm(wage_long, wage_tab)
## --- 5.1 Distance ------------

# Paste Program coordinates
enr_all[univ_loc, on = c(alt_school = "school"), `:=`(UX = i.X, UY = i.Y)]
enr_all[univ_loc, on = c(school_chinese = "school"), `:=`(cUX = i.X, cUY = i.Y)]

# Paste Exam Area coordinates
enr_all[exam_loc, on = "exam_area", `:=`(SX = i.X, SY = i.Y)]

## --- 5.3  Region ----

# exclude na location
enr_all[exam_loc, on = "exam_area", `:=`(City = i.City, Town = i.Town)]

# Define Region
enr_all<- enr_all %>%
  mutate(region = case_when(
    City %in% c("臺北市", "新北市", "基隆市", "桃園市", "新竹市","新竹縣") ~ "North",
    City %in% c("苗栗縣", "臺中市", "彰化縣", "南投縣", "雲林縣") ~ "Central",
    City %in% c("嘉義市","嘉義縣", "臺南市", "高雄市", "屏東縣","澎湖縣") ~ "South",
    City %in% c("宜蘭縣", "花蓮縣", "台東縣") ~ "East",
    TRUE ~ NA_character_ # Default if no match
  ))

## --- 5.4 Add Cluster -------------

enr_all[cdata, on = "y_id", `:=`(
  Ch = i.Ch, EN = i.EN , Math = i.Math, Social = i.Social, Nature = i.Nature) ]

enr_all[cdata, on = "y_id", `:=`(
  D1 = i.D1, D2 = i.D2 , D3 = i.D3, D4 = i.D4, D5 = i.D5) ]

enr_all[cdata, on = "y_id", `:=`(
  g1 = i.g1, g2 = i.g2, g3 = i.g3) ]


## --- 5.5 2nd stage info ----------

# Personal Info
enr_all[, s_stage := .N, by = y_id]
enr_all[, sch_num := uniqueN(alt_school), by = y_id]
enr_all[, fld_num := uniqueN(alt_field), by = y_id]
enr_all[, ad_num  := sum(status %in% c("正","正取")), by = y_id]
enr_all[, wait_num := sum(status %in% c("備","備取")), by = y_id]


Pythagorean <- function(a1,a2,b1,b2){
  round(sqrt((a1-b1)**2 + (a2-b2)**2))}

enr_all[, dist := Pythagorean(UX, UY, SX, SY)/100000]

# ----------------- 8. Reveal Preference  ----------------------------


# Waiting list
for (i in c(104:109)){
  assign(paste0("B", i), get(paste0("X", i))[ status_0 %in% c("備","備取")][, max(rank_0), 
      by = .(school_0, department_0)][, 
      .(Option = paste0(school_0, department_0), R =V1)][, year := i] )}

# Long format
B <- rbindlist( list(B104, B105, B106, B107, B108, B109) )
rm(X104,X105,X106,X107,X108,X109)

# Condition 1: Check if the status is admitted
cond_1 <- enr_all[, status %in% c("正","正取")] # a logical Vector
cond_1[is.na(cond_1)] <- FALSE # if the original value is NA, all the logical operation would be NA. Replace them with False.

# Match with the maximum rank of waitlisted applicants for the same program
m_dt <- enr_all[, .(Option, rank, year)]
m_dt <- m_dt[B, on= c("Option","year"), R := i.R] # match from `B+year`

# Condition 2: Check if waitlisted and rank is within the acceptable range
cond_2 = enr_all[, status  %in% c("備","備取")]  &  m_dt[, rank < R ] # waiting & (rank < max rank)
cond_2[is.na(cond_2)] <- FALSE

# Exclude the admitted school
rv_prf <- enr_all[ cond_1 | cond_2 | number == 1]

# Exclude observation that has only one choice
cht_count <- rv_prf[,.N, by = y_id]
rv_prf[ cht_count, on ="y_id", Set_size := i.N]
rv_prf <- rv_prf[ Set_size  != 1, ]

rm(B104,B105,B106,B107,B108,B109)

#------------------------ 9. Precluster data ---------

rv_prf[, loc := location ]
rv_prf[, gender := Gender ]

# make sure every obs has corresponding characteristics
rv_prf <- rv_prf[field != "" & alt_school != "", ]
rv_prf <- rv_prf[ ! is.na(Ch)] # GSAT Score

dat <- format(Sys.Date(), "%m%d")


dat <- format(Sys.Date(), "%m%d")
fwrite(rv_prf, paste0("01Data/pre_cluster_sample",dat,".csv"))







# ---------------------- 10. Samples ---------

## ------- 10.1 Cluster K = 4 ------

# Below is for Summary stats
choice_set_long <- copy(rv_prf)

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

latex_code <- cluster_table_temp(clustered_dt , c(2,1,4,3), latex = TRUE, ks = 4)
filename <- paste0("04Output/Tables/0221/cluster_tableK4_before",Sys.Date() ,".tex")
writeLines(latex_code, con = filename)

choice_set_long[clustered_dt, on = "y_id", cluster := i.cluster_id]
choice_set_long <- choice_set_long %>% 
  mutate( cluster_id = recode( as.character(cluster),
                               '1' = '2',
                               '2' = '1',
                               '3' = '4',
                               '4' = '3'))


source("00src/new_verify.R")
## -- 10.2 Main Sample ----
ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field")
estsam_1 <- verify_temp( choice_set_long, ref)  #subset


## --- 10.3  Earning Sample  ----
estsam_2 <- copy(choice_set_long)
# With Wage and Distance
estsam_2  <- estsam_2 [ ! is.na(c_wage) & ! is.na(alt_wage) 
                        & ! is.na(UX) & !is.na(SX)] # Wage and distance
estsam_2 <- verify_temp( estsam_2, ref)











# --- 11. Summary Tab -------

sample0 <- enr_all[, .(mdist = mean(dist, na.rm = TRUE),
                       we = sum( ! is.na(c_wage) & ! is.na(alt_wage) ) > 0
), 
by = .(y_id, year, ids, Gender, exam_area, location, school, Ch, 
       choice, c_wage, s_stage, sch_num, fld_num, ad_num, wait_num ) ]


sample1 <- estsam_1[, .( N = .N, 
                         mdist = mean(dist, na.rm = TRUE),
                         we = sum( ! is.na(c_wage) & ! is.na(alt_wage) ) > 0
), 
by = .(y_id, year, ids, Gender, exam_area, location, school, Ch, 
       choice, c_wage, s_stage, sch_num, fld_num, ad_num, wait_num ) ]


sample2 <- estsam_2[, .( N= .N, 
                         mdist = mean(dist, na.rm = TRUE),
                         we = sum( ! is.na(c_wage) & ! is.na(alt_wage) ) > 0
), 
by = .(y_id, year, ids, Gender, exam_area, location, school, Ch,
       choice, c_wage, s_stage, sch_num, fld_num, ad_num, wait_num ) ]


# Reveal preference choice set for raw data!!!!
sample0[ sample1, on = "y_id", N := i.N]
sample0[ is.na(N), N:= 0]

## --- 11.1 Cretae_Table --------

sch_lst <- unique(enr_all[,.(school, school_chinese)])
public <- sch_lst[ grepl(".*(國立|市立|國防).*", sch_lst$school_chinese) ]$school

summary_table <- function(dt) {
  city <- c("台北","台中","台南","高雄","桃園","新北")
  
  # FIXME not pure function
  sch_lst <- unique(enr_all[,.(school, school_chinese)])
  public <- sch_lst[ grepl(".*(國立|市立|國防).*", sch_lst$school_chinese) ]$school
  
  total_obs <- dt[, .N]  # 總樣本數
  city_obs <- dt[location %in% city, .N]  # 目標城市的樣本數
  total_sd <- dt[, uniqueN(choice)]
  
  dt[, Gender := factor(Gender, levels = c("Female", "Male", "Neutral", ""))]
  
  dt1 <- data.table(
    Var = c("Female","Male","Neutral","Unidentified",
            "2nd Stage","Institution","Broad Field","Accepted","Waitlist",
            "Size","distance",
            "City (%)", "GSAT(%)", "with earnings(%)","Public(%)","Obs"),
    Value = c(
      dt[, round((.N/total_obs)*100, 2) , by = Gender][order(Gender)]$V1, # Gender
      round( mean(dt$s_stage), 2),
      round( mean(dt$sch_num), 2),
      round( mean(dt$fld_num), 2),
      round( mean(dt$ad_num), 2),
      round( mean(dt$wait_num), 2),
      round(dt[, .(mean_count = mean(N))]$mean_count, 2),   #Set Size
      round( mean(dt$mdist, na.rm = TRUE), 2),
      round((city_obs / total_obs) * 100, 2),              # 計算百分比
      round( (dt[ ! is.na(Ch), .N ]/total_obs)*100 , 2),
      round( mean(dt$we)*100, 2),
      round( mean(dt$school %in% public)*100, 2),
      as.character(total_obs)   # Obs.  
    )
  )
}

# 計算兩個 data.table 的 summary
row2.1 <- summary_table(sample0)
row2.2 <- summary_table(sample1)
row2.3 <- summary_table(sample2)

# 合併表格
row2 <- merge(row2.1, row2.2, by = "Var", sort = FALSE)
row2 <- merge(row2, row2.3, by = "Var", sort = FALSE)

latex = TRUE
# 輸出
kb <- kbl(row2, format = if(latex == T){"latex"}, 
          caption = "Summary Statistics", 
          booktabs = T) %>% 
  kable_styling() %>%
  #add_header_above(c(" "= 1, "Sample I" = 2, "Sample II" = 2 ))%>%
  pack_rows("Gender (%)", 1, 4, italic = T) %>%
  pack_rows("Personal Application", 5, 9, italic = T) %>%
  footnote(number = c("Footnote 1; "),
           general = "This is generated by kableExtra",
           threeparttable = T)
kb

filename <- paste0("04Output/Tables/0221/Sum_stats",format(Sys.Date(), "%m%d") ,".tex")
writeLines(kb, con = filename)


## ----- 11.2 ASV Summary ------------------------------

all_prog <- enr_all[, .( Size = uniqueN(y_id) ), by = .( choice, c_wage, school, field, year )] 
# program size (From the 2nd stage)
prog_stat <- all_prog[ , .(mw = mean(c_wage), ms = mean(Size)), 
                       by = .(choice, school, field) ]
prog_stat[ ref_tab, on = c( choice= "V1"), eng_field := i.Eng_Field]


# Sample 0, 1,2
dt0 <- copy(prog_stat)
dt2 <- prog_stat[choice %in% unique(estsam_1$choice)]
dt3 <- prog_stat[choice %in% unique(estsam_2$choice)]

summary_table_3 <- function(dt){
  # Track
  temp <- dt[, round(.N/nrow(dt)*100 ), by = eng_field]
  all_size <- sum(dt$ms)
  
  df <- data.table(
    Var = c("Progrms","Institutions","Mean Size","Median Size",
            "Public","Public Size",temp$eng_field),
    
    c(as.character(nrow(dt)),
      dt[, uniqueN(school)],
      round(mean(dt$ms),2),
      round(median(dt$ms)),
      sum(dt$school %in% public),
      round(dt[school %in% public, sum(ms)/all_size ]*100,2),
      temp$V1
    )
  )
  return(df)
}

# 計算兩個 data.table 的 summary
row1 <- summary_table_3 (dt0)
row2 <- summary_table_3 (dt2)
row3 <- summary_table_3 (dt3)

# 合併表格
row12 <- merge(row1, row2, by = "Var", sort = FALSE)
row_sum <- merge(row12, row3, by = "Var", sort = FALSE)

latex = TRUE
# 輸出
kb <- kbl(row_sum, format = if(latex == T){"latex"}, 
          caption = "Summary Statistics", 
          booktabs = T) %>% 
  kable_styling() %>%
  #add_header_above(c(" "= 1, "Sample I" = 2, "Sample II" = 2 ))%>%
  footnote(number = c("Footnote 1; "),
           general = "This is generated by kableExtra",
           threeparttable = T)
kb

filename <- paste0("04Output/Tables/0221/Sum_alt_stats",format(Sys.Date(), "%m%d") ,".tex")
writeLines(kb, con = filename)

## ------------------11.3 Cluster After------------------------------------


# Function: cluster_table(dt, new_order, latex = TRUE, ks = k)
cluster_table_temp(clustered_dt[y_id %in% sample1$y_id] , c(2,1,4,3), latex = FALSE, ks = 4) # 暫時取消製作header的功能

latex_code <- cluster_table_temp(clustered_dt[y_id %in% sample1$y_id] , c(2,1,4,3), latex = TRUE, ks = 4)
filename <- paste0("04Output/Tables/0221/cluster_tableK4_after",Sys.Date() ,".tex")
writeLines(latex_code, con = filename)


# ---- 12  Base Line Model ---------------------------------

source("00src/DChoice_matrices.R")
source("00src/UHetero_MLE.r")
source("00src/MLE_Cov.r")
source("00src/visualization.R")


m_name <- c("mk4_all","mk4_male","mk4_female","mk4_urban","mk4_rural")

# Model 1 (cov_num=0, gen = 0, reg =0)

ref <- c("UFJU", "04Business_and_law", "輔仁大學企業管理學系","field") 

subsmp0 <- verify_combine( choice_set_long, ref) 
datalist_0 <- Pre_estimation(cov_num = 0, subsmp0$final_dt, subsmp0$p_list, ref , least = 0)

datalist_0$RES <- MLE_GLM_wrap(datalist_0) #Optional: summary(m1_result)
datalist_0$tab <- res_lst( datalist_0, ref, k=4 )

saveRDS(datalist_0, paste0("04Output/Result2/",m_name[1],"_",dat,".rds"))


# Baseline
make_table_new(datalist_0$tab, k = 4, latex = FALSE)

com_tab_0 <- make_table_new(datalist_0$tab, k = 4, latex = TRUE)
filename <- paste0("04Output/Tables/0221/coef_list_mk4_all",dat ,".tex")
writeLines(com_tab_0, con = filename)

