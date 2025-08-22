##############################################################
# CCS Attendance by Cohort - Document Creation                  
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-07-05
# Summary: A document that widens and cleans attendance data for CCS students,
#          separating by cohort
##############################################################

##############################################################
# Library Intros and Excel Imports                                
##############################################################

library(tidyverse)
library(dplyr)
library(excel.link)
library(writexl)
library(labelled)
library(janitor)

##############################################################
# Call in Attendance                               
##############################################################

attendance <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                           header = TRUE, top.left.cell = "A1", xl.sheet = "ADA") %>% clean_names() %>% 
  select(student_number,school_year,ada)

demographics <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                             header = TRUE, top.left.cell = "A1", xl.sheet = "Demographics") %>% clean_names() %>%
  select(student_number,campminder_id,student_name,sched_yearof_graduation)

##############################################################
# Modify ADA from Decimals to Percentages                       
##############################################################

attendance <- attendance %>% mutate(ada = round(ada*100,2))

##############################################################
# Widen Dataset                             
##############################################################

attendance <- pivot_wider(data = attendance, 
                          names_from = school_year, 
                          values_from = ada)

##############################################################
# Merge Dataset with Demographic Information and Reorganize Columns                            
##############################################################

attendance <- merge(x=demographics,y=attendance,all.y=TRUE)

colnames(attendance) <- c("school_id","campminder_id","student_name","grad_year",
                          "ada 2022-23","ada 2023-24","ada 2024-25")

attendance <- attendance %>% select(school_id,campminder_id,student_name,grad_year,
                                    `ada 2024-25`,`ada 2023-24`,`ada 2022-23`)

##############################################################
# Save Csvs by Cohort                            
##############################################################

attendance_cohort1 <- attendance %>% subset(`grad_year`==2027)
attendance_cohort2 <- attendance %>% subset(`grad_year`==2028)
attendance_cohort3 <- attendance %>% subset(`grad_year`==2029)
attendance_cohort4 <- attendance %>% subset(`grad_year`==2030)
attendance_cohort5 <- attendance %>% subset(`grad_year`==2031)

write_csv(attendance_cohort1,"../CCS Attendance over Time, Cohort 1_2027.csv")
write_csv(attendance_cohort2,"../CCS Attendance over Time, Cohort 2_2028.csv")
write_csv(attendance_cohort3,"../CCS Attendance over Time, Cohort 3_2029.csv")
write_csv(attendance_cohort4,"../CCS Attendance over Time, Cohort 4_2030.csv")
write_csv(attendance_cohort5,"../CCS Attendance over Time, Cohort 5_2031.csv")
write_csv(attendance,"../CCS Attendance over Time, All Cohorts.csv")


