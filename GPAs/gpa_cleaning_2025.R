##############################################################
# CCS GPA by Cohort - Document Creation                  
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-07-05
# Summary: A document that widens and cleans GPA data for CCS students,
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
# Call in GPA                               
##############################################################

gpa <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                    header = TRUE, top.left.cell = "A1", xl.sheet = "Grades") %>% clean_names() %>% select(!grade_count)

demographics <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                             header = TRUE, top.left.cell = "A1", xl.sheet = "Demographics") %>% clean_names() %>%
  select(student_number,campminder_id,student_name,sched_yearof_graduation)

gpa <- merge(x=demographics,y=gpa,all.y=TRUE)

##############################################################
# Pivot Wider                         
##############################################################

gpa <- pivot_wider(data = gpa, 
                   names_from = school_year, 
                   values_from = c(grade_level,unweighted_gpa))

colnames(gpa) <- c("school_id","campminder_id","student_name","grad_year","grade_2023-24",   
                   "grade_2024-25","gpa_2023-24","gpa_2024-25")

gpa <- gpa %>% select("school_id","campminder_id","student_name","grad_year","grade_2024-25","gpa_2024-25",
                      "grade_2023-24","gpa_2023-24")

##############################################################
# Cohort Split                   
##############################################################

gpa_cohort1 <- gpa %>% subset(`grad_year`==2027)
gpa_cohort2 <- gpa %>% subset(`grad_year`==2028)
gpa_cohort3 <- gpa %>% subset(`grad_year`==2029)
gpa_cohort3 <- gpa_cohort3 %>% select(-c("grade_2023-24","gpa_2023-24"))

##############################################################
# CSVs                 
##############################################################

write_csv(gpa_cohort1,"../CCS GPAs Time, Cohort 1_2027.csv")
write_csv(gpa_cohort2,"../CCS GPAs over Time, Cohort 2_2028.csv")
write_csv(gpa_cohort3,"../CCS GPAs over Time, Cohort 3_2029.csv")


