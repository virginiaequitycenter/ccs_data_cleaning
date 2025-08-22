##############################################################
# CCS Demographics by Cohort - Document Creation                  
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
# Call in Demographics                               
##############################################################

ccs_demo <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                         header = TRUE, top.left.cell = "A1", xl.sheet = "Demographics") %>% clean_names()

shp_demo <- xl.read.file("ccs_shp_demographics_July2025.xlsx",header = TRUE, top.left.cell = "A1") %>% clean_names()



colnames(ccs_demo) <- c("school_id","campminder_id","student_name","school","grad_year","lep_status",
                        "avid_status","sped_status","lunch_status","race_ethnicity","gender")

ccs_demo <- ccs_demo %>% mutate(gender=ifelse(gender=="F","Female",gender))
ccs_demo <- ccs_demo %>% mutate(gender=ifelse(gender=="M","Male",gender))



colnames(shp_demo) <- c("school_id","student_first_name","student_last_name","enrollment_yr","district","school","grade_2526","el","grad_year",
                        "gender","race","language","parent_ed","house_count","income","data_sharing_consent")

##############################################################
# Correct Grad Years for Students with Mismatch                              
##############################################################

# Removed for student privacy 

demo <- merge(x=ccs_demo,y=shp_demo,all.y=TRUE,all.x=TRUE)

##############################################################
# Add Missing Names                              
##############################################################

# Removed for student privacy

##############################################################
# Cut Duplicate Columns                           
##############################################################

demo <- demo %>% select(school_id,campminder_id,student_name,grad_year,enrollment_yr,school,grade_2526,gender,race,
                        lep_status,avid_status,sped_status,lunch_status,language,parent_ed,house_count,income,
                        data_sharing_consent)

##############################################################
# Edit Lunch Status and First Gen Status                           
##############################################################

demo <- demo %>% mutate(lunch_status=ifelse(lunch_status=="FDC","Free",lunch_status))
demo <- demo %>% mutate(lunch_status=ifelse(lunch_status=="RDC","Reduced",lunch_status))
demo <- demo %>% mutate(lunch_status=ifelse(lunch_status=="P","Paid",lunch_status))

demo <- demo %>% add_column(first_gen = "")
demo <- demo %>% mutate(first_gen = ifelse(parent_ed=="High School Diploma/GED"|
                                             parent_ed=="2-year Associate Degree (AA, AS, etc.)"|
                                             parent_ed=="Some college, no degree"|
                                             parent_ed=="Some High School","First Generation Student",first_gen))
demo <- demo %>% mutate(first_gen = ifelse(parent_ed=="Bachelor's Degree (B.A., B.S., etc.)"|
                                             parent_ed=="Master's Degree (M.A., M.S., M.Ed., etc.)"|
                                             parent_ed=="Professional Degree (M.D., J.D., Ph.D., Ed.D., etc.)","Not First Gen",first_gen))

demo <- demo %>% select(school_id,campminder_id,student_name,grad_year,enrollment_yr,school,grade_2526,gender,race,
                        lep_status,avid_status,sped_status,lunch_status,language,parent_ed,first_gen,house_count,income,
                        data_sharing_consent)

##############################################################
# Save to CSV            
##############################################################

demo_cohort1 <- demo %>% subset(`grad_year`==2027)
demo_cohort2 <- demo %>% subset(`grad_year`==2028)
demo_cohort3 <- demo %>% subset(`grad_year`==2029)
demo_cohort4 <- demo %>% subset(`grad_year`==2030)
demo_cohort5 <- demo %>% subset(`grad_year`==2031)

write_csv(demo_cohort1,"../CCS Demographics, Cohort 1_2027.csv")
write_csv(demo_cohort2,"../CCS Demographics, Cohort 2_2028.csv")
write_csv(demo_cohort3,"../CCS Demographics, Cohort 3_2029.csv")
write_csv(demo_cohort4,"../CCS Demographics, Cohort 4_2030.csv")
write_csv(demo_cohort5,"../CCS Demographics, Cohort 5_2031.csv")
write_csv(demo,"../CCS Demographics, All Cohorts.csv")

