##############################################################
# CCS SOLs by Cohort - Document Creation                  
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-07-05
# Summary: A document that widens and cleans SOL data for CCS students,
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
# Call in SOLs                               
##############################################################

sols <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                     header = TRUE, top.left.cell = "A1", xl.sheet = "SOLs") %>% clean_names() %>%
  select(!student_test_date)

demographics <- xl.read.file("../../AddNames_CCS_StudentReports_20250714.xlsx", password = "CCP2025",
                             header = TRUE, top.left.cell = "A1", xl.sheet = "Demographics") %>% clean_names() %>%
  select(student_number,campminder_id,student_name,sched_yearof_graduation)

sols <- merge(x=demographics,y=sols,all.y=TRUE)

##############################################################
# Split out English and Math                             
##############################################################

sols_math <- sols %>% subset(test_subject=="Math")  %>% select(!test_subject)
sols_read <- sols %>% subset(test_subject=="English")  %>% select(!test_subject)

##############################################################
# Math SOLs Cleaning                             
##############################################################

sols_math <- pivot_wider(data = sols_math, 
                         names_from = test_school_year, 
                         values_from = c(test_scaled_score,grade,proficiency_group))

sols_math <- sols_math %>% select("student_number","campminder_id","student_name","sched_yearof_graduation", 
                                  "grade_2024-2025","test_scaled_score_2024-2025","proficiency_group_2024-2025",
                                  "grade_2023-2024","test_scaled_score_2023-2024","proficiency_group_2023-2024",
                                  "grade_2022-2023","test_scaled_score_2022-2023","proficiency_group_2022-2023",
                                  "grade_2020-2021","test_scaled_score_2020-2021","proficiency_group_2020-2021",
                                  "grade_2018-2019","test_scaled_score_2018-2019","proficiency_group_2018-2019",
                                  "grade_2017-2018","test_scaled_score_2017-2018","proficiency_group_2017-2018")

colnames(sols_math) <- c("student_number","campminder_id","student_name","grad_year", 
                         "grade_2024-25","score_2024-25","performance_2024-25",
                         "grade_2023-24","score_2023-24","performance_2023-24",
                         "grade_2022-23","score_2022-23","performance_2022-23",
                         "grade_2020-21","score_2020-21","performance_2020-21",
                         "grade_2018-19","score_2018-19","performance_2018-19",
                         "grade_2017-18","score_2017-18","performance_2017-18")

sols_math_cohort1 <- sols_math %>% subset(`grad_year`==2027)
sols_math_cohort1 <- sols_math_cohort1 %>% select(-c("grade_2024-25","score_2024-25","performance_2024-25"))

sols_math_cohort2 <- sols_math %>% subset(`grad_year`==2028)

sols_math_cohort3 <- sols_math %>% subset(`grad_year`==2029)
sols_math_cohort3 <- sols_math_cohort3 %>% select(-c("grade_2017-18","score_2017-18","performance_2017-18"))

sols_math_cohort4 <- sols_math %>% subset(`grad_year`==2030)
sols_math_cohort4 <- sols_math_cohort4 %>% select(-c("grade_2017-18","score_2017-18","performance_2017-18",
                                                     "grade_2018-19","score_2018-19","performance_2018-19"))

sols_math_cohort5 <- sols_math %>% subset(`grad_year`==2031)
sols_math_cohort5 <- sols_math_cohort5 %>% select(-c("grade_2017-18","score_2017-18","performance_2017-18",
                                                     "grade_2018-19","score_2018-19","performance_2018-19",
                                                     "grade_2020-21","score_2020-21","performance_2020-21"))


write_csv(sols_math_cohort1,"../CCS Math SOLs over Time, Cohort 1_2027.csv")
write_csv(sols_math_cohort2,"../CCS Math SOLs over Time, Cohort 2_2028.csv")
write_csv(sols_math_cohort3,"../CCS Math SOLs over Time, Cohort 3_2029.csv")
write_csv(sols_math_cohort4,"../CCS Math SOLs over Time, Cohort 4_2030.csv")
write_csv(sols_math_cohort5,"../CCS Math SOLs over Time, Cohort 5_2031.csv")
write_csv(sols_math,"../CCS Math SOLs over Time, All Cohorts.csv")

##############################################################
# Reading SOLs Cleaning                             
##############################################################

sols_read <- pivot_wider(data = sols_read, 
                         names_from = test_school_year, 
                         values_from = c(test_scaled_score,grade,proficiency_group))

sols_read <- sols_read %>% select("student_number","campminder_id","student_name","sched_yearof_graduation", 
                                  "grade_2024-2025","test_scaled_score_2024-2025","proficiency_group_2024-2025",
                                  "grade_2023-2024","test_scaled_score_2023-2024","proficiency_group_2023-2024",
                                  "grade_2022-2023","test_scaled_score_2022-2023","proficiency_group_2022-2023",
                                  "grade_2020-2021","test_scaled_score_2020-2021","proficiency_group_2020-2021",
                                  "grade_2018-2019","test_scaled_score_2018-2019","proficiency_group_2018-2019",
                                  "grade_2017-2018","test_scaled_score_2017-2018","proficiency_group_2017-2018")

colnames(sols_read) <- c("student_number","campminder_id","student_name","grad_year", 
                         "grade_2024-25","score_2024-25","performance_2024-25",
                         "grade_2023-24","score_2023-24","performance_2023-24",
                         "grade_2022-23","score_2022-23","performance_2022-23",
                         "grade_2020-21","score_2020-21","performance_2020-21",
                         "grade_2018-19","score_2018-19","performance_2018-19",
                         "grade_2017-18","score_2017-18","performance_2017-18")

sols_read_cohort1 <- sols_read %>% subset(`grad_year`==2027)
sols_read_cohort1 <- sols_read_cohort1 %>% select(-c("grade_2024-25","score_2024-25","performance_2024-25",
                                                     "grade_2023-24","score_2023-24","performance_2023-24"))

sols_read_cohort2 <- sols_read %>% subset(`grad_year`==2028)
sols_read_cohort2 <- sols_read_cohort2 %>% select(-c("grade_2024-25","score_2024-25","performance_2024-25"))

sols_read_cohort3 <- sols_read %>% subset(`grad_year`==2029)
sols_read_cohort3 <- sols_read_cohort3 %>% select(-c("grade_2017-18","score_2017-18","performance_2017-18"))

sols_read_cohort4 <- sols_read %>% subset(`grad_year`==2030)
sols_read_cohort4 <- sols_read_cohort4 %>% select(-c("grade_2018-19","score_2018-19","performance_2018-19",
                                                     "grade_2017-18","score_2017-18","performance_2017-18"))

sols_read_cohort5 <- sols_read %>% subset(`grad_year`==2031)
sols_read_cohort5 <- sols_read_cohort5 %>% select(-c("grade_2020-21","score_2020-21","performance_2020-21",
                                                     "grade_2018-19","score_2018-19","performance_2018-19",
                                                     "grade_2017-18","score_2017-18","performance_2017-18"))


write_csv(sols_read_cohort1,"../CCS Reading SOLs over Time, Cohort 1_2027.csv")
write_csv(sols_read_cohort2,"../CCS Reading SOLs over Time, Cohort 2_2028.csv")
write_csv(sols_read_cohort3,"../CCS Reading SOLs over Time, Cohort 3_2029.csv")
write_csv(sols_read_cohort4,"../CCS Reading SOLs over Time, Cohort 4_2030.csv")
write_csv(sols_read_cohort5,"../CCS Reading SOLs over Time, Cohort 5_2031.csv")
write_csv(sols_read,"../CCS Reading SOLs over Time, All Cohorts.csv")


