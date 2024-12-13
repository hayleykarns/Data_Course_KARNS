#BIOL3100
#Exam 3
#Date: 11/4/24


#Prep: loading libraries, set working directory, read csv, and named as object.


library(ggplot2)
library(tidyr)
library(dplyr)



setwd('C:/Users/User/Desktop/Data_Course_KARNS/BIOL3100_Exams/Exam_3')

data<- read.csv("C:/Users/User/Desktop/Data_Course_KARNS/BIOL3100_Exams/Exam_3/FacultySalaries_1995.csv")
view(data)

faculty_data <- data %>%
  select(-FedID, -State, -NumFullProfs, -NumAssocProfs, -NumAssistProfs, -NumInstructors, -NumFacultyAll)



salary_data <- faculty_data %>%
  pivot_longer(cols = c("AvgFullProfSalary", "AvgAssocProfSalary", "AvgAssistProfSalary"),
               names_to = "Rank",
               values_to = "Salary")

salary_data$Rank <- factor(salary_data$Rank, 
                           levels = c("AvgAssistProfSalary", "AvgAssocProfSalary", "AvgFullProfSalary"),
                           labels = c("Assist", "Assoc", "Full"))
view(salary_data)


ggplot(salary_data, aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  facet_wrap(~ Tier) +
  scale_fill_manual(values = c("pink", "green", "blue")) +
  labs(x = "Rank", y = "Salary") +
  theme_minimal() +
  theme(legend.title = element_blank())
