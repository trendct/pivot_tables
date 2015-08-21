payroll <- read.csv("CT-Payroll-FY2014.csv", stringsAsFactors=FALSE)

# How many employees by department
# What's the average by department?
# What's the median by department?

# Let's make one big data frame out of that

# Quick look at departments, number of employees
depts <- table(payroll$AGENCY_DESCRIPTION)

# Quick sort to see which are the departments with the most employees
head(depts[order(-depts)], 10)

mean(payroll$TOTAL_AMT)
median(payroll$TOTAL_AMT)

# Ok, but which Department on average got paid the most?

depts_avg <- tapply(payroll$TOTAL_AMT, payroll$AGENCY_DESCRIPTION, mean)

# Quick
head(depts_avg[order(-depts_avg)], 10)

# But what about median? 
depts_median <- tapply(payroll$TOTAL_AMT, payroll$AGENCY_DESCRIPTION, median)
head(depts_median[order(-depts_median)], 10)

# Let's do some advanced stuff. Isolate the first name from the NAME field

payroll$first_name <- gsub(".*\\,", "", payroll$NAME)
payroll$first_name <- gsub(" .*", "", payroll$first_name)
library(stringr)
payroll$first_name <- str_to_title(payroll$first_name)

# Name guessing
# install.packages("gender")
#library(gender)

names <- payroll$first_name
names <- subset(names, !duplicated(names))

# This will take a long time to process
payroll_gender <- gender(names)

# So we have a list, but we need to turn it into a dataframe
payroll_gender <- do.call(rbind, lapply(payroll_gender, data.frame, stringsAsFactors=FALSE))


payroll_gender <- payroll_gender[c("name", "gender")]
colnames(payroll_gender) <- c("first_name", "gender")

payroll_gender <- subset(payroll_gender, !duplicated(payroll_gender$first_name))

library(dplyr)
payroll <- left_join(payroll, payroll_gender)
payroll$gender[is.na(payroll$gender)] <- "unknown"

table(payroll$gender)

# Whoa, there's a lot unknown. What's up with that?
# Oh, it's because of Students. Let's exclude them.

payroll <- subset(payroll, first_name!="Student")


table(payroll$gender)

# Much better!
# How many women work in state government versus men?

# Women 
gender <- data.frame(table(payroll$gender))
colnames(gender) <- c("gender", "count")
# Calculations!
gender$percent <- round((gender$count/sum(gender$count))*100, digits=2)


avg_pay <- tapply(payroll$TOTAL_AMT, payroll$gender, mean)
avg_pay <- data.frame(avg_pay)

gender<- cbind(gender, avg_pay)

median_pay <- tapply(payroll$TOTAL_AMT, payroll$gender, median)
median_pay <- data.frame(median_pay)

gender<- cbind(gender, median_pay)

# write.csv(gender, "gender.csv")

# How about department from department? 
# Same as above, but add another variable
gender_dept <- data.frame(table(payroll$AGENCY_DESCRIPTION, payroll$gender))
colnames(gender_dept) <- c("department", "gender", "count")

# bring in tidyr library
library(tidyr)
gender_dept <- spread(gender_dept, gender, count)

# Ok, average pay by gender in each department? 

# First have to subset by gender
fem <- subset(payroll, gender=="female")
male <- subset(payroll, gender=="male")
unknown <- subset(payroll, gender=="unknown")

# Female
fem_dept <- data.frame(tapply(fem$TOTAL_AMT, fem$AGENCY_DESCRIPTION, mean))
fem_dept$department <- rownames(fem_dept)
colnames(fem_dept) <- c("avg.female.pay", "department")
gender_dept <- merge(gender_dept, fem_dept, all=TRUE)

# Male
male_dept <- data.frame(tapply(male$TOTAL_AMT, male$AGENCY_DESCRIPTION, mean))
male_dept$department <- rownames(male_dept)
colnames(male_dept) <- c("avg.male.pay", "department")
gender_dept <- merge(gender_dept, male_dept, all=TRUE)

# diff
gender_dept$avg.diff <- gender_dept$avg.male.pay-gender_dept$avg.female.pay

head(gender_dept$avg.diff[order(-gender_dept$avg.diff)], 5)

head(gender_dept$avg.diff[order(gender_dept$avg.diff)], 5)

# Well, let's consider Median instead of Average

# Female
fem_dept <- data.frame(tapply(fem$TOTAL_AMT, fem$AGENCY_DESCRIPTION, median))
fem_dept$department <- rownames(fem_dept)
colnames(fem_dept) <- c("median.female.pay", "department")
gender_dept <- merge(gender_dept, fem_dept, all=TRUE)

# Male
male_dept <- data.frame(tapply(male$TOTAL_AMT, male$AGENCY_DESCRIPTION, median))
male_dept$department <- rownames(male_dept)
colnames(male_dept) <- c("median.male.pay", "department")
gender_dept <- merge(gender_dept, male_dept, all=TRUE)

# diff
gender_dept$median.diff <- gender_dept$median.male.pay-gender_dept$median.female.pay

head(gender_dept$median.diff[order(-gender_dept$median.diff)], 5)
head(gender_dept$median.diff[order(gender_dept$median.diff)], 5)

# Write to csv
# write.csv(gender_dept, "gender_dept.csv")

Pay by gender and department Connecticut
# Now for something completely quicker using pipes and dplyr

# As a whole
gender_df <- payroll %>%
  group_by(gender) %>%
  summarize(employees=n(), avg.salary=mean(TOTAL_AMT), median.salary=median(TOTAL_AMT)) %>%
  mutate(percent=round((employees/sum(employees)*100))) %>%
  arrange(desc(percent))

# How about with departments? add one word in and you're set
gender_df2 <- payroll %>%
  group_by(AGENCY_DESCRIPTION, gender) %>%
  summarize(employees=n(), avg.salary=mean(TOTAL_AMT), median.salary=median(TOTAL_AMT)) %>%
  mutate(percent=round((employees/sum(employees)*100))) %>%
  arrange(desc(percent))
