# ------------------------------------------------------------------------------
# Title: Kigawa-Blinder-Oaxaca Decomposition - Netherlands
# Purpose: Main code for the decomposition analysis
# Authors: Animesh Gadre
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Part 1: Loading survey data for work-schooling & background variables and merging them: years 2019 and 2023
# ------------------------------------------------------------------------------

# Loading necessary packages
library(haven)
library(tidyverse)
library(fastDummies)
library(oaxaca)
library(gridExtra)

# Reading the survey data for 2019
df1 <- read_dta("blinder_oaxaca_python/cw19l_EN_3.0p/cw19l_EN_3.0p.dta")
df2 <- read_dta("blinder_oaxaca_python/avars_201904_EN_1.0p/avars_201904_EN_1.0p.dta")

names(df1)[names(df1) == "cw19l_m"] <- "wave"
df19 <- merge(df1, df2, by=c("nomem_encr","wave"))

# Reading the survey data for 2023
data1 <- read_dta("blinder_oaxaca_python/cw23p_EN_1.0p/cw23p_EN_1.0p.dta")
data2 <- read_dta("blinder_oaxaca_python/avars_202304_EN_1.0p/avars_202304_EN_1.0p.dta")

names(data1)[names(data1) == "cw23p_m"] <- "wave"
df23 <- merge(data1, data2, by=c("nomem_encr","wave"))

# ------------------------------------------------------------------------------
# Part 2: Cleaning data to make it regression ready
# ------------------------------------------------------------------------------

# Renaming variables for convenience
df19 <- df19 %>%
  rename(education = oplzon,
         industry = cw19l402,
         type_of_org = cw19l122,
         commute = cw19l136,
         weekly_hours = cw19l127,
         occupation = cw19l404,
         type_of_contract = cw19l121,
         marital_status = burgstat,
         household_member = nomem_encr,
         gender = geslacht,
         age = leeftijd,
         monthly_income = brutoink_f,
         children = aantalki,
         net_income = nettoink_f,
         degree_of_urbanisation = sted,
         domestic_situation = woonvorm)

df23 <- df23 %>%
  rename(education = oplzon,
         industry = cw23p402,
         type_of_org = cw23p122,
         commute = cw23p136,
         weekly_hours = cw23p127,
         occupation = cw23p404,
         type_of_contract = cw23p121,
         marital_status = burgstat,
         household_member = nomem_encr,
         gender_i = geslacht,
         age = leeftijd,
         monthly_income = brutoink_f,
         children = aantalki,
         net_income = nettoink_f,
         degree_of_urbanisation = sted,
         domestic_situation = woonvorm)

# Subsetting the data for convenience
df19 = subset(df19, select = c(household_member, education, degree_of_urbanisation, 
                               industry, type_of_org, commute, weekly_hours, 
                               occupation, type_of_contract, marital_status, 
                               gender, age, monthly_income, children, 
                               net_income, domestic_situation))
df19 <- filter(df19, net_income > 0 & weekly_hours > 0)
df19 <- df19 %>% drop_na()

df23 = subset(df23, select = c(household_member, education, degree_of_urbanisation, 
                               industry, type_of_org, commute, weekly_hours, 
                               occupation, type_of_contract, marital_status, 
                               gender_i, age, monthly_income, children, 
                               net_income, domestic_situation))
df23 <- filter(df23, net_income > 0 & weekly_hours > 0)
df23 <- subset(df23, gender_i %in% c(1, 2))
df23 <- df23 %>% drop_na()

# A crude measure to capture work experience by converting educational level into years
df19 <- df19 %>%
   mutate(education_years = case_when(
     education == 1 ~ 12,
     education == 2 ~ 16,
     education == 3 ~ 17,
     education == 4 ~ 20,
     education == 5 ~ 21,
     education == 6 ~ 21,
     education %in% c(7, 8, 9) ~ 0,
     TRUE ~ NA_real_  
     ))
df19 <- df19 %>%
   mutate(potential_experience = age - education_years)

df23 <- df23 %>%
  mutate(education_years = case_when(
    education == 1 ~ 12,
    education == 2 ~ 16,
    education == 3 ~ 17,
    education == 4 ~ 20,
    education == 5 ~ 21,
    education == 6 ~ 21,
    education %in% c(7, 8, 9) ~ 0,
    TRUE ~ NA_real_  
  ))
df23 <- df23 %>%
  mutate(potential_experience = age - education_years)

# Creating a measure for logarithm of hourly wage
df19 <- df19 %>%
  mutate(daily_hours = weekly_hours/5,
         annual_income = net_income*12,
         daily_wage = annual_income/254,
         hourly_wage = daily_wage/daily_hours,
         log_wage = log(hourly_wage))

df23 <- df23 %>%
  mutate(daily_hours = weekly_hours/5,
         annual_income = net_income*12,
         daily_wage = annual_income/253,
         hourly_wage = daily_wage/daily_hours,
         log_wage = log(hourly_wage))

# Encoding the categorical variables and removing the baseline category
df19 <- dummy_cols(df19, select_columns = c("education", "type_of_contract", "industry", "occupation", "marital_status", 
                                            "degree_of_urbanisation", "type_of_org", "gender"), remove_first_dummy = TRUE)

df23 <- dummy_cols(df23, select_columns = c("education", "type_of_contract", "industry", "occupation", "marital_status", 
                                            "degree_of_urbanisation", "type_of_org", "gender_i"), remove_first_dummy = TRUE)

# Creating measures for full time work, civil status and urban region
df19 <- df19 %>%
  mutate(full_time = ifelse(weekly_hours > 32, 1, 0),
         civil_status = ifelse(domestic_situation %in% c(2, 3), 1, 0),
         urban = ifelse(degree_of_urbanisation %in% c(1, 2, 3), 1, 0))

df23 <- df23 %>%
  mutate(full_time = ifelse(weekly_hours > 32, 1, 0),
         civil_status = ifelse(domestic_situation %in% c(2, 3), 1, 0),
         urban = ifelse(degree_of_urbanisation %in% c(1, 2, 3), 1, 0))

# ------------------------------------------------------------------------------
# Part 3: Making some plots
# ------------------------------------------------------------------------------

# Graph stuff
gender_plot <- factor(df19$gender, labels=c("men", "women"))
df19 %>%
  mutate(gender_plot <- factor(gender, labels = c("men","women")))

gender_i_plot <- factor(df23$gender_i, labels=c("men", "women"))
df23 %>%
  mutate(gender_i_plot <- factor(gender_i, labels = c("men","women")))

# Plotting the distribution of wages and work experience
wd19 <- ggplot(df19, aes(x=log_wage, colour=gender_plot, fill=gender_plot)) + geom_density(alpha=0.3) + labs(x = "Log Hourly Wage", y = "Density", title = "Distribution of Wage by Gender: 2019")

wd23 <- ggplot(df23, aes(x=log_wage, colour=gender_i_plot, fill=gender_i_plot)) + geom_density(alpha=0.3) + labs(x = "Log Hourly Wage", y = "Density", title = "Distribution of Wage by Gender: 2023")

ed19 <- ggplot(df19, aes(x=potential_experience, colour=gender_plot, fill=gender_plot)) + geom_density(alpha=0.3) + labs(x = "Work Experience", y = "Density", title = "Distribution of Work Experience by Gender: 2019")

ed23 <- ggplot(df23, aes(x=potential_experience, colour=gender_i_plot, fill=gender_i_plot)) + geom_density(alpha=0.3) + labs(x = "Work Experience", y = "Density", title = "Distribution of Work Experience by Gender: 2023")


# ------------------------------------------------------------------------------
# Part 4: Main decomposition analysis
# ------------------------------------------------------------------------------

# We now have a regression ready dataset. The first model takes contract type as a category.
results1c <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + type_of_contract_2 + type_of_contract_3 + type_of_contract_4  | gender_2 | type_of_contract_2 + type_of_contract_3 + type_of_contract_4, data = df19)

results2c <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + type_of_contract_2 + type_of_contract_3 + type_of_contract_4 | gender_i_2 | type_of_contract_2 + type_of_contract_3 + type_of_contract_4, data = df23)

# Decomposition with occupation as a category
results1o <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + occupation_2 + occupation_3 + occupation_4 + occupation_5 + occupation_6 + occupation_7 + occupation_8 + occupation_9 | gender_2 | occupation_2 + occupation_3 + occupation_4 + occupation_5 + occupation_6 + occupation_7 + occupation_8 + occupation_9, data = df19)

results2o <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + occupation_2 + occupation_3 + occupation_4 + occupation_5 + occupation_6 + occupation_7 + occupation_8 + occupation_9 | gender_i_2 | occupation_2 + occupation_3 + occupation_4 + occupation_5 + occupation_6 + occupation_7 + occupation_8 + occupation_9, data = df23)

# Decomposition with type of organisation as a category
results1p <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + type_of_org_2  | gender_2 | type_of_org_2 , data = df19)

results2p <- oaxaca(formula = log_wage ~ potential_experience + children + civil_status + full_time + urban + commute + type_of_org_2  | gender_i_2 | type_of_org_2 , data = df23)


# Let's take a look at the explained and unexplained components
results1c$y
results1c$twofold$overall
results1o$twofold$overall
results1p$twofold$overall

results2c$y
results2c$twofold$overall
results2o$twofold$overall
results2p$twofold$overall

# We can analyse the specific variables of the model

variablesc <- c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "type_of_contract_2", "type_of_contract_3", "type_of_contract_4")

variableso <- c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "occupation_2", "occupation_3", "occupation_4", "occupation_5", "occupation_6", "occupation_7", "occupation_8", "occupation_9")

variablesp <- c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "type_of_org_2")

columns <- c("group.weight", "coef(explained)", "coef(unexplained A)", "coef(unexplained B)")

#Type of contract
results1c$twofold$variables[[5]][variablesc, columns]
results2c$twofold$variables[[5]][variablesc, columns]

#Occupation
results1o$twofold$variables[[5]][variableso, columns]
results2o$twofold$variables[[5]][variableso, columns]

#Type of organisation
results1p$twofold$variables[[5]][variablesp, columns]
results2p$twofold$variables[[5]][variablesp, columns]

#Plotting the results
plot(results1c, decomposition = "twofold", group.weight = -1)
plot(results2c, decomposition = "twofold", group.weight = -1)

plot(results1o, decomposition = "twofold", group.weight = -1)
plot(results2o, decomposition = "twofold", group.weight = -1)

plot(results1p, decomposition = "twofold", group.weight = -1)
plot(results2p, decomposition = "twofold", group.weight = -1)


plot(results1o, decomposition = "twofold", group.weight = -1, unexplained.split = TRUE, components = c("explained"), component.labels = c("explained" = "Explained Component: 2019"), variables = c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "occupation_2", "occupation_3", "occupation_4", "occupation_5", "occupation_6", "occupation_7", "occupation_8", "occupation_9"), variable.labels = c("potential_experience" = "Work Experience", "children" = "Child", "civil_status" = "Civil Status", "full_time" = "Full Time Work", "urban" = "Urban Region", "commute" = "Commute Time", "occupation_2" = "Higher Supervisory Profession", "occupation_3" = "Intermediate Academic or Independent Profession", "occupation_4" = "Intermediate Supervisory or Commercial Profession", "occupation_5" = " Other Mental Work", "occupation_6" = "killed and Supervisory Manual Work,", "occupation_7" = "Semi-Skilled Manual Work", "occupation_8" = "Unskilled and Trained Manual Work", "occupation_9" = "Agrarian Profession"))
plot(results2o, decomposition = "twofold", group.weight = -1, unexplained.split = TRUE, components = c("explained"), component.labels = c("explained" = "Explained Component: 2023"), variables = c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "occupation_2", "occupation_3", "occupation_4", "occupation_5", "occupation_6", "occupation_7", "occupation_8", "occupation_9"), variable.labels = c("potential_experience" = "Work Experience", "children" = "Child", "civil_status" = "Civil Status", "full_time" = "Full Time Work", "urban" = "Urban Region", "commute" = "Commute Time", "occupation_2" = "Higher Supervisory Profession", "occupation_3" = "Intermediate Academic or Independent Profession", "occupation_4" = "Intermediate Supervisory or Commercial Profession", "occupation_5" = " Other Mental Work", "occupation_6" = "killed and Supervisory Manual Work,", "occupation_7" = "Semi-Skilled Manual Work", "occupation_8" = "Unskilled and Trained Manual Work", "occupation_9" = "Agrarian Profession"))



plot(results1o, decomposition = "twofold", group.weight = -1, unexplained.split = TRUE, components = c("unexplained A", "unexplained B"), component.labels = c("unexplained A" = "In Favour of Men: 2019", "unexplained B" = "Against Women: 2019"), variables = c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "occupation_2", "occupation_3", "occupation_4", "occupation_5", "occupation_6", "occupation_7", "occupation_8", "occupation_9"), variable.labels = c("potential_experience" = "Work Experience", "children" = "Child", "civil_status" = "Civil Status", "full_time" = "Full Time Work", "urban" = "Urban Region", "commute" = "Commute Time", "occupation_2" = "Higher Supervisory Profession", "occupation_3" = "Intermediate Academic or Independent Profession", "occupation_4" = "Intermediate Supervisory or Commercial Profession", "occupation_5" = " Other Mental Work", "occupation_6" = "killed and Supervisory Manual Work,", "occupation_7" = "Semi-Skilled Manual Work", "occupation_8" = "Unskilled and Trained Manual Work", "occupation_9" = "Agrarian Profession"))
plot(results2o, decomposition = "twofold", group.weight = -1, unexplained.split = TRUE, components = c("unexplained A", "unexplained B"), component.labels = c("unexplained A" = "In Favour of Men: 2019", "unexplained B" = "Against Women: 2019"), variables = c("potential_experience", "children", "civil_status", "full_time", "urban", "commute", "occupation_2", "occupation_3", "occupation_4", "occupation_5", "occupation_6", "occupation_7", "occupation_8", "occupation_9"), variable.labels = c("potential_experience" = "Work Experience", "children" = "Child", "civil_status" = "Civil Status", "full_time" = "Full Time Work", "urban" = "Urban Region", "commute" = "Commute Time", "occupation_2" = "Higher Supervisory Profession", "occupation_3" = "Intermediate Academic or Independent Profession", "occupation_4" = "Intermediate Supervisory or Commercial Profession", "occupation_5" = " Other Mental Work", "occupation_6" = "killed and Supervisory Manual Work,", "occupation_7" = "Semi-Skilled Manual Work", "occupation_8" = "Unskilled and Trained Manual Work", "occupation_9" = "Agrarian Profession"))

