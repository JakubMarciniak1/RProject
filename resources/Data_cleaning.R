# data_location = file.path(".", "salaries.csv")
setwd("C:\\Moje rzeczy\\Uczelnia UAM\\matematyka zaoczna\\semestr 3\\Przetwarzanie i wizualizacja danych\\projekty R\\Pati i ja\\RProject")

data_location <- ".\\data set\\salaries.csv"


data <- read.csv(data_location)

library(dplyr)



job_titles_groups <- c("Data Science and AI/ML", "Analytics or BI", "Big Data and Cloud", "Data Management and Operations", "Data Quality and Architectural", "Visualization, Management, and Specialized")
# regexps done by chatGPT, i trust him more then I trust myself when it comes to regular exp
regex_group1 <- "Data\\sScience|AI|Machine\\sLearning"
regex_group2 <- "Analytics|BI"
regex_group3 <- "Big\\sData|Cloud"
regex_group4 <- "Data\\s(?:Analyst|Analytics|Management|Operations|Manager)"
regex_group5 <- "Data\\s(?:Quality|Architect|Developer|Engineer|Lead|Modeler|Modeller|DevOps)"



clean_data <- data %>%
  # omitting information about salary in different currencies, I don't think it is needed for our analysis
  select(-c("salary", "salary_currency")) %>%
  # renaming to salary 'cause specifying currency is not needed anymore
  rename(salary = salary_in_usd) %>%
  # omitting rows with nulls to avoid errors in the future
  na.omit() %>%
  rename(year = work_year) %>%
  # factoring for better understanding
  mutate(experience_level = factor(experience_level,
    levels = c("EN", "MI", "SE", "EX"),
    ordered = TRUE,
    labels = c("Junior", "Intermediate", "Expert", "Director")
  )) %>%
  mutate(employment_type = factor(employment_type,
    levels = c("PT", "FT", "CT", "FL"),
    labels = c("Part-time", "Full-time", "Contract", "Freelance")
  )) %>%
  mutate(remote_ratio = factor(remote_ratio,
    levels = c(0, 50, 100),
    ordered = TRUE,
    labels = c("In-Office", "Hybrid", "Remote")
  )) %>%
  rename(work_type = remote_ratio) %>%
  mutate(company_size = factor(company_size,
    levels = c("S", "M", "L"),
    ordered = TRUE,
    labels = c("<50", "50 - 250", ">250")
  )) %>%
  # grouping job_titles for clarity
  # should we group data using group_by()?
  mutate(job_group = case_when(
    # group 1
    grepl(
      regex_group1,
      job_title
    ) ~ job_titles_groups[1],
    # group 2
    grepl(
      regex_group2,
      job_title
    ) ~ job_titles_groups[2],
    # group 3
    grepl(
      regex_group3,
      job_title
    ) ~ job_titles_groups[3],
    # group 4
    grepl(
      regex_group4,
      job_title
    ) ~ job_titles_groups[4],
    # group 5
    grepl(
      regex_group5,
      job_title
    ) ~ job_titles_groups[5],
    # group 6
    .default = job_titles_groups[6]
  )) %>%
  # now i guess job_title can be used as factor?
  # mutate(job_title = factor(job_title,
  #                           levels = job_titles_groups)
  #        ) %>%
  # changing job_title name to field
  # rename(field = job_title) %>%
  # ordering by year, then salary?
  arrange(year, salary)
