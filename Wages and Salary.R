library(httr)
library(tidyverse)
library(glue)

# insert API key

# My actual API key is private

api_key <- "MY API KEY"

# API url

api_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

#payload body

payload <- glue('{
  "seriesid":["CXU900000LB1203M", "CXU900000LB1206M", "CXU900000LB1207M", 
  "CXU900000LB1208M", "CXU900000LB1204M", "CXU900000LB1205M", "CUUR0000SA0"],
  "startyear":"2003",
  "endyear":"2023",
  "registrationkey": "{{api_key}}"
}', .open = "{{", .close = "}}")

# CXU900000LB1203M: Salaries across all occupations
# CXU900000LB1206M: Salaries for service workers
# CXU900000LB1207M: Salaries for construction workers and mechanics
# CXU900000LB1208M: Salaries for fabricators and laborers
# CXU900000LB1204M: Salaries for managers and professionals
# CXU900000LB1205M: Salaries for tech, sales, and clerical workers
# CUUR0000SA0: Consumer price index

# Post request

response <- POST(api_url,
                 body = payload,
                 content_type("application/json"),
                 encode = "json")

# convert JSON to list

x <- content(response, "text") %>% 
  jsonlite::fromJSON()

# create tibble

salaries_test <- x$Results$series$data[[3]] %>%  as_tibble()
salaries_test

# Combine all tibbles into one

names <- c("overall_salary", "service_salary", "construction_salary", "laborer_salary", 
           "manager_salary", "tech_salary")
names[4]

salary_data <- x$Results$series$data[[1]] %>%  
  as_tibble() %>% 
  select(year, value) %>% 
  rename(overall_salary = value)

for (i in 1:6) {
  salary_data <- salary_data %>%
    mutate(!!names[i] := x$Results$series$data[[i]]$value)
}

# Group inflation data by year

CPI <- type_convert(x$Results$series$data[[7]]) %>%  
  as_tibble() %>%
  group_by(year) %>% 
  summarize(annual_CPI = sum(value))

# Move inflation data into main tibble

salary_data <- type_convert(salary_data)

CPI

salary_data <- salary_data %>% 
  inner_join(CPI)

# Create names of adjusted for inflation columns

names_adj = names <- c("overall_salary_adj", "service_salary_adj", "construction_salary_adj", "laborer_salary_adj", 
                       "manager_salary_adj", "tech_salary_adj")

CPI_2003 <- salary_data %>% 
  filter(year == 2003) %>% 
  select(annual_CPI)

CPI_2003 <- as.double(CPI_2003)

# Calculate inflation adjustment ratios

salary_data <- salary_data %>% 
  mutate(ratio = CPI_2003/annual_CPI)

salary_data$ratio

# Calculate adjusted for inflation salaries

for (i in 1:6) {
  salary_data <- salary_data %>%
    mutate(!!names_adj[i] := salary_data[i + 1] * ratio)
}

salary_data[] <- lapply(salary_data, function(x) if(is.list(x)) unlist(x) else x)

write_csv(salary_data, "/Users/Honors/Desktop/Portfolio Projects/salary data.csv")


