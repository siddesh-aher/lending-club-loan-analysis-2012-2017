
## ========================================================
## Lending Club ??? Final Case Analysis (2012???2017)
## Data Science Tools
## ========================================================

## -------------------------------
## 1. Load Required Libraries
## -------------------------------
library(tidyverse)
library(readr)
library(ggplot2)
library (usmap)
library(janitor)
library(scales)
library(dplyr)
library(ggthemes)
library(stringr)
library(lubridate)


## -------------------------------

#sETWD ACCOURING TO YOUR FILES

setwd("D:\\NEC\\OneDrive - New England College\\Data_Science_Tools -202547-CRN129\\Assignment 14\\Final Case Analysis")

## -------------------------------
#fILES LOADED

list.files()

## -------------------------------
# run the below to see if the files are loaded if it says character(0) that means no data loaded
ls() 

## -------------------------------
## 2. Import Lending Club Files. Stacking all six Lending Club files
## -------------------------------
# Update paths to where your files are stored
files <- list.files(pattern = "^data[0-9]+\\.csv$", full.names = TRUE)

loan_raw <- files %>% 
  map_df(~ read_csv(.x, col_types = cols(.default = "c")))

loan_raw <- clean_names(loan_raw)

##Then convert numeric columns:

loan_raw <- loan_raw %>%
  mutate(across(where(~ all(grepl("^[0-9 .-]*$", .x))), as.numeric))


##--Clean corrupted numeric values

loan_raw <- loan_raw %>%
  mutate(
    across(
      c(inq_last_6mths, pub_rec_bankruptcies),
      ~ ifelse(grepl("\\..*\\.", .x), NA, .x)
    )
  )



##-- Remove bad rows where the CSV header was duplicated

loan_raw <- loan_raw %>%
  filter(mths_since_last_delinq != "Unnamed: 28")


##--Run this to how the output: Should not be character(0)

str(loan_raw)
colnames(loan_raw)

## -------------------------------
## 3. Import State Files
## -------------------------------
states <- read_csv("states.csv",show_col_types = FALSE) %>% clean_names()

colnames(states)

## -------------------------------
## 3. Import States Region Files
## -------------------------------

regions <- read_csv("states_regions.csv",show_col_types = FALSE) %>% clean_names()

colnames(regions)

##--Verify that files actually exist
list.files()



## -------------------------------
## 4. Clean & Standardize State Names. Cleaning state codes

## -------------------------------

loan_clean <- loan_raw %>%
  mutate(
    addr_state = str_trim(addr_state),
    addr_state = str_to_upper(addr_state)
  ) %>%
  rename(state = addr_state)


## -------------------------------
## 5. Merge Lending Data With States & Regions
## -------------------------------

##loan_clean$state (abbreviations). To merge with states.csv, we need full state names:

state_abbrev <- c(
  AL="Alabama", AK="Alaska", AZ="Arizona", AR="Arkansas", CA="California",
  CO="Colorado", CT="Connecticut", DE="Delaware", FL="Florida", GA="Georgia",
  HI="Hawaii", ID="Idaho", IL="Illinois", IN="Indiana", IA="Iowa",
  KS="Kansas", KY="Kentucky", LA="Louisiana", ME="Maine", MD="Maryland",
  MA="Massachusetts", MI="Michigan", MN="Minnesota", MS="Mississippi",
  MO="Missouri", MT="Montana", NE="Nebraska", NV="Nevada", NH="New Hampshire",
  NJ="New Jersey", NM="New Mexico", NY="New York", NC="North Carolina",
  ND="North Dakota", OH="Ohio", OK="Oklahoma", OR="Oregon", PA="Pennsylvania",
  RI="Rhode Island", SC="South Carolina", SD="South Dakota", TN="Tennessee",
  TX="Texas", UT="Utah", VT="Vermont", VA="Virginia", WA="Washington",
  WV="West Virginia", WI="Wisconsin", WY="Wyoming"
)

loan_clean <- loan_clean %>%
  mutate(state_full = state_abbrev[state])

##Merge with states.csv using full state name

loan_states <- loan_clean %>%
  left_join(states, by = c("state_full" = "geography"))

## regions$State is full name.

loan_states_regions <- loan_states %>%
  left_join(regions %>% select(state, region, division), by = c("state_full" = "state"))


## colnames(states) -- check for column names

## -------------------------------
## 6. Remove Missing or Invalid States
## -------------------------------
# Check how many rows have missing population
sum(is.na(loan_states_regions$population))

# Remove rows with missing population
loan_states_regions <- loan_states_regions %>% 
  filter(!is.na(population))

# Inspect the cleaned dataframe
loan_states_regions

## ========================================================
## ANALYSIS SECTION
## ========================================================

## -------------------------------
## A. Distribution of loans by state
## -------------------------------
loans_by_state <- loan_states_regions %>%
  group_by(state_full) %>%
  summarise(num_loans = n()) %>%
  arrange(desc(num_loans))

view(head(loans_by_state, 5))

## Per capita loans
loans_per_capita <- loan_states_regions %>%
  group_by(state_full, population) %>%
  summarise(num_loans = n()) %>%
  mutate(loans_per_capita = num_loans / population)

view(head(loans_per_capita, 5))

## Distribution by region / division
loans_by_region <- loan_states_regions %>%
  group_by(region) %>%
  summarise(num_loans = n())

view(head(loans_by_region, 5))

loans_by_division <- loan_states_regions %>%
  group_by(division) %>%
  summarise(num_loans = n())


##Missing states check
all_states <- unique(states$geography)
observed_states <- unique(loan_states_regions$state_full)
missing_states <- setdiff(all_states, observed_states)
missing_states

## -------------------------------
## B. Average loan amounts by state / division
## -------------------------------


##Before using mean() or other numeric operations, always check the type:
str(loan_states_regions$loan_amnt) 

##Convert loan_amnt to numeric

loan_states_regions <- loan_states_regions %>%
  mutate(loan_amnt = as.numeric(loan_amnt))

##Average by State
avg_loan_state <- loan_states_regions %>%
  group_by(state_full) %>%
  summarise(avg_loan_amount = mean(loan_amnt, na.rm = TRUE))

view(head(avg_loan_state, 5))

##Average by division

avg_loan_division <- loan_states_regions %>%
  group_by(division) %>%
  summarise(avg_loan_amount = mean(loan_amnt, na.rm = TRUE))

view(head(avg_loan_division, 5))

## -------------------------------
## C. Loan Grade ??? Avg Interest Rate & Amount
## -------------------------------

##--Before using mean() or other numeric operations, always check the type:
str(loan_states_regions$int_rate) 

# Convert int_rate to numeric

loan_states_regions <- loan_states_regions %>%
  mutate(int_rate = as.numeric(str_remove(int_rate, "%")))



grade_summary <- loan_states_regions %>%
  group_by(grade) %>%
  summarise(
    avg_interest = mean(int_rate, na.rm = TRUE),
    avg_loan_amount = mean(loan_amnt, na.rm = TRUE),
    count = n()
  )

view(head(grade_summary, 10))

## -------------------------------
## D. Yearly frequency distribution (2012 - 2017)
## -------------------------------

# Convert issue_d to date (first day of the month)
loan_states_regions <- loan_states_regions %>%
  mutate(issue_date = my(issue_d))


yearly_summary <- loan_states_regions %>%
  mutate(issue_year = year(issue_date)) %>%
  group_by(state, issue_year) %>%
  summarise(
    loans = n(),
    avg_amount = mean(loan_amnt, na.rm = TRUE),
    avg_interest = mean(int_rate, na.rm = TRUE)
  )

view(head(yearly_summary, 6))

## -------------------------------
## E. Relationship between population & avg loan amount
## -------------------------------
pop_relationship <- loan_states_regions %>%
  group_by(state, population) %>%
  summarise(avg_loan = mean(loan_amnt, na.rm = TRUE))

cor(pop_relationship$population, pop_relationship$avg_loan, use = "complete.obs")

view(head(pop_relationship, 6))

## -------------------------------
## F. Grade vs median income
## -------------------------------

income_grade_by_state <- loan_states_regions %>%
  group_by(state, grade) %>%
  summarise(
    num_loans = n(),
    .groups = "drop"
  ) %>%
  left_join(
    loan_states_regions %>%
      select(state, median_income_households) %>%
      distinct(),
    by = "state"
  ) %>%
  arrange(desc(median_income_households))

view(head(income_grade_by_state, 6))


## ========================================================
## VISUALIZATION SECTION
## ========================================================

## -------------------------------
## 1. Plot: Interest Rates by Grade
## -------------------------------
ggplot(loan_states_regions, aes(x = grade, y = int_rate)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Interest Rate Distribution by Loan Grade",
    x = "Loan Grade",
    y = "Interest Rate (%)"
  ) +
  theme_minimal()

## -------------------------------
## 2. U.S. Map ??? Average Loan Amount by State
## -------------------------------
state_avg_map <- loan_states_regions %>%
  group_by(state) %>%
  summarise(avg_loan = mean(loan_amnt, na.rm = TRUE))

plot_usmap(data = state_avg_map, values = "avg_loan", color = "black") +
  scale_fill_continuous(name = "Avg Loan Amount", label = dollar) +
  theme(legend.position = "right") +
  labs(title = "Average Lending Club Loan Amount by State (2012 - 2017)")


## ------------------------------- 
## 3. Recipient Annual Income vs Loan Amount 
## -------------------------------  

#Convert to numeric first 

loan_states_regions <- loan_states_regions %>% 
  mutate( 
    annual_inc = as.numeric(annual_inc),
    loan_amnt  = as.numeric(loan_amnt)
  )

ggplot(loan_states_regions, aes(x = annual_inc, y = loan_amnt)) + 
  geom_point(alpha = 0.3) + 
  scale_x_continuous(labels = dollar) + 
  scale_y_continuous(labels = dollar) + 
  coord_cartesian(xlim = c(0, 1.5e6), ylim = c(0, 50000))+ 
  labs( title = "Relationship Between Annual Income & Loan Amount", 
      x = "Annual Income", 
      y = "Loan Amount" 
    ) + 
  theme_minimal()


## -------------------------------
## 4. Employment Length vs Loan Amount
## -------------------------------
ggplot(loan_states_regions, aes(x = emp_length, y = loan_amnt)) +
  geom_boxplot(fill = "purple", alpha = 0.6) +
  labs(
    title = "Employment Length vs Loan Amount",
    x = "Employment Length",
    y = "Loan Amount"
  ) +
  theme_minimal()

## -------------------------------

## 5. Regional Map ??? Any Interesting Insight
## -------------------------------
region_map <- loan_states_regions %>%
  group_by(state, region) %>%
  summarise(avg_interest = mean(int_rate, na.rm = TRUE))

plot_usmap(data = region_map, values = "avg_interest", color = "black") +
  scale_fill_continuous(name = "Avg Interest Rate") +
  labs(title = "Average Interest Rate by Region") +
  theme_minimal()


