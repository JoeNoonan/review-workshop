library(tidyverse)
library(janitor)

###
### Review
###

gsodi_long <- read_csv("gsodi_long.csv")


### Exploring the dataframe

### Find all of the variable names
names(gsodi_long)

### Look at the data like in Excel
View(gsodi_long)

### Summary of the dataframe (more complex)
str(gsodi_long)

### Dimensions of dataframe (rows by columns)
dim(gsodi_long)

### Top of the dataframe
head(gsodi_long)

### Bottom of the data frame 
tail(gsodi_long)


### Filtering and Selecting

### Works the same as in excel but you type it out! 

filter(.data = gsodi_long, ID_year == 2018)

filter(gsodi_long, ID_year == 2018, ID_country_name == "Honduras")

filter(gsodi_long, ID_year == 2018 & neg_sig_10_years == 1  | ID_year == 2018 & neg_sig_5_years == 1)

filter(gsodi_long, 
       ID_variable_name == "Representative Government",
       ID_year < 2000 & ID_year >= 1990, 
       value >= 0.50, 
       neg_sig_5_years == 1)


### Warm up
# Filter for countries in Latin America that have seen a negative 10 year development for Clean Elections.

# Filter for countries that have seen a 10 or 5 year positive development for Absence of Corruption



### Selecting
# What do you actually need to look at? 

select(gsodi_long, value)

# Selecting allows you to select variables.

select(gsodi_long, ID_year, ID_country_name, ID_region_name, ID_variable_name, value)
select(gsodi_long, ID_year, ID_country_name, ID_region_name, ID_variable_name, value, regime_status_name)

### The pipe function
# The pipe %>% (shft-ctrl-m) 

# What the pipe does is it takes the output of the function on the 
# left and feeds it to the right function as its first argument.

# Or in english "do this and then this"

# Filter and then select
filter(gsodi_long, ID_year == 2018, ID_country_name == "Honduras") %>% # Do this...
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, value) # then this. 

# Warm up filter only cases in Latin America in 1984
# then select ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, value


# Transforming variables in dplyr
# Making new variables! through mutate()
# mutate() works like this mutate(df, new_variable = (operation for new variable))

gsodi_long %>% 
  mutate(above_index = above_world_average + above_region_average) %>%  # Create the new variable, in this case just adding some flags for if a country is above or below the global average
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, 
         value, above_index, above_world_average, above_region_average) %>% # Select relevant variables 
  arrange(desc(above_index)) # Arrange by new variable above_index


# Warm up
# Create a variable where 2 = neg_5_years_change and neg_10_years change. 
# Use neg_sig_5_years + neg_sig_10_years 

# IMPORTANT! group_by function
# This is how you would create for regional averages

regional_value_mutate_df <- gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  mutate(regional_value_joe = mean(value, na.rm = TRUE))%>%  # mean is the function for average
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, 
         value, regional_value_joe, region_value)

filter(regional_value_mutate_df, ID_year == 2018, 
       ID_region_name == "Europe",
       ID_variable_name == "Clean Elections")




# Summarize data 
# Like mutate but collapses or distills the output of the group

# Compare the summarized operation with the mutated operation 

regional_value_summarize_df<- gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  summarize(regional_value_joe = mean(value, na.rm = TRUE))

# What is the difference between these two data frames? 

regional_value_summarize_df
regional_value_mutate_df

# Both mutate and summarize can make multiple new variable . 

gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  summarize(regional_value_joe = mean(value, na.rm = TRUE),
            regional_min = min(value, na.rm = TRUE),
            regional_max = max(value, na.rm = TRUE))

### Warm up
### By region and year, summarize how many attributes, subattributes, and subcomponents are in the top_25
### HINT: ID_a_sa_sc flags whether a variable is an attribute, subattribute, or subcomponent
### HINT: Use sum(top_25_rank)




### Janitor 
# "Data science is mostly counting things"
# tabyl() function from the janitor package.
# Uses tidyverse standards %>%, group_by etc.
# Used to make frequency tables. 


# One way tabyls
# Performance class of Reperesnative Governemnt (high, low, mid-range) in 2018 

filter(gsodi_long, ID_variable_name == "Representative Government", ID_year == 2018) %>% # Filter out critera
  tabyl(var1= perfom_class_var_name, show_missing_levels = FALSE)

### Adorn percentage formating (makes it look pretty)

filter(gsodi_long, ID_variable_name == "Representative Government", ID_year == 2018) %>% # Filter out critera
  tabyl(var1= perfom_class_var_name, show_missing_levels = FALSE) %>% 
  adorn_pct_formatting()

#Two way tabyls 

filter(gsodi_long, ID_variable_name == "Representative Government", ID_year == 2018) %>% # Filter out critera
  tabyl(var1 = perfom_class_var_name, var2 = regime_status_name, show_missing_levels = FALSE) %>% 
  adorn_totals(c("row", "col")) %>% # adds total column to the rows and collumns
  adorn_percentages("col") %>% # adds percentages for the columns 
  adorn_pct_formatting() %>% # adds percentage formating
  adorn_ns() # adds ns 




###
### Democratic Performance Patterns Analysis
###

###
# For the democratic performance patterns analysis we use a different datasheet from the usual "long" gsodi
# dataset.
#
# This is because the unit of measurment wouldn't work on the other dataframe where we are looking at
# Country-Variable-Year cases. 
###

gsodi_dem_patterns <- read_csv("gsodi_dem_patterns.csv")

# ASSIGNMENT: Explore this new dataset. What do you notice about how the data is organized? 
# Find the names of all the variables, the number of observations (rows), and columns


### ASSIGNMENT 
### Using tabyl:
### Find the percentage of African Countries (ID_region_name) by regime type (dem) in 2018 


### Find the percentage of Non-Democracies with 5/5 low attributes. 




# ASSIGNMENT: Which countries scored high on all five attributes in 1975?
# Use variable high_atrbs. Explore the variable to understand how it works. 


# ASSIGNMENT: Count (using R!) the number of countries that hiave high_atrbs on all five indices in 1975

# Using the count() function which simply counts the number of observations
# Example of count
# Count the number of countries in the dataset

count(gsodi_dem_patterns, ID_country_name, name = "num_countries")

# count(x = dataframe, variables to group by, name of count variable)
# REMEMBER, use ?count() to find out more information in the help file. 


# Example of count used with filter
filter(gsodi_dem_patterns, ID_region_name == "Africa") %>% 
  count(ID_country_name, name = "name_countries")


# ASSIGNMENT: Create a data frame that has the number of 5/5 high attribute countries, by year, from 1975-2018
# Hint: use group_by()



#ASSIGNMENT: How many different combinations of democratic performance levels exist in LAC in 2018?
# What is the most common democratic combination? 

# Use **some/all** of functions count(), group_by(), filter(), summarize(), group_by() 
# to find out the number of unique dem performance levels in LAC in 2018 and the frequency of them
# Multiple ways to solve! 



#ASSIGNMENT: 
# How many countries performed low, mid-range and high on Participatory Engagement per region in 2018?
# Use variable A_05c
# How would you visualize this data? (Determine what chart would be best, then try to make it!)



###
### GSODI Indices scores analysis 
###
### USE gsodi_long dataset

# ASSIGNMENT: Find average levels of Absence of Corruption by political regime type (regime_status_name)
# in Asia Pacific
# Use group_by() summarize()!
# Think about ideas for different visualizations with the data
# Think about ways to visualize at trends over time and a single year 
# How would you have to structure the data for each? 



### Visualization ideas


# Which region of the world has improved the most on Gender Equality since 1975?

### Tutorial

# First make regional values data set with the following variables:
# ID_year, ID_variable_name, ID_region_name, region_value, region_lower_value, region_upper_value
# Use the distinct() function to remove duplicates and transform this to a regional dataset not a country dataset


regional_value_df <- gsodi_long %>% 
  select(ID_year, ID_variable_name, ID_region_name, region_value, region_lower_value, region_upper_value) %>% # Select relevant variables
  distinct() # Remove duplicates

regional_value_1975_2018_df <- filter(regional_value_df, ID_year %in% c(1975, 2018)) %>% 
  group_by(ID_variable_name, ID_region_name) %>% 
  mutate(score_change_1975_2018 = region_value - lag(region_value, 1))

filter(regional_value_1975_2018_df,
       ID_variable_name == "Gender Equality",
       ID_year == 2018)



# How would we do this if we were looking at just countries? 
# HINT: look at the variables in gsodi_long.



# ASSIGNMENT: Which regions have seen most declines in the 
# individual dimensions of civic space? Bar graph and table
# Filter relevant variables, group_by and summarize
# Different ways of looking at it, aggregate score or number of negative declining countries
# Use neg_sig_5_years first! 

### Making a bar plot 


# How many countries have more than 30% of female legislators, 
# in % of the total number of countries per region?

### Example of technique 
### Need to create a flag where 1 = statement is true
### As a placeholder lets use Electoral Participation 

elect_part_df <- filter(gsodi_long, ID_variable_name == "Electoral Participation") %>% 
  mutate(elect_per_50 = ifelse(value > .50, 1, 0)) # ifelse() returns a value if a condition is met. In this case if value is over .50 then 1 for everything else 0.

### How would you take this and summarize it by region? 

elect_part_df %>% 
  group_by(ID_year, ID_region_name) %>%
  summarize(sum(elect_per_50, na.rm = TRUE))

### Use regional count table to pull in number of countries. 
