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


# We used group_by(ID_year,ID_variable_name, ID_region_name)



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
### By region, summarize how many attributes, subattributes, and subcomponents are in the top_25
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


### Warm up 
### Find the percentage of African Countries (ID_region_name) by regime type (dem) in 2018 

filter(gsodi_dem_patterns, ID_region_name == "Africa", ID_year == 2018) %>% # Filter out critera
  tabyl(var1= dem, show_missing_levels = FALSE)

### Find the percentage of Non-Democracies with 5/5 low attributes. 

filter(gsodi_dem_patterns, ID_year == 2018)  %>% # Filter out critera
  tabyl(var1 = low_atrbs , var2 =dem , show_missing_levels = FALSE) %>% 
  adorn_totals(c("row", "col")) %>% # adds total column to the rows and collumns
  adorn_percentages("col") %>% # adds percentages for the columns 
  adorn_pct_formatting() %>% # adds percentage formating
  adorn_ns() # adds ns 




# ASSIGNMENT: Which countries scored high on all five attributes in 1975?
# Use variable high_atrbs. Explore the variable to understand how it works. 


# Filter 
filter(gsodi_dem_patterns, high_atrbs == 5, ID_year == 1975) 

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

# Solution 
filter(gsodi_dem_patterns, high_atrbs == 5, ID_year == 1975)  %>% 
  count(high_atrbs, name = "count_countries")

# ASSIGNMENT: Create a data frame that has the number of 5/5 high attribute countries, by year, from 1975-2018
# Hint: use group_by()

filter(gsodi_dem_patterns, high_atrbs == 5) %>% 
  group_by(ID_year) %>%
  count(high_atrbs, name = "count_countries")

# Alternative

gsodi_dem_patterns %>%
  group_by(ID_year, high_atrbs) %>%
  summarise(
    count_countries = n())

#ASSIGNMENT: How many different combinations of democratic performance levels exist in LAC in 2018?
# What is the most common democratic combination? 

# Use **some/all** of functions count(), group_by(), filter(), summarize(), group_by() 
# to find out the number of unique dem performance levels in LAC in 2018 
# Multiple ways to solve! 

# Solution 1: First filtering for LAC
filter(gsodi_dem_patterns, 
       ID_region_name == "Latin America and the Caribbean" ) %>% 
  count(dem_perf_pattern, name = "num_countries") 

# More countries than in LAC, why?
# Did not filter for year! 
filter(gsodi_dem_patterns,  
       ID_region_name == "Latin America and the Caribbean", 
       ID_year == 2018) %>% 
  count(dem_perf_pattern, name = "num_countries") 

# Arrange to make more digesitble and save as an object. 

lac_dem_patterns_2018_summarized <- filter(gsodi_dem_patterns,
                                           ID_region_name == "Latin America and the Caribbean",
                                           ID_year == 2018) %>% 
  count(dem_perf_pattern, name = "num_countries") %>% 
  arrange(desc(num_countries))

# What is the unique number of democracy patterns in LAC?
# Two ways of identifying.

# Since count() only looks at unique cases to count, we can just look at the number of rows in the df

nrow(lac_dem_patterns_2018_summarized)

# Alternativly using the unique() function

lac_2018_df <- filter(gsodi_dem_patterns, 
                      ID_region_name == "Latin America and the Caribbean", 
                      ID_year == 2018) 

unique(lac_2018_df$dem_perf_pattern)


### Solution 2: More elegant solution to looking at # of patterns by region

dem_pattern_counts_by_region <- gsodi_dem_patterns %>% 
  group_by(ID_year, ID_region_name) %>% 
  count(dem_perf_pattern,
        name = "pattern_count")

### Why is this more elegant? 

### More advanced, adding percentage of region to dataframe.

# Make table to find country count per region (ignoring perf patterns)
region_country_count <- gsodi_dem_patterns %>% 
  group_by(ID_year, ID_region_name) %>%
  count(ID_region_name,
        name = "num_countries")

# Join tables together and mutate a new variable called "percent_pattern"

dem_pattern_counts_by_region_percent <- left_join(region_country_count, dem_pattern_counts_by_region) %>% 
  mutate(percent_pattern = pattern_count/num_countries)

filter(dem_pattern_counts_by_region_percent, 
       ID_region_name == "Latin America and the Caribbean",
       ID_year == 2018) %>% 
  arrange(desc(percent_pattern))

#ASSIGNMENT: 
# How many countries performed low, mid-range and high on Participatory Engagement per region in 2018?
# Use variable A_05c
# How would you visualize this data? (Determine what chart would be best, then try to make it!)


# Solution
gsodi_a_05c_count <- gsodi_dem_patterns %>% 
  group_by(ID_year, ID_region_name) %>% 
  count(A_05c,
        name = "A_05c_count")

# Then filter

gsodi_a_05c_count_2018 <- filter(gsodi_a_05c_count,
                                 ID_year == 2018,
                                 ID_region_name == "Asia and the Pacific ")

# Let's make a simple chart!

ggplot(data=gsodi_a_05c_count_2018, 
       aes(x=ID_region_name, 
           y=A_05c_count,
           fill = A_05c)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()

# How about with percentage of regions?

gsodi_a_05c_count_2018_percent <- left_join(gsodi_a_05c_count_2018,region_country_count) %>% 
  mutate(A_05c_count_percent = A_05c_count/num_countries)


ggplot(data=gsodi_a_05c_count_2018_percent, 
       aes(x=ID_region_name, 
           y=A_05c_count_percent,
           fill = A_05c)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()


#How many countries became democracies in 1975-2018?
#Which countries have not been classified as democracies in 1975-2018?


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

avg_score_region_regime_df <- gsodi_long %>% 
  group_by(ID_year, ID_region_name, ID_variable_name, regime_status_name) %>% 
  summarize(avg_score = mean(value, na.rm = TRUE))

# Filter to find final scores 

ap_abs_corrp_avg_score_2018_df<- filter(avg_score_region_regime_df, ID_year == 2018,
       ID_region_name == "Asia and the Pacific",
       ID_variable_name == "Absence of Corruption")

### Visualization ideas

## Bar graph

ggplot(data=ap_abs_corrp_avg_score_2018_df, 
       aes(x=regime_status_name, 
           y=avg_score)) +
  geom_bar(stat="identity") +
  theme_minimal()

## Line chart over time
## Need to make the data into a time series 


ap_abs_corrp_1990_2018 <- gsodi_long %>% 
  group_by(ID_year, ID_region_name, ID_variable_name, regime_status_name) %>% 
  summarize(avg_score = mean(value, na.rm = TRUE)) %>% 
  filter(ID_year %in% 1990:2018,
         ID_region_name == "Asia and the Pacific",
         ID_variable_name == "Absence of Corruption",
         !is.na(regime_status_name))

### Visualization over time 

ggplot(ap_abs_corrp_1990_2018, 
       aes(x = ID_year, y = avg_score, fill = regime_status_name, color = regime_status_name))+
  geom_line(size = 3) +
  theme_minimal()

# Which region of the world has improved the most on Gender Equality since 1975?

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



# Which regions have seen most declines in the 
# individual dimensions of civic space? Bar graph and table
# Filter relevant variables, group_by and summarize 

sig_declines_5_years_civic_space_2018 <- filter(gsodi_long, ID_variable_name %in% c("Civil Society Participation", "Media Integrity","Civil Liberties")) %>% 
  group_by(ID_region_name, ID_variable_name, ID_year) %>% 
  summarize(num_countries_neg_sig_5_years = sum(neg_sig_5_years)) %>% 
  filter(ID_year == 2018)

### Making a bar plot 

ggplot(data=sig_declines_5_years_civic_space_2018, 
       aes(x=ID_region_name, 
           y=num_countries_neg_sig_5_years,
           fill = ID_variable_name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal()


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
