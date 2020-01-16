###
# Tidyverse Assignment 1
# Filtering and Selecting
### 


# Find all countries in 2018 (ID_year) that...
# Are democracies (regime_status_name)
# that have seen a significant decrease in 5 years (neg_sig_5_years)
# for Checks on Government (ID_variable_name)

# AND THEN ( %>% )

# Select only ID_year, ID_country_name, ID_variable_name, value, neg_sig_5_years and 

filter(gsodi_long, ID_variable_name == "Checks on Government", 
       ID_year == 2018,
       neg_sig_5_years == 1, 
       regime_status_name == "Democracy") %>% 
  select(ID_year,ID_country_name, regime_status_name, ID_variable_name, value, neg_sig_5_years)


# You are writing a blog post on South Africa. You want to look at the change in Representative Government 
# For every decade and 2018 (1975, 1985, 1995, 2005, 2015, 2018)
# Write some code to get the scores quickly! 

filter(gsodi_long, ID_variable_name == "Representative Government",
       ID_country_name == "South Africa",
       ID_year %in% c(1975, 1985, 1995, 2005, 2015, 2018)) %>% 
  select(ID_year,ID_country_name, regime_status_name, ID_variable_name, value)

# Which country in South-East Asia (ID_subregion_name) had the highest score in 2004 (ID_year)
# for Social Group Equality (ID_variable_name)?
# Which one had the lowest?

filter(gsodi_long,
       ID_subregion_name == "South-East Asia",
       ID_variable_name == "Social Group Equality",
       ID_year == 2004) %>% 
  select(ID_year,ID_country_name, ID_variable_name, value) %>% 
  arrange(desc(value))

# Practice your own queiries. 
# Think about times you have written about the indices, what data can you get through the filter function?



