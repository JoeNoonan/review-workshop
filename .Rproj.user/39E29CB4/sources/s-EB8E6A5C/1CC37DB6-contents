library(tidyverse)
library(janitor)


### Read in data

### Reading Data Locally
gsodi_long <- read_csv("gsodi_long.csv")

### Reading Data from URL 
gsodi_wide <- read_csv("https://www.idea.int/gsod-indices/sites/default/files/gsodi_pv_3.csv")

### NOTE final format is important. These commands open .csv (comma seperated values)
### If you want to import excel data you need to use other functions. 

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

### Long data versus wide data

### Compare gsodi_long and gsodi_wide. What are the differences between these two datasets? 

### Filtering
###   and 
### Selecting

### Works the same as in excel but you type it out! 

filter(.data = gsodi_long, ID_year == 2018)

### You don't need the .data = argument. I've only included it for clarity. 


### Using %in% and c(x,x) lets you pick out specific criteria 

filter(gsodi_long, ID_year %in% c(1975, 2018))

filter(gsodi_long, ID_country_name == "United States")

filter(gsodi_long, ID_country_name %in% c("United States", "Sweden"))


# Boolean Operators in R
# Relational Operators
# < Less Than
# > Greater than
# <= Less than or equal to
# >= Greater than or equal to 
# == Equal to
# != equal to

### What are the differences between these two operations?

lt <- filter(gsodi_long, ID_year < 1980)

lte <- filter(gsodi_long, ID_year <= 1980)

### In base R to explore a variable you use $ to specify what variable of a dataframe.
### For instance to look at all values you would do gsodi_long$value
### This is used sometimes in the tidyverse, but not too much.

### Summary for each filtered data frame

summary(lt$ID_year)

summary(lte$ID_year)


# Logical Operators in R 
# ! Logical NOT
# & Logical 
# | Logical or 

# What do each of these queries return? Read each like a sentance! 

# "Return everything where..." 

# & operator is not needed normally

filter(gsodi_long, ID_year == 2018, ID_country_name == "Honduras")

# Is the same as...
filter(gsodi_long, ID_year == 2018 & ID_country_name == "Honduras")

# neg_sig_10_years is a flag for a significant decrease in the last 10 years, 1 means there has been
# regime_status_name is the class of regime

filter(gsodi_long, ID_year == 2018,  neg_sig_10_years == 1, regime_status_name != "Hybrid" )

filter(gsodi_long, ID_year == 2018 & neg_sig_10_years == 1  | ID_year == 2018 & neg_sig_5_years == 1)

# You can also break up filter queries so that you have one line for each critera

filter(gsodi_long, 
       ID_variable_name == "Representative Government",
       ID_year < 2000 & ID_year >= 1990, 
       value >= 0.50, 
       neg_sig_5_years == 1)



### Selecting 
# What do you actually need to look at? 

select(gsodi_long, value)

# Selecting allows you to select variables.

select(gsodi_long, ID_year, ID_country_name, ID_region_name, ID_variable_name, value)
select(gsodi_long, ID_year, ID_country_name, ID_region_name, ID_variable_name, value, regime_status_name)

# How do you combine these? 

# Ineffecient way

# Create a new dataframe of filtered data
gsodi_long_honduras_2018_1 <- filter(gsodi_long, ID_year == 2018, ID_country_name == "Honduras")

# Create a new dataframe selecting the proper variables. 
gsodi_long_honduras_2018_2 <- select(gsodi_long_honduras_2018_1, 
                                     ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, value)

gsodi_long_honduras_2018_2

# The pipe %>% (shft-ctrl-m) 

# What the pipe does is it takes the output of the function on the 
# left and feeds it to the right function as its first argument.

# Or in english "do this and then this"

# Filter and then select
filter(gsodi_long, ID_year == 2018, ID_country_name == "Honduras") %>% # Do this...
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, value) # then this. 

# Everything to the left is always placed in the first argument to the right.

gsodi_long %>% names()

# Is the same as 

names(gsodi_long)

# The Pipe function is the key to stringing together analysis!!! 


####
#### Tidyverse assignments 1
####

### Transformations! 
### Making new variables! through mutate()

# mutate() works like this mutate(df, new_variable = (operation for new variable))

gsodi_long %>% 
  mutate(above_index = above_world_average + above_region_average) %>%  # Create the new variable, in this case just adding some flags for if a country is above or below the global average
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, 
         value, above_index, above_world_average, above_region_average) %>% # Select relevant variables 
  arrange(desc(above_index)) # Arrange by new variable above_index

# mutate() is a very useful tool, however, I have already created most of the new variables that you'd need. 
# This is one of the advantages of the long dataset. One can store data about each of the indices variables through mutate. 
# For example if the country-year-variable is above or below the regional average 

# The group_by() function greatly expands the functionality of mutate()

# This is how you would create for regional averages

regional_value_mutate_df <- gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  mutate(regional_value_joe = mean(value, na.rm = TRUE))%>%  # mean is the function for average
  select(ID_year, ID_year, ID_country_name, ID_region_name, ID_variable_name, 
         value, regional_value_joe, region_value)

filter(regional_value_mutate_df, ID_year == 2018, 
       ID_region_name == "Europe",
       ID_variable_name == "Clean Elections")

# We used group_by(ID_year,ID_variable_name, ID_region_name), 
# What variables would you choose to  group make a global average?


# Summarize data 
# Like mutate but collapses or distills the output of the group

# Compare the summarized operation with the mutated operation 

regional_value_summarize_df<- gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  summarize(regional_value_joe = mean(value, na.rm = TRUE))

regional_value_summarize_df

regional_value_mutate_df

# Both mutate and summarize can make multiple new variable . 

gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  summarize(regional_value_joe = mean(value, na.rm = TRUE),
            regional_min = min(value, na.rm = TRUE),
            regional_max = max(value, na.rm = TRUE))

# Sometimes you have to use the ungroup() function if you want to regroup with different variables


gsodi_long %>% 
  group_by(ID_year, ID_variable_name, ID_region_name) %>% # Perform next operations by year, variable and region
  summarize(regional_value_joe = mean(value, na.rm = TRUE),
            regional_min = min(value, na.rm = TRUE),
            regional_max = max(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(regional_value_0_100 = regional_value_joe * 100)


####
#### Tidyverse Assignment 2
#### 

### WDI Example 


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



