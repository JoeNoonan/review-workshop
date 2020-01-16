###
# Tidyverse Assignment 1
# Mutating and Grouping 
### 

# Step 1. Using summarize, group_by and filter, find the max scores for all indices variables for each region in 2018. 
# HINT: the function for finding the maxium value is max(value, na.rm = TRUE))

gsodi_long %>% 
  group_by(ID_year, ID_region_name, ID_variable_name) %>% 
  summarize(max_score = max(value, na.rm = TRUE)) %>% 
  filter(ID_year == 2018)

# Step 2. Now add the minimum score, and find the range for each region (range is max score - min score)
# Hint: You'll have to use summarize and then mutate

gsodi_long %>% 
  group_by(ID_year, ID_region_name, ID_variable_name) %>% 
  summarize(max_score = max(value, na.rm = TRUE),
            min_score = min(value, na.rm =TRUE)) %>% 
  filter(ID_year == 2018) %>% 
  mutate(range = max_score - min_score) %>% 
  arrange(desc(range))

# Step 3 (optional, challenge):
# Now look at the average range by variable. Which variable has the highest average range within regions by year?

gsodi_long %>% 
  group_by(ID_year, ID_region_name, ID_variable_name) %>% 
  summarize(max_score = max(value, na.rm = TRUE),
            min_score = min(value, na.rm =TRUE)) %>% 
  filter(ID_year == 2018) %>% 
  mutate(range = max_score - min_score) %>% 
  ungroup() %>% 
  group_by(ID_year, ID_variable_name) %>% 
  summarize(average_regional_range = mean(range)) %>% 
  arrange(desc(average_regional_range))

