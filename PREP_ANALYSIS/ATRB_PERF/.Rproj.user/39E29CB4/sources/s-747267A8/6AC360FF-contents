library(readstata13)
library(tidyverse)
library(writexl)

gsodi_agg <- read.dta13("GSODI_nations.dta") %>% 
  mutate(dem_perf_pattern = paste("RG",A_01c, "FR", A_02c,"CG", A_03c, "IA", A_04c, "PE", A_05c, sep = "_")) %>% 
  drop_na(A_01c, A_02c,A_03c, A_04c, A_05c) %>% 
  mutate(A_01c_high = ifelse(A_01c == "High", 1, 0),
         A_02c_high = ifelse(A_02c == "High", 1, 0),
         A_03c_high = ifelse(A_03c == "High", 1, 0),
         A_04c_high = ifelse(A_04c == "High", 1, 0),
         A_05c_high = ifelse(A_05c == "High", 1, 0),
         high_atrbs = A_01c_high+A_02c_high+A_03c_high+A_04c_high+A_05c_high,
         A_01c_mid = ifelse(A_01c == "Mid-Range", 1, 0),
         A_02c_mid = ifelse(A_02c == "Mid-Range", 1, 0),
         A_03c_mid = ifelse(A_03c == "Mid-Range", 1, 0),
         A_04c_mid = ifelse(A_04c == "Mid-Range", 1, 0),
         A_05c_mid = ifelse(A_05c == "Mid-Range", 1, 0),
         mid_atrbs = A_01c_mid+A_02c_mid+A_03c_mid+A_04c_mid+A_05c_mid,
         A_01c_low = ifelse(A_01c == "Low", 1, 0),
         A_02c_low = ifelse(A_02c == "Low", 1, 0),
         A_03c_low = ifelse(A_03c == "Low", 1, 0),
         A_04c_low = ifelse(A_04c == "Low", 1, 0),
         A_05c_low = ifelse(A_05c == "Low", 1, 0),
         low_atrbs = A_01c_low+A_02c_low+A_03c_low+A_04c_low+A_05c_low,
         atrbs_aggregate = ((high_atrbs*2) + (1*mid_atrbs) +(0*low_atrbs))) %>% 
  select(cyear,  dem, dem_perf_pattern,
         A_01c, A_02c,A_03c, A_04c, A_05c,
         A_01c_high, A_02c_high, A_03c_high, A_04c_high, A_05c_high, high_atrbs,
         A_01c_mid, A_02c_mid, A_03c_mid, A_04c_mid, A_05c_mid, mid_atrbs,
         A_01c_low, A_02c_low, A_03c_low, A_04c_low, A_05c_low, low_atrbs, atrbs_aggregate) %>% 
  rename(ID_country_year = cyear)

gsodi_long <- read_csv("C:/IDEA/WORKSHOPS/JAN_REVIEW/gsodi_long_ci_rank_regions_adv_dec_regime_v5_2019.csv")

gsodi_long_dem_patterns <- as_tibble(inner_join(select(gsodi_long, ID_country_code, 
                                                       ID_country_name, ID_year, 
                                                       ID_country_year, ID_region, 
                                                       ID_subregion, ID_region_name, 
                                                       ID_subregion_name), gsodi_agg)) %>% 
  distinct()

write_csv(gsodi_long_dem_patterns, "gsodi_dem_patterns.csv")



dem_perf_patterns_general <-  filter(gsodi_long_dem_patterns,  
                                     ID_year == 2018) %>% 
  count(dem_perf_pattern) %>% 
  arrange(desc(n))

dem_perf_patterns_by_regime <- filter(gsodi_long_dem_patterns,  ID_year == 2018) %>% 
  group_by(dem) %>% 
  count(dem_perf_pattern) %>% 
  arrange(desc(dem), desc(n))

dem_perf_patterns_by_regime_unique <- filter(gsodi_long_dem_patterns,  ID_year == 2018) %>% 
  group_by(dem) %>% 
  summarize(num_unique_2018 = length(unique(dem_perf_pattern))) %>% 
  arrange(desc(num_unique_2018))

dem_perf_patterns_country <- filter(gsodi_long_dem_patterns, ID_year == 2018) %>% 
  select(ID_year, ID_country_name, ID_region_name, ID_subregion_name, dem, high_atrbs, mid_atrbs, low_atrbs, atrbs_aggregate) 

dem_perf_count <-  gsodi_long_dem_patterns %>% 
  group_by(ID_year) %>% 
  count(atrbs_aggregate) %>% 
  left_join(gsodi_long_dem_patterns%>% 
              group_by(ID_year) %>% 
              summarize(total_countries = n())) %>% 
  mutate(percent_pattern = n/total_countries) %>% 
  select(ID_year, atrbs_aggregate, percent_pattern) %>% 
  spread(atrbs_aggregate, percent_pattern) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  rename(`0 - All low` = `0`,
         `10 - All high` = `10`)

dem_perf_count_only_dems <-  filter(gsodi_long_dem_patterns, dem == "Democracy") %>% 
  group_by(ID_year) %>% 
  count(high_atrbs) %>% 
  left_join(filter(gsodi_long_dem_patterns, dem == "Democracy")%>% 
              group_by(ID_year) %>% 
              summarize(total_countries = n())) %>% 
  mutate(percent_pattern = n/total_countries) %>% 
  select(ID_year, high_atrbs, percent_pattern) %>% 
  spread(high_atrbs, percent_pattern) %>% 
  mutate_all(~replace(., is.na(.), 0)) 


dem_perf_low_count_dems <- filter(gsodi_long_dem_patterns, dem == "Democracy") %>% 
  group_by(ID_year) %>% 
  count(low_atrbs) %>% 
  left_join(filter(gsodi_long_dem_patterns, dem == "Democracy")%>% 
              group_by(ID_year) %>% 
              summarize(total_countries = n())) %>% 
  mutate(percent_pattern = n/total_countries) %>% 
  select(ID_year, low_atrbs, percent_pattern) %>% 
  spread(low_atrbs, percent_pattern) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(`1+` = `1`+`2`+`3`+`4`+`5`) %>% 
  select(ID_year, `0`, `1+`)



dem_low_counts_only_dems_region <- filter(gsodi_long_dem_patterns, dem == "Democracy") %>% 
  group_by(ID_year, ID_region_name) %>% 
  count(low_atrbs) %>% 
  left_join(filter(gsodi_long_dem_patterns, dem == "Democracy")%>% 
              group_by(ID_year, ID_region_name) %>% 
              summarize(total_countries = n())) %>% 
  mutate(percent_pattern = n/total_countries) %>% 
  select(ID_year, low_atrbs, percent_pattern) %>% 
  spread(low_atrbs, percent_pattern) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(`1+` = `1`+`2`+`3`+`4`+`5`) %>% 
  select(ID_region_name, ID_year, `0`, `1+`)






dem_performance_spreadsheet <- list(dem_perf_patterns_general, 
                                    dem_perf_patterns_by_regime, 
                                    dem_perf_patterns_by_regime_unique, 
                                    dem_perf_patterns_country, 
                                    dem_perf_count,
                                    dem_perf_count_only_dems,
                                    dem_perf_low_count_dems,
                                    dem_low_counts_only_dems_region)
names(dem_performance_spreadsheet) <- c("Count 2018 All",
                                        "Count 2018 by Regime",
                                        "Unique by Regime",
                                        "HighMidLow count by country", 
                                        "Countries by HighMidLow",
                                        "Dem # High Atributes",
                                        "Dem # Low Attributes",
                                        "Dem # Low Attributes by Region")

write_xlsx(dem_performance_spreadsheet, path = paste0(getwd(),"/Attribute Performance - Analysis - 2019 V5.xlsx"), col_names = TRUE)





##### High Performance Chart

library(glue)
library(showtext)



showtext_auto()
font_add("Meta OT",
         regular = "C:/Users/josnoo/AppData/Local/Microsoft/Windows/Fonts/MetaOT-Norm.otf", 
         bold = "C:/Users/josnoo/AppData/Local/Microsoft/Windows/Fonts/MetaOT-Bold.otf")

caption <- "\n Source: International IDEA, The Global State of Democracy Indices, 2019, <http://www.idea.int/gsod-indices>"
regime_category_colors <- c("#B50043", "#111344")



# define a custom function
str_pad_custom <- function(labels){
  new_labels <- stringr::str_pad(labels, 20, "right")
  return(new_labels)
}
#####

dem_perf_count_only_dems_chart <-  filter(gsodi_long_dem_patterns, dem == "Democracy") %>% 
  group_by(ID_year) %>% 
  count(high_atrbs) %>% 
  left_join(filter(gsodi_long_dem_patterns, dem == "Democracy")%>% 
              group_by(ID_year) %>% 
              summarize(total_countries = n())) %>% 
  mutate(percent_pattern = n/total_countries,
         high_atrb_name = glue("{high_atrbs}/5 high performing attributes"))

#####


df <- filter(dem_perf_count_only_dems_chart, high_atrbs == 0 | high_atrbs == 5)
df_label <- filter(df, ID_year %in% c(1975, 2018))
plot <- ggplot(df, 
               aes(x=ID_year,
                   y=percent_pattern,
                   group = high_atrb_name ,
                   color = high_atrb_name )) +
  geom_line(size = .9)+ 
  geom_point(data = df_label, size = 2) +
  scale_x_continuous("Year", breaks= c(1975,1980,1985, 1990, 1995, 2000, 2005, 2010, 2015, 2018), limits = c(1975, 2018)) + 
  scale_y_continuous("% of democracies", breaks = scales::pretty_breaks(n = 9), labels = scales::percent, limits = c(.15, .55))+
  ggtitle("Number of attributes with high performance")+
  labs(subtitle= "Trends in democracies over time",
       caption = caption)+
  theme_minimal()+ 
  theme(text = element_text(size=30, family = "Meta OT"), 
        legend.position="bottom", 
        legend.key.width = unit(.25,  unit = "cm"),
        legend.spacing.x = unit(0, unit = "cm"),
        legend.box.margin=margin(-12,-12,-12,-12), 
        plot.title = element_text(face="bold"),
        axis.title.x=element_blank(),  
        axis.title.y = element_text(margin=margin(0,5,0,0)),   
        legend.title=element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text.y = element_text(),
        plot.caption=element_text(size=15, hjust=1),
        panel.grid.major = element_line(size = .50),
        panel.grid.minor = element_line(size = .25),
        panel.grid = element_line(colour = "grey70"))+
  scale_colour_manual(labels = str_pad_custom,
                      values = regime_category_colors) +
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave(paste0(getwd(),"/num_high_performance_atrbs_over_time.png", sep="")
        , plot=plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)

