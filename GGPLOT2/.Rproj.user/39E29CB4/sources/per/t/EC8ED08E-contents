install.packages('ggthemes', 'gapminder', dependencies = TRUE)
library(gapminder)
library(tidyverse)
library(ggthemes)

### ggplot2 introduction  
### A quick overview of the process
### There are hundreds of little details you can learn but this is mostly focusing on the overall process



### Knows the data, but not the mapping 

ggplot(data = gapminder)

### Given data and mapping, but no geom

ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))

### Given data, mapping and geonom

base_plot <- ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point() #Tells the computer which type of chart to make

### What happens when you change the geonom? 

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_line() # Tells the computer to make a line chart, this doesn't make sense.


#### Or layer multiple genoms? 

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_line() +
  geom_point()



### Adding layer by layer
### Changing scale

base_plot

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point(alpha = 0.3) + #change the alphaa to make the dots more transparent 
  scale_x_log10(labels = scales::dollar) #Compresses outliers 

#### Adding labels 

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(labels = scales::dollar) + #Compresses outsiders 
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")


### But what do these dots represent? Which countries are they? 


ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent, # Color the outside of the points by contintnent
                          fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")


### Themes! 
### Examples of themes below
### Black and white theme

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     color = continent, # Color the outside of the points by contintnent
                     fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") +
  theme_bw()

### Theme minimal 

gapminder_minimal <- ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     color = continent, # Color the outside of the points by contintnent
                     fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") +
  theme_minimal()

### Theme light
ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     color = continent, # Color the outside of the points by contintnent
                     fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") +
  theme_light()

### Additional themes from ggthemes

### "Economist Theme"

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     color = continent, # Color the outside of the points by contintnent
                     fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") +
  theme_economist()


### WSJ Theme

ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     color = continent, # Color the outside of the points by contintnent
                     fill = continent))+ # Total fill by color
  geom_point() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.") +
  theme_fivethirtyeight()

### How to save plots

### Save as a png
ggsave(gapminder_minimal, filename = "figures/gapminder_minimal.png")

### Save as a pdf

ggsave(gapminder_minimal, filename = "figures/gapminder_minimal.pdf")

### Change size

ggsave(gapminder_minimal, filename = "figures/gapminder_minimal_large.png", 
       height = 8, width = 10, units = "in", scale = .7) ## Height and width canbe changed as well as units (inches, cm, pixels)



### Now lets look at gsodi data 

### Goal - Make a chart showing 
### Checks on Government in Venezuela from 
### 1998 to 2018 

### 
region_sub_region_key <- read_csv("region_sub_region_key.csv")
country_region_sub_region_key<- read_csv("country_region_sub_region_key.csv") 

### read GSOD
gsodi_long_ci <- read_csv("gsodi_rank_ci_long_with_regions_v5_2019.csv") %>%
  arrange(ID_country_region_sub, ID_country_name) %>% 
  mutate(ID_country_name_F = factor(.$ID_country_name, levels=(unique(.$ID_country_name))),
         lower_value_country = ifelse(ID_country_region_sub == 1, lower_value, NA),
         upper_value_country = ifelse(ID_country_region_sub == 1, upper_value, NA)) %>% 
  filter(!is.na(ID_variable_name))


venezuela_df <- filter(gsodi_long_ci, ID_country_name == "Venezuela", 
                       ID_variable_name == "Checks on Government",
                       ID_year %in% seq(1990, 2018, by = 1))

### Basic Plot

ggplot(data = venezuela_df, aes(x = ID_year, y = value)) +
  geom_line() +
  theme_minimal()


### Adding axis scales  and changing the scale 
ggplot(data = venezuela_df, aes(x = ID_year, y = value)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018), limits = c(1990, 2018))+ 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0, 1))
  

### That looks a bit better but lets add some labels.
ggplot(data = venezuela_df, aes(x = ID_year, y = value)) +
  geom_line() +
  theme_minimal() + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018), limits = c(1990, 2018))+ 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0, 1))+
  labs(x = "Year", y = "Checks on Government Score",
       title = "Venezuela's declining accountability",
       subtitle = "Data points are country-years",
       caption = "Source: Global State of Democracy Indices (2018)") 

### Lets make the line a bit thicker

ggplot(data = venezuela_df, aes(x = ID_year, y = value)) +
  geom_line(size = 1) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018), limits = c(1990, 2018))+ 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(0, 1))+
  labs(x = "Year", y = "Checks on Government Score",
       title = "Venezuela's declining accountability",
       subtitle = "Data points are country-years",
       caption = "Source: Global State of Democracy Indices (2018)") 

### What about confidence intervals? Can you guess how we would add those in? 





plot <- ggplot(filter(gsodi_long_ci, ID_country_name %in% lookup_df$ID_country_name|
                              ID_country_code %in% c(lookup_df$ID_subregion + 970,lookup_df$ID_region+990, 999)) %>% 
                       filter(ID_variable %in% variable_list[[j]]),
                     aes(x=ID_year,
                         y=value,
                         group = ID_country_name_F,
                         color = ID_country_name_F,
                         ymin = lower_value_country, 
                         ymax = upper_value_country))+
        geom_line(size = 1)+ 
        geom_ribbon(alpha =  .25, fill = "#111344" , colour=NA) +
        scale_x_continuous("Year", breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2018), limits = c(1975, 2018))+ 
        scale_y_continuous("Index score", breaks = scales::pretty_breaks(n = 10),limits=c(0, 1))+
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
              panel.grid = element_line(colour = "grey70")) +
        scale_colour_manual(values = IDEA_colors) +
        ggtitle(paste(names(variable_list[j])))+
        guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) +
        labs(caption = caption)
      {ggsave(filename= paste0(getwd(), "/countries/", lookup_df$ID_region_name, "/",
                               lookup_df$ID_country_name,
                               "/",
                               str_replace_all(paste0(which(variable_list == variable_list[j]),
                                                      "_",
                                                      names(variable_list[j]),
                                                      "_",
                                                      lookup_df$ID_country_name,
                                                      ".png"), fixed(" "), "_"))
              , plot=plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)}
    }


