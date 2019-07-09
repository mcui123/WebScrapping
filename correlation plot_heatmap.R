# correlation plots
library(ggplot2)
library(janitor)
library(scales)
library(maps)
library(viridis)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)

df <- read_csv("data.csv")
df <- df %>% clean_names()

df$gdp_purchasing_power_parity <- as.character(gsub("\\$","", df$gdp_purchasing_power_parity))
df$gdp_purchasing_power_parity <- as.character(gsub("\\,","", df$gdp_purchasing_power_parity))
df$gdp_per_capita_ppp <- as.character(gsub("\\$","", df$gdp_per_capita_ppp))
df$gdp_per_capita_ppp <- as.character(gsub("\\,","", df$gdp_per_capita_ppp))

df <- df %>% 
  mutate(maternal_mortality_rate = as.numeric((maternal_mortality_rate))) %>% 
  mutate(country = as.character(country)) %>% 
  mutate(population = as.numeric(population)) %>% 
  mutate(gdp_purchasing_power_parity = as.numeric(gdp_purchasing_power_parity)) %>% 
  mutate(gdp_real_growth_rate = as.numeric(gdp_real_growth_rate)) %>% 
  mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
  mutate(gdp_per_capita_ppp = as.numeric(gdp_per_capita_ppp)) %>%
  mutate(internet_users = as.numeric(internet_users)) %>% 
  mutate(health_expenditures_value = as.numeric(health_expenditures)*as.numeric(gdp_purchasing_power_parity)) %>% 
  mutate(education_expenditures_value = as.numeric(education_expenditures)*as.numeric(gdp_purchasing_power_parity)) %>% 
  filter(!as.character(continent) %in% c("antarctica")) %>% 
  mutate(gdp_group = ifelse(gdp_per_capita_ppp < 7918.2, "Low GDP countries",
                            ifelse(gdp_per_capita_ppp < 27127.4, "Middle GDP countries", "Large GDP countries")))

region <- sort(unique(df$continent))
topic <- c("health_expenditures_value", "maternal_mortality_rate", "infant_mortality_rate", "life_expectancy_at_birth",
           "median_age", "hiv_aids_adult_prevalence_rate", "total_fertility_rate", "obesity_adult_prevalence_rate",
           "children_under_the_age_of_5_years_underweight")

cor_data_all <- data.frame()
for (i in region) {
  md_sub <- df %>% 
    filter(continent %in% i) %>% 
    select(topic)
  
  cor_data <- round(cor(md_sub, use = "pairwise.complete.obs"),digits = 2)
  cor_data_test <- as.data.frame(cor_data)[1,]
  cor_data_test <- cor_data_test %>% 
    mutate(contient = paste(i))
  cor_data_all <- rbind(cor_data_all, cor_data_test)
}


cormat <- cor_data_all
melted_cormat <- melt(cor_data_all) %>% na.omit()



ggplot(melted_cormat, aes(variable, contient)) + 
  geom_tile(aes(fill = value), colour = "#50b6bb") + 
  scale_fill_gradient(low = "#45969b",  high = "#f96d15")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(variable, contient, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())+
  # legend.justification = c(1, 0),
  # legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal")+
  guides(fill = F,
         legend = F,
         color = F)































### second correlation matrix

cor_data_all <- data.frame()
for (i in unique(df$gdp_group)) {
  md_sub <- df %>% 
    filter(gdp_group %in% i) %>% 
    select(topic)
  
  cor_data <- round(cor(md_sub, use = "pairwise.complete.obs"),digits = 2)
  cor_data_test <- as.data.frame(cor_data)[1,]
  cor_data_test <- cor_data_test %>% 
    mutate(contient = paste(i))
  cor_data_all <- rbind(cor_data_all, cor_data_test)
}

cormat <- cor_data_all
melted_cormat <- melt(cor_data_all) %>% na.omit()



ggplot(melted_cormat, aes(variable, contient)) + 
  geom_tile(aes(fill = value), colour = "#50b6bb") + 
  scale_fill_gradient(low = "#45969b",  high = "#f96d15")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(variable, contient, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())+
  # legend.justification = c(1, 0),
  # legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal")+
  guides(fill = F,
         legend = F,
         color = F)

















## heatmap mortality
world_map <- map_data("world")
mort <- df %>% 
  select(country, continent, maternal_mortality_rate) %>% 
  mutate(maternal_mortality_rate = as.numeric(maternal_mortality_rate),
         country = as.character(country)) 
world_map <- left_join(world_map, mort, by = c("region" = "country")) %>%
  filter(!is.na(as.character(region))) %>% 
  mutate(mortality_level = ifelse(maternal_mortality_rate < 14.0,  "0.0 <= Maternal mortality rate < 14.0",
                                  ifelse(maternal_mortality_rate < 53.5, "14.0 <= Maternal mortality rate < 53.5",
                                         ifelse(maternal_mortality_rate < 229.0, "53.5 <= Maternal mortality rate < 229.0",
                                                "229.0 <= Maternal mortality rate <= 1360.0")))) %>% 
  mutate(mortality_level = as.factor(mortality_level)) %>% 
  mutate(mortality_level = ordered(mortality_level, levels = c("0.0 <= Maternal mortality rate < 14.0",
                                                               "14.0 <= Maternal mortality rate < 53.5",
                                                               "53.5 <= Maternal mortality rate < 229.0",
                                                               "229.0 <= Maternal mortality rate <= 1360.0")))
contient.lab.data <- world_map %>%
  group_by(continent) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = mortality_level))+
  geom_text(aes(label = continent), data = contient.lab.data,  size = 6, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "right")