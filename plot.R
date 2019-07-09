library(ggplot2)
library(janitor)
library(scales)
library(maps)
library(viridis)
library(reshape2)
library(dplyr)
library(readr)
library(readxl)

#df <- readxl::read_excel("data_xls.xlsx")
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



# chapter 1: 	Mortality

## boxplot
ggplot(df, aes(continent, maternal_mortality_rate, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Maternal mortality rate")+
  ggtitle("Maternal mortality rate boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_1.png")

## barplot
md <- df %>%
  group_by(country) %>% 
  summarise(n = mean(maternal_mortality_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_1_2 <- df %>% filter(country %in% md$country)

ggplot(md_1_2, aes(country, maternal_mortality_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, maternal_mortality_rate)),stat='identity') +
  geom_text(aes(label=maternal_mortality_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Maternal mortality rate")+
  ggtitle("Top 10 maternal mortality rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_2.png")

## barplot
md <- df %>% group_by(country) %>% 
  summarise(n = mean(maternal_mortality_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_1_3 <- df %>% filter(country %in% md$country)

ggplot(md_1_3, aes(country, maternal_mortality_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, maternal_mortality_rate)),stat='identity') +
  geom_text(aes(label=maternal_mortality_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Maternal mortality rate")+
  ggtitle("Bottom 10 maternal mortality rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_3.png")


## boxplot2
ggplot(df, aes(continent, infant_mortality_rate, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Infant mortality rate")+
  ggtitle("Infant mortality rate boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_4.png")

## barplot2
md <- df %>% group_by(country) %>% 
  summarise(n = mean(infant_mortality_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_1_5 <- df %>% filter(country %in% md$country)

ggplot(md_1_5, aes(country, infant_mortality_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, infant_mortality_rate)),stat='identity') +
  geom_text(aes(label=infant_mortality_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Infant mortality rate")+
  ggtitle("Top 10 Infant mortality rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_5.png")

## barplot2
md <- df %>% group_by(country) %>% 
  summarise(n = mean(infant_mortality_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_1_6 <- df %>% filter(country %in% md$country)

ggplot(md_1_6, aes(country, infant_mortality_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, infant_mortality_rate)),stat='identity') +
  geom_text(aes(label=infant_mortality_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Infant mortality rate")+
  ggtitle("Bottom 10 Infant mortality rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Mortality_6.png")









# Chpater 2:	Life expectancy & aging 


## boxplot1
ggplot(df, aes(continent, life_expectancy_at_birth, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Life expectancy at birth")+
  ggtitle("Life expectancy at birth boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_1.png")

## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(life_expectancy_at_birth, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_2_2 <- df %>% filter(country %in% md$country)

ggplot(md_2_2, aes(country, life_expectancy_at_birth, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, life_expectancy_at_birth)),stat='identity') +
  geom_text(aes(label=life_expectancy_at_birth), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Life expectancy at birth")+
  ggtitle("Top 10 Life expectancy at birth barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_2.png")


## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(life_expectancy_at_birth, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_2_3 <- df %>% filter(country %in% md$country)

ggplot(md_2_3, aes(country, life_expectancy_at_birth, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, life_expectancy_at_birth)),stat='identity') +
  geom_text(aes(label=life_expectancy_at_birth), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Life expectancy at birth")+
  ggtitle("Bottom 10 Life expectancy at birth barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_3.png")


## boxplot2
ggplot(df, aes(continent, population_growth_rate, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Population growth rate")+
  ggtitle("Population growth rate boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_4.png")


## barplot2
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(population_growth_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_2_4 <- df %>% filter(country %in% md$country)

ggplot(md_2_4, aes(country, population_growth_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, population_growth_rate)),stat='identity') +
  geom_text(aes(label=population_growth_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("population growth rate")+
  ggtitle("Top 10 population growth rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_5.png")

## barplot2
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(population_growth_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_2_5 <- df %>% filter(country %in% md$country)
ggplot(md_2_5, aes(country, population_growth_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, population_growth_rate)),stat='identity') +
  geom_text(aes(label=population_growth_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("population growth rate")+
  ggtitle("Bottom 10 population growth rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_6.png")


## boxplot3
ggplot(df, aes(continent, median_age, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Median age")+
  ggtitle("Median age boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_7.png")

## barplot3
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(median_age, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_3_2 <- df %>% filter(country %in% md$country)

ggplot(md_3_2, aes(country, median_age, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, median_age)),stat='identity') +
  geom_text(aes(label=median_age), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Median age")+
  ggtitle("Top 10 median age barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_8.png")

## barplot3
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(median_age, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_3_3<- df %>% filter(country %in% md$country)

ggplot(md_3_3, aes(country, median_age, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, median_age)),stat='identity') +
  geom_text(aes(label=median_age), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Median age")+
  ggtitle("Bottom 10 median age barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_9.png")

## boxplot4
ggplot(df, aes(continent, total_fertility_rate, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Total fertility rate")+
  ggtitle("Total fertility rate boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_10.png")

## barplot4
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(total_fertility_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md_4_2 <- df %>% filter(country %in% md$country)

ggplot(md_4_2, aes(country, total_fertility_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, total_fertility_rate)),stat='identity') +
  geom_text(aes(label=total_fertility_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Total fertility rate")+
  ggtitle("Top 10 total fertility rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_11.png")

## barplot4
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(total_fertility_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md_4_3 <- df %>% filter(country %in% md$country)

ggplot(md_4_3, aes(country, total_fertility_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, total_fertility_rate)),stat='identity') +
  geom_text(aes(label=total_fertility_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Total fertility rate")+
  ggtitle("Bottom 10 total fertility rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Life expectancy & aging_12.png")


## heatmap mortality
world_map <- map_data("world")
mort <- df %>% 
  select(country, continent, maternal_mortality_rate) %>% 
  mutate(maternal_mortality_rate = as.numeric(maternal_mortality_rate),
         country = as.character(country)) 
world_map <- left_join(world_map, mort, by = c("region" = "country")) %>%
  filter(!is.na(as.character(region))) %>% 
  mutate(mortality_level = ifelse(maternal_mortality_rate < 14.0,  "3.0 <= Maternal mortality rate < 14.0",
                        ifelse(maternal_mortality_rate < 53.5, "14.0 <= Maternal mortality rate < 53.5",
                        ifelse(maternal_mortality_rate < 229.0, "53.5 <= Maternal mortality rate < 229.0",
                               "229.0 <= Maternal mortality rate <= 1360.0"))))
contient.lab.data <- world_map %>%
  group_by(continent) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = mortality_level))+
  geom_text(aes(label = continent), data = contient.lab.data,  size = 6, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "right")


## heatmap life expectancy
world_map <- map_data("world")
life <- df %>% 
  select(country, continent, life_expectancy_at_birth) %>% 
  mutate(life_expectancy_at_birth = as.numeric(life_expectancy_at_birth),
         country = as.character(country)) 
world_map <- left_join(world_map, life, by = c("region" = "country")) %>%
  filter(!is.na(as.character(region))) %>% 
  mutate(life_expectancy_at_birth_level = ifelse( life_expectancy_at_birth< 67.75,  " 52.10 <= Total fertility rate < 67.75",
                                  ifelse(life_expectancy_at_birth < 73.14, "67.75 <= Total fertility rate < 73.14",
                                         ifelse(life_expectancy_at_birth < 78.90, "73.14 <= Total fertility rate < 78.90",
                                                "78.90 <= Total fertility rate <= 89.40"))))
contient.lab.data <- world_map %>%
  group_by(continent) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = life_expectancy_at_birth_level))+
  geom_text(aes(label = continent), data = contient.lab.data,  size = 6, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "right")



## heatmap total fertility rate
world_map <- map_data("world")
fert <- df %>% 
  select(country, continent, total_fertility_rate) %>% 
  mutate(total_fertility_rate = as.numeric(total_fertility_rate),
         country = as.character(country)) 
world_map <- left_join(world_map, fert, by = c("region" = "country")) %>%
  filter(!is.na(as.character(region))) %>% 
  mutate(total_fertility_rate_level = ifelse( total_fertility_rate< 1.698,  " 0.840 <= Total fertility rate < 1.698",
                                                  ifelse(total_fertility_rate < 2.000, "1.698 <= Total fertility rate < 2.000",
                                                         ifelse(total_fertility_rate < 2.895, "2.000 <= Total fertility rate < 2.895",
                                                                "2.895 <= Total fertility rate <= 6.350"))))
contient.lab.data <- world_map %>%
  group_by(continent) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = total_fertility_rate_level))+
  geom_text(aes(label = continent), data = contient.lab.data,  size = 6, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "right")






# chapter 3: HIV

## boxplot3
ggplot(df, aes(continent, hiv_aids_adult_prevalence_rate, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("HIV aids adult prevalence rate", limits = c(0,10))+
  ggtitle("HIV aids adult prevalence rate boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("HIV_1.png")

## barplot3
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(hiv_aids_adult_prevalence_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, hiv_aids_adult_prevalence_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, hiv_aids_adult_prevalence_rate)),stat='identity') +
  geom_text(aes(label=hiv_aids_adult_prevalence_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("HIV aids adult prevalence rate")+
  ggtitle("Top 10 HIV aids adult prevalence rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("HIV_2.png")

## barplot3
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(hiv_aids_adult_prevalence_rate, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, hiv_aids_adult_prevalence_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, hiv_aids_adult_prevalence_rate)),stat='identity') +
  geom_text(aes(label=hiv_aids_adult_prevalence_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("HIV aids adult prevalence rate")+
  ggtitle("Bottom 10 HIV aids adult prevalence rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("HIV_3.png")



# chapter 4: health_expenditures

## boxplot1
ggplot(df, aes(continent, health_expenditures_value, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Health expenditures")+
  ggtitle("Health expenditures boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("health expenditure_1.png")

## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(health_expenditures_value, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, health_expenditures_value/(10^12), fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country,  health_expenditures_value/(10^12))),stat='identity') +
  geom_text(aes(label= health_expenditures_value/(10^12)), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Health expenditures in k bilion")+
  ggtitle("Top 10 Health expenditures barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("health expenditure_2.png")

## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(health_expenditures_value, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country,  health_expenditures_value/(10^9), fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, health_expenditures_value/(10^9))),stat='identity') +
  geom_text(aes(label=health_expenditures_value/(10^9)), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Health expenditures in bilion")+
  ggtitle("Bottom 10 Health expenditures barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("health expenditure_3.png")




# Chapter 5: correlation
region <- sort(unique(df$continent))
topic <- c("health_expenditures_value", "maternal_mortality_rate", "infant_mortality_rate", "life_expectancy_at_birth",
           "median_age", "hiv_aids_adult_prevalence_rate", "total_fertility_rate", "obesity_adult_prevalence_rate",
           "children_under_the_age_of_5_years_underweight")


## heatmap first
md_cor_1 <- df %>% 
  filter(as.character(continent) %in% region[1]) %>% #Check which region you want to show
  select_at(.vars = topic) %>% 
  na.omit()
cormat <- round(cor(md_cor_1),4)
melted_cormat <- melt(cormat)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#50b6bb", high = "#f96d15", mid = "#45969b", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  ggtitle(paste0("Correlation matrix heatmap for ", region[1], " continent"))
#Check and adapte which region you have selected



## Individual scatter plot
md_cor <- df %>% 
  filter(as.character(continent) %in% region[1]) %>% #Check which region you want to show
  select_at(.vars = topic[c(1,2)]) %>% #Check which variable you want to calculate the correlation
  na.omit()
a <- round(cor(md_cor$health_expenditures_value, md_cor$maternal_mortality_rate), digits = 4) #Paste the correct column name here
ggplot(md_cor, aes(x = health_expenditures_value, y = maternal_mortality_rate))+ #Paste the correct column name here
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Health expenditures value")+ #Paste the correct column name here
  scale_y_continuous("Maternal mortality rate")+ #Paste the correct column name here
  ggtitle(paste0("Health expenditures value vs\nMaternal mortality rate, \ncorrelation is ", a, "."))+ #Paste the correct column name here
  theme_bw()
ggsave("Individual scatter plot_correlation_1.png")






















# Part 2
## World's biggest economy
## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = sum(gdp_purchasing_power_parity, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, gdp_purchasing_power_parity/(10^12), fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, gdp_purchasing_power_parity/(10^12))),stat='identity') +
  geom_text(aes(label=gdp_purchasing_power_parity/(10^12)), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("gdp purchasing power parity in k bilion")+
  ggtitle("Top 10 gdp purchasing power parity barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("World's biggest economy_1.png")

ggplot(md, aes(country, gdp_real_growth_rate, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, gdp_purchasing_power_parity)),stat='identity') +
  geom_text(aes(label=gdp_real_growth_rate), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("gdp real growth rate")+
  ggtitle("Top 10 gdp real growth rate barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("World's biggest economy_2.png")


## okun's law:
md <- df %>% select(gdp_purchasing_power_parity, unemployment_rate) %>% na.omit()
a <- round(cor(x = md$gdp_purchasing_power_parity, y = md$unemployment_rate),digits = 4)
ggplot(df, aes(x = gdp_purchasing_power_parity, y = unemployment_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("GDP purchasing power parity")+
  scale_y_continuous("Unemployment rate")+
  ggtitle(paste0("All countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_1.png")


md <- df %>% select(unemployment_rate,gdp_purchasing_power_parity) %>% na.omit()
a <- round(cor(x = md$unemployment_rate, y = md$gdp_purchasing_power_parity),digits = 4)
ggplot(df, aes(x = unemployment_rate, y = gdp_purchasing_power_parity))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Unemployment rate")+
  scale_y_continuous("GDP purchasing power parity")+
  ggtitle(paste0("All countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_1_1_1.png")










md <- df %>% select(gdp_purchasing_power_parity, unemployment_rate) %>% na.omit()
a <- round(cor(x = md$gdp_purchasing_power_parity, y = md$unemployment_rate),digits = 4)
ggplot(df, aes(x = gdp_purchasing_power_parity, y = unemployment_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("GDP purchasing power parity (log scale)")+
  scale_y_log10("Unemployment rate (log scale)")+
  ggtitle(paste0("All countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_1 log scale.png")


## okun's law2:
### plot1
md <- df %>% filter(gdp_group == "Low GDP countries") %>%  select(gdp_per_capita_ppp, unemployment_rate) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$unemployment_rate),digits = 4)
ggplot(md, aes(x = gdp_per_capita_ppp, y = unemployment_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("GDP per capita ppp")+
  scale_y_continuous("Unemployment rate")+
  ggtitle(paste0("Low GDP countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_2_1.png")

### plot2
md <- df %>% filter(gdp_group == "Middle GDP countries") %>%  select(gdp_per_capita_ppp, unemployment_rate) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$unemployment_rate),digits = 4)
ggplot(md, aes(x = gdp_per_capita_ppp, y = unemployment_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("GDP per capita ppp")+
  scale_y_continuous("Unemployment rate")+
  ggtitle(paste0("Middle GDP countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_2_2.png")

### plot3
md <- df %>% filter(gdp_group == "Large GDP countries") %>%  select(gdp_per_capita_ppp, unemployment_rate) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$unemployment_rate),digits = 4)
ggplot(md, aes(x = gdp_per_capita_ppp, y = unemployment_rate))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("GDP per capita ppp")+
  scale_y_continuous("Unemployment rate")+
  ggtitle(paste0("Large GDP countries GDP vs\nUnemployment rate,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("okuns law_2_3.png")


## internet user vs. GDP PPP
### plot1
md <- df %>% filter(gdp_group == "Low GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$internet_users),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Internet users")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Low GDP countries\ninternet users vs GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_1.png")


md <- df %>% filter(gdp_group == "Low GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$internet_users)),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Internet users (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Low GDP countries\ninternet users vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_1(log scale).png")


### plot2
md <- df %>% filter(gdp_group == "Middle GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$internet_users),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Internet users")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Middle GDP\ncountries internet users vs. GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_2.png")


md <- df %>% filter(gdp_group == "Middle GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$internet_users)),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Internet users (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Middle GDP countries\ninternet users vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_2(log scale).png")



### plot3
md <- df %>% filter(gdp_group == "Large GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$internet_users),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Internet users")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Large GDP countries\ninternet users vs. GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_3.png")

  

md <- df %>% filter(gdp_group == "Large GDP countries") %>%  select(gdp_per_capita_ppp, internet_users) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$internet_users)),digits = 4)
ggplot(md, aes(x = internet_users, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Internet users (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Large GDP countries\ninternet users vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("internet user vs. GDP PPP_2_3(log scale).png")


## Education plots
### top 10  education_expenditures_value
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = sum(education_expenditures_value, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, education_expenditures_value/(10^12), fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, education_expenditures_value/(10^12))),stat='identity') +
  geom_text(aes(label=education_expenditures_value/(10^12)), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Education expenditures value (k bilion)")+
  ggtitle("Top 10 education expenditures value barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("top 10  education_expenditures_value_1.png")

### top 10  education_expenditures
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = sum(education_expenditures, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, education_expenditures, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, education_expenditures)),stat='identity') +
  geom_text(aes(label=education_expenditures), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Education expenditures (%)")+
  ggtitle("Top 10 education expenditures barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("top 10  education_expenditures_1.png")


## education_expenditures_value vs gdp ppp (3 groups)
### plot1
md <- df %>% filter(gdp_group == "Low GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$education_expenditures_value),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Education expenditures value")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Low GDP countries\neducation expenditures value vs. GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_1.png")



md <- df %>% filter(gdp_group == "Low GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$education_expenditures_value)),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Education expenditures value (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Low GDP countries\neducation expenditures value vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_1(log scale).png")


### plot2
md <- df %>% filter(gdp_group == "Middle GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$education_expenditures_value),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Education expenditures value")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Middle GDP countries\neducation expenditures value vs. GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_2.png")


md <- df %>% filter(gdp_group == "Middle GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$education_expenditures_value)),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Education expenditures value (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Middle GDP countries\neducation expenditures value vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_2(log scale).png")


### plot3
md <- df %>% filter(gdp_group == "Large GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = md$gdp_per_capita_ppp, y = md$education_expenditures_value),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Education expenditures value")+
  scale_y_continuous("GDP per capita")+
  ggtitle(paste0("Large GDP countries\neducation expenditures value vs. GDP,\ncorrelation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_3.png")



md <- df %>% filter(gdp_group == "Large GDP countries") %>%  select(gdp_per_capita_ppp, education_expenditures_value) %>% na.omit()
a <- round(cor(x = log(md$gdp_per_capita_ppp), y = log(md$education_expenditures_value)),digits = 4)
ggplot(md, aes(x = education_expenditures_value, y = gdp_per_capita_ppp))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Education expenditures value (log scale)")+
  scale_y_log10("GDP per capita (log scale)")+
  ggtitle(paste0("Large GDP countries\neducation expenditures value vs. GDP,\nlog correlation is ", a, "."))+
  theme_bw()
ggsave("education_expenditures_value vs gdp ppp_3(log scale).png")

# correlation again
group <- unique(df$gdp_group)
topic <- c("health_expenditures_value", "maternal_mortality_rate", "infant_mortality_rate", "life_expectancy_at_birth",
           "median_age", "hiv_aids_adult_prevalence_rate", "total_fertility_rate", "obesity_adult_prevalence_rate",
           "children_under_the_age_of_5_years_underweight")


## heatmap first
md_cor_2 <- df %>% 
  filter(as.character(gdp_group) %in% group[1]) %>% #Check which group want to show
  select_at(.vars = topic) %>% 
  na.omit()
cormat <- round(cor(md_cor_2),4)
melted_cormat <- melt(cormat)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#50b6bb", high = "#f96d15", mid = "#45969b", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  ggtitle(paste0("Correlation matrix heatmap for ", group[1]))
#Check and adapte which group you have selected



## Individual scatter plot
md_cor <- df %>% 
  filter(as.character(gdp_group) %in% group[1]) %>% #Check which group you want to show
  select_at(.vars = topic[c(1,2)]) %>% #Check which variable you want to calculate the correlation
  na.omit()
a <- round(cor(md_cor$health_expenditures_value, md_cor$maternal_mortality_rate), digits = 4) #Paste the correct column name here
ggplot(md_cor, aes(x = health_expenditures_value, y = maternal_mortality_rate))+ #Paste the correct column name here
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Health expenditures value")+ #Paste the correct column name here
  scale_y_continuous("Maternal mortality rate")+ #Paste the correct column name here
  ggtitle(paste0("For Large GDP group, health expenditures value vs\nmaternal mortality rate,\ncorrelation is ", a, "."))+ #Paste the correct column name here
  theme_bw()
ggsave("correlation_large_gdp_example.png")



md_cor <- df %>% 
  filter(as.character(gdp_group) %in% group[1]) %>% #Check which group you want to show
  select_at(.vars = topic[c(1,2)]) %>% #Check which variable you want to calculate the correlation
  na.omit()
a <- round(cor(log(md_cor$health_expenditures_value), log(md_cor$maternal_mortality_rate)), digits = 4) #Paste the correct column name here
ggplot(md_cor, aes(x = health_expenditures_value, y = maternal_mortality_rate))+ #Paste the correct column name here
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10("Health expenditures value (log scale)")+ #Paste the correct column name here
  scale_y_log10("Maternal mortality rate (log scale)")+ #Paste the correct column name here
  ggtitle(paste0("For Large GDP group, health expenditures value vs\nmaternal mortality rate,\nlog correlation is ", a, "."))+ #Paste the correct column name here
  theme_bw()
ggsave("correlation_large_gdp_example (log scale).png")

