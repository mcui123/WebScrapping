# correlation plots
library(ggplot2)
library(janitor)
library(scales)
library(maps)
library(viridis)
library(reshape2)
library(dplyr)
library(tidyr)

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
  
  cor_data <- cor(md_sub, use = "pairwise.complete.obs")
  cor_data_test <- as.data.frame(cor_data)[1,]
  cor_data_test <- cor_data_test %>% 
    mutate(contient = paste(i))
  cor_data_all <- rbind(cor_data_all, cor_data_test)
}
write.csv(cor_data_all, "correlation_matrix_of_health_expenditures_value_and_all_continent.csv")


### first correlation matrix
md_1 <- df %>%
  select(health_expenditures_value, maternal_mortality_rate, continent, gdp_group) %>% 
  mutate(label = "maternal_mortality_rate") %>% 
  rename(target = maternal_mortality_rate)

md_2 <- df %>%
  select(health_expenditures_value, infant_mortality_rate, continent, gdp_group) %>% 
  mutate(label = "infant_mortality_rate")%>% 
  rename(target = infant_mortality_rate)

md_3 <- df %>%
  select(health_expenditures_value, life_expectancy_at_birth, continent, gdp_group) %>% 
  mutate(label = "life_expectancy_at_birth")%>% 
  rename(target = life_expectancy_at_birth)

md_4 <- df %>%
  select(health_expenditures_value, median_age, continent, gdp_group) %>% 
  mutate(label = "median_age")%>% 
  rename(target = median_age)

md_5 <- df %>%
  select(health_expenditures_value, hiv_aids_adult_prevalence_rate, continent, gdp_group) %>% 
  mutate(label = "hiv_aids_adult_prevalence_rate")%>% 
  rename(target = hiv_aids_adult_prevalence_rate)

md_6 <- df %>%
  select(health_expenditures_value, total_fertility_rate, continent, gdp_group) %>% 
  mutate(label = "total_fertility_rate")%>% 
  rename(target = total_fertility_rate)

md_7 <- df %>%
  select(health_expenditures_value, obesity_adult_prevalence_rate, continent, gdp_group) %>% 
  mutate(label = "obesity_adult_prevalence_rate")%>% 
  rename(target = obesity_adult_prevalence_rate)

md_8 <- df %>%
  select(health_expenditures_value, children_under_the_age_of_5_years_underweight, continent, gdp_group) %>% 
  mutate(label = "children_under_the_age_of_5_years_underweight")%>% 
  rename(target = children_under_the_age_of_5_years_underweight)


md_all <- rbind(md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8) %>% na.omit()
md_all <- md_all %>% mutate(health_expenditures_value_in_k_bilion = health_expenditures_value/(10^12))

md_all_1 <- md_all %>% filter(continent %in% region[c(1:5)])
md_all_2 <- md_all %>% filter(continent %in% region[c(6:10)])

ggplot(md_all_1, aes(health_expenditures_value_in_k_bilion, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(label ~ continent)

ggplot(md_all_2, aes(health_expenditures_value_in_k_bilion, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(label ~ continent)



ggplot(md_all_1, aes(health_expenditures_value, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(label ~ continent)

ggplot(md_all_2, aes(health_expenditures_value, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  facet_wrap(label ~ continent)


ggplot(md_all, aes(health_expenditures_value, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  facet_wrap(label ~ continent)


### second correlation matrix

cor_data_all <- data.frame()
for (i in unique(df$gdp_group)) {
  md_sub <- df %>% 
    filter(gdp_group %in% i) %>% 
    select(topic)
  
  cor_data <- cor(md_sub, use = "pairwise.complete.obs")
  cor_data_test <- as.data.frame(cor_data)[1,]
  cor_data_test <- cor_data_test %>% 
    mutate(contient = paste(i))
  cor_data_all <- rbind(cor_data_all, cor_data_test)
}
write.csv(cor_data_all, "correlation_matrix_of_health_expenditures_value_and_all_gdp_group.csv")



ggplot(md_all, aes(health_expenditures_value_in_k_bilion, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(label ~ gdp_group)

ggplot(md_all, aes(health_expenditures_value, target))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  facet_wrap(label ~ gdp_group)






# chapter 4: health_expenditures

## boxplot1
ggplot(df, aes(continent, health_expenditures, fill = continent, alpha = 0.3))+
  geom_boxplot()+
  geom_point(aes(color= continent), size = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 4, dodge.width = 1))+
  guides(color = F, fill = F, alpha = F)+
  scale_x_discrete("Continent")+
  scale_y_continuous("Health expenditures")+
  ggtitle("Health expenditures boxplot per continent")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("health expenditure_percentage_1.png")

## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(health_expenditures, na.rm = T)) %>%
  ungroup() %>% 
  arrange(desc(n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country, health_expenditures, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country,  health_expenditures)),stat='identity') +
  geom_text(aes(label= health_expenditures), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Health expenditures")+
  ggtitle("Top 10 Health expenditures barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("health expenditure_percentage_2.png")

## barplot1
md <- df %>% 
  mutate(country = as.character(country)) %>% 
  group_by(country) %>% 
  summarise(n = mean(health_expenditures, na.rm = T)) %>%
  ungroup() %>% 
  arrange((n))
md <- md [1:10,]
md <- df %>% filter(country %in% md$country)

ggplot(md, aes(country,  health_expenditures, fill = continent, alpha = 0.3))+
  geom_bar(aes(x = reorder(country, health_expenditures)),stat='identity') +
  geom_text(aes(label=health_expenditures), vjust=0.3, color="black",
            position = position_dodge(1), size=3.5)+
  scale_x_discrete("Country")+
  scale_y_continuous("Health expenditures")+
  ggtitle("Bottom 10 Health expenditures barplot per country")+
  coord_flip()+
  guides(color = F, alpha = F)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("health expenditure_percentage_3.png")

cormat <- cor_data_all
melted_cormat <- melt(cor_data_all) %>% na.omit()


# Correlation heatmap 

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

