# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# functions
rename_countries <- function(data) {
  data <- data %>% 
    mutate(country = if_else(grepl('Bolivia', country), 'Bolivia', country)) %>% 
    mutate(country = if_else(grepl('Venezuela', country), 'Venezuela', country)) %>% 
    mutate(country = if_else(grepl('United Kingdom', country), 'UK', country)) %>% 
    mutate(country = if_else(grepl('Tanzania', country), 'Tanzania', country)) %>% 
    mutate(country = if_else(grepl('Palestinian', country), 'Palestina', country)) %>% 
    mutate(country = if_else(grepl('United States', country), 'USA', country)) %>% 
    mutate(country = if_else(grepl('Yemen', country), 'Yemen', country)) %>% 
    mutate(country = if_else(grepl('Egypt', country), 'Egypt', country)) %>% 
    mutate(country = if_else(grepl('Moldova', country), 'Rep. Moldova', country)) %>% 
    mutate(country = if_else(grepl('Slovak', country), "Slovakia", country)) %>% 
    mutate(country = if_else(grepl('Congo, Dem', country), "Congo, Dem. Rep.", country)) %>% 
    mutate(country = if_else(grepl('Ivoire', country), "Côte d'Ivoire", country)) %>% 
    mutate(country = if_else(grepl('Ivore', country), "Côte d'Ivoire", country)) 
}


rename_jobs <- function(data) {
  data <- data %>% 
    mutate(classif2.label = if_else(grepl('households', classif2.label), 'Households', classif2.label)) %>% 
    mutate(classif2.label = if_else(grepl('Total', classif2.label), 'Total', classif2.label))
}

# load data
data_gdp <- read.csv('data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_85121.csv', skip = 2)[1:69]
data_edu <- read.csv('data/EMP_TEMP_SEX_AGE_EDU_NB_A-20250709T1738.csv')
data_eco <- read.csv('data/EMP_TEMP_SEX_AGE_ECO_NB_A-20250709T1646.csv')
names(data_eco)[1] <- 'country'
names(data_edu)[1] <- 'country'
names(data_gdp)[1] <- 'country'
names(data_gdp)[5:69] <- sub("^X", "", names(data_gdp)[5:69])
data_gdp <- data_gdp %>% 
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "gdppc"
  ) %>%
  select('country','gdppc','year') %>% 
  rename_countries()
mapping <- read.csv('misc/reported_work_mapping.csv')

# analyse - paid jobs
################################################################################
data_eco1 <- mapping %>% 
  filter(classif2.label != 'Economic activity (ISIC-Rev.3.1): P. Activities of private households as employers and undifferentiated production activities of private households') %>% 
  dplyr::left_join(data_eco, by = 'classif2.label') %>% 
  filter(classif1.label == 'Age (Youth, adults): 15+') %>% 
  group_by(category, country, sex = sex.label, year = time) %>%
  summarise(value = sum(obs_value, na.rm = T)) %>% 
  ungroup()


data_edu1 <- data_edu %>% 
  filter(classif1.label == 'Age (Youth, adults): 15+',
         classif2.label %in% 
           c("Education (Aggregate levels): Total",
             "Education (Aggregate levels): Basic",
             "Education (Aggregate levels): Intermediate",
             "Education (Aggregate levels): Advanced",
             "Education (Aggregate levels): Level not stated",
             "Education (Aggregate levels): Less than basic")) %>% 
  select(edu.level = classif2.label, country, sex = sex.label, year = time, value = obs_value) %>% 
  group_by(country, sex, year) %>% 
  summarise(ratio_edu_intermidiate = sum(value[edu.level %in% c("Education (Aggregate levels): Advanced",
                                                                "Education (Aggregate levels): Intermediate")] /
                                           value[edu.level == 'Education (Aggregate levels): Total'])) %>% 
  ungroup() %>% 
  distinct()

data <- data_eco1 %>% 
  dplyr::left_join(data_edu1,
                   by = c('country','sex','year')) %>% 
  filter(!is.na(category)) %>% 
  # percentage of working female from the total working group
  group_by(year,country,category) %>% 
  mutate(rate_working_w = value[sex == "Female"] / value[sex == "Total"]) %>% 
  ungroup() %>% 
  # percentage of outdoor vs indoor working
  group_by(year,country,sex) %>% 
  mutate(rate_working_in = value[category == "Indoor"] / value[category == "Total"]) %>% 
  ungroup()

data_final <- merge(
  data %>% 
    filter(sex == 'Female',category == 'Total') %>% 
    select(country,year,ratio_edu_intermidiate_F = ratio_edu_intermidiate) %>% 
    distinct(),
  data %>% 
    filter(sex == 'Total',category == 'Indoor') %>% 
    select(country,year,rate_working_in_T = rate_working_in) %>% 
    distinct(),
  by = c('country','year')
) %>% 
  merge(
    data %>%
      filter(sex == 'Female',category == 'Indoor') %>%
      select(country,year,rate_working_in_W = rate_working_in) %>%
      distinct(),
    by = c('country','year')
  ) %>% 
  merge(
    data %>%
      filter(sex == 'Female',category == 'Total') %>%
      select(country,year,rate_working_W = rate_working_w) %>%
      distinct(),
    by = c('country','year')
  ) %>% 
  rename_countries()


# single year
pl <- ggplot(data_final %>% 
               filter(year == 2006), aes(
                 x = ratio_edu_intermidiate_F,
                 y = rate_working_in_T,
                 size = rate_working_W,
                 color = rate_working_in_W,
                 label = country
               )) +
  geom_point(alpha = 0.6) +
  geom_text(vjust = -0.8, size = 3) +
  scale_size_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_viridis_c(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "From Clasrooms to Offices: The education effect on Women's Work",
    x = "Ratio Women with at least Intermediate Education",
    y = "Rate Working in Indoor Jobs",
    size = "Rate Working Women",
    color = "Rate Women\nWorking Indoor"
  ) +
  theme_minimal()
ggsave(pl, file = 'figures/2006_bubble_eduF.png', width = 10, heigh = 8)


# time evolution
data_final_time <- data_final %>% 
  mutate(color_group = case_when(
    country %in% c("Netherlands", 
                   "Bolivia", 
                   "Pakistan", "Spain",
                   "Venezuela") ~ country,
    TRUE ~ "gray"
  ))

pl <- ggplot() +
  geom_point(data_final_time %>% 
               filter(color_group == 'gray'), 
             mapping = aes(
               x = year,
               y = rate_working_W,
               size = rate_working_in_W,
               label = country
             ),
             color = 'gray70',
             alpha = 0.3) +
  geom_point(data_final_time %>% 
               filter(color_group != 'gray'), 
             mapping = aes(
               x = year,
               y = rate_working_W,
               size = rate_working_in_W,
               color = color_group,
               label = country
             ),
             alpha = 0.8) +
  geom_text(data_final_time %>% 
              filter(color_group != 'gray') %>% 
              group_by(country) %>% 
              mutate(max_year = max(year)) %>% 
              ungroup() %>% 
              filter(year == max_year) %>% 
              mutate(label_x = ifelse(country == "Bolivia", year + 1, year)), 
            mapping = aes(
              x = label_x,
              y = rate_working_W,
              size = rate_working_in_W,
              color = color_group,
              label = country
            ),
            vjust = -2, size = 3, alpha = 1) +
  scale_size_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_viridis_d() +
  labs(
    title = "Women in the Labour force along time",
    x = "",
    y = "Rate Working Women",
    size = "Rate Women\nWorking Indoor"
  ) +
  theme_minimal() + 
  guides(
    color = "none",
    shape = "none",
    fill = "none",
    alpha = "none"
  )
ggsave(pl, file = 'figures/time_bubbles.png', width = 10, heigh = 8)



# analyse - unpaid jobs
################################################################################
data_eco1 <- mapping %>% 
  filter(classif2.label %in% c('Economic activity (ISIC-Rev.3.1): Total',
                               'Economic activity (ISIC-Rev.3.1): P. Activities of private households as employers and undifferentiated production activities of private households')) %>% 
  dplyr::left_join(data_eco, by = 'classif2.label') %>% 
  filter(classif1.label == 'Age (Youth, adults): 15+') %>% 
  group_by(classif2.label, country, sex = sex.label, year = time) %>%
  summarise(value = sum(obs_value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(country, sex, year) %>%
  mutate(
    rate_value = {
      num <- value[classif2.label == "Economic activity (ISIC-Rev.3.1): P. Activities of private households as employers and undifferentiated production activities of private households"]
      denom <- value[classif2.label == "Economic activity (ISIC-Rev.3.1): Total"]
      
      if (length(num) == 1 && length(denom) == 1 && denom != 0) {
        num / denom
      } else {
        NA_real_
      }
    }
  ) %>% 
  ungroup() %>% 
  rename_jobs()


data_edu1 <- data_edu %>% 
  filter(classif1.label == 'Age (Youth, adults): 15+',
         classif2.label %in% 
           c("Education (Aggregate levels): Total",
             "Education (Aggregate levels): Basic",
             "Education (Aggregate levels): Intermediate",
             "Education (Aggregate levels): Advanced",
             "Education (Aggregate levels): Level not stated",
             "Education (Aggregate levels): Less than basic")) %>% 
  select(edu.level = classif2.label, country, sex = sex.label, year = time, value = obs_value) %>% 
  group_by(country, sex, year) %>% 
  summarise(ratio_edu_intermidiate = sum(value[edu.level %in% c("Education (Aggregate levels): Advanced",
                                                                "Education (Aggregate levels): Intermediate")] /
                                           value[edu.level == 'Education (Aggregate levels): Total'])) %>% 
  ungroup() %>% 
  distinct()

data <- data_eco1 %>% 
  dplyr::left_join(data_edu1,
                   by = c('country','sex','year')) %>% 
  filter(!is.na(classif2.label)) %>% 
  # percentage of working female from the total working group
  group_by(year,country,classif2.label) %>% 
  mutate(rate_working_w = value[sex == "Female"] / value[sex == "Total"]) %>% 
  ungroup() 

data_final <- merge(
  data %>% 
    filter(sex == 'Female',classif2.label == 'Total') %>% 
    select(country,year,ratio_edu_intermidiate_F = ratio_edu_intermidiate) %>% 
    distinct(),
  data %>%
    filter(sex == 'Female',classif2.label == 'Households') %>%
    select(country,year,rate_household_W = rate_value) %>%
    distinct(),
  by = c('country','year')
) %>% 
  rename_countries() %>% 
  merge(data_gdp,
        by = c('country','year'))



# single year
pl <- ggplot(data_final %>% 
               filter(year == 2006), aes(
                 x = ratio_edu_intermidiate_F,
                 y = rate_household_W,
                 color = gdppc,
                 label = country
               )) +
  geom_point(alpha = 0.6, size = 3) +
  geom_text(vjust = -0.8, size = 3) +
  scale_size_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_viridis_c(labels = function(x) paste0(x, " US$")) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Education brings women out of household work",
    x = "Ratio Women with at least Intermediate Education",
    y = "Rate Household Women",
    color = 'GDP per capita'
  ) +
  theme_minimal()
ggsave(pl, file = 'figures/2006_bubble_paindVSunpaid.png', width = 10, heigh = 8)
