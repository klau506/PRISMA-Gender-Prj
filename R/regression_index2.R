gc()
rm(list=ls())

library(rlang)
library(readxl)
library(dplyr)
library(modelsummary)
library(tinytable)


setwd('C:/Users/Mansi/OneDrive - unive.it/Desktop/PRISMA')
data1 <- read_excel("HDR25_Statistical_Annex_GII_Table.xlsx")
data2 <- read.csv("EMP_TEMP_SEX_AGE_EDU_NB_A-20250709T1738.csv")

names(data1) <- c('rank', 'country', 'index', 'F', 'M', 'M/F', 'education')  # Converts names to syntactically valid R names
data1 = data1 %>% 
  dplyr::mutate(F_vs_total = F / (F + M))

head(data1)
data1$education <- as.numeric(data1$education)

data2= data2 %>%
dplyr::filter(classif1.label=='Age (Youth, adults): 15+', classif2.label%in% c('Education (Aggregate levels): Total', 'Education (Aggregate levels): Intermediate', 'Education (Aggregate levels): Advanced'))
data2= data2 %>%
  dplyr::filter(sex.label=='Female')


education_summary <- data2 %>%
  group_by(country=ref_area.label, time) %>%
  summarise(
    education = sum(obs_value[classif2.label %in% c(
      'Education (Aggregate levels): Intermediate',
      'Education (Aggregate levels): Advanced')]) /
      sum(obs_value[classif2.label %in% c('Education (Aggregate levels): Total')]),
    .groups = 'drop')

education_summary=education_summary%>%
dplyr::filter(time=='2006')


# Dataset per modelli che controllano per education (rimuove NA in education)
#data_with_edu <- data[!is.na(data$education), ]
data_with_edu = merge(data1, education_summary, by=c('country'))

data_with_edu=data_with_edu[c(1:5, 8:10)]
names(data_with_edu)[names(data_with_edu) == "education.y"] <- "education"


#Nessun Controllo per education
model1 <- lm(`F_vs_total` ~ index, data = data_with_edu)
summary(model1)

#Controllo per education
model2 <- lm(`F_vs_total` ~ index + education, data = data_with_edu)
summary(model2)

#Interazione tra index e education
model3 <- lm(`F_vs_total` ~ index * education, data = data_with_edu)
summary(model3)


model4 <- lm(`F_vs_total` ~ education, data = data_with_edu)
summary(model4)


#check robustness: Multicollinearità (solo per modelli con education)
library(car)
vif(model2)  # Se VIF > 5, problemi di correlazione tra predittori
vif(model1)  # Se VIF > 5, problemi di correlazione tra predittori



# Etichette più leggibili
names <- c(
  "index" = "Index",
  "education" = "Education",
  "index:education" = "Index × Education",
  "(Intercept)" = "Intercept")

# Tabella comparativa
modelsummary(
  list("Model 1" = model1, "Model 2" = model2, "Model 3" = model4),
  coef_map = names,
  statistic = "({std.error})",     # mostra errore standard
  stars = TRUE,                    # aggiunge * per la significatività
  gof_omit = "IC|Log|RMSE"    )     # omette AIC, BIC ecc., personalizzabile



library(ggplot2)

# Per Model 1: semplice regressione index vs M/F
ggplot(data_with_edu, aes(x = index, y = `F_vs_total`)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  coord_cartesian(ylim = c(NA, 0.6)) +  
  labs(title = "Model 1: Female/total ~ index",
       x = "Gender ineq. index", y = "Rate Working Woman") +
  theme_minimal()
ggsave(file='model_1B.png', width = 10, height =6)


# Per Model 2: controllo per education
ggplot(data_with_edu, aes(x = index, y = `F_vs_total`, color = education)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE,  color = "green") +
  coord_cartesian(ylim = c(NA, 0.6)) +  
  labs(title = "Model 2: Female/total ~ index + education",
       x = "Gender ineq. index", y = "Rate Working Woman") +
  theme_minimal()
ggsave(file='model_2B.png', width = 10, height =6)




# single year
pl <- ggplot(data_with_edu,
               aes(
                 x = F_vs_total,
                 y = index,
                 color = country,
                 label = country
               )) +
  geom_point(alpha = 0.6) +
  geom_text(vjust = -0.8, size = 3) +
  scale_x_continuous(breaks = seq(0, 0.6, by = 0.05),) +
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05),) +
  labs(
    title = "Gender inequelity index and relation with women at work",
    x = "Women at work versut total labour force",
    y = "Gender Inequality Index",
    size = "Rate Working Women",
  ) +
  theme_minimal()+
theme(legend.position = "none")
pl
ggsave(pl, file = 'index_vs_F_vs_total.png', width = 14, heigh = 8)




