gc()
rm(list=ls())

library(rlang)
library(readxl)
library(dplyr)
library(modelsummary)
library(tinytable)
 

setwd('C:/Users/Mansi/OneDrive - unive.it/Desktop/PRISMA')
data <- read_excel("HDR25_Statistical_Annex_GII_Table.xlsx")

names(data) <- c('rank', 'country', 'index', 'F', 'M', 'M/F', 'education')  # Converts names to syntactically valid R names

head(data)
data$education <- as.numeric(data$education)

data = data %>% 
  dplyr::mutate(F_vs_total = F / (F + M))

# Dataset per modelli che controllano per education (rimuove NA in education)
data_with_edu <- data[!is.na(data$education), ]


#Nessun Controllo per education
model1 <- lm(`F_vs_total` ~ index, data = data)
summary(model1)

#Controllo per education
model2 <- lm(`F_vs_total` ~ index + education, data = data_with_edu)
summary(model2)

#Interazione tra index e education
model3 <- lm(`F_vs_total` ~ index * education, data = data_with_edu)
summary(model3)
 

model4 <- lm(`F_vs_total` ~ education, data = data)
summary(model4)


#check robustness: Multicollinearità (solo per modelli con education)
library(car)
vif(model2)  # Se VIF > 5, problemi di correlazione tra predittori
vif(model3)  # Se VIF > 5, problemi di correlazione tra predittori


# Assicurati di avere questi pacchetti
install.packages("modelsummary")
install.packages('tinytable')


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
ggplot(data, aes(x = index, y = `F_vs_total`)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  coord_cartesian(ylim = c(NA, 0.6)) +  
  labs(title = "Model 1: Female/total ~ index",
       x = "Gender ineq. index", y = "Rate Working Woman") +
  theme_minimal()
ggsave(file='model_1.png', width = 10, height =6)


# Per Model 2: controllo per education
ggplot(data_with_edu, aes(x = index, y = `F_vs_total`, color = education)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE,  color = "green") +
  coord_cartesian(ylim = c(NA, 0.6)) +  
  labs(title = "Model 2: Female/total ~ index + education",
       x = "Gender ineq. index", y = "Rate Working Woman") +
  theme_minimal()
ggsave(file='model_2.png', width = 10, height =6)








