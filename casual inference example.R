
rm(list = ls())
library(openxlsx)
library(ipw)
library("cobalt")
library(survey)
library("boot")
library(dplyr)
library(dagitty)
library(ggdag)
library(tidyverse)
library(halfmoon)###半月图
library(propensity)###逆概率加权
library(causaldata)
library(estimatr)
library(ggplot2)

df<-read.xlsx('hos_data_no_imputation.xlsx',sheet = 1)


df <- df %>% 
  mutate(label_f =factor(label))

df |>
  group_by(label_f) |> 
  summarize(
    mean_sofa_change = mean(sofa), 
    sd = sd(sofa),
    .group ='1'
  )


df|>
  ggplot(aes(sofa, fill = label_f)) +
  geom_density(color = NA, alpha = .8)+
  theme_minimal()


propensity_model <- glm(
  label ~ ethnicity + admission_age, 
  family = binomial(), 
  data = df
)

df <- propensity_model |>
  broom::augment(type.predict = "response", data = df) |>
  mutate(wts = wt_ate(.fitted, label))

df |>
  select(label, .fitted, wts) |>
  head()


ggplot(df, aes(.fitted)) +
  geom_mirror_histogram(aes(fill = label_f), bins = 50) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")+
  theme_minimal()


ggplot(df, aes(.fitted)) +
  geom_mirror_histogram(aes(group = label_f), bins = 50) +
  geom_mirror_histogram(
    aes(fill = label_f, weight = wts),
    bins = 50,
    alpha = .5 ) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")+
  theme_minimal()


plot_df <- tidy_smd(
  df,
  c(sofa),
  .group = label,.wts = wts)


ggplot( plot_df,aes(x = abs(smd),
                    y = variable,group = method,color = method)) +
  geom_love()+
  theme_minimal()


df |>
  ggplot(aes(x=x))+
  geom_density(aes(x = wts, y = ..density..),fill= "#404080")+
  theme_minimal()



ipw_model <- lm( 
  label ~ sofa,
  data = df, 
  weights = wts) 


ipw_estimate <- ipw_model %>% 
  broom::tidy(conf.int = TRUE) %>% 
  filter(term == "sofa")

ipw_estimate


#sofa
tidy_ggdag<-dagify(label~sofa,admission_age~label,admission_age~sofa,ethnicity~sofa,ethnicity~label)%>%tidy_dagitty()

ggdag(tidy_ggdag) +
  theme_dag()


