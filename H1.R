library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
rm(list = ls())

setwd("C:/Users/nehak/OneDrive - University College London/Uni/Dissertation")
df <- read.csv("dictionary.csv")

# Standardise and dates
df$Date <- as.Date(df$Date, tryFormats = c("%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y"))

# time periods and create metrics
cut_date <- as.Date("2020-01-01")
df <- df %>%
  mutate(
    period = ifelse(Date < cut_date, "2014-2019", "2020 onward"),
    threat_based = national_security + crime + threat_lang,
    risk_based = privacy_rights + tech_regulation + inclusion_equity,
    total_cyber = threat_based + risk_based,
    threat_prop = ifelse(total_cyber > 0, threat_based / total_cyber, NA),
    risk_prop = ifelse(total_cyber > 0, risk_based / total_cyber, NA),
    year = year(Date)
  )

# Means by period
means <- df %>%
  group_by(period) %>%
  summarise(
    mean_threat = mean(threat_prop, na.rm = TRUE),
    mean_risk = mean(risk_prop, na.rm = TRUE),
    n = n()
  )
print(means)

# T-test
t_test <- t.test(threat_prop ~ period, data = df)
print(t_test)

# Yearly average proportions for plotting
yearly <- df %>%
  group_by(year) %>%
  summarise(
    mean_threat = mean(threat_prop, na.rm = TRUE),
    mean_risk = mean(risk_prop, na.rm = TRUE)
  )

# plot: yearly averages for threat-based & risk-based discourse
ggplot(yearly, aes(x = year)) +
  geom_line(aes(y = mean_threat, color = "Threat-based"), size = 1) +
  geom_point(aes(y = mean_threat, color = "Threat-based"), size = 2) +
  theme_minimal() +
  labs(x = "Year",
    y = "Mean Proportion",
    colour = "Key"
  )

# Standard deviations for each period
sd(df$threat_prop[df$period == "2014-2019"], na.rm = TRUE)
sd(df$threat_prop[df$period == "2020 onward"], na.rm = TRUE)

# Means for each period
mean(df$threat_prop[df$period == "2014-2019"], na.rm = TRUE)
mean(df$threat_prop[df$period == "2020 onward"], na.rm = TRUE)

# Sample sizes for each period
length(df$threat_prop[df$period == "2014-2019" & !is.na(df$threat_prop)])
length(df$threat_prop[df$period == "2020 onward" & !is.na(df$threat_prop)])

##1.2

# Descriptive statistics for total cybersecurity volume by period
mean(df$total_cyber[df$period == "2014-2019"], na.rm = TRUE)
sd(df$total_cyber[df$period == "2014-2019"], na.rm = TRUE)
length(df$total_cyber[df$period == "2014-2019" & !is.na(df$total_cyber)])

mean(df$total_cyber[df$period == "2020 onward"], na.rm = TRUE)
sd(df$total_cyber[df$period == "2020 onward"], na.rm = TRUE)
length(df$total_cyber[df$period == "2020 onward" & !is.na(df$total_cyber)])

# T-test total volume between periods
t.test(total_cyber ~ period, data = df)

# yearly summary data
yearly_summary <- df %>%
  group_by(year) %>%
  summarise(
    total_volume = sum(total_cyber, na.rm = TRUE),
    mean_threat = mean(threat_prop, na.rm = TRUE),
    .groups = 'drop'
  )

print(yearly_summary)

# Dual-axis plot  volume and threat proportion
ggplot(yearly_summary, aes(x = year)) +
  geom_line(aes(y = total_volume), color = "blue", size = 1) +
  geom_point(aes(y = total_volume), color = "blue", size = 2) +
  geom_line(aes(y = mean_threat * max(yearly_summary$total_volume, na.rm = TRUE)), 
            color = "red", size = 1) +
  geom_point(aes(y = mean_threat * max(yearly_summary$total_volume, na.rm = TRUE)), 
             color = "red", size = 2) +
  scale_y_continuous(
    name = "Total Cybersecurity Discourse Volume",
    sec.axis = sec_axis(~ . / max(yearly_summary$total_volume, na.rm = TRUE), 
                        name = "Mean Threat Discourse Proportion")
  ) +
  labs(x = "Year", 
       title = "") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )


