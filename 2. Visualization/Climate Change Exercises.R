library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Q1.

temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

temp_carbon

# Q2.

a <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year, carbon_emissions)

b <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

c <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

d <- which(a$year == b)
e <- which(a$year == c)

a$carbon_emissions[d] / a$carbon_emissions[e] 

# Q3.

a <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year, temp_anomaly)

b <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

c <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

d <- which(a$year == b)
e <- which(a$year == c)

a$temp_anomaly[d] - a$temp_anomaly[e] 

# Q4.

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year,temp_anomaly)) +
  geom_point() 

p + geom_hline(aes(yintercept = 0), col = "blue")

# Q5.

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# Q6.

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year,temp_anomaly, label = year)) +
  geom_point() +
  geom_label()

p + geom_hline(aes(yintercept = 0), col = "blue")

# Q7.

colorblind_palette <- c("black", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")

temp_carbon %>%
  filter(!is.na(temp_anomaly),!is.na(land_anomaly),!is.na(ocean_anomaly)) %>%
  select(Year = year, Global = temp_anomaly, Land = land_anomaly, Ocean = ocean_anomaly) %>%
  gather(Region, Temp_anomaly, Global:Ocean) %>%
  ggplot(aes(Year, Temp_anomaly, col = Region)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = 0), col = colorblind_palette[8], lty = 2) +
  geom_label(aes(x = 2005, y = -.08), col = colorblind_palette[8],label = "20th century mean", size = 4) +
  ylab("Temperature anomaly (degrees C)") +
  scale_color_manual(values = colorblind_palette) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")

# Q8.

greenhouse_gases

greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Q9.

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year,carbon_emissions, label = year)) +
  geom_line() +
  geom_vline(xintercept = 2014) +
  geom_vline(xintercept = 1970)
  
# Q11.

historic_co2 %>%
  ggplot(aes(year,co2)) +
  geom_line()

# Q12.

historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-800000,-775000) +
  geom_hline(yintercept = 275)

historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-375000,-330000) +
  geom_hline(yintercept = 180) +
  geom_hline(yintercept = 300)
  
historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-140000,-120000) +
  geom_hline(yintercept = 200) +
  geom_hline(yintercept = 280)

historic_co2 %>%
  ggplot(aes(year,co2,color=source)) +
  geom_line() +
  xlim(-1700,2018) +
  geom_hline(yintercept = 275) +
  geom_hline(yintercept = 400)