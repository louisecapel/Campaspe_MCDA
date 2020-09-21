# This file uses code taken from "dam_level_unemployment.R"
# Code is specifically related to the creation of the models used for this study
#
# Models with increased complexity which produced worse fits or those that used 
# variables not considered (e.g. crop sold) were not included.
# 
# A few models with more complexity than needed were included for comparison

set.seed(76)

source("src/package_loading.R")

#data required for the modeling
dl_unempt <- read.csv(("data/mthly_dam_levels_unempt.csv"), na.strings = "NA")
grain <- read.csv("data/yearly_grain_production.csv")

#interpolating unemployment rates for missing values and setting date formats
dl_unempt$unemployment_rate <- na.approx(dl_unempt$unemployment_rate)
dl_unempt$date <- as.Date(dl_unempt$date, "%d/%m/%y")

# Creating lagged dam level variables
dl_unempt$avg_level_6m_lag <- Lag(dl_unempt$avg_level, shift = 6)
dl_unempt$avg_level_12m_lag <- Lag(dl_unempt$avg_level, shift = 12) 
dl_unempt$avg_level_24m_lag <- Lag(dl_unempt$avg_level, shift = 24)


# Subset of data based on change in census collection method
dl_unempt_Loddon <- filter(dl_unempt, date <= as.Date("1997-08-31")) 
dl_unempt_Goulburn <- filter(dl_unempt, date >= as.Date("1997-09-01"))

summary(dl_unempt_Goulburn)
summary(dl_unempt_Loddon)

# Test correlation

cor.test(dl_unempt_Loddon$avg_level, dl_unempt_Loddon$unemployment_rate, method = "pearson")
cor.test(dl_unempt_Goulburn$avg_level, dl_unempt_Goulburn$unemployment_rate, method = "pearson")
cor.test(dl_unempt$avg_level, dl_unempt$unemployment_rate, method = "pearson")


### Generate Regressions

# mdl_lagged_dl <- lm(unemployment_rate ~ avg_level, 
#                     data = dl_unempt)
# # Adj. R-squared = 0.2422
# 
# 
# mdl_lagged_2yr_dl <- lm(unemployment_rate ~ avg_level + avg_level_12m_lag + avg_level_24m_lag, 
#                  data = dl_unempt)
# # Adj. R-squared = 0.2996
# 
# 
# #using only the data from when Campaspe was included in the Goulburn region. 
# unempt_Goulburn_OLS <- lm(unemployment_rate ~ avg_level, data = dl_unempt_Goulburn)
# # Adj. R-squared = 0.2131



# Including yield data
#Filtered to 1990 because that is the year from which grain yield data is available

yearly_dl_unempt <- filter(dl_unempt, date >= as.Date("1990-01-01") & date <= as.Date("2013-12-01"))

yearly_dl_unempt$year <- format(as.Date(yearly_dl_unempt$date, format = "%d/%m/%y"), "%Y") 
yearly_dl_unempt$month <- format(as.Date(yearly_dl_unempt$date, format = "%d/%m/%y"), "%m")

yr_dl_unempt <- aggregate(cbind(avg_level, unemployment_rate) ~ year, 
                          yearly_dl_unempt, mean)
yr_dl_unempt$wheat_produced <- grain$wheat_produced
yr_dl_unempt$canola_produced <- grain$canola_produced
yr_dl_unempt$barley_produced <- grain$barley_produced
yr_dl_unempt$avg_level_12m_lag <- Lag(yr_dl_unempt$avg_level, shift = 1)
yr_dl_unempt$avg_level_24m_lag <- Lag(yr_dl_unempt$avg_level, shift = 2)
yr_dl_unempt$wheat_produced_12m_lag <- Lag(yr_dl_unempt$wheat_produced, shift = 1)
yr_dl_unempt$wheat_produced_24m_lag <- Lag(yr_dl_unempt$wheat_produced, shift = 2)
yr_dl_unempt$canola_produced_12m_lag <- Lag(yr_dl_unempt$canola_produced, shift = 1)
yr_dl_unempt$canola_produced_24m_lag <- Lag(yr_dl_unempt$canola_produced, shift = 2)
yr_dl_unempt$barley_produced_12m_lag <- Lag(yr_dl_unempt$barley_produced, shift = 1)
yr_dl_unempt$barley_produced_24m_lag <- Lag(yr_dl_unempt$barley_produced, shift = 2)
yr_dl_unempt$avg_produced <- rowMeans(yr_dl_unempt[, 5:7])
yr_dl_unempt$avg_produced_12m_lag <- Lag(yr_dl_unempt$avg_produced, shift = 1)
yr_dl_unempt$avg_produced_24m_lag <- Lag(yr_dl_unempt$avg_produced, shift = 2)


# Base models for comparison
mdl_dl_no_crop <- lm(unemployment_rate ~ avg_level,
                       data = yr_dl_unempt)
summary(mdl_dl_no_crop)
#R-squared = 0.422

mdl_dl_lagged_no_crop <- lm(unemployment_rate ~ avg_level + avg_level_12m_lag + avg_level_24m_lag,
                     data = yr_dl_unempt)
summary(mdl_dl_lagged_no_crop)
# R-squared = 0.5051

mdl_poly_dl <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, raw=TRUE),
                   data = yr_dl_unempt)
summary(mdl_poly_dl)
# R-squared = 0.6742


# More complex models

mdl_2yr_poly_wheat = lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 4, 
                                   raw=TRUE) 
          + poly(wheat_produced * wheat_produced_12m_lag * wheat_produced_24m_lag, 4, raw = TRUE), 
          data = yr_dl_unempt)
summary(mdl_2yr_poly_wheat)
# R-squared = 0.9232


mdl_2yr_poly_crops <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, 
                                     raw=TRUE) 
            + poly(wheat_produced * wheat_produced_24m_lag, 3, raw = TRUE)
            + poly(canola_produced * canola_produced_24m_lag, 3, raw = TRUE)
            + poly(barley_produced * barley_produced_24m_lag, 3, raw = TRUE), 
            data = yr_dl_unempt)
summary(mdl_2yr_poly_crops)
# R-squared = 0.9447 - 9 D.F.

mdl_2yr_poly_avg_crops <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, raw=TRUE) 
            + poly(avg_produced * avg_produced_12m_lag * avg_produced_24m_lag, 3, raw = TRUE), 
            data = yr_dl_unempt)
summary(mdl_2yr_poly_avg_crops)
# R-squared = 0.9328 14 D.F.  p<0.001


mdl_2yr_avg_crops <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, 
                                                       raw=TRUE) 
                              + avg_produced + avg_produced_12m_lag + avg_produced_24m_lag, 
            data = yr_dl_unempt)
summary(mdl_2yr_avg_crops)
# R-squared = 0.906 14 D.F. p<0.001



mdl_2yr_wheat <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, raw=TRUE) 
            + wheat_produced + wheat_produced_12m_lag + wheat_produced_24m_lag, 
            data = yr_dl_unempt)
summary(mdl_2yr_wheat)
# R-squared = 0.8943 15 D.F. p<0.001


mdl_2yr_barley <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, raw=TRUE) 
            + barley_produced + barley_produced_12m_lag + barley_produced_24m_lag, 
            data = yr_dl_unempt)
summary(mdl_2yr_barley)
# R-squared = 0.8733


mdl_2yr_canola <- lm(unemployment_rate ~ poly(avg_level * avg_level_12m_lag * avg_level_24m_lag, 3, raw=TRUE) 
            + canola_produced + canola_produced_12m_lag + canola_produced_24m_lag, 
            data = yr_dl_unempt)
summary(mdl_2yr_canola)
# R-squared = 0.9293 15 D.F p < 0.001

model.frame(mdl_2yr_poly_avg_crops)

#Plot model results against historical data

#Extract fitted values and add NA to the first 3 values as the model is lagged and therefore
#the fitted values and historic values are different lengths 

fitted_values <- fitted(mdl_2yr_poly_avg_crops)
fitted_values <- as.numeric(fitted_values)
fitted_values <- c(rep(NA, 3), fitted_values)

historic_unemployment <- yr_dl_unempt$unemployment_rate
year <- as.numeric(yr_dl_unempt$year)

model_comparison <- as.data.frame(cbind(year, historic_unemployment, fitted_values))

model_plot <- ggplot(model_comparison, aes(year, fitted_values)) +
  geom_line(linetype = "dashed") + 
  geom_line(aes(year, historic_unemployment)) +
  xlab("Year") + ylab("Unemployment rate") + 
  ggtitle("Historic and fitted unemployment values")
    
model_plot
