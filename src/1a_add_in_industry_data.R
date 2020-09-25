#This code adds in industry data relating to the Shepparton region

source("src/common.R")

#data required for the modelling
yr_dl_unempt <- read.csv(("data/yr_dl_unempt.csv"), na.strings = "NA")
shep_industry <- read.csv(("data/Shepparton_industry_data.csv"), na.strings = "NA")


#aggregating quarterly and monthly data to yearly and combining datasets

shep_industry_yr <- aggregate(shep_industry, by = list(shep_industry$Year), FUN = mean)
shep_industry_yr$Year <- as.numeric(shep_industry_yr$Year)
shep_industry_yr <- filter(shep_industry_yr, Year <= 2013)

dl_unempt_from_1999 <- filter(yr_dl_unempt, year >= 1999)
shep_industry_yr <- cbind(shep_industry_yr, dl_unempt_from_1999)


#Generate regressions
mdl_ag_no_crop <- lm(Ag.FT ~ Avg_level, data = shep_industry_yr)
summary(mdl_ag_no_crop)
#R-square = 0.2

mdl_ag_dl_avg_crop <- lm(Ag.FT ~ Avg_level + avg_produced, data = shep_industry_yr)
summary(mdl_ag_dl_avg_crop)
#R-square = 0.5375 12 D.F. 

