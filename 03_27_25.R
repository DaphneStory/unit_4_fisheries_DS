#DPS

#0.3.27.25

library(AER)
library(tidyverse)

load('data/RAMLDB v4.66/R Data/Dbdata[asmt][v4.66].RData')

collapse = timeseries_values_views %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_match_catch = cummax(TCbest),
         current_collapse = TCbest < 0.1 * historical_match_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()


source('build_collapse_table.r')


glimpse(collapse)


glimpse(metadata)
unique(metadata$FisheryType)


## join metadata to collapse table

model_data = collapse %>%
  group_by(stockid) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(metadata) %>%
  mutate(FisheryType = as.factor(FisheryType)) # allows for fish types to be viewd as the same

dim(collapse)
length(unique(collapse$stockid))
dim(model_data)

glimpse(model_data)

##run model

model_l = glm(ever_collapsed ~ FisheryType, data=model_data, family="binomial")
summary(model_l)

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
##allows us to see intercept acting as a place holder for flatfish

library(pscl)
pscl::pR2(model_l)


###Make Predictions

##FisheryType = unique(model_data$FisheryType)

FisheryType = model_data %>% distinct(FisheryType)
model_l_predict = predict(model_l, newdata = FisheryType, se.fit=T, type="response")


collapse_fishery_type_predictions = cbind(FisheryType, model_l_predict)

##Plot
ggplot(data = collapse_fishery_type_predictions) +
  geom_bar(aes(x=FisheryType, y=fit, fill=FisheryType), 
           stat="identity", show.legend = F) +
  geom_errorbar(aes(ymin = fit -se.fit, ymax = fit +se.fit, x=FisheryType), width=0.2) +
  ylab("Probability a stock will collapse") +
  coord_flip()


##Poisson Model


glimpse(timeseries_values_views)

## pull out BdivBmsypref, Biomass divided by B maximum sustaiable yield 
## and UdivUmsypref

u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = sum(UdivUmgtpref > 1) / yrs_data,
            ratio_yrs_high_stock = sum(BdivBmgtpref > 1) / yrs_data) %>%
  ungroup() %>%
  left_join(metadata %>% select(stockid, FisheryType))

head(u_summary)


collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary)
head(collapse_summary)

##Zero problem
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)

###truncate data
collapse_summary_zero_trunc = collapse_summary %>%
  filter(yrs_collapsed >0)


model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_high_stock + FisheryType,
              offset(log(yrs_data)), data = collapse_summary_zero_trunc, family="poisson")
summary(model_p)

AER::dispersiontest(model_p)

model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_high_stock + FisheryType,
              offset(log(yrs_data)), data = collapse_summary_zero_trunc, family="quasipoisson")
summary(model_qp)
