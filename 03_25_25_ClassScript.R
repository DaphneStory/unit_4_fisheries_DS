#DPS

#03.25.25

#RAM


library(tidyverse)
load('data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData')

head(timeseries)
dim(timeseries)
glimpse(tsmetrics)


##join timeseries and tsmetrics so metadata is next to data

timeseries_tsmetrics = timeseries %>%
  left_join(tsmetrics, by=c("tsid" = "tsunique"))

dim(timeseries)
dim(timeseries_tsmetrics)
glimpse(timeseries_tsmetrics)

glimpse(timeseries_values_views)
glimpse(taxonomy)
glimpse(stock)


fish = timeseries_values_views %>%
  left_join(stock, by=c("stockid", "stocklong")) %>%
  left_join(taxonomy, by=c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)
glimpse(fish)

head(fish)

dim(fish)  
dim(timeseries_values_views)

ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stocklong),
            data=fish %>% filter(TCbest>3e6))
  #theme(legend.position = "none")


##Cod Collapse

cod_can = fish %>%
  filter(scientificname =="Gadus morhua",
         region =="Canada East Coast", 
         !is.na(TCbest))
unique(cod_can$region)
head(cod_can)
dim(cod_can)


ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stocklong), data=cod_can)


cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch_mt = sum(TCbest))
head(cod_can_total)

ggplot() +
  geom_line(aes(x=year, y=total_catch_mt), data=cod_can_total)


dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
dat_max
dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch_mt), 
         collapse = total_catch_mt <= 0.1*historical_max_catch)
tail(cod_collapse)

cod_collapse_yr = cod_collapse %>% 
  filter(collapse == T) %>%
  summarize(year = min(year)) %>%
  pull(year)

class(cod_collapse_yr)


ggplot() +
  geom_line(aes(x=year, y=total_catch_mt, color=collapse), 
            data=cod_collapse) +
  geom_vline(xintercept = cod_collapse_yr)

collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_match_catch = cummax(TCbest),
         current_collapse = TCbest < 0.1 * historical_match_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

glimpse(collapse)


collapse_year = collapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet==T) %>%
  summarize(first_collapse_yr = min(year)) %>%
  ungroup()
glimpse(collapse_year)

##how many stocks collapsed that year
ggplot() +
  geom_histogram(aes(x=first_collapse_yr),
                 binwidth = 1,
                 fill="salmon",
                 color="black",
                 data=collapse_year)

n_stocks=length(unique(collapse$stockid))

collapse_ts = collapse_year %>%
  count(first_collapse_yr) %>%
  mutate(cum_first_collapse_yr = cumsum(n),
         ratio_collapsed_yet = cum_first_collapse_yr /n_stocks)

head(collapse_ts)

ggplot() +
  geom_line(aes(x=first_collapse_yr, y=cum_first_collapse_yr), 
            data=collapse_ts)

ggplot() +
  geom_line(aes(x=first_collapse_yr, y=ratio_collapsed_yet), 
            data=collapse_ts)
