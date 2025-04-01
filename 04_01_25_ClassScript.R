#DPS

#4.1.25
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(GGally)

##Generate predictions on new data and plot 



median_ratio_yrs_high_stock = median(collapse_summary_zero_trunc$ratio_yrs_high_stock)

newdata = expand.grid(FisheryType = unique(collapse_summary_zero_trunc$FisheryType),
                      ratio_yrs_high_stock = median_ratio_yrs_high_stock,
                      ratio_yrs_overfished = seq(from=0, to=1, by=0.1))
head(newdata)
##Numbers are different from MG already

model_qp_predict = predict(model_qp, newdata = newdata, type="response", se.fit=T)
head(model_qp_predict)


collapse_time_predictions = cbind(newdata, model_qp_predict)
head(collapse_time_predictions)


ggplot(data=collapse_time_predictions) +
  geom_line(aes(x=ratio_yrs_overfished, y=fit, color=FisheryType)) +
  geom_ribbon(aes(x=ratio_yrs_overfished, ymin = fit-se.fit, ymax=fit+se.fit, fill=FisheryType), 
              alpha =0.25) +
  geom_point(data=collapse_summary_zero_trunc,
             aes(x=ratio_yrs_overfished, y=yrs_collapsed, color=FisheryType)) +
  facet_wrap(~FisheryType)


####PCA

#Principal Component Analysis

library(palmerpenguins)

##pcas work with continuous variables

#first separate continuous variables from categorical

peng_drop_na = penguins %>%
  drop_na()
dim(penguins)
dim(peng_drop_na)
summary(peng_drop_na)

head(peng_drop_na)

peng_num = peng_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(peng_num)
dim(peng_num)


peng_meta = peng_drop_na %>%
  select(species, sex, island, year)
head(peng_meta)

#run pca

peng_pca = prcomp(peng_num, scale. = T, center = T)
peng_pca

class(peng_pca)
typeof(peng_pca)
summary(peng_pca)
str(peng_pca)

summary(peng_pca)$importance[2,]
#OR
(peng_pca$sdev)^2 / sum(peng_pca$sdev^2)


peng_pca$rotation

peng_pca$x ##for peng x, pcX blah blah


plot(peng_pca)


pca_scree = data.frame(pc = c(1:4),
                       var = (peng_pca$sdev)^2 / sum(peng_pca$sdev^2))
pca_scree


ggplot(data=pca_scree, aes(x=pc, y=var)) +
  geom_col() +
  geom_point() +
  geom_line() +
  theme_bw()

## Biplot

peng_pca_meta = cbind(peng_meta, peng_pca$x)
  
ggplot() +
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), data=peng_pca_meta) +
  coord_fixed(ratio=1)

library(ggbiplot)

biplot(peng_pca)
ggbiplot(peng_pca, scale=1, obs.scale=1, groups=peng_meta$species, ellipse=T, 
         choices =c(3,4)) # PC 3 versus PC 4 as opposed to PC1 and PC2


