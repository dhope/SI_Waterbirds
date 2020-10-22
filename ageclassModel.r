### Sidney Island Analysis 2018
# David Hope, based on scripts from
# Anna Drake and Mark Drever
###########################

##  1. Data import and clean ----------------------


require(lme4)
# require(doBy)
# require(MASS)
library(magrittr)
require(tidyverse)
require(lubridate)
require(readxl)
require(lubridate)
source('R/miscFunctions.r')
select <- dplyr::select


## read in data
# Newest data
dat_2017_2018 <- map_df(list.files("data_/", "PeepCounts_Disturbance", full.names = T),
                   read_xls, sheet = "Locations", 
                   col_types=c("date", "date", "text", "text", rep("numeric", 8), "text", "text", "text")) %>% 
  mutate(surveyed = !grepl("not surveyed", NOTES)) %>% 
  filter(AREA!="End") %>% group_by(DATE) %>% 
  summarize_at(.vars = vars(`Wesa-A`, `Wesa-J`, `Wesa-u`, `Lesa-A`, `Lesa-J`, `Lesa-u`, 
                            Unid_Peeps, TOTAL, surveyed), 
               .funs = sum, na.rm=T) %>% 
  dplyr::rename(WESA.ad = `Wesa-A`, WESA.juv = `Wesa-J`,
                WESA.unk =`Wesa-u`, LESA.ad = `Lesa-A`, LESA.juv = `Lesa-J`,
                LESA.unk = `Lesa-u`,UID.W.L = Unid_Peeps, Grand.Total = TOTAL) %>% 
  mutate(
    DateR = as_date(DATE),
    Date = as.character(DateR),
         Year = year(DateR),
         DOY = yday(DateR)) %>% select(-DATE)

sidney <- read_csv('data_/Sidney Island Peep_Counts 1990 to 2016.csv') %>% 
  dplyr::rename(WESA.ad = `WESA ad`, WESA.juv = `WESA juv`,
         WESA.unk = `WESA unk`, LESA.ad = `LESA ad`, LESA.juv = `LESA juv`,
         LESA.unk = `LESA unk`,UID.W.L = `UID W/L`, Grand.Total = `Grand Total`) %>%
  #assign dates and calculate day of year (DOY) and centered DOY (c.DOY)
  mutate(DateR = mdy(Date),
         Year = year(DateR),
         DOY = yday(DateR)) %>% 
  bind_rows(dat_2017_2018) %>% 
  filter(DOY < 255) %>% 
  # # species.age fractions - variable preparation
  mutate( Year.factor = as.factor(Year),
    c.DOY = DOY - diff(range(DOY)) /2,
         non.id = UID.W.L / Grand.Total,
         WESA = WESA.ad + WESA.juv + WESA.unk,
         LESA = LESA.ad + LESA.juv + LESA.unk,
         p.WESA = WESA / (WESA + LESA),
         p.WESA.juv.obs = WESA.juv / (WESA.ad + WESA.juv),
         p.LESA.juv.obs = LESA.juv / (LESA.ad + LESA.juv),
         DOY_rescaled = arm::rescale(DOY)
         ) %>% arrange(Year, DOY)
           

## Summary data for MS --------

nyrs <- n_distinct(sidney$Year)
birds <- sum(sidney$Grand.Total)
percent_id <- (1-sum(sidney$UID.W.L)/birds) * 100
med_id <- (1-median(sidney$UID.W.L / sidney$Grand.Total, na.rm=T)) * 100
rang_id <- (1-range(sidney$UID.W.L / sidney$Grand.Total, na.rm=T)) * 100
ndays <- nrow(sidney)

wesa_aged <- with(sidney[sidney$WESA.ad!=0 | sidney$WESA.juv!=0,], median(WESA.ad + WESA.juv, na.rm=T))
wesa_aged_range <- with(sidney[sidney$WESA.ad!=0 | sidney$WESA.juv!=0,], range(WESA.ad + WESA.juv, na.rm=T))
nrow(sidney[sidney$WESA.ad!=0 | sidney$WESA.juv!=0,])

lesa_aged <- with(sidney[sidney$LESA.ad!=0 | sidney$LESA.juv!=0,], median(LESA.ad + LESA.juv, na.rm=T))
lesa_aged_range <- with(sidney[sidney$LESA.ad!=0 | sidney$LESA.juv!=0,], range(LESA.ad + LESA.juv, na.rm=T))
nrow(sidney[sidney$LESA.ad!=0 | sidney$LESA.juv!=0,])

sd_doy <- sd(sidney$DOY)
mean_doc <- mean(sidney$DOY)

#########  MIXED EFFECTS MODEL FOR ADULT TO JUVENILE COMPOSITION -------------------
#### WESA ############## -------
require(lme4)
lme.wesa <- glmer(  cbind(WESA.juv, WESA.ad)  ~ DOY_rescaled    + (DOY_rescaled  |Year.factor),  family = binomial,
                    data = sidney %>% filter(!Year %in% c(1990,1998,1999,2012, 2013))  )
lme.wesa2 <- glmer(  cbind(WESA.juv, WESA.ad)  ~ DOY_rescaled    + (1  |Year.factor),  family = binomial, 
                     data = sidney %>% filter(!Year %in% c(1990,1998,1999,2012, 2013))  )

#########  MIXED EFFECTS MODEL FOR ADULT TO JUVENILE COMPOSITION #### LESA ##############
lme.LESA <- glmer(  cbind(LESA.juv,LESA.ad)  ~ DOY_rescaled    + (DOY_rescaled  |Year.factor),  family = binomial,
                    data = sidney %>% filter(!Year %in% c(1990,1998,1999,2012, 2013)   ))

#######  CALCULATE SPECIES-SPECIFIC COUNTS #########################

sidney %<>% 
  mutate(
    WESA.est = ifelse(Grand.Total == 0, 0, Grand.Total*p.WESA),
    LESA.est = ifelse(Grand.Total == 0, 0, Grand.Total* (1-p.WESA)),
    p.WESA.juv.pred = predict(lme.wesa, newdata =., type = 'response', allow.new.levels=T),
    p.LESA.juv.pred = predict(lme.LESA, newdata =., type = 'response',  allow.new.levels=T),
    WESA.juv.est = round(WESA.est*p.WESA.juv.pred),
    LESA.juv.est = round(LESA.est*p.LESA.juv.pred), 
    WESA.ad.est = round(WESA.est*(1-p.WESA.juv.pred)),
    LESA.ad.est = round(LESA.est*(1-p.LESA.juv.pred)) )

#export resulting file
write_rds(sidney,"output/Sidney_1990_2018.rds")


#### predicted values for each year , assign means to missing values

newdat <- expand.grid(Year = unique(sidney$Year), DOY = unique(sidney$DOY)) %>% 
  mutate(DOY_rescaled = (DOY - mean_doc ) / 2 / sd_doy, Year.factor = as.factor(Year))
mm<-model.matrix(as.formula("~DOY_rescaled"),newdat)
newdat$p.WESA.juv.pred  <-  predict(lme.wesa, newdata =newdat, type = 'response', allow.new.levels=T)
newdat$p.WESA.juv.pred_noran  <-  predict(lme.wesa, newdata =newdat, type = 'response', allow.new.levels=T, re.form=NA)
newdat$p.LESA.juv.pred  <-  predict(lme.LESA, newdata =newdat, type = 'response', allow.new.levels=T)
newdat$p.LESA.juv.pred_noRan  <-  predict(lme.LESA, newdata =newdat, type = 'response', allow.new.levels=T, re.form=NA)
newdat$y<-logit2prob(mm%*%fixef(lme.LESA) )
newdat$y_wesa<-logit2prob(mm%*%fixef(lme.wesa) )

fullsi <- sidney %>% mutate(.fitted = predict(lme.wesa, newdata = ., type = 'response', allow.new.levels=T),
	.fitted_lesa = predict(lme.LESA, newdata = ., type = 'response', allow.new.levels=T))

require(lubridate)
theme_set(cowplot::theme_cowplot(font_size=12,font_family = "sans")) 
s1_fig_propAJ_wesa <- 
  ggplot(newdat, aes(mdy("01-01-2013") + DOY, p.WESA.juv.pred, group = Year) ) + 
  # geom_density(data = sidney, aes(x=DOY,y=Grand.Total))+
  geom_line(aes(y=p.WESA.juv.pred_noran), linetype=2, alpha=1) + 
  geom_line() +
  geom_point(data = fullsi, aes(y = .fitted  ), shape = 19, alpha = 0.2) +
  geom_point(data = sidney, aes(y = p.WESA.juv.obs  )) +
  

  # geom_vline(xintercept = 213) +
  facet_wrap(~Year, nrow = 9) +
  labs( title = 'Western Sandpipers',
        x = 'Date', y = 'Proportion of Juvenile Western Sandpiper in Counts')  +
  guides(x = guide_axis(angle=45))

ggsave("output/S1_WESA_AJ.pdf",s1_fig_propAJ_wesa , width = 12, height = 8, units = 'in')
ggsave("output/A1_1_WESA_AJ.png",s1_fig_propAJ_wesa  , 
       width = 16, height = 32, units = 'cm', dpi = 600)
ggsave("output/A1_1_WESA_AJ.tiff",s1_fig_propAJ_wesa , 
       width = 16, height = 32, units = 'cm', dpi = 300)


# bootImport <- read_rds("output/fig2dat_w_boot.rds")
# newdat <- bootImport$newdat
# fullsi <- bootImport$fullsi

s2_fig_propAJ_lesa <- 
  ggplot(newdat, aes(mdy("01-01-2013") + DOY, p.LESA.juv.pred, group = Year) ) + 
  geom_line(aes(y=p.LESA.juv.pred_noRan),alpha= 1, linetype=2) + #geom_vline(xintercept = 213)+
  geom_line() +
  geom_point(data = fullsi, aes(y = .fitted_lesa  ), shape = 19, alpha = 0.2) +
  geom_point(data = sidney, aes(y = p.LESA.juv.obs  )) +
 
  facet_wrap(~Year, nrow = 9) +
  labs( title = 'Least Sandpipers',
        x = 'Date', y = 'Proportion of Juvenile Least Sandpiper in Counts')+
  guides(x = guide_axis(angle=45))

ggsave("output/S2_LESA_AJ.pdf",s2_fig_propAJ_lesa + cowplot::theme_cowplot(), 
       width = 12, height = 8, units = 'in')
ggsave("output/A1_2_LESA_AJ.png",s2_fig_propAJ_lesa , 
       width = 16, height = 32, units = 'cm', dpi = 600)
ggsave("output/A1_2_LESA_AJ.tiff",s2_fig_propAJ_lesa , 
       width = 16, height = 32, units = 'cm', dpi = 600)

## Calculate Bootstrapped CI -----
#we have to define a function that will be applied to the nsim simulations
# #here we basically get a merMod object and return the fitted values
# bb_wesa<-bootMer(lme.wesa,FUN=predFun,nsim=200) #do this 200 times
# #as we did this 200 times the 95% CI will be bordered by the 5th and 195th value
# bb_se_wesa<-apply(bb_wesa$t,2,function(x) x[order(x)][c(5,195)])
# newdat$blo_wesa <-logit2prob(bb_se_wesa[1,])
# newdat$bhi_wesa <-logit2prob(bb_se_wesa[2,])
# 
# bb<-bootMer(lme.LESA,FUN=predFun,nsim=200) #do this 200 times
# #as we did this 200 times the 95% CI will be bordered by the 5th and 195th value
# bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)])
# newdat$blo<-logit2prob(bb_se[1,])
# newdat$bhi<-logit2prob(bb_se[2,])
# 
# write_rds(newdat , "output/ageclasspred.rds")


