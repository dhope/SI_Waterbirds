## Figures for Sidney Island Analysis 2018
# David Hope, based on scripts from
# Anna Drake and Mark Drever
###########################
require(tidyverse)
require(cowplot)
library(patchwork)
require(lubridate)
source("generatePredictions.r")
# theme_set(theme_cowplot(font_size=8))
theme_set(theme_cowplot(font_size=12,font_family = "sans")) 
# source("ageclassModel.r")
newdat <- read_rds("output/ageclasspred.rds")
# Final Figure 2 ---- 
 #plot_grid(
WESA_AJ <-   
  ggplot(newdat, aes(DOY + mdy("1-1-2013"), y_wesa )) + 
    geom_ribbon(aes(ymin=blo_wesa, ymax=bhi_wesa), alpha = 0.2)+
    geom_line( aes(y=y_wesa)) +
  stat_summary(fun.data = 'mean_cl_boot', aes(y = p.WESA.juv.obs), alpha = 0.3,
               na.rm=T,position = position_nudge(x = 0.1),
               data =sidney %>% filter(!Year %in% c(1990,1998,1999,2012, 2013) ) ) +
  labs(y = "Proportion of juveniles", x="", title = "        Western Sandpipers", subtitle = "(A)") +
    coord_cartesian(xlim = c(mdy("7-15-2013"), mdy("8-15-2013"))) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1))  
LESA_AJ <- 
   ggplot(newdat, aes(DOY + mdy("1-1-2013"), y )) + 
    geom_ribbon(aes(ymin=blo, ymax=bhi), alpha = 0.2)+
    geom_line() +
    stat_summary(fun.data = 'mean_cl_boot',  aes(y = p.LESA.juv.obs), alpha = 0.3,
                 na.rm=T,position = position_nudge(x = -0.1),
                 data =sidney %>% filter(!Year %in% c(1990,1998,1999,2012, 2013) ) ) +

    labs(y = "Proportion of juveniles", x ="",title="        Least Sandpipers", subtitle = "(B)") +
  coord_cartesian(xlim = c(mdy("7-15-2013"), mdy("8-15-2013"))) +
    theme(axis.text.x = element_text(angle = 45,  hjust = 1)) 
# ,labels="AUTO",
#   nrow =2)+
#   draw_label("Proportion of juveniles", x=  0.02, y=0.55, vjust= 1.5, angle=90)

# Figure2a <- WESA_AJ /LESA_AJ

# ggsave("output/Figure2.png", Figure2, width = 7.5, height = 15, units = 'cm', dpi = 300)
# ggsave("output/Figure2_Final.tiff", Figure2, width = 15, height = 30, units = 'cm', dpi = 600)

# Figure 3 ------
# source("generatePredictions.r")

cols <- c("Adults"='black',#,"dodgerblue1",
          "Juveniles"='darkgrey')#"red4")
#c("Adults"="Blue","Juveniles"="Red")
shps <- c("Adults"=1,"Juveniles"=2)

wesaplot_average <-
  ggplot(doy.models, aes(DOY + mdy("1-1-2013"), pred.wesa.ad)) +
  geom_ribbon(data =pred.doys[pred.doys$DOY < quant_a[[2]],],
              aes(ymax = exp(pred.wesa.ad + 2*se.a.w), ymin = exp(pred.wesa.ad - 2*se.a.w), y=NULL),
              alpha = 0.2) +
  geom_ribbon(data =pred.doys[pred.doys$DOY > quant_j[[1]],],
              aes(ymax = exp(pred.wesa.juv + 2*se.j.w), ymin = exp(pred.wesa.juv - 2*se.j.w), y=NULL),
              alpha = 0.2) +
  # geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a[[2]]), ],
  #           linetype = 1,
  #           alpha = 0.5, aes(group = Year)) +
  # geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j[[1]]), ],
  #           linetype = "dashed",
  #           alpha = 0.5, 
  #           aes(group = Year, y = pred.wesa.juv)) +
  geom_line(colour = 'black',#"dodgerblue1",
            linetype=1,
            data = pred.doys[pred.doys$DOY < quant_a[[2]],],aes(y=exp(pred.wesa.ad)), size = 1.5) +
  geom_line(colour = 'black',linetype=2,#, "red4", 
            aes(y = pred.wesa.juv), 
            data = doy.models[(doy.models$DOY > quant_j[[1]]), ], 
            size = 1.5) +
  coord_trans(y = scales::log1p_trans()) +#, limy = c(0, 5000)) +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000, 2000, 5000))+
  labs( subtitle="(C)",#"Western Sandpipers",
        y = expression(paste("Average Daily Count (",log[10] + 1, " scale)")), x = "") +
 
  
  # geom_point(data = sidney_adults, aes(y = WESA.ad.est), colour = "dodgerblue1", alpha = 0.5, shape =1) +
  stat_summary(data = sidney_adults, geom = 'pointrange',
               fun.data = 'mean_cl_boot',na.rm=T,#size = 0.5,fatten=0.5,
               aes(y = WESA.ad.est), colour = 'black',#"dodgerblue1", 
               alpha = 1, shape =1) +
  # geom_point(data = sidney_juveniles, aes(y = WESA.juv.est), colour = "red4", alpha = 0.5, shape = 2) +
  stat_summary(data = sidney_juveniles,geom='pointrange' ,na.rm=T,
               fun.data = 'mean_cl_boot',#size = 0.5,fatten=0.5,
               aes(y = WESA.juv.est), colour ='black',#, "red4", 
               alpha = 0.7, shape = 2) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1)) 
  # theme(axis.text.x = element_text(angle = 45, hjust = .75, vjust = 1))


lesaplot_average <-
  ggplot(doy.models, aes(DOY + mdy("1-1-2013"), pred.lesa.ad)) +
  geom_ribbon(data =pred.doys[pred.doys$DOY < quant_a[[2]],],
              aes(ymax = exp(pred.lesa.ad + 2*se.a.l), ymin = exp(pred.lesa.ad - 2*se.a.l), y=NULL),
              alpha = 0.2) +
  geom_ribbon(data =pred.doys[pred.doys$DOY > quant_j[[1]],],
              aes(ymax = exp(pred.lesa.juv + 2*se.j.l), ymin = exp(pred.lesa.juv - 2*se.j.l), y=NULL),
              alpha = 0.2) +
 
  coord_trans(y = scales::log1p_trans()) +#, limy = c(0, 5000)) +
  labs( subtitle="(D)",#"Least Sandpipers",
        y =  expression(paste("Average Daily Count (",log[10] + 1, " scale)")), x = "") +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000))+
  # geom_line(data=yrs.doys, linetype = 'dashed', aes(group=Year))+
  # geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a_lesa[[2]]), ],
            # alpha = 0.5, linetype = 1, aes(group = Year)) +
  # geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j_lesa[[1]]), ],
            # alpha = 0.5, linetype = 2, aes(group = Year,
    # y = pred.lesa.juv
  # )) +
  geom_line(colour = 'black',#"dodgerblue1", 
            data = doy.models[(doy.models$DOY < quant_a_lesa[[2]]), ], size = 1.5) +
  geom_line(colour = 'black',linetype=2,#, "red4", 
            aes(y = pred.lesa.juv), data = doy.models[(doy.models$DOY > quant_j_lesa[[1]]), ], 
            size = 1.5)+
  
  stat_summary(data = sidney_adults_lesa, geom = 'pointrange',
               fun.data = 'mean_cl_boot',na.rm=T,#size = 0.5,fatten=0.5,
               aes(y = LESA.ad.est), colour = 'black',#"dodgerblue1", 
               alpha = 1, shape =1) +
  # geom_point(data = sidney_adults_lesa, aes(y = LESA.ad.est), colour = "dodgerblue1", alpha = 0.5, shape = 1) +
  stat_summary(data = sidney_juveniles_lesa,geom='pointrange' ,na.rm=T,
               fun.data = 'mean_cl_boot',#size = 0.5,fatten=0.5,
               aes(y = LESA.juv.est), colour ='black',#, "red4", 
               alpha = 0.7, shape = 2) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1)) 
  # geom_point(data = sidney_juveniles_lesa, aes(y = LESA.juv.est), colour = "red4", alpha = 0.5, shape = 2) +
  # theme(axis.text.x = element_text(angle = 45, hjust = .75, vjust = 1))



# Figure2b <-
#   wesaplot_average / lesaplot_average + plot_layout()
  # cowplot::plot_grid(wesaplot_average, lesaplot_average, labels = "AUTO", nrow=2)
# ggsave("output/Figure3.pdf", Figure3, width = 8, height = 6, units = "in", dpi = 300)
# ggsave("output/Figure3.png", Figure3, 
#        width = 1000/300, height = 2000/300, units = 'in',
#        # width = 7.5, height = 15, units = "cm",
#        dpi = 300)
# ggsave("output/Figure3_final.tiff", Figure3, 
#        width = 15, height = 30, units = 'cm',
#        # width = 7.5, height = 15, units = "cm",
#        dpi = 600)
rm(wesaplot_average, lesaplot_average, Figure3)


## Figure 4

gam_wesa <- ggplot(sum_pred_gam, aes(Year_all, y = exp(.pred_wesa))) +
  geom_ribbon( aes(x = Year_all, ymin = exp(.pred_wesa-1.96*.se_wesa),
                                            ymax = exp(.pred_wesa+1.96*.se_wesa),
                   y =NULL), fill ='black', alpha = 0.1) + 
  geom_ribbon(data = sum_pred_gam_j, aes(x = Year_all, ymin = exp(.pred_wesa_j-1.96*.se_wesa_j),
                                         ymax = exp(.pred_wesa_j+1.96*.se_wesa_j), 
                                         y =NULL), fill ='black', alpha = 0.1) +
  geom_line(data = sum_pred_gam_j, aes(y=exp(.pred_wesa_j)), 
            colour = 'darkgrey', linetype =2) + 
  geom_line(colour = 'black', linetype = 2)+  coord_trans(y = scales::log1p_trans()) +
  stat_summary(position=position_nudge(0.1),fun.data = 'mean_cl_boot', 
               data = sidney_adults %>% filter(Year %in% yrsinAnalysis$adults), 
               aes(x=Year,y = WESA.ad.est, colour = "Adults", shape= "Adults"),  alpha = 1) +
  stat_summary(position=position_nudge(-0.1), fun.data = 'mean_cl_boot', 
               data = sidney_juveniles %>% filter(Year %in% yrsinAnalysis$juv), 
               aes(x=Year,y = WESA.juv.est, shape ="Juveniles",colour = "Juveniles"),
               alpha = 1) +
  scale_colour_manual(name="Age",values=cols, 
                      guide = guide_legend(override.aes=aes(alpha=NA))) +
  scale_shape_manual(name="Age",values=shps) +

  labs(x = "", subtitle="(E)",#"Western Sandpipers",
       y = expression(paste("Average Daily Count (",log[10] + 1, " scale)")) ) +
  theme(legend.position = c(.05,.2)) +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000, 2000, 5000))

gam_lesa <- ggplot(sum_pred_gam, aes(Year_all, y = exp(.pred_lesa))) +
  geom_ribbon( aes(x = Year_all, ymin = exp(.pred_lesa-1.96*.se_lesa),
                   ymax = exp(.pred_lesa+1.96*.se_lesa), y =NULL), fill ='black', alpha = 0.1) + 
  geom_ribbon(data = sum_pred_gam_j, aes(x = Year_all, ymin = exp(.pred_lesa_j-1.96*.se_lesa_j),
                                         ymax = exp(.pred_lesa_j+1.96*.se_lesa_j), y =NULL), fill ='black', alpha = 0.1) +
  
  geom_line(data = sum_pred_gam_j, aes(y=exp(.pred_lesa_j)),
            colour = 'darkgrey', linetype =1) + 
  geom_line(colour = 'black', linetype = 2)+ 
  coord_trans(y = scales::log1p_trans()) +
  stat_summary(position=position_nudge(0.1),shape =1, fun.data = 'mean_cl_boot', 
               data = sidney_adults%>% filter(Year %in% yrsinAnalysis$adults), 
               aes(x=Year,y = LESA.ad.est), colour = "black", alpha = 1) +
  stat_summary(position=position_nudge(-0.1),shape =2, fun.data = 'mean_cl_boot', 
               data = sidney_juveniles%>% filter(Year %in% yrsinAnalysis$juv), 
               aes(x=Year,y = LESA.juv.est), colour = "darkgrey", alpha = 1) +
  labs(x = "", #title = "B.",
       subtitle="(F)",#"Least Sandpipers",
       y =  expression(paste("Average Daily Count (",log[10] + 1, " scale)"))) +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000, 2000, 5000))

# Figure2c <- gam_wesa / gam_lesa
  
  #plot_grid(gam_wesa, gam_lesa, nrow=2, labels = "AUTO")
# ggsave("output/Figure4.pdf",Figure4, width = 8, height = 8)
# ggsave("output/Figure4.png",Figure4,
#        width = 1000/300, height = 2000/300, units = 'in',
#        # width = 7.5, height = 15, units = "cm",
#        dpi = 300)
# ggsave("output/Figure4_final.tiff",Figure4,
#        width = 1000/300, height = 2000/300, units = 'in',
#        # width = 7.5, height = 15, units = "cm",
#        dpi = 600)

# rm(Figure4, gam_wesa, gam_lesa)


FigRep <- (WESA_AJ  | wesaplot_average |  gam_wesa) /
  (LESA_AJ | lesaplot_average | gam_lesa)  
FigRep
ggsave("output/Fig2_final.tiff",FigRep, width = 30, height = 20, units = 'cm', dpi = 600)
ggsave("output/Fig2_final.png",FigRep, width = 30, height = 20, units = 'cm', dpi = 600)
# Figure 5 - Climate Correlates -----------------------------------------------

require(lubridate)

labs_vars <- list("ma")
names(ranefbyYr) <- c("Year", "Adult WESA", "Adult LESA", "Juvenile WESA", "Juvenile LESA" )
climate_covariates <- read_rds("output/modelcovariates_all.rds")
hyp <- read_csv("data_/Hyp_key.csv")
cleanNames <- read_csv("data_/CleanVariableNames.csv")
name_id <- tibble(oldnames =names(climate_covariates),
cleanNames = c("Year", "Temperature pre-laying", "TMAX_May", "days_above_zero_may",
                               "days_above_zero_tmin_may", "Temperature post-laying", "TMAX_June","days_above_zero_june" ,     "days_above_zero_tmin_june",
                              "min_snowmelt"   ,           "Snowmelt Day of Year"  ,            "Month"   ,                 
                                "Horizontal Wind"     ,                    "Vertical Wind"        ,                 "Mean Temperature"     ,                
                              "Total preciptation"    ,          "AO"    ,                    "PNA"      ,                
                                "PDO"     ,                 "ALPI"      ,                "NPI" , "EP-NP" ) )

cols_months <- c("Aug" = "red4","Jul" = "dodgerblue1", "Nope" = "black" )
climate_plots <- climate_covariates %>% gather("model_name", "Estimate", -Year, -Month) %>% 
  left_join(hyp, by = "model_name" ) %>% 
  left_join(name_id, by = c("model_name" = "oldnames")) %>% 
  mutate(model_name = cleanNames) %>% 
  bind_rows(
    ranefbyYr %>% gather("model_name", "Estimate",-Year) %>% 
      mutate(#model_name = factor(model_name, levels = c("Adult WESA", "Juvenile WESA", "Adult LESA", "Juvenile LESA" )),
        hypothesis = "Random Effects", Month = 7) 
    )%>% 
  filter(!is.na(hypothesis)& (hypothesis=="Local"| Month == 7 | hypothesis == "Random Effects")) %>% 
  mutate( Month1 = ifelse(hypothesis == "Local",
                          as.character(month(mdy(paste(Month,"-1-", "2013", sep="")), label = T)), "Nope"),
          model_name = factor(model_name, levels = c("Snowmelt Day of Year","Temperature pre-laying", "Temperature post-laying",
                                                       "Mean Temperature", "Total preciptation","Horizontal Wind","Vertical Wind", 
                                                       "PNA", "ALPI", "PDO", "AO",
                                                       "Adult WESA", "Juvenile WESA", "Adult LESA", "Juvenile LESA" )),
          hypothesis = factor(hypothesis, levels = c("Random Effects", "Local", "Breeding", "Global"))) %>%
  group_by(hypothesis) %>% nest() %>% 
  mutate(plot_out = map2(.x = data, .y = hypothesis, .f = 
  ~ggplot(.x , aes(Year, Estimate, colour = Month1, group = Month, shape = Month1)) + 
                                   # month(mdy(paste0("1-",Month, "2013"), label=T)),NULL))) +
  facet_grid(model_name~., scales='free_y') +
  geom_point() + labs(x="", y = "", title = .y) +
    # scale_colour_brewer(type = 'qual', palette = 'Set1') +
    scale_colour_manual(values = cols_months) +
    theme(legend.position = 'none', plot.margin = unit(c(0,0,0,0), "cm"), title = 
            element_text(family = 'sans', size = 10),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          strip.text = element_text(size = 7, colour = "black", angle = 90)) +
    scale_x_continuous(breaks = c(1990,2000,2010),minor_breaks = c(1995,2005,2015)) + 
  geom_line())) 


Appendix_A2_1 <- 
plot_grid(plotlist = climate_plots$plot_out[c(4,2,1,3)], ncol=4, label_size = 10)

# ggsave("output/AppendixFigA2_1.pdf",Appendix_A2_1, width = 16, height = 12, units = 'in', dpi = 300)
ggsave("output/AppendixFigA2_1.png",Appendix_A2_1, width = 16, height = 12, units = 'cm', dpi = 600)
ggsave("output/AppendixFigA2_1.tiff",Appendix_A2_1, width = 16, height = 12, units = 'cm', dpi = 600)
rm(climate_plots, Appendix_X)

# Figure 6 - Effects plot ------------------------------------------------------------
aicTables <- read_rds("output/aicTables.rds") %>% 
  group_by(group) %>% 
  arrange(desc(w)) %>% 
  mutate(group,modelOrder = row_number())
coefficients_by_id_w_aic <- read_rds("output/coefficients_modmsonly.rds") %>% 
  filter( !is.na(p.value) | effect != 'fixed') %>% 
  mutate(p.value = replace_na(p.value, 1)) %>% 
  left_join(aicTables, by = c("group", "model_name" = "mnames")) %>% 
  left_join(hyp) %>% 
  left_join(spID, by = c("group" = "var")) %>% 
  left_join(cleanNames, by = c("term" = "varname")) %>% 
  mutate(Species = factor(Species,levels = sort(unique(Species),decreasing = T))) 
                        
f3dat <- coefficients_by_id_w_aic %>% 
  filter(non_id==0.8& effect == "fixed" & 
           (dAICc < 6 | model_name == "a" | (!grepl("global", model_name) & sign(conf.low)==sign(conf.high))) &
           model_name != "global" & 
           !grepl("ns", term) &
           !term %in% c("(Intercept)", "DOY_rescaled", "I(DOY_rescaled^2)", "Year", "I(Year^2)")) %>% 
  mutate(cleanName = factor(cleanName,levels = unique(cleanName[order(hypothesis)]), ordered = T),
         glob = grepl("global", model_name),
         modtyep = ifelse(glob, "Full Hypothesis", "Single Variable")
  ) %>% mutate(hypothesis=ifelse(is.na(hypothesis), "Age", hypothesis))

Figure3 <- 
ggplot(f3dat %>% filter(model_name != "u*v"),
       aes(cleanName, estimate ,size = w, colour = modtyep)) + 
  geom_pointrange( position=position_dodge(0.5),
                   size = 0.5,
                   aes(ymin=conf.low, ymax=conf.high,
                       group = interaction(model_name, Age),
                       shape = Age))+
  # geom_pointrange(data =filter(f5dat, glob), colour = "grey",
  #                 # position = position_nudge(0.4),
  #                 position=position_dodge(0.3),
  #                 size = 0.5,
  #                 aes(ymin=conf.low, ymax=conf.high, group = interaction(model_name, Age), shape = Age))+
  # geom_pointrange(data =filter(f5dat, !glob),
  #                 aes(ymin=conf.low, ymax=conf.high, group = interaction(model_name, Age), shape = Age),
  #                 # fatten = 0.9,
  #                 size = 0.5, 
  #                 position=position_dodge(0.6)) +
  geom_pointrange(data = f3dat %>% filter(model_name == "u*v"),
    # position=position_nudge(0.3),
                   size = 0.5,shape = 1,
                   aes(ymin=conf.low, ymax=conf.high,
                       group = interaction(model_name, Age),
                       shape = Age))+
  facet_grid(hypothesis~Species, scales = 'free_y', space = 'free_y') + coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_colour_manual(values = list("Full Hypothesis"="grey", "Single Variable"="black"))+
  # scale_colour_brewer(type='qual', palette = 'Set2') +
  labs(y="Standardized Estimate", x ="Model Term", colour = "Model type") +
  theme(legend.position = c(.87,.2),
        legend.box.background = element_rect(fill = 'grey')) 
Figure5+ 
  theme_light(base_size = 12,base_family = 'sans' ) +
  scale_size_manual(values = c(0.1)) +
  theme(legend.position = c(.87,.2),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.margin = margin(0,1,0,1),
        legend.spacing = unit(0, "cm"),
        legend.box.margin = margin(0,0,0,0),
        legend.background = element_rect(fill=NA),
        legend.box.background = element_rect(fill = 'grey')) 
# ggsave("output/Figure5.pdf", Figure5 + theme(legend.position = 'none'), width = 12, height = 8, units ='in', dpi = 300)
ggsave("output/2020_Figure3_final.png", Figure5+ 
         theme_light(base_size = 12,base_family = 'sans' ) +
         scale_size_manual(values = c(0.1)) +
         theme(legend.position = c(.75,.18),
               legend.margin = margin(0,1,0,1),
               legend.spacing = unit(0, "cm"),
               legend.box.margin = margin(0,0,0,0),
               legend.background = element_rect(fill=NA),
               legend.box.background = element_rect(fill = 'grey')) ,#line = element_line(size = 1), 
                                             # text = element_text(size = 8),
                                             # axis.text = element_text(size = 8)), 
       width = 2000/300, height = 1500/300, units = 'in',
       # width = 7.5, height = 15, units = "cm",
       dpi = 300)
       # width = 16, height = 12, units ='cm', dpi = 300)
ggsave("output/2020_Figure3_final.tiff",Figure5+ 
         theme_light(base_size = 12,base_family = 'sans' ) +
         scale_size_manual(values = c(0.1)) +
         theme(legend.position = c(.75,.175),
               legend.background = element_rect(fill=NA),
               legend.margin = margin(1,1,.2,1),
               legend.spacing = unit(0, "cm"),
               legend.box.margin = margin(0,0,0,0),
               legend.box.background = 
                 element_rect(fill = 'grey')) ,#line = element_line(size = 1), 
                                             # text = element_text(size = 8),
                                             # axis.text = element_text(size = 8)), 
       width = 2000/300, height = 1500/300, units = 'in',
       # width = 7.5, height = 15, units = "cm",
       dpi = 600)
       # width = 16, height = 12, units ='cm', dpi = 300)


# Figure 5 
source("A_J_diffs.r")





wesaplot_average_yrs <-
  ggplot(doy.models, aes(DOY + mdy("1-1-2013"), pred.wesa.ad)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a[[2]]), ], linetype = "dashed",
            alpha = 1, aes(group = Year, 
                           y= pred.wesa.ad_rs)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a[[2]]), ], 
            linetype = "solid",alpha = 1, aes(group = Year)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j[[1]]), ], 
            linetype = "dashed",alpha = 1, aes(group = Year,
                                               y = pred.wesa.juv_rs)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j[[1]]), ],
            linetype = "solid",alpha = 1, aes(group = Year,
                                              y = pred.wesa.juv)) +
  cowplot::theme_cowplot() +
  coord_trans(y = scales::log1p_trans()) +#, limy = c(0, 5000)) +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000, 2000, 5000))+
  labs(title = "Western Sandpipers", y = expression(paste("Count (",log[10] + 1, " scale)")), x = "") +
  facet_wrap(~Year, ncol = 3)+
  geom_point(data = sidney_adults, aes(y = WESA.ad.est), colour = "dodgerblue1", alpha = 0.8, shape =1) +
  geom_point(data = sidney_juveniles, aes(y = WESA.juv.est), colour = "red4", alpha = 0.8, shape = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = .75, vjust = 1))


lesaplot_average_yrs <-
  ggplot(doy.models, aes(DOY + mdy("1-1-2013"), pred.lesa.ad)) +
  # geom_ribbon(data =pred.doys[pred.doys$DOY < quant_a[[2]],],
  #             aes(ymax = exp(pred.wesa.ad + 2*se.a.w), ymin = exp(pred.wesa.ad - 2*se.a.w), y=NULL),
  #             alpha = 0.2) +
  # geom_ribbon(data =pred.doys[pred.doys$DOY > quant_j[[1]],],
  #             aes(ymax = exp(pred.wesa.juv + 2*se.j.w), ymin = exp(pred.wesa.juv - 2*se.j.w), y=NULL),
  #             alpha = 0.2) +
  geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a_lesa[[2]]), ], linetype = "dashed",
            alpha = 1,
            aes(group = Year, 
                y= pred.lesa.ad_rs)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY < quant_a_lesa[[2]]), ], linetype = "solid",
            alpha = 1, aes(group = Year)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j_lesa[[1]]), ], linetype = "dashed",
            alpha = 1, aes(group = Year,
                             y = pred.lesa.juv_rs)) +
  geom_line(data = yrs.doys[(yrs.doys$DOY > quant_j_lesa[[1]]), ], linetype = "solid"
            ,alpha = 1, aes(group = Year,
                              y = pred.lesa.juv)) +
  # geom_line(colour = "dodgerblue1", data = pred.doys[pred.doys$DOY < quant_a[[2]],],aes(y=exp(pred.wesa.ad)), size = 1) +
  # geom_line(colour = "red4", aes(y = pred.wesa.juv), data = doy.models[(doy.models$DOY > quant_j[[1]]), ], size = 1) +
  cowplot::theme_cowplot() +
  
  coord_trans(y = scales::log1p_trans()) +#, limy = c(0, 5000)) +
  scale_y_continuous(breaks = c(0, 1, 10, 50, 100, 250, 500, 1000, 2000, 5000))+
  labs(title = "Least Sandpipers", y = expression(paste("Count (",log[10] + 1, " scale)")), x = "") +
  facet_wrap(~Year, ncol = 3)+
  
  geom_point(data = sidney_adults, aes(y = LESA.ad.est), colour = "dodgerblue1", alpha = 0.8, shape =1) +
  geom_point(data = sidney_juveniles, aes(y = LESA.juv.est), colour = "red4", alpha = 0.8, shape = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = .75, vjust = 1))




# ggsave("output/App4.pdf", wesaplot_average_yrs,
#        width = 8, height = 6, units = "in", dpi = 300)
ggsave("output/AppA3_1_final.png", wesaplot_average_yrs, 
       width = 20, height = 45, units = "cm", dpi = 600)
ggsave("output/AppA3_1_final.tiff", wesaplot_average_yrs, 
       width = 20, height = 45, units = "cm", dpi = 600)
# ggsave("output/App5.pdf", lesaplot_average_yrs, width = 8, height = 6, units = "in", dpi = 300)
ggsave("output/AppA3_2_final.png", lesaplot_average_yrs, 
       width = 20, height = 45, 
       units = "cm", dpi = 600)
ggsave("output/AppA3_2_final.tiff", lesaplot_average_yrs, 
       width = 20, height = 45, 
       units = "cm", dpi = 600)
