require(tidyverse)

require(cowplot)
require(glmmTMB)

# if(!exists("fullresults_all_dist")) {
#   modelres <-  read_rds("output/model_dist_comparisons.rds")
#   } else{
#   modelres <- fullresults_all_dist 
# }


# Juvenile WESA -----------------------------------------------------------

f_int <- fixef(models_for_ms$WESA.ad.est$null)$cond[[1]]
f_int_lesa <- fixef(models_for_ms$LESA.ad.est$null)$cond[[1]]
# ranneffe_adults_W <- ranef(models_for_ms$WESA.ad.est$null) %>%
#   .[["cond"]] %>%
#   .[["Year.factor"]] %>%
#   rownames_to_column("Year") %>%
#   mutate_all(as.numeric) %>%
#   rename(adults = `(Intercept)`) %>% 
#   mutate(DOY_rescaled = 0, Year.factor = as.factor(Year)) %>% 
#   mutate(pred = predict(models_for_ms$WESA.juv.est$a, newdata = .))
# 
# # ggplot(ranneffe_adults_W, aes(adults, pred)) + geom_line()

wesa_aj <- sjPlot::plot_model(models_for_ms$WESA.juv.est$a, type = 'pred', terms = 'adults [all]') + 
  labs(x = "Random intercept ")
calc_e <- function(dat, x__,y__){

  x_ <- enquo(x__)
  y <- enquo(y__)
  # y_ <- enquo(y); x_ <- enquo(x)
  ename <- paste0(quo_name(x_), "_", quo_name(y))
  y_d <- paste0("y_", quo_name(y))
  
  dat %>% 
    mutate(xlag = dplyr::lag(!!x_),
      x_out = (!!x_ - xlag) / xlag,
      ytmp := ifelse(is.na(!!y),NA,(!!y - lag(!!y))/lag(!!y)),
      !!ename := ytmp/x_out) %>% 
    rename(!!y_d := ytmp)
    
}


wesa_aj_dat <- wesa_aj$data %>% mutate(adults = exp(x + f_int)) %>% calc_e( adults, predicted) %>% 
  calc_e(adults, conf.low) %>% calc_e(adults, conf.high)
sa_aj_dat <- wesa_aj$data %>% mutate(adults = exp(x + f_int))# 


wesa_aj <- expand.grid(DOY_rescaled = 0, adults = seq(-2.5, 1.35, length.out = 100), Year.factor = NA)
wesa_pred <- predict(models_for_ms$WESA.juv.est$a, newdata = wesa_aj, type = 'link',  se.fit =T)
wesa_aj$.pred <- wesa_pred$fit
wesa_aj$.lci <- exp(wesa_aj$.pred - wesa_pred$se*1.96 )
wesa_aj$.uci <- exp(wesa_aj$.pred + wesa_pred$se*1.96)
wesa_aj$adults_x <- exp(wesa_aj$adults +f_int)


lesa_aj <- expand.grid(DOY_rescaled = 0, adults = seq(-2.5, 1.2, length.out = 100), Year.factor =NA)
lesa_pred <- predict(models_for_ms$LESA.juv.est$a, newdata = lesa_aj, type = 'link',  se.fit =T)
lesa_aj$.pred <- lesa_pred$fit
lesa_aj$.lci <- exp(lesa_aj$.pred - lesa_pred$se*1.96 )
lesa_aj$.uci <- exp(lesa_aj$.pred + lesa_pred$se*1.96)
lesa_aj$adults_x <- exp(lesa_aj$adults +f_int_lesa)
 
# lesa_aj <- sjPlot::plot_model(models_for_ms$LESA.juv.est$a, type = 'pred', terms = 'adults')
# lesa_aj_dat <- lesa_aj$data %>% mutate(adults = exp(x + f_int_lesa))%>% calc_e( adults, predicted)

aj_plot <- 
  ggplot(wesa_aj, aes(adults_x, exp(.pred))) + 
  geom_ribbon(aes(ymin = .lci, ymax= .uci), alpha = 0.2) +
  geom_ribbon(data = lesa_aj, aes(ymin = .lci, ymax= .uci), alpha = 0.2) +
  geom_line()+
  geom_line(data = lesa_aj, linetype = 2) +
  labs(x = "Annual adult abundance", y = "Annual juvenile abundance")

# ggsave(aj_plot, filename = "output/ajPlot.pdf", width = 8, height = 8)
# ggsave(aj_plot, filename = "output/Figure7.png", width = 7.5, height = 7.5, units = 'cm', dpi = 300)

# aj_plot + geom_point()



coef_yank <- 
  coefficients_by_id_w_aic %>% 
  filter(model_name == "null" & 
           term == "(Intercept)" & 
           component == "cond" & non_id == 0.8) %>% 
  select(group, estimate, conf.low, conf.high) %>% 
  separate(group,into =  c("Species", "Age", "est")) %>% 
  mutate(Age = ifelse(Age == "ad", "Adult", "Juvenile")) %>% 
  select(-est) %>% 
  tidyr::unite("Estiamtes", estimate:conf.high, sep = "_") %>% 
  spread(Age, Estiamtes) %>% 
  separate(Adult, c("Estimate", "lci", "uci"), sep = "_") %>% 
  separate(Juvenile, c("Estimate_j", "lci_j", "uci_j"), sep = "_") %>% 
  mutate_at(vars(-Species), as.numeric)


ranef_byage <-            
  ranefbyYr %>% gather("Group", "Ranef", -Year) %>% 
  tidyr::separate(Group, c("Age", "Species"), sep = " ") %>% 
  spread(Age, Ranef) %>% left_join(coef_yank)

Figure6 <- 
aj_plot + 
  scale_color_brewer(type = 'qual', palette = 'Set1', direction = 1) +
  geom_point(data = ranef_byage, aes(x = exp(Adult + Estimate), 
                                          y = exp(Juvenile + Estimate_j), 
                                          # ymin = exp(Juvenile + lci_j),
                                          # ymax = exp(Juvenile + uci_j),
                                          colour = Species, shape = Species
  )
  ) 
Fig7_v2 <- Figure7 +
  geom_errorbarh(data = ranef_byage, aes(x = NULL,# exp(Adult + Estimate),
                                          y = exp(Juvenile + Estimate_j),
                                          xmin = exp(Adult + lci),
                                          xmax = exp(Adult + uci),
                                          colour = Species
  )
  )+
  geom_errorbar(data = ranef_byage, aes(x = exp(Adult + Estimate),
                                     y = NULL,#exp(Juvenile + Estimate_j),
                                     ymin = exp(Juvenile + lci_j),
                                     ymax = exp(Juvenile + uci_j),
                                     colour = Species #shape = Species
                                     )
                  ) +
  coord_cartesian(xlim=c(0,300), ylim=c(0,1200))

ggsave("output/Figure7.pdf", Figure6, width = 8, height = 8)
ggsave("output/Figure7_alt.pdf", Fig7_v2, width = 8, height = 8)
ggsave(Figure6+ theme(legend.position = 'none'), filename = "output/Figure6_final.tiff", 
       # width = 7.5, height = 7.5, units = 'cm',
       width = 1000/300, height = 1000/300, units = 'in',
       dpi = 600)
