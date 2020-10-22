## Examine non.id role on results
# David Hope
# October 31, 2018

# //
#   _.-"""""'//-'""""-._
#      .', ,  , , : : ` ` `  `.
#     / , , \'-._ : :_.-'/ ` ` \
#    / , ,  :\(_)\  /(_)/ : ` ` \
#   | , ,  ,  \__//\\__/ . . ` ` |
#   | . .:_  : : '--`: : . _: ; :|
#   | : : \\_  _' : _: :__// , , |
#    \ ` ` \ \/ \/\/ \_/  / , , /
#     \ ` ` \_/\_/\_/\_/\/ , , /
#      `._ ` . :  :  :  , , _.'
#         `-..............-' 

require(tidyverse)
require(magrittr)
non.id <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

source("R/RunClimateModels.r")
coefficients_models_for_ms <- climateModelRun(0.8, return_type = 'coef')
write_rds(coefficients_models_for_ms, "output/coefficients_modmsonly.rds")
coefficients_by_id <- map_df(non.id, climateModelRun, return_type= "coef")
write_rds(coefficients_by_id, "output/coefficients_by_id_lessyrs.rds")
coefficients_by_id <- read_rds("output/coefficients_by_id_lessyrs.rds") %>% 
  filter( !is.na(p.value) | effect != 'fixed') %>% 
  mutate(p.value = replace_na(p.value, 1),
         Species = ifelse(grepl("WESA", group), "Western Sandpipers", "Least Sandpipers"),
         Age = ifelse(grepl("juv", group), "Juvenile", "Adult"))

# unique(coefficients_by_id$term) %>% as.data.frame() %>%  write_csv("data_/clean_mod_vars.csv")
cleavvars <- read_csv("data_/clean_mod_vars.csv")
coefficients_by_id %<>% left_join(cleavvars, by ='term')


print_model_coef <- function(i,j,r="fixed" )
  { #cat(i,j)
  if(j == "a" & grepl(".ad.", i)){return(NA)}
  dat <- filter(coefficients_by_id, group == i & model_name == j & effect == r)
  age <- unique(dat$Age)
  spp <- unique(dat$Species)
  ggplot(dat,
               aes(non_id, estimate, colour = p.value < 0.05, 
                   shape = component)) +
          facet_wrap(~cleanTerm, scales = 'free_y') + 
          geom_pointrange(aes(ymax= conf.high, ymin = conf.low))+
          geom_hline(yintercept = 0, linetype = 'longdash') +
    theme_light()+
          labs(x = "Non-identified proportion threshold", 
               y = "Estimate \u00B1 95% CI",
               title = paste(age, spp)) + # , "\nModel:",j 
    theme(legend.position = 'none')+
    scale_color_brewer(type= 'qual', palette = 'Set1', direction = -1)
  }

indiv.plots <- expand.grid(i = as.character(unique(coefficients_by_id$group)), 
                           j = as.character(unique(coefficients_by_id$model_name)))

pdf("output/Appendix3.pdf", width = 10, height = 10)

map2(indiv.plots[,1],indiv.plots[,2], print_model_coef)
    
      
dev.off()


legend.only <- cowplot::ggdraw(cowplot::get_legend(print_model_coef(indiv.plots[1,1], indiv.plots[1,2])+ theme(legend.position = 'right')))
ggsave("output/nonid_legend.png", legend.only,bg='transparent')


pdf("output/non.idrancoef.pdf")

map2(indiv.plots[,1],indiv.plots[,2], print_model_coef, r = "ran_pars")


dev.off()
