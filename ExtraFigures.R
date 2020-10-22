require(tidyverse)
source("r/RunClimateModels.r")

models_for_ms <- read_rds("output/random_slope_models.rds")
y_vars <- list("WESA.ad.est", "WESA.juv.est", "LESA.ad.est", "LESA.juv.est")
dat_all <- climateModelRun(non_id_limit = 0.8, prepdata = T)
names(dat_all) <- y_vars
genp <- function(mod, dat, saj, var, range_ = c(-1.1, 1.5)) {
  d <- expand_grid(
    DOY_rescaled = 0,
    {{ var }} := seq(range_[1], range_[2], length.out = 100),
    Year.factor = NA
  )
  # browser()
  p <- predict(mod[[as_label(enquo(saj))]][[as_label(enquo(var))]],
    newdata = d, type = "link", se.fit = T
  )

  d$.pred <- p$fit
  d$.lci <- exp(d$.pred - p$se * 1.96)
  d$.uci <- exp(d$.pred + p$se * 1.96)
  d$.pred_exp <- exp(d$.pred)
  # browser()
  dat_out <-
    mod[[as_label(enquo(saj))]][[as_label(enquo(var))]][["frame"]] %>%
    mutate(
      {{ var }} := {{ var }} * (2 * sd(dat[[as_label(enquo(saj))]][[as_label(enquo(var))]], na.rm = T)) +
        mean(dat[[as_label(enquo(saj))]][[as_label(enquo(var))]], na.rm = T)
    ) %>%
    group_by(Year.factor, {{ var }}) %>%
    summarize(
      n = n(),
      meanN = mean({{ saj }}), .groups = "drop"
    )
  return(list(
    pred = d %>%
      mutate(
        {{ var }} := {{ var }} * (2 * sd(dat_all[[as_label(enquo(saj))]][[as_label(enquo(var))]],
                                         na.rm = T)) +
          mean(dat_all[[as_label(enquo(saj))]][[as_label(enquo(var))]],
               na.rm = T)
      ),
    dat_out = dat_out
  ))
}

wesa_aALPI <- genp(models_for_ms, dat = dat_all, WESA.ad.est, ALPI)
wesa_aSM <- genp(models_for_ms, dat = dat_all, WESA.ad.est, max_snowmelt)
wesa_jSM <- genp(models_for_ms, dat = dat_all, WESA.juv.est, max_snowmelt)
lesa_jSM <- genp(models_for_ms, dat = dat_all,LESA.juv.est, max_snowmelt)
lesa_aALPI <- genp(models_for_ms, dat = dat_all, LESA.ad.est, ALPI)
wesa_aPNA <- genp(models_for_ms, dat = dat_all, WESA.ad.est, PNA, range_ = c(-1, 0.8))
wesa_aALPI <- genp(models_for_ms, dat = dat_all, WESA.ad.est, ALPI)
Lesa_aJune <- genp(models_for_ms, dat = dat_all, LESA.ad.est, TMIN_June, c(-0.6, 1.4))
Wesa_jJune <- genp(models_for_ms, dat = dat_all, WESA.juv.est, ALPI, range_ = c(-1.6, 1.5))
Wesa_jMay <- genp(models_for_ms, dat = dat_all, WESA.juv.est, TMIN_May, range_ = c(-1.0, 1.0))
lesa_jMay <- genp(models_for_ms, dat = dat_all, LESA.juv.est, TMIN_May, range_ = c(-1.0, 1.0))
lesa_jJune <- genp(models_for_ms, dat = dat_all, LESA.juv.est, TMIN_June, range_ = c(-1.0, 1.0))
lesa_jPDO <- genp(models_for_ms, dat = dat_all, LESA.juv.est, PDO, range_ = c(-1.0, 1.0))
lesa_jPNA <- genp(models_for_ms, dat = dat_all, LESA.juv.est, PNA, range_ = c(-1.0, 1.0))

SM_A_WESA <-
  ggplot(wesa_aSM$dat_out, aes
         (max_snowmelt, meanN, size = n)) +
  geom_line(data = wesa_aSM$pred, aes(x = max_snowmelt, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = wesa_aSM$pred, aes(
      x = max_snowmelt, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
                  breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Western Sandpipers",
    x = "Date of snowmelt",
    size = "N surveys"
  )
SM_A_WESA

SM_J_WESA <-
  ggplot(wesa_jSM$dat_out, aes
         (max_snowmelt, meanN, size = n)) +
  geom_line(data = wesa_jSM$pred, aes(x = max_snowmelt, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = wesa_jSM$pred, aes(
      x = max_snowmelt, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
                  breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Western Sandpipers",
    x = "Date of snowmelt",
    size = "N surveys"
  )
SM_J_WESA

Col1Row1 <-
  ggplot(wesa_aALPI$dat_out, aes
  (ALPI, meanN, size = n)) +
  geom_line(data = wesa_aALPI$pred, aes(x = ALPI, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = wesa_aALPI$pred, aes(
      x = ALPI, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
                  breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Western Sandpipers",
    x = "Aleutian Low Pressure index (ALPI)",
    size = "N surveys"
  )



Col2Row1 <-
  ggplot(lesa_aALPI$dat_out, aes(ALPI, meanN, size = n)) +
  geom_line(data = lesa_aALPI$pred, aes(x = ALPI, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = lesa_aALPI$pred, aes(
      x = ALPI, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
    breaks = c(5, 10, 15, 20, 25, 30),
    labels = c(5, 10, 15, 20, 25, 30)
  ) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Least Sandpipers",
    x = "Aleutian Low Pressure index (ALPI)",
    size = "N surveys"
  )


Col1Row2 <-
  ggplot(wesa_aPNA$dat_out, aes(PNA, meanN, size = n)) +
  geom_line(data = wesa_aPNA$pred, aes(x = PNA, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = wesa_aPNA$pred, aes(
      x = PNA, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
    breaks = c(5, 10, 15, 20, 25, 30), 
                  labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Western Sandpipers",
    x = "Pacific/North American\nteleconnection (PNA) index",
    size = "N surveys"
  )


Col2Row2 <-
  ggplot(Lesa_aJune$dat_out, aes(TMIN_June, meanN, size = n)) +
  geom_line(data = Lesa_aJune$pred, aes(x = TMIN_June, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = Lesa_aJune$pred, aes(
      x = TMIN_June, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
    breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Adult Least Sandpipers",
    x = "Mean daily minimum temperature\n(June; celcius)",
    size = "N surveys"
  )



Col3Row1 <-
  ggplot(Wesa_jJune$dat_out, aes(ALPI, meanN, size = n)) +
  geom_line(data = Wesa_jJune$pred, aes(x = ALPI, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = Wesa_jJune$pred, aes(
      x = ALPI, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
    breaks = c(5, 10, 15, 20, 25, 30),
    labels = c(5, 10, 15, 20, 25, 30)
  ) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle = "Juvenile Western Sandpipers",
    x = "Aleutian Low Pressure index (ALPI)",
    size = "N surveys"
  )


Col3Row2 <-
  ggplot(Wesa_jMay$dat_out, aes(TMIN_May, meanN, size = n)) +
  geom_line(data = Wesa_jMay$pred, aes(x = TMIN_May, y = .pred_exp), size = 1, alpha = 0.5) +
  geom_ribbon(
    data = Wesa_jMay$pred, aes(
      x = TMIN_May, y = .pred_exp,
      ymin = .lci, ymax = .uci
    ),
    size = 1, alpha = 0.2, colour = "grey"
  ) +
  geom_point() +
  scale_size_area(limits = c(1,30),
    breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
  ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
  labs(
    y = "Mean estimated count", subtitle =  "Juvenile Western Sandpipers",
    x = "Mean daily minimum temperature\n(2nd half May; celcius)",
    size = "N surveys"
  )


library(patchwork)
FigureExtra <- 
((Col1Row1 / Col1Row2) | (Col2Row1 / Col2Row2) | (Col3Row1 / Col3Row2)) +
  plot_layout(guides = "collect")
# Part1 + Part2 + Part3 + Part4 + Part5 + Part1Lesa_A + plot_layout(guides = 'collect')


ggsave("output/FigExtra.tiff",FigureExtra, width = 30, height = 20, units = 'cm', dpi = 600)
ggsave("output/FigExtra.png",FigureExtra, width = 30, height = 20, units = 'cm', dpi = 600)






lesa_jMay
genPlot <- function(p,var_, xlab_, subtit){
  ggplot(p$dat_out, aes({{var_}}, meanN, size = n)) +
    geom_line(data = p$pred, aes(x = {{var_}}, y = .pred_exp), size = 1, alpha = 0.5) +
    geom_ribbon(
      data = p$pred, aes(
        x = {{var_}}, y = .pred_exp,
        ymin = .lci, ymax = .uci
      ),
      size = 1, alpha = 0.2, colour = "grey"
    ) +
    geom_point() +
    scale_size_area(limits = c(1,30),
                    breaks = c(5, 10, 15, 20, 25, 30), labels = c(5, 10, 15, 20, 25, 30)) +
    ggrepel::geom_text_repel(aes(label = Year.factor), size = 4, alpha = 0.7) +
    labs(
      y = "Mean estimated count", subtitle =  subtit,
      x = xlab_,
      size = "N surveys"
    )
}


p1 <- genPlot(lesa_jMay, TMIN_May,"Mean daily minimum temperature\n(2nd half May; celcius)", "") +
  theme(legend.position = 'none')
p2 <- genPlot(lesa_jJune, TMIN_June,
              "Mean daily minimum temperature\n(June; celcius)", 
              "")+
  theme(legend.position = 'none')
p3 <- genPlot(lesa_jPDO, PDO,"Pacific Decadal Oscillation (PDO) index", "")+
  theme(legend.position = 'none')
p4 <- genPlot(lesa_jPNA, PNA,"Pacific/North American teleconnection (PNA) index", "")+
  theme(legend.position = 'none')
p5 <- genPlot(lesa_jSM, max_snowmelt,"Snowmelt date", "")

app5 <- 
((p1 / p2 ) | (p3/p4) | (p5 / patchwork::guide_area()  +  plot_layout(guides = 'collect') )) +
  plot_annotation(title = "Juvenile Least Sandpipers")

ggsave( "output/FigureS5.1.png",app5, width = 30, height = 20, units = 'cm', dpi = 600)


lesa_jJune
lesa_jPDO
lesa_jPNA
















