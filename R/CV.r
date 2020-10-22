## Cross - validation
# http://sjspielman.org/bio5312_fall2017/files/kfold_supplement.pdf
source("R/RunClimateModels.r")

cv_function <- function(mod,dat, Species = NA, Age = NA,model_name = NA, return_df =F,...) {
  require(modelr)
  require(broom)
  require(broom.mixed)
  require(glmmTMB)
  require(splines)
  vars_ignore <- c("(Intercept)"   ,    "DOY_rescaled"     , "I(DOY_rescaled^2)" )
  f <- as.formula(formula(mod))
  g_ <- "gam" %in% class(mod)
  if(g_){
    mod_to_rescale <- "Year"

    }
    else{mod_to_rescale <- c("Year", names(fixef(mod)$cond)[! names(fixef(mod)$cond) %in% vars_ignore & !grepl("ns",names(fixef(mod)$cond) )])}
  # inMod <- function(x) grepl(x, mod$call)
  data_ <- dat %>% mutate_at( vars(mod_to_rescale), arm::rescale)#mod$frame #%>% mutate(Year = as.numeric(as.character(Year.factor)))
  # if(grepl("gam", model_name)) data_ <- mod$frame %>% select_if(!grepl("ns",names(.))) %>% 
  #   mutate(Year = as.numeric(as.character(Year.factor)))
  fam <- mod$modelInfo$family
  # data_ <- climateModelRun(
  #   non_id_limit = 0.8, return_type = "both", ran_var = "(1|Year.factor)",
  #   fam = nbinom2,
  #   zigrep = "WESA.ad", prepdata = T
  # )
  set.seed(10392)
  data_ %>%
    ungroup() %>%
    crossv_kfold(10) %>% 
    mutate(model = ifelse(g_,purrr::map(train, ~ gam(formula = f,
                                                     data = as.data.frame(.x) , family = nb)),
                          purrr::map(train, ~update(object = mod, data = .x))) ) -> trained.models

  map2_dbl(trained.models$model, trained.models$test, mae) -> test.rmse
  mae_mod <- mae(mod, data_)# mean(residuals(mod) %>% abs, na.rm=T)
  # summary(data_$dat_a$WESA.ad.est)
  print(summary(test.rmse))
  print(sd(test.rmse))
  print(as.data.frame(test.rmse) %>% ggplot(aes(x = "", y = test.rmse)) + geom_boxplot() + scale_y_log10())
  map2_dbl(trained.models$model, trained.models$train, mae) -> train.rmse
  as.numeric(test.rmse) -> test.rmse2
  as.numeric(train.rmse) -> train.rmse2
  ## Run a test on train/test rmse, remembering that these are PAIRED by k-fold!
  wt <- wilcox.test(test.rmse2, train.rmse2, paired = T)
  print(wt)
  df <-   tibble(Species = Species, Age = Age, model_name = model_name, Train.mae = train.rmse, Test.mae = test.rmse, 
                 wilcox = wt$p.value, cv_run = 1:10, mae_mod = mae_mod)
  if(isTRUE(return_df)) return(df)
  else  return(list(train.mae = train.rmse, trained.models = trained.models, test.mae = test.rmse, wt= wt, df=df))
}
# trained.models %>%
#   mutate(pred = map2(model, test, ~predict(object = .x, newdata = .y, type = "response", allow.new.levels=T))) -> test.predictions
#
# trained.models %>%
#   unnest(fitted = map2(model, test, ~broom.mixed::augment(.x, newdata = .y)),
#          pred = map2(model, test, ~predict(.x, .y, type = "response"))) -> test.predictions
#
# test.predictions%>% select(.id, WESA.ad.est, pred ) %>%
#   ggplot(aes(WESA.ad.est, pred)) + geom_point()
#
# test.predictions %>%  group_by(.id) %>%   summarize(auc =  pROC::multiclass.roc(WESA.ad.est, .fitted)$auc) %>%  select(auc)
#
#
#
# train.predictions <- trained.models %>% unnest(  fitted = map2(model, train, ~broom.mixed::augment(.x, newdata = .y)),
#                                                  pred =map2( model, train, ~predict( .x, .y, type = "response") ))
#
#
# train.predictions %>%  group_by(.id) %>% summarize(auc =  pROC::multiclass.roc(WESA.ad.est, .fitted)$auc) %>%
#   ### outcome from the true data, .fitted from augment's output. Run roc() on these columns and pull out $auc!
#   select(auc)
#
#
# test.predictions %>%  select(.id, WESA.ad.est, pred ) %>%  mutate(pred = ifelse(pred>=    0.5, "malignant", "benign")
#


# cvfun1 <- function(pred_id, pcut = 0.5) {
#   train <- subset(dd, !(ID %in% pred_id))
#   mm <- suppressWarnings(update(model1, data = train))
#   test <- subset(dd, ID %in% pred_id)
#   prob <- predict(mm, type = "response", newdata = test, re.form = ~0)
#   outcome <- as.numeric(prob > pcut)
#   acc <- mean(outcome == test$EVENT)
#   return(acc)
# }
