# make a table with 
library(gt)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)


# #################
# read in all model output files an make a file with the predicted in and out of sample MSPE & adjusted r-sq
#################
oos.list <- list()
oos.inc.files <- paste0("outputs/SSM_pred_obs/model_",1:6, "pred.obs.out.of.sample.inc.rds" )
oos.list <- lapply(oos.inc.files, readRDS)

for(i in 1:length(oos.list)){
  oos.list[[i]]$model.name  <- paste0("model_", i)
}

oos.df <- do.call(rbind, oos.list)

oos.df

oos.summary <- oos.df %>% group_by(model.name)  %>% summarise(MSPE = mean((diainc-mean.ci)^2, na.rm =TRUE),
                                                              RMSPE = sqrt(mean((diainc-mean.ci)^2, na.rm =TRUE)),
                                                              MAPE = mean(abs(diainc-mean.ci), na.rm = TRUE)) #, 
#PPL = sum((diainc - mean.ci)^2) + sum(predvar)) #,
# V1 = mean(diainc-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
# V2 = (mean((diainc-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
# V3 = (mean((diainc-mean.ci)^2)^(1/2)))#, # goodness of fit estimate (small = better fit)
# #PPL = sum((diainc - mean.ci)^2) + sum(predvar)) # posterior predictive loss

oos.summary$validation <- "out-of-sample increments"

summary.stats <- summary(lm(diainc ~ mean.ci, data =oos.df))
#all.regress <- oos.df %>% group_by(model.name) %>% do(lm(diainc ~ mean.ci, .)) #%>% ungroup()

summary.stats.list <- rsquared.list <- list()
for(i in 1:length(oos.list)){
  oos.mod <- oos.df %>% filter(model.name %in% paste0("model_", i))
  #lms <- lm(diainc ~ mean.ci, data = oos.mod)
  summary.stats.list[[i]] <- summary(lm(diainc ~ mean.ci, data = oos.mod))
  rsquared.list[[i]] <- data.frame(model.name = paste0("model_", i),
                                   rsquared = summary.stats.list[[i]]$r.squared, 
                                   adj.rsquared = summary.stats.list[[i]]$adj.r.squared)
}

oos.rsquared <- do.call(rbind, rsquared.list)
oos.table <- left_join(oos.summary, oos.rsquared)

write.csv(oos.table, "outputs/SSM_pred_obs/out_of_sample_increment_validation.csv", row.names = FALSE)


# do the same for in sample increments

ins.list <- list()
ins.inc.files <- paste0("outputs/SSM_pred_obs/model_",1:6, "pred.obs.in.sample.inc.rds" )
ins.list <- lapply(ins.inc.files, readRDS)

for(i in 1:length(ins.list)){
  ins.list[[i]]$model.name  <- paste0("model_", i)
}

ins.df <- do.call(rbind, ins.list)

ins.df

ins.summary <- ins.df %>% group_by(model.name)  %>% summarise(MSPE = mean((diainc-mean.ci)^2, na.rm =TRUE),
                                                              RMSPE = sqrt(mean((diainc-mean.ci)^2, na.rm =TRUE)),
                                                              MAPE = mean(abs(diainc-mean.ci), na.rm = TRUE))#, 
#PPL = sum((diainc - mean.ci)^2) + sum(predvar)) #,
# V1 = mean(diainc-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
# V2 = (mean((diainc-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
# V3 = (mean((diainc-mean.ci)^2)^(1/2)))#, # goodness of fit estimate (small = better fit)
# #PPL = sum((diainc - mean.ci)^2) + sum(predvar)) # posterior predictive loss

ins.summary$validation <- "in-sample increments"

summary.stats <- summary(lm(diainc ~ mean.ci, data =ins.df))
#all.regress <- ins.df %>% group_by(model.name)%>% do(lm(diainc ~ mean.ci, .)) #%>% ungroup()

summary.stats.list <- rsquared.list <- list()
for(i in 1:length(ins.list)){
  ins.mod <- ins.df %>% filter(model.name %in% paste0("model_", i))
  #lms <- lm(diainc ~ mean.ci, data = ins.mod)
  summary.stats.list[[i]] <- summary(lm(diainc ~ mean.ci, data = ins.mod))
  rsquared.list[[i]] <- data.frame(model.name = paste0("model_", i),
                                   rsquared = summary.stats.list[[i]]$r.squared, 
                                   adj.rsquared = summary.stats.list[[i]]$adj.r.squared)
}

ins.rsquared <- do.call(rbind, rsquared.list)
ins.table <- left_join(ins.summary, ins.rsquared)

write.csv(ins.table, "outputs/in_sample_increment_validation.csv", row.names = FALSE)

# get in sample diameters:
dbh.list <- list()
dbh.inc.files <- paste0("outputs/SSM_pred_obs/model_",1:6, "pred.obs.within.sample.dbh.rds" )
dbh.list <- lapply(dbh.inc.files, readRDS)

for(i in 1:length(dbh.list)){
  dbh.list[[i]]$model.name  <- paste0("model_", i)
}

dbh.df <- do.call(rbind, dbh.list)

dbh.df

dbh.summary <- dbh.df %>% group_by(model.name)  %>% summarise(MSPE = mean((z.data-mean.ci)^2, na.rm =TRUE),
                                                              RMSPE = sqrt(mean((z.data-mean.ci)^2, na.rm =TRUE)),
                                                              MAPE = mean(abs(z.data-mean.ci), na.rm = TRUE))#, 
#PPL = sum((diainc - mean.ci)^2) + sum(predvar)) #,
# V1 = mean(diainc-mean.ci)/(sum(predvar)^(1/2))/n(), # estimate of bias in predictors over time (close to 0 = unbiased)
# V2 = (mean((diainc-mean.ci)^2)/(sum(predvar)/n())^(1/2)),  # estimate of accuracy of MSPEs (close to 1 = accurate)
# V3 = (mean((diainc-mean.ci)^2)^(1/2)))#, # goodness of fit estimate (small = better fit)
# #PPL = sum((diainc - mean.ci)^2) + sum(predvar)) # posterior predictive loss

dbh.summary$validation <- "in-sample diameters"

summary.stats <- summary(lm(z.data ~ mean.ci, data =dbh.df))
#all.regress <- dbh.df %>% group_by(model.name)%>% do(lm(diainc ~ mean.ci, .)) #%>% ungroup()

summary.stats.list <- rsquared.list <- list()
for(i in 1:length(dbh.list)){
  dbh.mod <- dbh.df %>% filter(model.name %in% paste0("model_", i))
  #lms <- lm(diainc ~ mean.ci, data = dbh.mod)
  summary.stats.list[[i]] <- summary(lm(z.data ~ mean.ci, data = dbh.mod))
  rsquared.list[[i]] <- data.frame(model.name = paste0("model_", i),
                                   rsquared = summary.stats.list[[i]]$r.squared, 
                                   adj.rsquared = summary.stats.list[[i]]$adj.r.squared)
}

dbh.rsquared <- do.call(rbind, rsquared.list)
dbh.table <- left_join(dbh.summary, dbh.rsquared)

write.csv(dbh.table, "outputs/in_sample_diameter_validation.csv", row.names = FALSE)



all.inc.table <-rbind(ins.table, oos.table, dbh.table)

# model description
model.table <- 
  data.frame(model = 1:6,
             model.name = paste0("model_", 1:6),
             predictors = c("tree random effect", 
                            "tree random effect + tree size", 
                            "tree random effect + tree size + annual Tmax", 
                            "tree random effect + tree size + annual Tmax + annual Precip", 
                            "tree random effect + tree size + annual Tmax + annual Precip + MAP + MAT", 
                            "tree random effect + tree size + annual Tmax + annual Precip + MAP + MAT + all two-way interactions")
  )

all.inc.table<- left_join(all.inc.table, model.table)
# make the table
all.inc.table %>% select(-MSPE, -MAPE, -adj.rsquared, -model.name) %>%
  select(model, predictors, RMSPE, rsquared, validation) %>% group_by(validation) %>% gt() |>
  tab_header(
    title = md("**Model Validation**")) |>
  # tab_spanner(
  #   label = "Model Description",
  #   columns = c(model, predictors)
  # )  |>
  tab_spanner(
    label = "Model Performance",
    columns = c(RMSPE, rsquared)
  )  |>
  tab_style(
    style = cell_fill(color = "gray98"),
    locations = cells_title()
  ) |>

  fmt_number(
    columns = c(RMSPE, rsquared
                ),
    decimals = 4,
    use_seps = FALSE
  )|>
  tab_style(
    style = cell_borders(
      sides = c( "b"),
      color = "darkgrey",
      weight = px(3)
    ),
    locations = list(cells_column_labels(), cells_stubhead())
  )|>
cols_label(
  rsquared = html("R<sup>2"))|> gtsave("/Users/kellyheilman/Documents/Treering_FIA_fusion_PIPO/outputs/model_validation_table.png", expand = 10)


