generate_mixed_model_tbl <- function(modelFit, modelLabel = 'model'){
  # function that takes in a fitted linear mixed model object

  require(tidyverse)
  source('format_pvalue.R')

  summary(modelFit) -> modelSummary

  confint(modelFit, oldNames = FALSE, prof.scale = 'varcov') %>% 
    as_tibble(rownames = 'parameter') -> confidenceIntervals

  # Fixed effects -----------------------------------------------------------

  # Extract the coefficient list element
  # make it a tibble with the rownames in a column called 'parameter'
  # round the table entries
  # remove the parenthesis around "Intercept"
  # format the table
  modelSummary %>%
    magrittr::extract2('coefficients') %>%
    as_tibble(rownames = 'parameter') %>%
    left_join(confidenceIntervals, by = 'parameter') %>%
    mutate(Estimate = round(Estimate, digits = 3),
           `Std. Error` = round(`Std. Error`, digits = 3),
           df = round(df),
           `t value` = round(`t value`, 2),
           parameter = str_remove_all(parameter, '\\(?\\)?'),
           `2.5 %` = round(`2.5 %`, digits = 3),
           `97.5 %` = round(`97.5 %`, digits = 3)) %>%
    transmute(`Fixed effects` = parameter,
              `Estimate (SE)` = str_glue('{Estimate} ({`Std. Error`})'),
              `t (df)` = str_glue('{`t value`} ({df})'),
              `p value` = format_pvalue(`Pr(>|t|)`),
              `95% confidence interval` = str_glue('[{`2.5 %`},{`97.5 %`}]')) -> FixedEffects.Tbl

  write_csv(x = FixedEffects.Tbl, path = str_glue('{modelLabel}_fixedEffects.csv'))

  # Random effects ----------------------------------------------------------
  # Create the random effects section of the model table.

  # 1. Extract the variance-covariance of the random effects from the model fit
  # 2. Make the output a data.frame, ordering according to the lower triangle of
  #    the variance covariance matrix.
  # 3. Drop the column that contains the default standard deviation/correlation
  #    display.

  VarCorr(modelFit) %>% 
    as.data.frame(order = 'lower.tri') %>% 
    dplyr::select(-sdcor) -> RandEff_VarCov

  nRandEffects <- nrow(RandEff_VarCov)

  confidenceIntervals %>% 
    slice(1:nRandEffects) -> condfidenceIntervals.RandEff

  GrpVarName <- getME(modelFit, 'flist') %>% names()

  bind_cols(RandEff_VarCov, condfidenceIntervals.RandEff) %>%
    dplyr::select(-grp, -var1, -var2) %>%
    dplyr::select(parameter, vcov, `2.5 %`, `97.5 %`) %>%
    mutate(parameter = str_replace(parameter, '\\(Intercept\\)', 'Intercept')) %>%
    mutate(parameter = str_replace(parameter, '(?<=var)_', '(')) %>%
    mutate(parameter = str_replace(parameter, '(?<=cov)_', '(')) %>%
    mutate(parameter = str_replace(parameter, '\\.', ',')) %>%
    mutate(parameter = str_replace(parameter, '\\|', ')')) %>%
    mutate(parameter = str_remove(parameter, GrpVarName)) %>%
    mutate(vcov = round(vcov, 4),
           `2.5 %` = round(`2.5 %`, 4),
           `97.5 %` = round(`97.5 %`, 4)) %>%
    add_column(`Likelihood ratio test` = '', `p value` = '') %>%
    transmute(`Random effect` = parameter,
              `Estimate` = vcov,
              `Likelihood ratio test` = `Likelihood ratio test`,
              `p value` = `p value`,
              `95% confidence interval` = str_glue('[{format(`2.5 %`, scientific = FALSE)},{format(`97.5 %`, scientific = FALSE)}]')) -> RandomEffects.Tbl

  write_csv(x = RandomEffects.Tbl, path = str_glue('{modelLabel}_randEffects.csv'))

}