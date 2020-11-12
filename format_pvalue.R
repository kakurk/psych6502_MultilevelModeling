format_pvalue <- function(p){
  # take a pvalue vector and replace the numeric representations with informative strings
  case_when(p < .001 ~ '<.001',
            p < .01 ~ '<.01',
            p < .05 ~ '<.05',
            TRUE ~ round(p, digits = 2) %>% as.character())
}