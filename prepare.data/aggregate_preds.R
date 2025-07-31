# load pkgs
library(tidyverse)

# path where interpolation results are stored
pth = 'C:/Users/midolo/OneDrive - CZU v Praze/czu/intrplEU_EIV/interpolated_EIV_change/preds/'

# loop & export
eiv_names <- c('L','M','N','R','T')
for (i in eiv_names) {
  
  # load data
  dat <- paste0(pth, 'EIV_', i,  '.preds.rf.csv.gz') %>%
    read_csv(show_col_types = FALSE) %>%
    dplyr::select(x, y, habitat, year, contains('eiv_pred_')) %>%
    filter(year >= 1960 & year <= 2020)

   # manually centering plots by rounding coordinates
   fact <- 1000*10 # 10 km distance
   dat <- dat %>%
     mutate(x = round(x / fact) * fact,
            y = round(y / fact) * fact) %>%
     group_by(x, y, habitat) %>%
     summarise(n = n(),
               # number of plots
               across(where(is.numeric), mean, .names = "{.col}"),
               # average predictions across nearest plots per year
               .groups = 'drop')
   
   # export
   write_rds(
    dat,
    paste0(i,'_data.rds')
   )
}