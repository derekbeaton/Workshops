# PLSC, PLSR, and PLS-canonical via Derek's GPLS package (see the libraries file for install)
## this helps us see how they're all related
# no resampling

source('R/_libraries.R')

TOY_DATA <- read.csv("./data/OND01_TOY_DATA.csv")

TOY_DATA %>%
  drop_na ->
  TOY_DATA_COMPLETE_CASES


TOY_DATA_COMPLETE_CASES %>%
  select(SUBJECT, AGE, NAGM_PERCENT, NAWM_PERCENT) %>%
  tibble::column_to_rownames(.,"SUBJECT") ->
  X_numeric


TOY_DATA_COMPLETE_CASES %>%
  select(SUBJECT, starts_with("TMT"), starts_with("Stroop")) %>%
  tibble::column_to_rownames(.,"SUBJECT") ->
  Y_numeric


## plain PLSC. This will be identical to what we see with TExPosition
plscor_res <- GPLS::pls_cor(X_numeric, Y_numeric, T, T, T, T)

## plain PLSR, with one of the variations of the SIMPLS algorithm
plsreg_res <- GPLS::pls_reg(X_numeric, Y_numeric, T, T, T, T)

## plain PLS-canonical because we like bonus material
plscan_res <- GPLS::pls_can(X_numeric, Y_numeric, T, T, T, T)


## component 1 is the same everywhere:
plscor_res$d
plsreg_res$d
plscan_res$d

plscor_res$u[,1]
plsreg_res$u[,1]
plscan_res$u[,1]

plscor_res$v[,1]
plsreg_res$v[,1]
plscan_res$v[,1]

plscor_res$lx[,1]
plsreg_res$lx[,1]
plscan_res$lx[,1]

plscor_res$ly[,1]
plsreg_res$ly[,1]
plscan_res$ly[,1]