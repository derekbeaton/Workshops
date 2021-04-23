# PLSR via pls, with internal cross-validation
  ### for more details and an expanded approach, see Chapter 6 of Intro. to Statistical Learning in R.

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



XY_data_frame <- data.frame(
  X = I(as.matrix(X_numeric)),
  Y = I(as.matrix(Y_numeric))
)


## a bit yuck, but whatever.
plsr_res <- plsr(Y ~ X, data = XY_data_frame)

## various diagnostic plots
plot(plsr_res)
plot(plsr_res, plottype = "scores", comps = 1:3)
plot(plsr_res, "loadings", comps = 1:3,legendpos = "topleft")


### but let's just focus on prediction of Y from subsets of components
plot(plsr_res, ncomp = 1) # from just 1 component (the first)
plot(plsr_res, ncomp = 2) # from just 2 components (the first and second)
plot(plsr_res, ncomp = 3) # from just 3 components (all of them)


### would you like to see a neat trick?

lm_res <- lm(Y~X,data = XY_data_frame)

## predicted/fitted values are the same
lm_res$fitted.values / plsr_res$fitted.values[,,3]

## residuals are the same
residuals(lm_res) / residuals(plsr_res)[,,3]





### plsr with cross-validation to select number of components
plsr_res_CV <- plsr(Y ~ X, data = XY_data_frame, validation = "CV", ncomp = 3)

## in the cross-validation plots we're looking for the *smallest* root mean square error of prediction (RMSEP)
plot(RMSEP(plsr_res_CV))

### it looks like each of the Y (cognition) variables would benefit from different numbers of components:

# Just 1 component: TMT_A, Stroop_color
# Just 2 components: TMT_B, Stroop_word, Stroop_switch
# All 3 components: Stroop_inhibit




