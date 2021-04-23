# PLSR via tidymodels with repeated cross-validation
  ### taken directly from https://www.tidymodels.org/learn/models/pls/

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




XY_together <- cbind(X_numeric, Y_numeric)
## step 1: normalize recipe
norm_rec <- 
  recipe(TMT_A_sec + TMT_B_sec + Stroop_color_sec + Stroop_word_sec + Stroop_inhibit_sec + Stroop_switch_sec ~ ., data = XY_together) %>%
  step_normalize(everything()) 

## step 2: set up k-folds
folds <- vfold_cv(XY_together, repeats = 10)

folds <- 
  folds %>%
  mutate(recipes = map(splits, prepper, recipe = norm_rec))


## step 3: a function we need
get_var_explained <- function(recipe, ...) {
  
  # Extract the predictors and outcomes into their own matrices
  y_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_outcomes())
  x_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_predictors())
  
  # The pls package prefers the data in a data frame where the outcome
  # and predictors are in _matrices_. To make sure this is formatted
  # properly, use the `I()` function to inhibit `data.frame()` from making
  # all the individual columns. `pls_format` should have two columns.
  pls_format <- data.frame(
    endpoints = I(y_mat),
    measurements = I(x_mat)
  )
  # Fit the model
  mod <- plsr(endpoints ~ measurements, data = pls_format)
  
  # Get the proportion of the predictor variance that is explained
  # by the model for different number of components. 
  xve <- explvar(mod)/100 
  
  # To do the same for the outcome, it is more complex. This code 
  # was extracted from pls:::summary.mvr. 
  explained <- 
    drop(pls::R2(mod, estimate = "train", intercept = FALSE)$val) %>% 
    # transpose so that components are in rows
    t() %>% 
    as_tibble() %>%
    # Add the predictor proportions
    mutate(predictors = cumsum(xve) %>% as.vector(),
           components = seq_along(xve)) %>%
    # Put into a tidy format that is tall
    pivot_longer(
      cols = c(-components),
      names_to = "source",
      values_to = "proportion"
    )
}


## step 4: k-folds for the function above

folds <- 
  folds %>%
  mutate(var = map(recipes, get_var_explained),
         var = unname(var))

## step 5: extraction of variance
  ### this will put all the k-folds from above into a single data frame. So we have a resampled estimate of explained variance for each response variable
  ### the last line here aggregates it into a mean, although we do in fact have a *distribution* for each item
variance_data <- 
  bind_rows(folds[["var"]]) %>%
  filter(components <= 3) %>%
  group_by(components, source) %>%
  summarize(proportion = mean(proportion))

## step 6: taking a look
  ## what we see here is mean explained variance for the predictors (all Xs) and cognition
  ## X will always end up with a full 100% of explained variance with all components
  ## It takes 2 components to explain a lot of X
  ## the response variables aren't great. But TMT_A has the highest explained variance
  ## that being said: there's almost no increase in information when using more components.
  ### so we just want 1 component.
ggplot(variance_data, aes(x = components, y = proportion, col = source)) + 
  geom_line() + 
  geom_point()

