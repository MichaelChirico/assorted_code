####################################
# Continuous ASPES Method Practice #
####################################

# Initial session clean-up
rm(list=ls(all=TRUE))
gc()

library(data.table)
# # For data in .sas7bdat or .dta format
# library(haven)
# # For data in .xls/.xlsx format
# library(readxl)

#***************Change me!!****************
setwd("~/Documents/assorted_code/ASPES") #*
#******************************************

# Set random seed for replicability
set.seed(103857)

# Data import; uncomment to use your
#   preferred data source
# # For .dta format
# data <- setDT(read_dta("Stata_mock_data.dta"))
# # For .sas7bdat format
# data <- setDT(read_sas("SAS_mock_data.sas7bdat"))
# # For .xlsx format
# data <- setDT(read_excel("mock_data.xlsx"))
# For .csv format
data <- fread("mock_data.csv")

# Data dictionary:
#   - y : outcome of interest
#   - Ma : actual mediator
#   - Mp : predicted mediator (defined below)
#   - T : treatment dummy (redefined as treat below)
#   - x1,...,5 : baseline student-level covariates
#   - z1,...,5 : baseline student-level covariates
#   - w1,...,5 : baseline student-level covariates

# Since T is special in R
setnames(data, "T", "treat")

# The full base model includes all 15 x, z, and w variables
incl_vars <- paste0(rep(c("x","z","w"), each = 5), 1:5)

# *****     PART 1A     *****

# First, remove any variables which are 
#   linearly indistinguishable (perfect or
#   near-perfect multicollinearity)
# 1) Fit full model
# 2) Delete any covariates with missing
#    coefficients
full_model_coef <- 
  #Note that treat==0 observations are
  #  automatically excluded since Ma
  #  is missing for all such rows
  data[ , lm(Ma ~ ., data = .SD)$coefficients, 
        .SDcols = c("Ma", incl_vars)]

incl_vars <-
  setdiff(incl_vars, 
          names(full_model_coef[which(is.na(full_model_coef))]))

# Using step-wise backwards selection on p-values
# Algorithm:
#   1) Regress full model
#   2) Is the highest-p-value coefficient
#      significant at level alpha? If so, finish;
#      if not, delete this regressor and repeat
alpha <- .05
while (TRUE){
  #data = .SD lets us use . in lm to 
       #  stand in for the full model,
       #  while .SDcols below is what we update
       #  from step to step after each elimination
  data[, 
       if (x <- sort(summary(lm(Ma ~ ., data = .SD))$
                     #First row is intercept; exclude
                     coefficients[-1L, "Pr(>|t|)"], 
                     decreasing=TRUE)[1L] <= alpha) 
         #a bit advanced, but basically we want to
         #  send a break command if our exercise is
         #  complete, i.e. the least-significant
         #  regressor is anyway significant at level alpha
         evalq(break, parent.env(environment()))
       #if x is not significant, delete it, being sure
       #  to pass the assignment up through the 
       #  environments with <<- instead of <-
       else incl_vars <<- setdiff(incl_vars, names(x)), 
       #Setting .SDcols allows us to exclude y, id, and treat,
       #  none of which are being considered for selection
       .SDcols = c("Ma", incl_vars)]
  if (!length(incl_vars)) 
    stop("No significant predictors of mediator; ",
         "consider lowering alpha or getting better covariates")
}

# *****    PART 1B     *****

# Assign CV groups at random
#   using rep_len to guarantee equal representation
n_groups <- 10
data[ , CV_group := sample(rep_len(1:n_groups, .N))]

# Use out-of-sample prediction to predict MP for each group
# Approach:
# 1) Fix CV_group k
# 2) Regress Ma on incl_vars (as chosen above)
#    __among observations in **other** CV groups__
# 3) Using these coefficients, predict the value
#    of Ma (using the corresponding data _within_ CV_group k)
# 4) repeat for all groups

# Set index for slight speed-up
setkey(data, CV_group)
data[ , Mp :=
        predict(
          #run the regression on the _rest_
          #  of the data, i.e., excluding this CV_group
          data[!.(.BY$CV_group),
               lm(Ma ~ ., data = .SD),
               .SDcols = c("Ma", incl_vars)],
          #use current group's data for prediction
          newdata = .SD), by = CV_group]

# Assess prediction performance
#   Objects of interest:
#     - t-stat on Mp: Is Mp significantly related to Ma?
#     - R-squared: Is there a lot of unexplained variation?
data[ , summary(lm(Ma ~ Mp - 1))]

# Plot:
data[order(Mp), {
  plot(Mp, Ma)
  matplot(Mp, predict(lm(Ma ~ poly(Mp, 2)), 
                      .SD, interval = "confidence"),
          add = TRUE, type = "l", lty = c(1, 2, 2),
          lwd = 3, col = "blue")}]

# *****     PART 2     *****

# **Optional -- re-scale predictor
# data[ , Mp := Mp - mean(Mp)]

# Finally, full regression:
data[ , summary(lm(y ~ Mp*treat + ., data = .SD)),
      .SDcols = c("y", "Mp", "treat", incl_vars)]

# Coefficient interpretation:
#   beta_Mp + beta_Mp:treat is the marginal effect 
#     of the mediator on the outcome for 
#     individuals in the treatment group
#   beta_Mp:treat is the indirect effect of the 
#     treatment on the outcome, operating through the mediator
#   beta_treat + beta_Mp:treat is the total 
#     effect of treatment on the outcome