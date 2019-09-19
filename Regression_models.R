
##########----------------------------------------------------------------------------------------------------##########
#         Modelling university attendance at age 19 and 22 
##########----------------------------------------------------------------------------------------------------##########


##########----------------------------------------------------------------------------------------------------##########
#         This version uses consistent weights AND has a dummy for missing ATAR scores
##########----------------------------------------------------------------------------------------------------##########



##### 1995 model #####


LSAY_1995_data_age_22.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_1995_data[which(!is.na(LSAY_1995_data$WT2003C)),]), 
                                     weights = LSAY_1995_data[which(!is.na(LSAY_1995_data$WT2003C)),"WT2003C"])


Prediction_model_1995 <- as.character("Gender+Indigenous+Achievement+NESB+State+
                                   Parent_SES_quartile+Parent_edu+
                                      School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                      Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend")
Inference_model_1995 <- as.character("Gender+Indigenous+Achievement+NESB+State+
                              Parent_SES_quartile+Parent_edu+
                                     School_SES_quartile+Sector+Location_RR_wave1+Educ_occupation_quartile+
                                     Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend+Books")

Formulas <- as.data.frame(c(Prediction_model_1995,Inference_model_1995), stringsAsFactors=FALSE)
Formulas <- as.character(Formulas[,1])

Formulas_age_22 <- paste(rep(paste("Uni_attend_age_22","~"), each = length(Formulas)), Formulas)

Formulas_age_22_Go8 <- gsub("Closest_uni", "Closest_uni_Go8", Formulas_age_22)
Formulas_age_22_Go8 <- gsub("Uni_attend_age_22", "top_8_university_age_22", Formulas_age_22_Go8)

models_1995 <- c(lapply(Formulas_age_22,FUN = function(X) svyglm(X, design=LSAY_1995_data_age_22.w, family=quasibinomial(link="probit"),x=TRUE)),
                 lapply(Formulas_age_22_Go8,FUN = function(X) svyglm(X, design=LSAY_1995_data_age_22.w, family=quasibinomial(link="probit"),x=TRUE)))


# Modify the variance-covariance matrix so that robust standard errors are computed.
# The vcovHC() function returns the variance-covariance matrix under the assumption of “HC” (Heteroskedasticity-consistent) estimation.
models_1995 <- lapply(models_1995, function(x){
  x$cov.unscaled <- vcovHC(x, type="HC1")
  return(x)
})




##### 1998 model #####

LSAY_1998_data_age_22.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_1998_data[which(!is.na(LSAY_1998_data$WT2006C)),]), 
                                     weights = LSAY_1998_data[which(!is.na(LSAY_1998_data$WT2006C)),"WT2006C"])


Prediction_model_1998 <- as.character("Gender+Indigenous+Achievement+NESB+State+
                                   Parent_SES_quartile+Parent_edu+
                                      School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                      Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend")
Inference_model_1998 <- as.character("Gender+Indigenous+Achievement+NESB+State+
                                     Parent_SES_quartile+Parent_edu+
                                     School_SES_quartile+Sector+Location_RR_wave1+Educ_occupation_quartile+
                                     Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend+Books")
Formulas <- as.data.frame(c(Prediction_model_1998,Inference_model_1998), stringsAsFactors=FALSE)

Formulas <- as.character(Formulas[,1])

Formulas_age_22 <- paste(rep(paste("Uni_attend_age_22","~"), each = length(Formulas)), Formulas)

Formulas_age_22_Go8 <- gsub("Closest_uni", "Closest_uni_Go8", Formulas_age_22)
Formulas_age_22_Go8 <- gsub("Uni_attend_age_22", "top_8_university_age_22", Formulas_age_22_Go8)

models_1998 <- c(lapply(Formulas_age_22,FUN = function(X) svyglm(X, design=LSAY_1998_data_age_22.w, family=quasibinomial(link="probit"),x=TRUE)),
                 lapply(Formulas_age_22_Go8,FUN = function(X) svyglm(X, design=LSAY_1998_data_age_22.w, family=quasibinomial(link="probit"),x=TRUE)))

# Modify the variance-covariance matrix so that robust standard errors are computed.
# The vcovHC() function returns the variance-covariance matrix under the assumption of “HC” (Heteroskedasticity-consistent) estimation.
models_1998 <- lapply(models_1998, function(x){
  x$cov.unscaled <-vcovHC(x, type="HC1")
  return(x)
})




##### 2003 model #####

Prediction_model_2003 <- as.character("Gender+Indigenous+PISA+NESB+State+ATAR_group_imputed+ATAR_missing_dummy+Grade+
                                   Parent_SES_quartile+Parent_edu+
                                      School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                      Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend")

Inference_model_2003 <- as.character("Gender+Indigenous+PISA+NESB+State+
                                     Parent_SES_quartile+Parent_edu+
                                     School_SES_quartile+Sector+Location_RR_wave1+Educ_occupation_quartile+
                                     Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend+Books")  


Formulas <- as.data.frame(c(Prediction_model_2003,Inference_model_2003), stringsAsFactors=FALSE)
Formulas <- as.character(Formulas[,1])

Formulas_age_22 <- paste(rep(paste("Uni_attend_age_22","~"), each = length(Formulas)), Formulas)

Formulas_age_22_Go8 <- gsub("Closest_uni", "Closest_uni_Go8", Formulas_age_22)
Formulas_age_22_Go8 <- gsub("Uni_attend_age_22", "top_8_university_age_22", Formulas_age_22_Go8)

# Prediction weights #
LSAY_2003_data_age_22.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003_data$WT2010CP)),]), 
                                     weights = LSAY_2003_data[which(!is.na(LSAY_2003_data$WT2010CP)),"WT2010CP"])

#Inference weights#
LSAY_2003_data_age_22_i.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003_data$WT2010CPi)),]), 
                                       weights = LSAY_2003_data[which(!is.na(LSAY_2003_data$WT2010CPi)),"WT2010CPi"])

designs_age_22 <- list(LSAY_2003_data_age_22.w,LSAY_2003_data_age_22_i.w)
designs_age_22_Go8 <- list(LSAY_2003_data_age_22.w,LSAY_2003_data_age_22_i.w)


models_2003 <- c(mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                         z = Formulas_age_22, y = designs_age_22, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                  mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                         z = Formulas_age_22_Go8, y = designs_age_22_Go8, SIMPLIFY = FALSE, USE.NAMES = FALSE))

# Modify the variance-covariance matrix so that robust standard errors are computed.
# The vcovHC() function returns the variance-covariance matrix under the assumption of “HC” (Heteroskedasticity-consistent) estimation.
models_2003 <- lapply(models_2003, function(x){
  x$cov.unscaled <-vcovHC(x, type="HC1")
  return(x)
})



##### 2006 model #####

Prediction_model_2006 <- as.character("Gender+Indigenous+PISA+NESB+State+ATAR_group_imputed+ATAR_missing_dummy+Grade+
                                   Parent_SES_quartile+Parent_edu+
                                      School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                      Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend")

Inference_model_2006 <- as.character("Gender+Indigenous+PISA+NESB+State+
                                     Parent_SES_quartile+Parent_edu+
                                     School_SES_quartile+Sector+Location_RR_wave1+Educ_occupation_quartile+
                                     Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend+Books")

Formulas <- as.data.frame(c(Prediction_model_2006,Inference_model_2006), stringsAsFactors=FALSE)
Formulas <- as.character(Formulas[,1])

Formulas_age_22 <- paste(rep(paste("Uni_attend_age_22","~"), each = length(Formulas)), Formulas)

Formulas_age_22_Go8 <- gsub("Closest_uni", "Closest_uni_Go8", Formulas_age_22)
Formulas_age_22_Go8 <- gsub("Uni_attend_age_22", "top_8_university_age_22", Formulas_age_22_Go8)


# Prediction weights #
LSAY_2006_data_age_22.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2013CP)),]), 
                                     weights = LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2013CP)),"WT2013CP"])


# Inference weights #
LSAY_2006_data_age_22_i.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2013CPi)),]), 
                                     weights = LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2013CPi)),"WT2013CPi"])

designs_age_22 <- list(LSAY_2006_data_age_22.w,LSAY_2006_data_age_22_i.w)
designs_age_22_Go8 <- list(LSAY_2006_data_age_22.w,LSAY_2006_data_age_22_i.w)

models_2006 <- c(mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                        z = Formulas_age_22, y = designs_age_22, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                 mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                        z = Formulas_age_22_Go8, y = designs_age_22_Go8, SIMPLIFY = FALSE, USE.NAMES = FALSE))

# Modify the variance-covariance matrix so that robust standard errors are computed.
# The vcovHC() function returns the variance-covariance matrix under the assumption of “HC” (Heteroskedasticity-consistent) estimation.
models_2006 <- lapply(models_2006, function(x){
  x$cov.unscaled <-vcovHC(x, type="HC1")
  return(x)
})




##### 2009 model #####

Dependent_variables_age_22 <- c("Uni_attend_age_22","top_8_university_age_22")


Prediction_model_2009 <- as.character("Gender+Indigenous+PISA+NESB+State+ATAR_group_imputed+ATAR_missing_dummy+Grade+
                                   Parent_SES_quartile+Parent_edu+
                                      School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                      Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend")

Inference_model_2009 <- as.character("Gender+Indigenous+PISA+NESB+State+
                                     Parent_SES_quartile+Parent_edu+
                                     School_SES_quartile+Sector+Location_RR_wave1+Educ_occupation_quartile+
                                     Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend+Books")


Formulas <- as.data.frame(c( Prediction_model_2009,Inference_model_2009), stringsAsFactors=FALSE)

Formulas <- as.character(Formulas[,1])

Formulas_age_22 <- paste(rep(paste("Uni_attend_age_22","~"), each = length(Formulas)), Formulas)

Formulas_age_22_Go8 <- gsub("Closest_uni", "Closest_uni_Go8", Formulas_age_22)
Formulas_age_22_Go8 <- gsub("Uni_attend_age_22", "top_8_university_age_22", Formulas_age_22_Go8)


# Prediction weights #
LSAY_2009_data_age_22.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009_data$WT2016CP)),]), 
                                     weights = LSAY_2009_data[which(!is.na(LSAY_2009_data$WT2016CP)),"WT2016CP"])


# Inference weights #
LSAY_2009_data_age_22_i.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009_data$WT2016CPi)),]), 
                                       weights = LSAY_2009_data[which(!is.na(LSAY_2009_data$WT2016CPi)),"WT2016CPi"])

designs_age_22 <- list(LSAY_2009_data_age_22.w,LSAY_2009_data_age_22_i.w)
designs_age_22_Go8 <- list(LSAY_2009_data_age_22.w,LSAY_2009_data_age_22_i.w)

Formulas_age_22_non_Go8 <- gsub("Uni_attend_age_22", "Other_university_age_22", Formulas_age_22)
Inference_model_2009_non_Go8 <- svyglm(Formulas_age_22_non_Go8[2], design=LSAY_2009_data_age_22_i.w, family=quasibinomial(link="probit"),x=TRUE)


models_2009 <- c(mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                        z = Formulas_age_22, y = designs_age_22, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                 mapply(function (z,y) svyglm(z, design=y, family=quasibinomial(link="probit"),x=TRUE), 
                        z = Formulas_age_22_Go8, y = designs_age_22_Go8, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                 list(Inference_model_2009_non_Go8))

# Modify the variance-covariance matrix so that robust standard errors are computed.
# The vcovHC() function returns the variance-covariance matrix under the assumption of “HC” (Heteroskedasticity-consistent) estimation.
models_2009 <- lapply(models_2009, function(x){
  x$cov.unscaled <- vcovHC(x, type="HC1")
  return(x)
})


rm(Formulas_age_22,Formulas_age_22_Go8,
   LSAY_1995_data_age_22.w,LSAY_1998_data_age_22.w,
   LSAY_2003_data_age_22.w,LSAY_2003_data_age_22_i.w,
   LSAY_2006_data_age_22.w,LSAY_2006_data_age_22_i.w,
   LSAY_2009_data_age_22.w,LSAY_2009_data_age_22_i.w,
   designs_age_22,designs_age_22_Go8,Formulas_age_22_non_Go8)

Prediction_model_1995 <- models_1995[[1]]
Inference_model_1995 <- models_1995[[2]]
Prediction_model_1995_Go8 <- models_1995[[3]]
Inference_model_1995_Go8 <- models_1995[[4]]

Prediction_model_1998 <- models_1998[[1]]
Inference_model_1998 <- models_1998[[2]]
Prediction_model_1998_Go8 <- models_1998[[3]]
Inference_model_1998_Go8 <- models_1998[[4]]

Prediction_model_2003 <- models_2003[[1]]
Inference_model_2003 <- models_2003[[2]]
Prediction_model_2003_Go8 <- models_2003[[3]]
Inference_model_2003_Go8 <- models_2003[[4]]

Prediction_model_2006 <- models_2006[[1]]
Inference_model_2006 <- models_2006[[2]]
Prediction_model_2006_Go8 <- models_2006[[3]]
Inference_model_2006_Go8 <- models_2006[[4]]

Prediction_model_2009 <- models_2009[[1]]
Inference_model_2009 <- models_2009[[2]]
Prediction_model_2009_Go8 <- models_2009[[3]]
Inference_model_2009_Go8 <- models_2009[[4]]
Inference_model_2009_non_Go8 <- models_2009[[5]]

rm(models_1995,models_1998,models_2003,models_2006,models_2009)

