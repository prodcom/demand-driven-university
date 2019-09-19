
##########----------------------------------------------------------------------------------------------------##########
#         Bootstrap, clean version
##########----------------------------------------------------------------------------------------------------##########

Additional_students_function_BS <- function(df2 = df2_boot, model1 = Prediction_model_1_boot,model2 = Prediction_model_2_boot){
  
  Variables <- all.vars(as.formula(model2)) 
  dependent_variable <- paste(all.vars(as.formula(model2))[1])
  
  Predictions <- df2 %>% select(ID_new,one_of(Variables)) %>% na.omit() %>% 
    bind_cols(.,as_tibble(predict(model1, newdata = ., type = "response"))) %>% rename(Prediction1 = value) %>% 
    bind_cols(.,as_tibble(predict(model2, newdata = ., type = "response"))) %>% rename(Prediction2 = value) %>% 
    select(ID_new,Prediction1,Prediction2) %>% as_tibble()
  
  Predictions <- Predictions %>% right_join(.,df2 %>% select(ID,ID_new,one_of(dependent_variable)), by="ID_new")
  
  Predictions <- Predictions %>% mutate(Other_student = ifelse(df2 %>% pull(paste0(dependent_variable)) == 1,
                                                              ifelse(Predictions$Prediction1 > Predictions$Prediction2,1,
                                                                     Predictions$Prediction1/Predictions$Prediction2),0),
                                        Additional_student =  ifelse(df2 %>% pull(paste0(dependent_variable)) == 1,
                                                                     ifelse(Predictions$Prediction1 > Predictions$Prediction2,0,
                                                                            (Predictions$Prediction2-Predictions$Prediction1)/Predictions$Prediction2),0),
                                        Never_student = ifelse(df2 %>% pull(paste0(dependent_variable)) == 0,
                                                             ifelse(Predictions$Prediction1 < Predictions$Prediction2,1,
                                                                    (1-Predictions$Prediction1)/(1-Predictions$Prediction2)),0),
                                        Fewer_student = ifelse(df2 %>% pull(paste0(dependent_variable)) == 0,
                                                       ifelse(Predictions$Prediction1 < Predictions$Prediction2,0,
                                                              (Predictions$Prediction1-Predictions$Prediction2)/(1-Predictions$Prediction2)),0))

  return(Predictions)
}



clean_bootstap_results <- function(boot_results = Boot_result2){ 
  Boot_result <- bind_rows(Boot_result)
  variable_names <- data.frame()
  variable_names <- as.data.frame(c("Total","Equity_group","PQ1", "PQ2", "PQ3", "PQ4", "Not indigenous", "Indigenous", "Independent",
                                    "Catholic", "Government", "Not university", "University", "Male",
                                    "Female", "Under 40km", "40 to 80km", "Over 80km", "Uni", "No_uni",
                                    "English speaking background", "Non-English", "lower", "mid-lower", "Regional_remote",
                                    "mid-upper", "upper", "Disadvantaged", "Not_disadvantaged",
                                    "Aspiration","Parent_SES_quartile","Closest_uni_distance_g2","Parent_edu","PISA_quartile","NESB","Sector",
                                    "Location_RR","Gender","Non-traditional_areas","ATAR_group_imputed","No_ATAR"))
  colnames(variable_names)[1] <- "Old_names"
  variable_names$New_names <- c("Total","Equity group","Q1", "Q2", "Q3", "Q4", "Non-Indigenous", "Indigenous", "Independent",
                                "Catholic", "Public", "No aspiration", "Aspiration", "Male",
                                "Female", "Under 40km", "40 to 80km", "Over 80km", "University",
                                "Below university", "English speaking", "NESB", "Q1", "Q2", "Regional or remote",
                                "Q3", "Q4", "Disadvantaged", "Not disadvantaged",
                                "Aspiration","Socioeconomic status","Distance to a university","Parent's education","PISA score","Ethnicity","School sector",
                                "Location","Gender","Non-traditonal areas of study","ATAR","No ATAR")

  Boot_result$Category <- ifelse(is.na(with(variable_names,New_names[match(Boot_result$Name,Old_names)])),
                                 Boot_result$Name,with(variable_names,New_names[match(Boot_result$Name,Old_names)]))

  Boot_result$Name <- Boot_result$Equity_group

  Boot_result$Variable <- ifelse(is.na(with(variable_names,New_names[match(Boot_result$Equity_group,Old_names)])),
                                 Boot_result$Equity_group,with(variable_names,New_names[match(Boot_result$Equity_group,Old_names)]))

  Boot_result <- Boot_result[c(6,7,8,2,3,4,5)] # change column order
  return(Boot_result)
}



# Boot function returns dataframes for all iterations
boot_function <- function(df1 = data_model_only_2003, df2 = data_model_only_2009){  #df1,df2,weight1,weight2){ 
  
  df1_boot <- df1[sample(nrow(df1), size=nrow(df1),replace=TRUE,prob=df1$Sample_weight), ]
  df2_boot <- df2[sample(nrow(df2), size=nrow(df2),replace=TRUE,prob=df2$Sample_weight), ]
  df2_boot$ID_new <- seq.int(nrow(df2_boot)) # Create a unique identifier
  
  Prediction_model_1_boot <- glm(Uni_attend_age_22~Gender+Indigenous+PISA+NESB+State+ATAR_group_imputed+ATAR_missing_dummy+Grade+
                                   Parent_SES_quartile+Parent_edu+
                                   School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                   Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend, 
                                 data=df1_boot, family=binomial(link="probit"),x=TRUE)
  
  Prediction_model_2_boot <- glm(Uni_attend_age_22~Gender+Indigenous+PISA+NESB+State+ATAR_group_imputed+ATAR_missing_dummy+Grade+
                                   Parent_SES_quartile+Parent_edu+
                                   School_SES_quartile+Sector+Log_closest_uni+Educ_occupation_quartile+
                                   Attend_year_12+Hours_worked_in_year_12+Hours_worked_age_of_yr_12_interaction_attend, 
                                 data=df2_boot, family=binomial(link="probit"),x=TRUE)
  
  Prediction_new <- Additional_students_function_BS(df2 = df2_boot, model1 = Prediction_model_1_boot, 
                                                    model2 = Prediction_model_2_boot)
  
  df2_boot$Additional_student <- Prediction_new$Additional_student
  df2_boot$Never_student <- Prediction_new$Never_student
  df2_boot$Fewer_student <- Prediction_new$Fewer_student
  df2_boot$Other_student <- Prediction_new$Other_student
  
  df2_boot <- df2_boot %>% select(ID,STIDSTD,Additional_student,Never_student,Fewer_student,Other_student) # only keep neeeded columns
  
  return(df2_boot)
  
}


Run_bootstrap <- function(cohort=2009,number_of_bootstrap_samples = 5000){ 
  if(cohort %ni% c(2006,2009))  stop("The cohort must be 2006 or 2009")
  
  Variables_1 <- all.vars(as.formula(Prediction_model_2003))
  df1 <- LSAY_2003_data %>% select(ID,STIDSTD,Sample_weight,one_of(Variables_1))
  
  Variables_2 <- all.vars(as.formula(get(paste0("Prediction_model_",cohort))))
  df2 <- get(paste0("LSAY_",cohort,"_data")) %>% select(ID,STIDSTD,Sample_weight,one_of(Variables_2),Equity_group,Location_RR_wave1,PISA_quartile,Total)
  
  assign("data_model_only_2003", df1,.GlobalEnv)
  assign(paste0("data_model_only_",cohort), df2,.GlobalEnv)

ptm <- proc.time()
start_time_boot <- Sys.time()
no_cores <- max(1,detectCores() - 2) # use all cores except 2
cat("Setting up clusters for parallel computation. Process will use",paste(no_cores),"CPU cores ... \n")
cl <- makeCluster(no_cores)
clusterExport(cl, list("boot_function","Additional_students_function_BS","data_model_only_2003",paste0("data_model_only_",cohort)))
clusterEvalQ(cl, list(library(dplyr),library(tibble),library(survey),library(mvtnorm)))

cat("Beginning bootstrap. May take several minutes ... \n")
Boot_results <- parLapply(cl,1:number_of_bootstrap_samples, function(x) {boot_function(df1=data_model_only_2003,df2=get(paste0("data_model_only_",cohort)))}) # run function 5000 iterations
stopCluster(cl)
cat("CPU time taken for bootstrap: ")
cat(seconds_to_period((proc.time() - ptm)["elapsed"]) ) # 5000 iterations takes around 16 mins (5 mins with parallel)
cat("\n")
end_time_boot <- Sys.time()
cat("System time taken for bootstrap: ")
print(end_time_boot - start_time_boot)
cat("\n")
table.length <- nrow(Boot_results[[1]]) ## Assuming all tables are of the same length, which they should be
gc()
Boot_results_dt <- data.table::rbindlist(Boot_results)
gc()
Boot_results_dt[, tableid := rep(1:(nrow(Boot_results_dt) / table.length), each = table.length)] # add tableID column
cat("Saving bootstrap data to CSV ... \n")
data.table::fwrite(Boot_results_dt, file = paste0("Boot_results_",cohort,".csv"))
cat("Saving bootstrap data to the global environment ... \n")
assign(paste0("Boot_results_",cohort), Boot_results_dt,.GlobalEnv)
}






Outcome_groups_total <- c("Other_student","Additional_student","Never_student")
Outcome_groups <- c("AT_outcome","AS_outcome","NT_outcome") # They need to be in the same order 
Strata_variables_2006 <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST04Q01","AUSIMMIG","W_FSTUWT")
Strata_variables_derived_2006 <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile","Parent_SES_quartile","Location_RR_wave1")
Strata_variables_2009 <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST04Q01","IMMIG","W_FSTUWT")
Strata_variables_derived_2009 <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile",
                                   "Parent_SES_quartile","Location_RR_wave1")

### The outcome variable must be in LSAY_2006_data
#Boot_results_dt <- Boot_results_2009
#Cohort <- 2009
# OUTCOME FUNCTION DATA.TABLE version

outcome_function <- function(Outcome_variable,Boot_results_dt,Cohort,Age){ 
  
  Prediction_model <- get(paste0("Prediction_model_",Cohort))
  Strata_variables <- get(paste0("Strata_variables_",Cohort))
  Strata_variables_derived <- get(paste0("Strata_variables_derived_",Cohort))
  Uni_dropout <- paste0("Uni_dropout_age_",Age) #c("Uni_dropout_age_23","Uni_dropout_age_25")
  Uni_dropout_outcome <- paste0("Uni_dropout_age_",Age,"_outcome")    #c("Uni_dropout_age_23_outcome","Uni_dropout_age_25_outcome")
  Uni_complete <- paste0("Uni_complete_age_",Age)
  Not_dropout <- c("Other_student_not_dropout","Additional_student_not_dropout")
  Not_dropout_outcome <-  c("Other_student_not_dropout_outcome","Additional_student_not_dropout_outcome")
  Year = ifelse(Cohort == 2006, 2014, 2017)
  
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(STIDSTD)) 
  LSAY_raw <- get(paste0("LSAY_",Cohort)) %>% mutate(ID = as.numeric(STIDSTD)) %>% select(ID,Strata_variables)
  
  LSAY_data$Dropout <- ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "Yes", 1,
                              ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "No",0, NA))
  
  LSAY_data$Completed <- ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "Yes", 1,
                                ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "No",0, NA))
  
  LSAY_data$Undertaking <- ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 1, 1,
                                  ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 0,0, NA))
  
  LSAY_data$Postgrad <- ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %in% "Complete and undertaking further studies", 1,
                               ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %ni% NA,0, NA))

  LSAY_data$Employed_FT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Full_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Employed_PT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Part_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Unemployed <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Unemployed", 1,
                                 ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$NILF <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "NILF", 1,
                           ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))  
  
  LSAY_data$Average_weekly_pay <- LSAY_data %>% pull(paste0("Average_weekly_pay_",Year))
  LSAY_data$Average_hourly_pay <- LSAY_data %>% pull(paste0("Average_hourly_pay_",Year))
  LSAY_data$Average_hours_worked <- LSAY_data %>% pull(paste0("Average_weekly_hours_worked_",Year))
  
  LSAY_data$Job_does_not_utilise_skills <- LSAY_data %>% pull(paste0("Job_does_not_utilise_skills_",Year))
  LSAY_data$Not_satisfied_with_work <- LSAY_data %>% pull(paste0("Not_satisfied_with_work_",Year))
  LSAY_data$Not_satisfied_with_career_path <- LSAY_data %>% pull(paste0("Not_satisfied_with_career_path_",Year))
  
  LSAY_data <- dummy(LSAY_data %>% pull(paste0("ANZSCO",Year))) %>% as.data.frame() %>%  rownames_to_column("row_number") %>% as_tibble() %>% 
    right_join(.,LSAY_data %>%  rownames_to_column("row_number"), by = "row_number" ) %>% select(-row_number)
  
  Variables <- all.vars(as.formula(Prediction_model)) 
  LSAY_data <- LSAY_data %>% select(ID,STIDSTD,Outcome_variable,Variables,Strata_variables_derived)
  
  ID_prediction_model <- LSAY_data %>% drop_na(Variables) %>% pull(STIDSTD)  # Student ID's of those in the predictive model
  Attrit_miss <- LSAY_data %>% drop_na(Variables,Outcome_variable) %>% pull(STIDSTD)
  
  data_model_only_logit <- enframe(LSAY_data$STIDSTD, name = NULL) %>% rename(STIDSTD = value) %>% mutate(ID = as.numeric(STIDSTD)) #Create an empty tibble only containing ID's of those in the Probit model
  data_model_only_logit$Attrit_miss <- ifelse(data_model_only_logit$ID %in% Attrit_miss,1,0) #Merge in the attrition of these IDs
  
  suppressWarnings(
    data_model_only_logit <- left_join(data_model_only_logit,LSAY_raw[c("ID",Strata_variables)],
                                            by="ID") # Merge in strata variables for logit
  )
  
  rm(LSAY_raw)
  
  suppressWarnings(
    data_model_only_logit <- left_join(data_model_only_logit,LSAY_data[c("ID",Strata_variables_derived)],
                                            by="ID") # Merge in strata variables for logit
  )
  

  data_model_only_logit[Strata_variables,"Attrit_miss"] <- lapply(data_model_only_logit[Strata_variables,"Attrit_miss"], factor)
  
  data_model_only_logit.w <- svydesign(ids = ~0, data = as.data.frame(data_model_only_logit[which(!is.na(data_model_only_logit$W_FSTUWT)),]), 
                                            weights = data_model_only_logit[which(!is.na(data_model_only_logit$W_FSTUWT)),"W_FSTUWT"])
  
  mylogit <- svyglm(paste("Attrit_miss", paste(c(Strata_variables,Strata_variables_derived), collapse=" + "), sep=" ~ "), 
                    design = data_model_only_logit.w, family=quasibinomial(link="logit"))
  
  rm(data_model_only_logit.w)
  
  data_model_only_logit <- na.omit(data_model_only_logit) %>% filter(STIDSTD %in% ID_prediction_model) # only include observations in the predictive model
  data_model_only_logit$Prediction <- predict(mylogit, newdata = data_model_only_logit, type = "response")
  data_model_only_logit$Prediction_inverse <- 1/data_model_only_logit$Prediction
  
  data_model_only_logit$Weight <- ifelse(data_model_only_logit$Attrit_miss == 1,
                                              data_model_only_logit$Prediction_inverse,NA) # Do not provide Weight for attriters
  
  data_model_only_logit$Weight <- data_model_only_logit$Weight * data_model_only_logit$W_FSTUWT # multiply by the original PISA sampling weight
  
  data_model_only_logit$Attrit_miss <- as.numeric(sapply(data_model_only_logit$Attrit_miss, as.character))
  data_model_only_logit$Weight <- data_model_only_logit$Weight / 
    sum(data_model_only_logit$Weight,na.rm=TRUE) * sum(data_model_only_logit$Attrit_miss) # Make the Weight sum to the number of observations in the outcome model (i.e. 'normalise' the weight)
  
  # clean the data.table and merge in outcomes #

  Boot_results_outcome = merge(Boot_results_dt,data_model_only_logit[c("STIDSTD","Weight")], by="STIDSTD",all.x=TRUE) # merge in the new weights 
  rm(Boot_results_dt)
  Boot_results_outcome = merge(Boot_results_outcome,LSAY_data[c("STIDSTD",Outcome_variable,Uni_dropout,Uni_complete)], by="STIDSTD",all.x=TRUE) # merge in the outcome variable
  gc()
  Boot_results_outcome = Boot_results_outcome[!is.na(get(Outcome_variable))] # drop observations where there is no outcome 
  Boot_results_outcome[, Never_student:=Never_student + Fewer_student] # Combine never takers and Fewer_students by putting Fewer_students in the never taker group

  Boot_results_outcome = Boot_results_outcome[, !c("Fewer_student"), with=FALSE] # drop Fewer_students
  setcolorder(Boot_results_outcome, c("STIDSTD", Outcome_groups_total,"tableid","Weight",Outcome_variable,Uni_dropout)) # if they are not in the right order it causes the AT,AS,NT outcomes not to add to 100%
  Boot_results_outcome = Boot_results_outcome[, (Outcome_groups) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Outcome_groups_total]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Uni_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Uni_dropout]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout) := 
                                                lapply(.SD, function(x) x * (1-get(Uni_dropout))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Not_dropout]  #
    
  # summarise the outcomes for the bootstrap #
  gc()  
  new_table = Boot_results_outcome[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(Outcome_groups_total,Outcome_groups,Uni_dropout,Uni_dropout_outcome,Not_dropout,Not_dropout_outcome), by = tableid]
  new_table[, AT_prop := AT_outcome / Other_student * 100]
  new_table[, AS_prop := AS_outcome / Additional_student * 100]
  new_table[, NT_prop := NT_outcome / Never_student * 100]
  new_table[, Dropout_prop := get(Uni_dropout_outcome) / get(Uni_dropout) * 100]
  new_table[, AT_not_drop_prop := Other_student_not_dropout_outcome / Other_student_not_dropout * 100]
  new_table[, AS_not_drop_prop := Additional_student_not_dropout_outcome / Additional_student_not_dropout * 100]
  gc()

  standard_errors = new_table[, lapply(.SD, sd, na.rm = TRUE), 
                                  .SDcols = c(Outcome_groups,paste0(gsub("_.*","",Outcome_groups),"_prop"),
                                              Not_dropout_outcome,"Dropout_prop","AT_not_drop_prop","AS_not_drop_prop")] #####
  
  means = new_table[, lapply(.SD, mean, na.rm = TRUE), 
                              .SDcols = c(Outcome_groups,paste0(gsub("_.*","",Outcome_groups),"_prop"),
                                          Not_dropout_outcome,"Dropout_prop","AT_not_drop_prop","AS_not_drop_prop")] #####

  # Return the weights for the outcome variable #
  
  Weights_dt = as.data.table(merge(data_model_only_logit,LSAY_data[c("STIDSTD",Outcome_variable)], by="STIDSTD",all.x=TRUE))
  Weights_dt = Weights_dt[!is.na(get(Outcome_variable))] # drop observations where there is no outcome 
  Weights_dt[, (setdiff(names(Weights_dt), c("STIDSTD","Attrit_miss","Weight",Outcome_variable))) := NULL] # keep only needed columns 

  rm(data_model_only_logit,LSAY_data)
  return(list(new_table,standard_errors,Weights_dt,means))
}

###### Just obtain outcome weights #####

outcome_weights_function <- function(Outcome_variable,Boot_results_dt,Cohort, Age){
  
  Prediction_model <- get(paste0("Prediction_model_",Cohort))
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(STIDSTD)) 
  LSAY_raw <- get(paste0("LSAY_",Cohort)) %>% mutate(ID = as.numeric(STIDSTD))  
  Strata_variables <- get(paste0("Strata_variables_",Cohort))
  Strata_variables_derived <- get(paste0("Strata_variables_derived_",Cohort))
  age = Age
  cohort = Cohort
  Year = Age_table %>% filter(Age == age, Cohort == cohort) %>% pull(Year)
  print(Year)

  
  LSAY_data$Dropout <- ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "Yes", 1,
                              ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Completed <- ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "Yes", 1,
                                     ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Undertaking <- ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 1, 1,
                                       ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 0,0, NA))
  
  
  LSAY_data$Postgrad <- ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %in% "Complete and undertaking further studies", 1,
                                    ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %ni% NA,0, NA))
  
  LSAY_data$Employed_FT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Full_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Employed_PT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Part_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Unemployed <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Unemployed", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$NILF <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "NILF", 1,
                                 ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Average_weekly_pay <- LSAY_data %>% pull(paste0("Average_weekly_pay_",Year))
  LSAY_data$Average_hourly_pay <- LSAY_data %>% pull(paste0("Average_hourly_pay_",Year))
  LSAY_data$Average_hours_worked <- LSAY_data %>% pull(paste0("Average_weekly_hours_worked_",Year))
  
  LSAY_data$Job_does_not_utilise_skills <- LSAY_data %>% pull(paste0("Job_does_not_utilise_skills_",Year))
  LSAY_data$Not_satisfied_with_work <- LSAY_data %>% pull(paste0("Not_satisfied_with_work_",Year))
  LSAY_data$Not_satisfied_with_career_path <- LSAY_data %>% pull(paste0("Not_satisfied_with_career_path_",Year))
  
  LSAY_data <- dummy(LSAY_data %>% pull(paste0("ANZSCO",Year))) %>% as.data.frame() %>%  rownames_to_column("row_number") %>% as_tibble() %>% 
    right_join(.,LSAY_data %>%  rownames_to_column("row_number"), by = "row_number" ) %>% select(-row_number)
  
  LSAY_data$Manager_or_professional <- ifelse(LSAY_data %>% pull(paste0("Manager")) == 1 |
                                                LSAY_data %>% pull(paste0("Professionals")) == 1, 1, 0)
                                                
  Variables <- all.vars(as.formula(Prediction_model)) 
  
  ID_prediction_model <- LSAY_data %>% drop_na(Variables) %>% pull(ID)  # Student ID's of those in the predictive model
  
  Attrit_miss <- LSAY_data %>% drop_na(Variables,Outcome_variable) %>% pull(ID) # Student ID's where they are in the prediction model AND have an outcome
  
  data_model_only_logit <- enframe(LSAY_data$STIDSTD, name = NULL) %>% rename(STIDSTD = value) %>% mutate(ID = as.numeric(STIDSTD))  #Create an empty tibble only containing ID's of those in the Probit model
  data_model_only_logit$Attrit_miss <- ifelse(data_model_only_logit$STIDSTD %in% Attrit_miss,1,0) #Merge in the attrition of these IDs
  
  suppressWarnings(
    data_model_only_logit <- left_join(data_model_only_logit,LSAY_raw[c("ID",Strata_variables)],
                                       by="ID") # Merge in strata variables for logit
  )
  
  suppressWarnings(
    data_model_only_logit <- left_join(data_model_only_logit,LSAY_data[c("ID",Strata_variables_derived)],
                                       by="ID") # Merge in strata variables for logit
  )
  
  data_model_only_logit[Strata_variables,"Attrit_miss"] <- lapply(data_model_only_logit[Strata_variables,"Attrit_miss"], factor)
  
  data_model_only_logit.w <- svydesign(ids = ~0, data = as.data.frame(data_model_only_logit[which(!is.na(data_model_only_logit$W_FSTUWT)),]), 
                                       weights = data_model_only_logit[which(!is.na(data_model_only_logit$W_FSTUWT)),"W_FSTUWT"])
  
  mylogit <- svyglm(paste("Attrit_miss", paste(c(Strata_variables,Strata_variables_derived), collapse=" + "), sep=" ~ "), 
                    design = data_model_only_logit.w, family=quasibinomial(link="logit"))
  
  data_model_only_logit <- na.omit(data_model_only_logit) %>% filter(ID %in% ID_prediction_model) 
  data_model_only_logit$Prediction <- predict(mylogit, newdata = data_model_only_logit, type = "response")
  data_model_only_logit$Prediction_inverse <- 1/data_model_only_logit$Prediction
  
  data_model_only_logit$Weight <- ifelse(data_model_only_logit$Attrit_miss == 1,
                                         data_model_only_logit$Prediction_inverse,NA) # Do not provide Weight for attriters
  
  data_model_only_logit$Weight <- data_model_only_logit$Weight * data_model_only_logit$W_FSTUWT # multiply by the orignial PISA sampling weight
  
  data_model_only_logit$Attrit_miss <- as.numeric(sapply(data_model_only_logit$Attrit_miss, as.character))
  data_model_only_logit$Weight <- data_model_only_logit$Weight / 
    sum(data_model_only_logit$Weight,na.rm=TRUE) * sum(data_model_only_logit$Attrit_miss) # Make the Weight sum to the number of observations in the outcome model (i.e. 'normalise' the weight)
  
  Weights_dt = as.data.table(merge(data_model_only_logit,LSAY_data[c("ID",Outcome_variable)], by="ID",all.x=TRUE))
  Weights_dt = Weights_dt[!is.na(get(Outcome_variable))] # drop observations where there is no outcome 
  Weights_dt[, (setdiff(names(Weights_dt), c("STIDSTD","Attrit_miss","Weight",Outcome_variable))) := NULL] # keep only needed columns 
  
  #
  return(Weights_dt)
  
}


Confidence_interval_outcomes <- function(Outcome_variable,Weights,Standard_errors,Mean_bootstrap,Student_IDs_to_drop,
                                         Student_IDs_to_keep,Cohort,Age){ 
  
  Uni_dropout <- paste0("Uni_dropout_age_",Age) 
  Uni_dropout_outcome <- paste0("Uni_dropout_age_",Age,"_outcome")    
  Uni_complete <- paste0("Uni_complete_age_",Age)
  Complete <- c("Other_student_complete","Additional_student_complete")
  Not_dropout <- c("Other_student_not_dropout","Additional_student_not_dropout")
  Not_dropout_outcome <-  c("Other_student_not_dropout_outcome","Additional_student_not_dropout_outcome")
  Complete_outcome <-  c("Other_student_complete_outcome","Additional_student_complete_outcome")
  Outcome_groups_total <- c("Other_student","Additional_student","Never_student")
  Outcome_groups <- c("AT_outcome","AS_outcome","NT_outcome") # They need to be in the same order 
  
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(STIDSTD)) 
  age = Age
  cohort = Cohort
  Year = Age_table %>% filter(Age == age, Cohort == cohort) %>% pull(Year)
  
  LSAY_data$Dropout <- ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "Yes", 1,
                              ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Completed <- ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "Yes", 1,
                                ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Undertaking <- ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 1, 1,
                                  ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 0,0, NA))
  
  
  LSAY_data$Postgrad <- ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %in% "Complete and undertaking further studies", 1,
                               ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %ni% NA,0, NA))
  
  LSAY_data$Employed_FT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Full_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Employed_PT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Part_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Unemployed <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Unemployed", 1,
                                 ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$NILF <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "NILF", 1,
                           ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Average_weekly_pay <- LSAY_data %>% pull(paste0("Average_weekly_pay_",Year))
  LSAY_data$Average_hourly_pay <- LSAY_data %>% pull(paste0("Average_hourly_pay_",Year))
  LSAY_data$Average_hours_worked <- LSAY_data %>% pull(paste0("Average_weekly_hours_worked_",Year))
  
  LSAY_data$Job_does_not_utilise_skills <- LSAY_data %>% pull(paste0("Job_does_not_utilise_skills_",Year))
  LSAY_data$Not_satisfied_with_work <- LSAY_data %>% pull(paste0("Not_satisfied_with_work_",Year))
  LSAY_data$Not_satisfied_with_career_path <- LSAY_data %>% pull(paste0("Not_satisfied_with_career_path_",Year))
  
  LSAY_data <- dummy(LSAY_data %>% pull(paste0("ANZSCO",Year))) %>% as.data.frame() %>%  rownames_to_column("row_number") %>% as_tibble() %>% 
    right_join(.,LSAY_data %>%  rownames_to_column("row_number"), by = "row_number" ) %>% select(-row_number)
  
  LSAY_data$Manager_or_professional <- ifelse(LSAY_data %>% pull(paste0("Manager")) == 1 |
                                                LSAY_data %>% pull(paste0("Professionals")) == 1, 1, 0)
  
  if(!missing(Student_IDs_to_keep)) { 
    Data_source = as.data.table(LSAY_data)[Student_IDs_to_keep, on=.(STIDSTD)]
  } else {
  if(!missing(Student_IDs_to_drop)) { 
    Data_source = as.data.table(LSAY_data)[!Student_IDs_to_drop, on=.(STIDSTD)]
  } else {
      Data_source = as.data.table(LSAY_data)
  }
  }
  
  Data_source[, (setdiff(names(Data_source), c("STIDSTD",Outcome_groups_total,Outcome_variable,Uni_dropout,Uni_complete))) := NULL] # keep only needed columns 
  setcolorder(Data_source, c("STIDSTD", Outcome_groups_total,Outcome_variable,Uni_dropout,Uni_complete)) # if they are not in the right order it causes the AT,AS,NT outcomes not to add to 100%
  #####
  Data_source = Data_source[, (Outcome_groups) := lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Outcome_groups_total]  # Calculate outcomes by uni attendance
  Data_source = Data_source[, (Uni_dropout_outcome) := lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Uni_dropout]  # Calculate outcomes by uni attendance
  Data_source = Data_source[, (Not_dropout) := lapply(.SD, function(x) x * (1-get(Uni_dropout))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Data_source = Data_source[, (Not_dropout_outcome) := lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Not_dropout]  #
  Data_source = Data_source[, (Complete) := lapply(.SD, function(x) x * (get(Uni_complete))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Data_source = Data_source[, (Complete_outcome) := lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Complete]
  #####

  
  Data_source <- as_tibble(Data_source)
  weights <- as_tibble(Weights)
  suppressWarnings(
  Data_source <- left_join(Data_source,weights[c("STIDSTD","Weight")],
                                          by="STIDSTD") # merge in the new weights 
  )

  Data_source = Data_source[!is.na(Data_source[,Outcome_variable]),] # drop observations where there is no outcome 

  Data_source.w <- svydesign(ids = ~0, data = as.data.frame(Data_source[which(!is.na(Data_source$Weight)),]), 
                                weights = Data_source[which(!is.na(Data_source$Weight)),"Weight"])
  
  Outcome_totals <- enframe(c(Outcome_groups_total,Uni_dropout,Not_dropout, Complete), name = NULL)
  colnames(Outcome_totals)[1] <- "University_attendance"
  Outcome_totals$Total <- sapply(c(Outcome_groups_total,Uni_dropout,Not_dropout, Complete), function(x) {svytotal(~get(x),design = Data_source.w,na.rm=TRUE,keep.var=FALSE)[1]})
  Outcome_totals[,Outcome_variable] <- sapply(c(Outcome_groups,Uni_dropout_outcome,Not_dropout_outcome, Complete_outcome), 
                                              function(x) {svytotal(~get(x),design = Data_source.w,na.rm=TRUE,keep.var=FALSE)[1]})
  Outcome_totals[,paste0(Outcome_variable,".prop")] <- Outcome_totals[,Outcome_variable] / Outcome_totals$Total * 100
  
  colnames(Mean_bootstrap)[which(names(Mean_bootstrap) == "Dropout_prop")] = paste0("Uni_dropout_age_",Age)

  suppressWarnings(
    Outcome_totals <- left_join(Outcome_totals,as.data.frame(t(Mean_bootstrap %>% 
                                                                 select(AT_prop,AS_prop,NT_prop,paste0("Uni_dropout_age_",Age),AT_not_drop_prop,AS_not_drop_prop,
                                                                        AT_complete_prop,AS_complete_prop) %>% 
                                                                 rename(Other_student = AT_prop, Additional_student = AS_prop, 
                                                                        Never_student = NT_prop,
                                                                        #Uni_dropout = Dropout_prop,
                                                                        Other_student_not_dropout  = AT_not_drop_prop,
                                                                        Additional_student_not_dropout = AS_not_drop_prop,
                                                                        Other_student_complete = AT_complete_prop,
                                                                        Additional_student_complete = AS_complete_prop))) %>% 
                              
                                  rownames_to_column(var = "University_attendance") %>% rename(Mean_bootstrap = V1), by="University_attendance")
  )
  
  colnames(Standard_errors)[which(names(Standard_errors) == "Dropout_prop")] = paste0("Uni_dropout_age_",Age)
  
  suppressWarnings(
  Outcome_totals <- left_join(Outcome_totals,as.data.frame(t(Standard_errors %>% 
                                                               select(AT_prop,AS_prop,NT_prop,paste0("Uni_dropout_age_",Age),AT_not_drop_prop,AS_not_drop_prop,
                                                                      AT_complete_prop,AS_complete_prop) %>% 
    rename(Other_student = AT_prop, Additional_student = AS_prop, Never_student = NT_prop,
           Other_student_not_dropout  = AT_not_drop_prop,
           Additional_student_not_dropout = AS_not_drop_prop,
           Other_student_complete = AT_complete_prop,
           Additional_student_complete = AS_complete_prop))) %>% 
    rownames_to_column(var = "University_attendance") %>% rename(Standard_errors = V1), by="University_attendance")
  )
  

  Outcome_totals <- Outcome_totals %>% mutate(CI_top = get(paste0(Outcome_variable,".prop")) + 1.96 * Standard_errors,
                            CI_bottom = get(paste0(Outcome_variable,".prop")) - 1.96 * Standard_errors )
  
  return(Outcome_totals)
}



# Just return SE
outcome_SE_function <- function(Outcome_variable,Boot_results_dt,Weights_dt,Cohort,Age){ 
  
  Uni_dropout <- paste0("Uni_dropout_age_",Age) #c("Uni_dropout_age_23","Uni_dropout_age_25")
  Uni_dropout_outcome <- paste0("Uni_dropout_age_",Age,"_outcome")    #c("Uni_dropout_age_23_outcome","Uni_dropout_age_25_outcome")
  Uni_complete <- paste0("Uni_complete_age_",Age)
  Complete <- c("Other_student_complete","Additional_student_complete")
  Not_dropout <- c("Other_student_not_dropout","Additional_student_not_dropout")
  Not_dropout_outcome <-  c("Other_student_not_dropout_outcome","Additional_student_not_dropout_outcome")
  Complete_outcome <-  c("Other_student_complete_outcome","Additional_student_complete_outcome")
  Outcome_groups_total <- c("Other_student","Additional_student","Never_student")
  Outcome_groups <- c("AT_outcome","AS_outcome","NT_outcome") # They need to be in the same order 
  age = Age
  cohort = Cohort
  Year = Age_table %>% filter(Age == age, Cohort == cohort) %>% pull(Year)
  
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(STIDSTD)) 
  
  LSAY_data$Dropout <- ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "Yes", 1,
                              ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Completed <- ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "Yes", 1,
                                ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "No",0, NA))
  
  
  LSAY_data$Undertaking <- ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 1, 1,
                                  ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 0,0, NA))
  
  
  LSAY_data$Postgrad <- ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %in% "Complete and undertaking further studies", 1,
                               ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %ni% NA,0, NA))
  
  LSAY_data$Employed_FT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Full_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Employed_PT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Part_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Unemployed <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Unemployed", 1,
                                 ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$NILF <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "NILF", 1,
                           ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Average_weekly_pay <- LSAY_data %>% pull(paste0("Average_weekly_pay_",Year))
  LSAY_data$Average_hourly_pay <- LSAY_data %>% pull(paste0("Average_hourly_pay_",Year))
  LSAY_data$Average_hours_worked <- LSAY_data %>% pull(paste0("Average_weekly_hours_worked_",Year))
  
  LSAY_data$Job_does_not_utilise_skills <- LSAY_data %>% pull(paste0("Job_does_not_utilise_skills_",Year))
  LSAY_data$Not_satisfied_with_work <- LSAY_data %>% pull(paste0("Not_satisfied_with_work_",Year))
  LSAY_data$Not_satisfied_with_career_path <- LSAY_data %>% pull(paste0("Not_satisfied_with_career_path_",Year))
  
  LSAY_data <- dummy(LSAY_data %>% pull(paste0("ANZSCO",Year))) %>% as.data.frame() %>%  rownames_to_column("row_number") %>% as_tibble() %>% 
    right_join(.,LSAY_data %>%  rownames_to_column("row_number"), by = "row_number" ) %>% select(-row_number)
  
  LSAY_data$Manager_or_professional <- ifelse(LSAY_data %>% pull(paste0("Manager")) == 1 |
                                                LSAY_data %>% pull(paste0("Professionals")) == 1, 1, 0)
  
  list_condition <- sapply(Weights_dt, function(x) Outcome_variable %in% names(x))
  Weights <- Weights_dt[list_condition]

  data_model_only_logit <- as.data.table(Weights[[1]])
  
  Boot_results_outcome = merge(Boot_results_dt,data_model_only_logit[,c("STIDSTD","Weight")], by="STIDSTD",all.x=TRUE) # merge in the new weights 
  Boot_results_outcome = merge(Boot_results_outcome,LSAY_data[,c("STIDSTD",Outcome_variable,Uni_dropout,Uni_complete)], by="STIDSTD",all.x=TRUE) # merge in the outcome variable
  gc()
  Boot_results_outcome = Boot_results_outcome[!is.na(get(Outcome_variable))] # drop observations where there is no outcome 
  Boot_results_outcome[, Never_student:=Never_student + Fewer_student] # Combine never takers and Fewer_students by putting Fewer_students in the never taker group
  Boot_results_outcome[,"Never_student" := round(.SD,0), .SDcols="Never_student"]
  Boot_results_outcome = Boot_results_outcome[, !c("Fewer_student"), with=FALSE] # drop Fewer_students
  setcolorder(Boot_results_outcome, c("STIDSTD", Outcome_groups_total,"tableid","Weight",Outcome_variable,Uni_dropout)) # if they are not in the right order it causes the AT,AS,NT outcomes not to add to 100%
  Boot_results_outcome = Boot_results_outcome[, (Outcome_groups) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Outcome_groups_total]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Uni_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Uni_dropout]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout) := 
                                                lapply(.SD, function(x) x * (1-get(Uni_dropout))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Not_dropout]  #\
  Boot_results_outcome = Boot_results_outcome[, (Complete) := 
                                                lapply(.SD, function(x) x * (get(Uni_complete))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Complete_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Complete]

  # summarise the outcomes for the bootstrap #
  gc()  
  new_table = Boot_results_outcome[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(Outcome_groups_total,Outcome_groups,Uni_dropout,Uni_dropout_outcome,
                                                                                  Not_dropout,Not_dropout_outcome,Complete,Complete_outcome), by = tableid]
  new_table[, AT_prop := AT_outcome / Other_student * 100]
  new_table[, AS_prop := AS_outcome / Additional_student * 100]
  new_table[, NT_prop := NT_outcome / Never_student * 100]
  new_table[, Dropout_prop := get(Uni_dropout_outcome) / get(Uni_dropout) * 100]
  new_table[, AT_not_drop_prop := Other_student_not_dropout_outcome / Other_student_not_dropout * 100]
  new_table[, AS_not_drop_prop := Additional_student_not_dropout_outcome / Additional_student_not_dropout * 100]
  new_table[, AT_complete_prop := Other_student_complete_outcome / Other_student_complete * 100]
  new_table[, AS_complete_prop := Additional_student_complete_outcome / Additional_student_complete * 100]
  standard_errors = new_table[, lapply(.SD, sd, na.rm = TRUE), 
                              .SDcols = c(Outcome_groups,paste0(gsub("_.*","",Outcome_groups),"_prop"),
                                          Not_dropout_outcome,"Dropout_prop","AT_not_drop_prop","AS_not_drop_prop",
                                          Complete_outcome,"AT_complete_prop","AS_complete_prop")]
  return(standard_errors)
  
}


# Just return mean
outcome_mean_BS_function <- function(Outcome_variable,Boot_results_dt,Weights_dt,Cohort,Age){ 
  
  Uni_dropout <- paste0("Uni_dropout_age_",Age) #c("Uni_dropout_age_23","Uni_dropout_age_25")
  Uni_dropout_outcome <- paste0("Uni_dropout_age_",Age,"_outcome")    #c("Uni_dropout_age_23_outcome","Uni_dropout_age_25_outcome")
  Uni_complete <- paste0("Uni_complete_age_",Age)
  Complete <- c("Other_student_complete","Additional_student_complete")
  Not_dropout <- c("Other_student_not_dropout","Additional_student_not_dropout")
  Not_dropout_outcome <-  c("Other_student_not_dropout_outcome","Additional_student_not_dropout_outcome")
  Complete_outcome <-  c("Other_student_complete_outcome","Additional_student_complete_outcome")
  Outcome_groups_total <- c("Other_student","Additional_student","Never_student")
  Outcome_groups <- c("AT_outcome","AS_outcome","NT_outcome") # They need to be in the same order 
  age = Age
  cohort = Cohort

  Year = Age_table %>% filter(Age == age, Cohort == cohort) %>% pull(Year)
  
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(STIDSTD)) 
  
  LSAY_data$Dropout <- ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "Yes", 1,
                              ifelse(LSAY_data %>% pull(paste0("drop_out_",Year)) %in% "No",0, NA))
  
  LSAY_data$Completed <- ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "Yes", 1,
                                ifelse(LSAY_data %>% pull(paste0("Completed_",Year)) %in% "No",0, NA))
  
  LSAY_data$Undertaking <- ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 1, 1,
                                  ifelse(LSAY_data %>% pull(paste0("undertaking_",Year)) %in% 0,0, NA))
  
  LSAY_data$Postgrad <- ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %in% "Complete and undertaking further studies", 1,
                               ifelse(LSAY_data %>% pull(paste0("Uni_completion_age_",Age)) %ni% NA,0, NA))
  
  LSAY_data$Employed_FT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Full_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Employed_PT <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Part_time", 1,
                                  ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Unemployed <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "Unemployed", 1,
                                 ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$NILF <- ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %in% "NILF", 1,
                           ifelse(LSAY_data %>% pull(paste0("Employment_status_",Year)) %ni% NA,0, NA))
  
  LSAY_data$Average_weekly_pay <- LSAY_data %>% pull(paste0("Average_weekly_pay_",Year))
  LSAY_data$Average_hourly_pay <- LSAY_data %>% pull(paste0("Average_hourly_pay_",Year))
  LSAY_data$Average_hours_worked <- LSAY_data %>% pull(paste0("Average_weekly_hours_worked_",Year))
  
  LSAY_data$Job_does_not_utilise_skills <- LSAY_data %>% pull(paste0("Job_does_not_utilise_skills_",Year))
  LSAY_data$Not_satisfied_with_work <- LSAY_data %>% pull(paste0("Not_satisfied_with_work_",Year))
  LSAY_data$Not_satisfied_with_career_path <- LSAY_data %>% pull(paste0("Not_satisfied_with_career_path_",Year))
  
  LSAY_data <- dummy(LSAY_data %>% pull(paste0("ANZSCO",Year))) %>% as.data.frame() %>% rownames_to_column("row_number") %>% as_tibble() %>% 
    right_join(.,LSAY_data %>% rownames_to_column("row_number"), by = "row_number") %>% select(-row_number)
  
  LSAY_data$Manager_or_professional <- ifelse(LSAY_data %>% pull(paste0("Manager")) == 1 |
                                                LSAY_data %>% pull(paste0("Professionals")) == 1, 1, 0)
  
  list_condition <- sapply(Weights_dt, function(x) Outcome_variable %in% names(x))
  Weights <- Weights_dt[list_condition]
  
  data_model_only_logit <- as.data.table(Weights[[1]])
  
  Boot_results_outcome = merge(Boot_results_dt,data_model_only_logit[,c("STIDSTD","Weight")], by="STIDSTD",all.x=TRUE) # merge in the new weights 
  Boot_results_outcome = merge(Boot_results_outcome,LSAY_data[,c("STIDSTD",Outcome_variable,Uni_dropout,Uni_complete)], by="STIDSTD",all.x=TRUE) # merge in the outcome variable
  gc()
  #Boot_results_outcome = Boot_results_outcome[!is.na(get(Outcome_variable))] # drop observations where there is no outcome 
  Boot_results_outcome = na.omit(Boot_results_outcome, cols=c(Outcome_variable, "Weight")) # drop observations where there is no outcome or no weight
  Boot_results_outcome[, Never_student:=Never_student + Fewer_student] # Combine never takers and Fewer_students by putting Fewer_students in the never taker group
  Boot_results_outcome[,"Never_student" := round(.SD,0), .SDcols="Never_student"]
  Boot_results_outcome = Boot_results_outcome[, !c("Fewer_student"), with=FALSE] # drop Fewer_students
  setcolorder(Boot_results_outcome, c("STIDSTD", Outcome_groups_total,"tableid","Weight",Outcome_variable,Uni_dropout)) # if they are not in the right order it causes the AT,AS,NT outcomes not to add to 100%
  Boot_results_outcome = Boot_results_outcome[, (Outcome_groups) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Outcome_groups_total]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Uni_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Uni_dropout]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout) := 
                                                lapply(.SD, function(x) x * (1-get(Uni_dropout))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Not_dropout_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Not_dropout]  #
  Boot_results_outcome = Boot_results_outcome[, (Complete) := 
                                                lapply(.SD, function(x) x * (get(Uni_complete))), .SDcols = c("Other_student","Additional_student")]  # Calculate outcomes by uni attendance
  Boot_results_outcome = Boot_results_outcome[, (Complete_outcome) := 
                                                lapply(.SD, function(x) x * get(Outcome_variable)), .SDcols = Complete]  #

  # summarise the outcomes for the bootstrap #
  gc()  
  new_table = Boot_results_outcome[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(Outcome_groups_total,Outcome_groups,Uni_dropout,Uni_dropout_outcome,
                                                                                  Not_dropout,Not_dropout_outcome,Complete,Complete_outcome), by = tableid]
  new_table[, AT_prop := AT_outcome / Other_student * 100]
  new_table[, AS_prop := AS_outcome / Additional_student * 100]
  new_table[, NT_prop := NT_outcome / Never_student * 100]
  new_table[, Dropout_prop := get(Uni_dropout_outcome) / get(Uni_dropout) * 100]
  new_table[, AT_not_drop_prop := Other_student_not_dropout_outcome / Other_student_not_dropout * 100]
  new_table[, AS_not_drop_prop := Additional_student_not_dropout_outcome / Additional_student_not_dropout * 100]
  new_table[, AT_complete_prop := Other_student_complete_outcome / Other_student_complete * 100]
  new_table[, AS_complete_prop := Additional_student_complete_outcome / Additional_student_complete * 100]
  
  Bootstrap_means = new_table[, lapply(.SD, mean, na.rm = TRUE), 
                              .SDcols = c(Outcome_groups,paste0(gsub("_.*","",Outcome_groups),"_prop"),
                                          Not_dropout_outcome,"Dropout_prop","AT_not_drop_prop","AS_not_drop_prop",
                                          Complete_outcome,"AT_complete_prop","AS_complete_prop")] #####
  return(Bootstrap_means)
  
}
  


##########----------------------------------------------------------------------------------------------------##########
#         Bootstrap tables and charts function
##########----------------------------------------------------------------------------------------------------##########

memory.limit(size=50000)
gc()
bootstrap_tables_and_charts <- function(Outcome_variables = c("Completed", "Undertaking", "Dropout", "Postgrad"),
                                        Cohort = 2009, Age = 23, Graduates = FALSE) { # If Graduates is TRUE then additional & other students who have not graduated are excluded 
  ptm <- proc.time()
  start_time <- Sys.time()
  if (paste0("Boot_results_",Cohort) %ni% ls(envir = .GlobalEnv)) {
    if (file.exists(paste0("Boot_results_",Cohort,".csv"))) { 
      cat("Boostrap results were stored in a previous run. Loading results from CSV file ... \n")
      assign(paste0("Boot_results_",Cohort),fread(paste0("Boot_results_",Cohort,".csv")), envir = .GlobalEnv) } else {
        Run_bootstrap(cohort=Cohort,number_of_bootstrap_samples = 1000) } }
  
  gc()
  no_cores <- max(1,detectCores() - 2) # use all cores except 1
  cat("Setting up clusters for parallel computation. Process will use",paste(no_cores),"CPU cores ... \n")
  cl <- makeCluster(no_cores)
  clusterExport(cl, list("outcome_weights_function",paste0("LSAY_",Cohort,"_data"),paste0("LSAY_",Cohort),
                         paste0("Strata_variables_",Cohort),paste0("Strata_variables_derived_",Cohort),
                         paste0("Prediction_model_",Cohort),paste0("Boot_results_",Cohort),
                         "%ni%","outcome_SE_function", "Age_table"))
  clusterExport(cl, list("Outcome_variables", "Age","Cohort"),envir=environment())
  clusterEvalQ(cl, list(library(dplyr),library(tibble),library(tidyr),library(survey),library(mvtnorm),library(data.table),
                        library(lme4)))
  
  cat("Calculating new survey weights for the bootstrap sample. This may take several minutes ... \n")
  Boot_result_weights <- parLapply(cl,Outcome_variables,function(x) {
    outcome_weights_function(Outcome_variable = x, Cohort=Cohort,Age = Age,
                             Boot_results_dt=get(paste0("Boot_results_",Cohort)))})
  
  gc()
  cat("Calculating the standard errors for each bootstrap sample ... \n")
  
  #Boot_result_outcomes <- lapply(Outcome_variables,function(x) outcome_function(Outcome_variable=x,Boot_results_dt=Boot_results_filter))
  clusterExport(cl, list("Boot_result_weights"),envir=environment())
  Boot_result_SE <- parLapply(cl,Outcome_variables,function(x) outcome_SE_function(Outcome_variable = x, Cohort=Cohort,Age=Age,
                                                                                   Boot_results_dt=get(paste0("Boot_results_",Cohort)),
                                                                                   Weights_dt=Boot_result_weights))
  
  gc()
  cat("Calculating the average outcomes across the bootstrap samples ... \n")
  clusterExport(cl, list("Boot_result_SE"),envir=environment())
  Boot_result_mean <- lapply(Outcome_variables,function(x) outcome_mean_BS_function(Outcome_variable = x, Cohort=Cohort, Age=Age,
                                                                                    Boot_results_dt=get(paste0("Boot_results_",Cohort)),
                                                                                    Weights_dt=Boot_result_weights))
  gc()
  cat("Calculating the confidence intervals ... \n")
  clusterExport(cl, list("Boot_result_mean"),envir=environment())
  Table_all <- lapply(1:length(Outcome_variables),
                      function(x) Confidence_interval_outcomes(Outcome_variable=Outcome_variables[x],Weights=Boot_result_weights[[x]], Cohort=Cohort,Age=Age,
                                                               Standard_errors=Boot_result_SE[[x]],Mean_bootstrap=Boot_result_mean[[x]]))
  stopCluster(cl)
  gc()
  
  cat("Tabulating the results ... \n")
  Table <- Table_all %>% lapply(., function(x) {
    x[,c(2:3)] <- NULL
    x})  %>% # delete unnecessary columns
    lapply(., function(x) {
      x$Outcome <- names(x)[2] 
      x
    }) %>% 
    lapply(., function(x) {
      names(x)[2] <- "Mean"
      x }) %>%  bind_rows(.)
  
  if(Graduates == TRUE) {
    Table <- Table %>% mutate(University_attendance = recode(University_attendance, Other_student_complete = "Other students",
                                                             Never_student = "Non-attenders",
                                                             Additional_student_complete = "Additional students",
                                                             Uni_dropout_age_23 = "Students who dropped out",
                                                             Uni_dropout_age_25 = "Students who dropped out")) %>% 
      filter(University_attendance %in% c("Other students", "Additional students",
                                          "Non-attenders", "Students who dropped out")) %>% 
      mutate(University_attendance = factor(University_attendance, levels = c("Other students", "Additional students",
                                                                              "Non-attenders", "Students who dropped out"))) 
  } else {
    Table <- Table %>% mutate(University_attendance = recode(University_attendance, Other_student = "Other students",
                                                             Never_student = "Non-attenders",
                                                             Additional_student = "Additional students",
                                                             Uni_dropout_age_23 = "Students who dropped out",
                                                             Uni_dropout_age_25 = "Students who dropped out")) %>% 
      filter(University_attendance %in% c("Other students", "Additional students",
                                          "Non-attenders", "Students who dropped out")) %>% 
      mutate(University_attendance = factor(University_attendance, levels = c("Other students", "Additional students",
                                                                              "Non-attenders", "Students who dropped out"))) 
  }
  
  Table <- Table %>% 
    mutate(Outcome = str_remove(Outcome, ".prop"),
           Outcome = str_replace_all(Outcome, "_", " "),
           Outcome = recode_factor(Outcome, Completed = "Completed", Undertaking = "Undertaking", Dropout = "Dropped out",Postgrad = "Additional study",
                                   `Employed FT` = "Full-time work",`Employed PT` = "Part-time work",
                                   NILF = "Not in the labour force")) %>% 
    mutate_if(is.numeric, list(~ifelse(str_detect(Outcome, "Average"),./100,. ))) # The bootstrap function calculates percentages (for categorical variables like employment status). We need to divide the result by 100 if the outcome is a value (e.g. average weekly pay)
  
  end_time <- Sys.time()
  cat("Total CPU time taken: ")
  cat(seconds_to_period((proc.time() - ptm)["elapsed"]) ) 
  cat("\n")
  cat("Total system time taken: ")
  print(end_time - start_time)
  
  return(Table)
}



