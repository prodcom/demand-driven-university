start_time <- Sys.time()
##########----------------------------------------------------------------------------------------------------##########
#         Function for calculating weights
##########----------------------------------------------------------------------------------------------------##########

# The Consistent_weights_function calculates new survey weights that are caclulated consistently across cohorts and waves and also take into account missing variables 
# The missing variables are supplied to the Consistent_weights_function through the "Variables" argument.


Consistent_weights_function <- function(Strata_variables,Strata_variables_derived,Survey_weights,Variables,Cohort,Year,ID,Prefix){
  
  LSAY_data <- get(paste0("LSAY_",Cohort,"_data")) #%>% 
  LSAY_raw <- get(paste0("LSAY_",Cohort)) %>% 
    mutate(ID = as.numeric(get(ID))) # Create a numeric ID column to ensure consistency in data type for this function
  
  Attrit_miss <- LSAY_data %>% select(one_of(Variables),ID) %>% filter(complete.cases(.)) %>% pull(ID) # Student ID's of those in the predictive model
  
  LSAY_logit <- enframe(LSAY_data$ID, name = NULL, value = "ID")  %>% 
    mutate(Attrit_missing = ifelse(ID %in% Attrit_miss,1,0)) %>% #Create a tibble that identifies who is in attrit_miss
    left_join(.,LSAY_raw[c("ID",Survey_weights,Strata_variables)],by="ID") %>% # Merge in strata variables for logit
    left_join(.,LSAY_data[c("ID",Strata_variables_derived)],by="ID") #%>% # Merge in derived strata variables for logit
  
  LSAY_logit[Strata_variables] <- lapply(LSAY_logit[Strata_variables], factor)
  
  LSAY_logit.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_logit[which(!is.na(LSAY_logit[,Survey_weights])),]), 
                            weights = LSAY_logit[which(!is.na(LSAY_logit[,Survey_weights])),Survey_weights])
  
  mylogit <- svyglm(paste("Attrit_missing", paste(c(Strata_variables,Strata_variables_derived), collapse=" + "), sep=" ~ "), 
                    design = LSAY_logit.w, family=quasibinomial(link="logit"))
  
  LSAY_logit <- na.omit(LSAY_logit) %>% filter(ID %in% Attrit_miss) %>% # only include observations in the predictive model
    mutate(Prediction = predict(mylogit, newdata = ., type = "response")) %>% 
    mutate(Prediction_inverse = 1/Prediction) %>% 
    mutate(Weight = Prediction_inverse * get(Survey_weights)) %>% 
    mutate(Weight = as.numeric(Weight / sum(Weight) * sum(as.numeric(sapply(Attrit_missing, as.character))))) %>% 
    left_join(.,LSAY_data[c("ID",ID)],by="ID") %>% # Return the original student ID field into the result
    rename_at(vars('Weight'), list(~paste0("WT",Year,Prefix)))
              
  return(LSAY_logit)
}

# The Create_weights_function runs the Consistent_weights_function, supplying it with the necessary inputs.
# An 'if statement' in the Create_weights_function starts checks if the results have already been saved and if so loads them in and doesn't do anything else
# The weights are appended to a list (each cohort gets its own list of weights), which are then merged into the LSAY data at the end of this R file and saved to an RData file

Create_weights_function <- function(Name, Cohort = 2003) {
  ID <- if (Cohort %in% c(1995,1998)) "stuidno" else if (Cohort %in% c(2003,2006,2009)) "STIDSTD"
  env <- new.env() 
  if (file.exists(paste0("Weights_",Cohort,".RData"))) { # If the file has been saved before and is is not already loaded, then load it
    if(!exists(paste0("Weights_",Cohort),envir = .GlobalEnv)) {
      cat(paste("Weights for the",Cohort,"cohort saved from a previous run, therefore loading them in. \n"))
      cat(paste0("If you wish to re-run the weights, delete the file Weights_",Cohort,".RData and re-run this. \n"))
    load(paste0("Weights_",Cohort,".RData"), envir=env) 
    loadedObjects <- objects(env, all=TRUE) 
    stopifnot(length(loadedObjects)==1) 
    assign(paste0("Weights_",Cohort),env[[loadedObjects]],.GlobalEnv)}} else { # If the file does not exist, create the weights
      assign(paste0(Name), Consistent_weights_function(Strata_variables=Strata_variables,Strata_variables_derived=Strata_variables_derived,
                                                     Survey_weights=Survey_weights,Variables=Variables,
                                                     Cohort=Cohort,Year=Year,ID=ID,Prefix=Prefix) %>% 
             select(-one_of(c(Strata_variables,Strata_variables_derived,Survey_weights,ID)),
                    -c(Prediction,Prediction_inverse,Attrit_missing)),envir = env) 
    
      if(!exists(paste0("Weights_",Cohort),envir = .GlobalEnv)) { # If the list of weights for this cohort has not already been been created, then create it 
        cat(paste("Calculating weights for the",Cohort,"cohort ... \n"))
        assign(paste0("Weights_",Cohort),list(get(paste0(Name), envir = env)), envir = .GlobalEnv) } else{ # If the list of weights has already been been created for this cohort, then just append these new weights to the existing list of weights
      assign(paste0("Weights_",Cohort),append(get(paste0("Weights_",Cohort), envir = env),
                                              list(get(paste0(Name), envir = env))), envir = .GlobalEnv) }
    }
  rm(env)
}


##########----------------------------------------------------------------------------------------------------##########
#         Some variables for missing data checks
##########----------------------------------------------------------------------------------------------------##########


Variables_age_19_probit  <- c("Uni_attend_age_19","Gender","Indigenous","PISA","NESB","State","ATAR_group_imputed","ATAR_missing_dummy","Grade",
                              "Parent_SES_quartile","Parent_edu","School_SES_quartile","Sector","Log_closest_uni",
                              "Educ_occupation_quartile","Attend_year_12","Hours_worked_in_year_12","Hours_worked_age_of_yr_12_interaction_attend")

Variables_age_22_probit <- c("Uni_attend_age_22","Gender","Indigenous","PISA","NESB","State","ATAR_group_imputed","ATAR_missing_dummy","Grade",
                             "Parent_SES_quartile","Parent_edu","School_SES_quartile","Sector","Log_closest_uni",
                             "Educ_occupation_quartile","Attend_year_12","Hours_worked_in_year_12","Hours_worked_age_of_yr_12_interaction_attend")


Variables_age_19_probit_i  <- c("Uni_attend_age_19","Gender","Indigenous","PISA","NESB","State",
                                "Parent_SES_quartile","Parent_edu","School_SES_quartile","Sector","Location_RR_wave1",
                                "Educ_occupation_quartile","Attend_year_12",
                                "Hours_worked_in_year_12","Hours_worked_age_of_yr_12_interaction_attend","Books")

Variables_age_22_probit_i <- c("Uni_attend_age_22","Gender","Indigenous","PISA","NESB","State",
                               "Parent_SES_quartile","Parent_edu","School_SES_quartile","Sector","Location_RR_wave1",
                               "Educ_occupation_quartile","Attend_year_12",
                               "Hours_worked_in_year_12","Hours_worked_age_of_yr_12_interaction_attend","Books")


##########----------------------------------------------------------------------------------------------------##########
#         Consistent weights calcs
##########----------------------------------------------------------------------------------------------------##########
# Note some of these may be redundant


##### Calculate consistent weights 1995 #####
#############################################

Strata_variables <- c("schno","indig","sex","M_ACHQ","R_ACHQ")
Strata_variables_derived <- c("HISCED","IMMIG","Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "WT95GEN"

name <- "Weights_1995_age_19"
Variables <- "Uni_attend_age_19"
cohort <- 1995
Age <- 19
Year <- 2000
Prefix <-"C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_22"
Variables <- "Uni_attend_age_22"
cohort <- 1995
Age = 22
Year=2003
Prefix="C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_25A"
Variables <- "Uni_attend_all_years"
cohort <- 1995
Age = 25
Year=2006
Prefix="CA"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_25MP"
Variables <- c("Uni_attend_all_years","Manager_prof2006")
cohort <- 1995
Age = 25
Year=2006
Prefix="MP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_23_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_23")
cohort <- 1995
Age = 23
Year=2004
Prefix="CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_23_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2004","Uni_complete_age_23")
cohort <- 1995
Age = 23
Year=2004
Prefix="MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_25_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2006","Uni_complete_age_25")
cohort <- 1995
Age = 25
Year=2006
Prefix="MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1995_age_25_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_25")
cohort <- 1995
Age = 25
Year=2006
Prefix="CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data



##### Calculate consistent weights 1998 #####
#############################################

Strata_variables <- c("SCHNO_R","indig","sex","AA002")
Strata_variables_derived <- c("HISCED","IMMIG","Achievement_Math_quartile","Achievement_Read_quartile",
                              "Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "GENWT98"

name <- "Weights_1998_age_19"
Variables <- "Uni_attend_age_19"
cohort <- 1998
Age <- 19
Year <- 2003
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_22"
Variables <- "Uni_attend_age_22"
cohort <- 1998
Age <- 22
Year <- 2006
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_25A"
Variables <- "Uni_attend_all_years"
cohort <- 1998
Age <- 25
Year <- 2009
Prefix <- "CA"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_25MP"
Variables <- c("Uni_attend_all_years","Manager_prof2009")
cohort <- 1998
Age <- 25
Year <- 2009
Prefix <- "MP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_23_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_23")
cohort <- 1998
Age <- 23
Year <- 2007
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_23_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2007","Uni_complete_age_23")
cohort <- 1998
Age <- 23
Year <- 2007
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_25_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2009","Uni_complete_age_25")
cohort <- 1998
Age <- 25
Year <- 2009
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_1998_age_25_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_25")
cohort <- 1998
Age <- 25
Year <- 2009
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data



##### Calculate consistent weights 2003 #####
#############################################

Strata_variables <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST03Q01","IMMIG")
Strata_variables_derived <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile",
                              "Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "W_FSTUWT"

name <- "Weights_2003_age_19"
Variables <- "Uni_attend_age_19"
cohort <- 2003
Age <- 19
Year <- 2007
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_22"
Variables <- "Uni_attend_age_22"
cohort <- 2003
Age <- 22
Year <- 2010
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_23"
Variables <- "Uni_attend_age_23"
cohort <- 2003
Age <- 23
Year <- 2011
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_25"
Variables <- "Uni_attend_all_years"
cohort <- 2003
Age <- 25
Year <- 2013
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_25A"
Variables <- "Uni_attend_all_years"
cohort <- 2003
Age <- 25
Year <- 2013
Prefix <- "CA"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_25MP"
Variables <- c("Uni_attend_all_years","Manager_prof2013")
cohort <- 2003
Age <- 25
Year <- 2013
Prefix <- "MP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_19_probit" ##########################################################
Variables <- Variables_age_19_probit
cohort <- 2003
Age <- 19
Year <- 2007
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_22_probit" #######################
Variables <- Variables_age_22_probit
cohort <- 2003
Age <- 22
Year <- 2010
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_23_probit"
Variables <- c(Variables_age_22_probit,"Uni_dropout_age_23")
cohort <- 2003
Age <- 23
Year <- 2011
Prefix <- "CPU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_23_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_23")
cohort <- 2003
Age <- 23
Year <- 2011
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_23_all"
Variables <- c(Variables_age_22_probit,"Manager_prof2011","Uni_attend_age_22","Uni_completion")
cohort <- 2003
Age <- 23
Year <- 2011
Prefix <- "ALL"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_23_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2011","Uni_complete_age_23")
cohort <- 2003
Age <- 23
Year <- 2011
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_25_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2013","Uni_complete_age_25")
cohort <- 2003
Age <- 25
Year <- 2013
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_25_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_25")
cohort <- 2003
Age <- 25
Year <- 2013
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_19_probit_i"
Variables <- Variables_age_19_probit_i
cohort <- 2003
Age <- 19
Year <- 2007
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_22_probit_i"
Variables <- Variables_age_22_probit_i
cohort <- 2003
Age <- 22
Year <- 2010
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_22_distance"
Variables <- c("Distance_moved_to_uni")
cohort <- 2003
Age <- 22
Year <- 2010
Prefix <- "_Distance"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2003_age_22_movement"
Variables <- c("Movement_data")
cohort <- 2003
Age <- 22
Year <- 2010
Prefix <- "_Movement"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

##### Calculate consistent weights 2006 #####
#############################################

Strata_variables <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST04Q01","IMMIG")
Strata_variables_derived <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile",
                              "Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "W_FSTUWT"

name <- "Weights_2006_age_19"
Variables <- "Uni_attend_age_19"
cohort <- 2006
Age <- 19
Year <- 2010
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data


name <- "Weights_2006_age_22"
Variables <- "Uni_attend_age_22"
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_23"
Variables <- "Uni_attend_age_22"
cohort <- 2006
Age <- 23
Year <- 2014
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25"
Variables <- "Uni_attend_all_years"
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25A"
Variables <- "Uni_attend_all_years"
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "CA"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25MP"
Variables <- c("Uni_attend_all_years","Manager_prof2016")
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "MP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_19_probit"
Variables <-Variables_age_19_probit
cohort <- 2006
Age <- 19
Year <- 2010
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_22_probit"
Variables <- Variables_age_22_probit
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_23_probit"
Variables <- c(Variables_age_22_probit,"Uni_dropout_age_23")
cohort <- 2006
Age <- 23
Year <- 2014
Prefix <- "CPU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_23_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_23")
cohort <- 2006
Age <- 23
Year <- 2014
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data


name <- "Weights_2006_age_23_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2014","Uni_complete_age_23")
cohort <- 2006
Age <- 23
Year <- 2014
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_23_ALL"
Variables <- c(Variables_age_22_probit,"Manager_prof2014","Uni_attend_age_22","Uni_completion")
cohort <- 2006
Age <- 23
Year <- 2014
Prefix <- "ALL"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2016","Uni_complete_age_25")
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_25")
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_25_probit_all"
Variables <- c(Variables_age_22_probit,"Manager_prof2016","Uni_attend_age_22","Uni_completion")
cohort <- 2006
Age <- 25
Year <- 2016
Prefix <- "ALL"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_part_time"
Variables <- c(Variables_age_22_probit,"Uni_attend_age_22",
               "Study_status_commencement","Hours_worked_commencement",
               "Worked_first_year_university","No_ATAR")
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "PT"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_19_probit_i"
Variables <- Variables_age_19_probit_i
cohort <- 2006
Age <- 19
Year <- 2010
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_22_probit_i"
Variables <- Variables_age_22_probit_i
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_22_distance"
Variables <- c("Distance_moved_to_uni")
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "_Distance"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2006_age_22_movement"
Variables <- c("Movement_data")
cohort <- 2006
Age <- 22
Year <- 2013
Prefix <- "_Movement"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data


##### Calculate consistent weights 2009 #####
#############################################

Strata_variables <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST04Q01","IMMIG")
Strata_variables_derived <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile",
                              "Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "W_FSTUWT"

name <- "Weights_2009_age_19"
Variables <- "Uni_attend_age_19"
cohort <- 2009
Age <- 19
Year <- 2013
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_22"
Variables <- "Uni_attend_age_22"
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_23"
Variables <- "Uni_attend_all_years"
cohort <- 2009
Age <- 22
Year <- 2017
Prefix <- "C"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_19_probit"
Variables <- Variables_age_19_probit
cohort <- 2009
Age <- 19
Year <- 2013
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_22_probit"
Variables <- Variables_age_22_probit
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "CP"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_23_probit"
Variables <- c(Variables_age_22_probit,"Uni_dropout_age_23")
cohort <- 2009
Age <- 23
Year <- 2017
Prefix <- "CPU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_23_CU"
Variables <- c("Uni_attend_age_22","Uni_complete_age_23")
cohort <- 2009
Age <- 23
Year <- 2017
Prefix <- "CU"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_23_all"
Variables <- c(Variables_age_22_probit,"Manager_prof2017","Uni_attend_age_22","Uni_completion")
cohort <- 2009
Age <- 23
Year <- 2017
Prefix <- "ALL"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_23_MPC"
Variables <- c("Uni_attend_age_22","Manager_prof2017","Uni_complete_age_23")
cohort <- 2009
Age <- 23
Year <- 2017
Prefix <- "MPC"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_part_time"
Variables <- c(Variables_age_22_probit,"Uni_attend_age_22",
               "Study_status_commencement","Hours_worked_commencement",
               "Worked_first_year_university","No_ATAR")
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "PT"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_19_probit_i"
Variables <- Variables_age_19_probit_i
cohort <- 2009
Age <- 19
Year <- 2013
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_22_probit_i"
Variables <- Variables_age_22_probit_i
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "CPi"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_22_distance"
Variables <- c("Distance_moved_to_uni")
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "_Distance"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

name <- "Weights_2009_age_22_movement"
Variables <- c("Movement_data")
cohort <- 2009
Age <- 22
Year <- 2016
Prefix <- "_Movement"
Create_weights_function(Name=name,Cohort=cohort) # calculate consistent weights and append to data

# Clean up stuff that's no longer needed

rm(name,Variables,cohort,Age,Year,Prefix,Strata_variables,Strata_variables_derived,Survey_weights,
   Variables_age_19_probit,Variables_age_22_probit,Variables_age_19_probit_i,Variables_age_22_probit_i)

# Save weights to RData file, if not already saved from previous run
Cohorts <- c(1995,1998,2003,2006,2009)
for (cohort in Cohorts) { 
  if (!file.exists(paste0("Weights_",cohort,".RData"))) {
    save(list=paste0("Weights_",cohort),file=paste0("Weights_",cohort,".RData")) 
     }
}

# Merge weights with LSAY data (replaces weights if already in LSAY data)
cat(paste("Merging the weights with the LSAY data \n")) 
for (cohort in Cohorts) {
  if (!exists(paste0("Weights_",cohort,"_tibble"),envir = .GlobalEnv)) {
  assign(paste0("Weights_",cohort,"_tibble"),reduce(get(paste0("Weights_",cohort)),left_join, by = "ID"),.GlobalEnv)
    Weight_columns <- names(get(paste0("Weights_",cohort,"_tibble"))) [names(get(paste0("Weights_",cohort,"_tibble"))) %ni% "ID"]
    suppressWarnings( 
    assign(paste0("LSAY_",cohort,"_data"),get(paste0("LSAY_",cohort,"_data")) %>% select(-one_of(Weight_columns)) %>% # drop weight columns if already exist 
             left_join(.,get(paste0("Weights_",cohort,"_tibble")), by = "ID")))
  }
}


LSAY_1995_data$WT2006C <- LSAY_1995_data$WT2006CA
LSAY_1998_data$WT2009C <- LSAY_1998_data$WT2009CA

rm(Weights_1995,Weights_1998,Weights_2003,Weights_2006,Weights_2009,
   Weights_1995_tibble,Weights_1998_tibble,Weights_2003_tibble,Weights_2006_tibble,Weights_2009_tibble)

end_time <- Sys.time() 
cat("System time taken for loading / calculating weights and merging with LSAY data \n")
print(end_time - start_time)
cat("\n")
