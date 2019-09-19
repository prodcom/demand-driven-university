

##########----------------------------------------------------------------------------------------------------##########
#         Missing data imputation - also runs on "no_ATAR" group just PMM 
##########----------------------------------------------------------------------------------------------------##########


ATAR_imputation_function <- function(cohort) {
  
  ID_column <- ifelse(cohort %in% c(1995,1998), "stuidno", "STIDSTD")
  LSAY <- get(paste0("LSAY_",cohort)) %>% 
    mutate_at(vars(ID_column), list(as.numeric)) 
  LSAY_data <- get(paste0("LSAY_",cohort,"_data")) %>% 
    mutate_at(vars(ID_column), list(as.numeric)) 
  
  
  
  LSAY_data$ATAR_received <- ifelse(LSAY_data$ATAR_group %in% "Not_reported","ATAR_received",
                                    ifelse(LSAY_data$ATAR_group %in% "No_ATAR","No_ATAR",
                                           ifelse(LSAY_data$ATAR_group %in% "Not_available",NA,
                                                  ifelse(is.na(LSAY_data$ATAR_group),NA,"ATAR_received"))))

  if(cohort %in% c(1995,1998)) {
    LSAY_data_miss <- LSAY_data %>% select(Achievement,ATAR,ATAR_received,Completed_year_12,Parent_SES_quartile,Gender,
                                  NESB,State,Parent_edu,School_SES_quartile,Sector,Location_RR_wave1) }
  
  if(cohort %in% c(2003,2006,2009)) {
    LSAY_data_miss <- LSAY_data %>% select(PISA,ATAR,ATAR_received,Completed_year_12,Parent_SES_quartile,Gender,Plan_complete_yr_12,
                                  NESB,State,Parent_edu,School_SES_quartile,Sector,Location_RR_wave1,Educ_occupation_quartile) }
  
  LSAY_data_miss$Enter_Score_received <- as.factor(LSAY_data_miss$ATAR_received)
  
  
  #MICE Multivariate Imputation by Chained Equations
  
  
  LSAY_data_mice_imputed <- mice(LSAY_data_miss, m=10, maxit = 5, method = 'pmm', seed = 500)
  
  
  LSAY_data$ATAR_mice <- complete(LSAY_data_mice_imputed, 1)[,"ATAR"]
  
  LSAY_data$ATAR_group_imputed <- cut(LSAY_data$ATAR_mice, 
                                      breaks=c(-Inf, 50, 60, 70, 80, 90, 100), 
                                      labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()
  
  LSAY_data$ATAR_group_imputed <-  ifelse(complete(LSAY_data_mice_imputed, 1)[,"Enter_Score_received"] %in% "No_ATAR","No_ATAR",
                                          LSAY_data$ATAR_group_imputed) 
  
  ATAR_group_order <- c("90-100","80-90","70-80","60-70","50-60","0-50","No_ATAR")
  
  LSAY_data$ATAR_group_imputed <- factor(LSAY_data %>% pull(ATAR_group_imputed), levels = ATAR_group_order)  
  
  LSAY_data_imputations <- LSAY_data %>% select(one_of(ID_column),ATAR,Enter_Score_received,ATAR_received,ATAR_group,
                                                ATAR_mice,ATAR_group_imputed) 
  
  LSAY_data_imputations[,paste0(ID_column)] <- get(paste0("LSAY_",cohort,"_data"))[,paste0(ID_column)]
  
  return(LSAY_data_imputations)
  
  
}
