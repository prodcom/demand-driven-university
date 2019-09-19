
##########----------------------------------------------------------------------------------------------------##########
#        Function to generate summary statistics
##########----------------------------------------------------------------------------------------------------##########



Summary_statistics_function <- function(cohort=2009,equity_group="Low_SES",age=22,combine_undertaking="Yes",Normalise=TRUE) {
  
  if(age==19) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+5,cohort+4) # year at age 19
    Outcome_variable <- paste0("Outcome_age_19") 
    Uni_attend <- "Uni_attend_age_19" 
    Uni_complete <- "Uni_completion"  }
  
  if(age==22) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+9,cohort+8) # year at age 23
    Outcome_variable <- paste0("Outcome_age_23") 
    Uni_attend <- "Uni_attend_age_22" 
    Uni_complete <- "Uni_completion_age_23"  }
  
  if(age==23) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+9,cohort+8) # year at age 23
    Outcome_variable <- paste0("Outcome_age_23") 
    Uni_attend <- "Uni_attend_age_23" 
    Uni_complete <- "Uni_completion_age_23"  }
  
  if(age==25) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+11,cohort+10) # year at age 25
    Outcome_variable <- paste0("Outcome_age_25") 
    Uni_attend <- "Uni_attend_age_22" 
    Uni_complete <- "Uni_completion"  }
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  
  
  
  if(age == 22) {
    weights_C <- ifelse(cohort==1995,paste0("WT",Year-1,"C"),
                        ifelse(cohort==1998,paste0("WT",Year-1,"C"),
                               ifelse(cohort==2003,paste0("WT",Year-1,"C"),
                                      ifelse(cohort==2006,paste0("WT",Year-1,"C"),
                                             ifelse(cohort==2009,paste0("WT",Year-1,"C"),NA)))))  #Consistent weights on uni attend
  } 
  
  if(age == 23 | age == 19) {
    
    weights_C <- ifelse(cohort==1995,paste0("WT",Year,"C"),
                        ifelse(cohort==1998,paste0("WT",Year,"C"),
                               ifelse(cohort==2003,paste0("WT",Year,"C"),
                                      ifelse(cohort==2006,paste0("WT",Year,"C"),
                                             ifelse(cohort==2009,paste0("WT",Year,"C"),NA))))) #Consistent weights on uni attend 
  }
  
  if(age == 25) {
    weights_C <- ifelse(cohort==1995,paste0("WT",Year-3,"C"),
                        ifelse(cohort==1998,paste0("WT",Year-3,"C"),
                               ifelse(cohort==2003,paste0("WT",Year-3,"C"),
                                      ifelse(cohort==2006,paste0("WT",Year-3,"C"),
                                             ifelse(cohort==2009,paste0("WT",Year-3,"C"),NA)))))  #Consistent weights on uni attend
  } 
  
  
  
  weights_CU <- ifelse(cohort==1995,paste0("WT",Year,"CU"),
                       ifelse(cohort==1998,paste0("WT",Year,"CU"),
                              ifelse(cohort==2003,paste0("WT",Year,"CU"),
                                     ifelse(cohort==2006,paste0("WT",Year,"CU"),
                                            ifelse(cohort==2009,paste0("WT",Year,"CU"),NA))))) #Consistent weights on uni attend & complete
  
  weights_MPC <- ifelse(cohort==1995,paste0("WT",Year,"MPC"),
                        ifelse(cohort==1998,paste0("WT",Year,"MPC"),
                               ifelse(cohort==2003,paste0("WT",Year,"MPC"),
                                      ifelse(cohort==2006,paste0("WT",Year,"MPC"),
                                             ifelse(cohort==2009,paste0("WT",Year,"MPC"),NA))))) #Consistent weights on MP occupation, attend & complete
  
  design_C = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights_C])),]), 
                       weights = LSAY_data[which(!is.na(LSAY_data[,weights_C])),weights_C])
  
  design_CU = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights_CU])),]), 
                        weights = LSAY_data[which(!is.na(LSAY_data[,weights_CU])),weights_CU])
  
  design_MPC = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights_MPC])),]), 
                         weights = LSAY_data[which(!is.na(LSAY_data[,weights_MPC])),weights_MPC])
  
  Number_of_equity_groups <- sum(!is.na(unique(LSAY_data[,paste0(equity_group)])))
  
  if(Number_of_equity_groups == 1) {
    
    Table_C <- as.data.frame(svytotal(~get(Uni_attend), design = design_C,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
      rownames_to_column() %>% rename(Uni_attend = rowname, Count = total) %>% 
      mutate(Equity_group = paste0(equity_group),
             Category = paste0(equity_group),
             Uni_attend = recode(Uni_attend,`get(Uni_attend)0`='Did not attend',`get(Uni_attend)1`='Attended')) %>% 
      select(Uni_attend,Category,Equity_group,Count)
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- as_tibble(svyby(~PISA, by = ~get(Uni_attend)+get(equity_group), design = design_C,svymean,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
        rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
        rename(Equity_group = equity_group,
               PISA_score = statistic) %>%  
        #gather(key="Equity_group",value="Count",-c(Uni_attend)) %>% 
        mutate(Uni_attend =  recode_factor(Uni_attend,`1`='Attended',`0`='Did not attend'),
      Category = paste0(equity_group))

######
Table_C_PISA_quartile <- as_tibble(svyby(~get(Uni_attend), by = ~get(equity_group)+PISA_quartile, design = design_C,svymean,
                                         keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
  rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
  rename(Attendance_rate = Uni_attend1,
         Equity_group = equity_group) %>%  
  mutate(Category = paste0(equity_group)) %>% select(-Uni_attend0)
    } else {
      Table_C_PISA <- NA
      Table_C_PISA_quartile <- NA
    }
    
    Table_CU <- as_tibble(svyby(~get(Uni_attend), by = ~get(Uni_complete), design = design_CU,svytotal,
                                keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      gather(key="Uni_attend", value = "Count", -Uni_complete) %>% 
      mutate(Equity_group = paste0(equity_group),
             Category = paste0(equity_group),
             Uni_attend = recode(.$Uni_attend,`Uni_attend0`='Did not attend',`Uni_attend1`='Attended'),
             Uni_complete = factor(recode(.$Uni_complete,`Complete and undertaking further studies`='Postgraduate',
                                          `Currently undertaking`='Undertaking'),levels = c("Complete","Postgraduate","Undertaking","Dropped out","Never commenced")),
             Uni_complete_postgrad_complete = recode(Uni_complete, Postgraduate = "Complete"),
             Uni_complete_postgrad_undertaking = recode(Uni_complete, Postgraduate = "Undertaking"),
             type = as.factor(case_when(
               Uni_attend == ' Did not attend' ~ 'Never commenced', TRUE  ~ as.character(Uni_attend))))
    
    Table_MPC <- as_tibble(svyby(~get(Uni_attend), by = ~get(Uni_complete)+get(Outcome_variable), design = design_MPC,svytotal,
                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      rename(Outcome=Outcome_variable) %>% 
      gather(key="Uni_attend", value = "Count", -c(Uni_complete,Outcome))  %>% 
      mutate(Equity_group = paste0(equity_group),
             Category = paste0(equity_group),
             Uni_attend = recode(.$Uni_attend,`Uni_attend0`='Did not attend',`Uni_attend1`='Attended'),
             Outcome = factor(Outcome,levels = c("Manager or professional","Other occupation","Not working")) ,
             Uni_complete = factor(recode(.$Uni_complete,`Complete and undertaking further studies`='Postgraduate',
                                          `Currently undertaking`='Undertaking'),levels = c("Complete","Postgraduate","Undertaking","Dropped out","Never commenced")),
             Uni_complete_postgrad_complete = recode(Uni_complete, Postgraduate = "Complete"),
             Uni_complete_postgrad_undertaking = recode(Uni_complete, Postgraduate = "Undertaking"),
             type = as.factor(case_when(
               Uni_attend == ' Did not attend' ~ 'Never commenced', TRUE  ~ as.character(Uni_attend))),
             Category = paste0(equity_group))
  }
  
  if(Number_of_equity_groups > 1) {   
    
    Table_C <- as_tibble(svyby(~get(equity_group), by = ~get(Uni_attend), design = design_C,svytotal,
                               keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      rename_at(vars(contains('equity_group')), list(~str_remove(.,"equity_group"))) %>%  
      gather(key="Equity_group",value="Count",-c(Uni_attend)) %>% 
      mutate(Uni_attend =  recode_factor(.$Uni_attend,`1`='Attended',`0`='Did not attend'),
             Category = paste0(equity_group))
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- as_tibble(svyby(~PISA, by = ~get(Uni_attend)+get(equity_group), design = design_C,svymean,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
        rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
        rename(Equity_group = equity_group,
               PISA_score = statistic) %>%  
        #gather(key="Equity_group",value="Count",-c(Uni_attend)) %>% 
        mutate(Uni_attend =  recode_factor(.$Uni_attend,`1`='Attended',`0`='Did not attend'),
               Category = paste0(equity_group))
      
      
      Table_C_PISA_quartile <- as_tibble(svyby(~get(Uni_attend), by = ~get(equity_group)+PISA_quartile, design = design_C,svymean,
                                               keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
        rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
        rename(Attendance_rate = Uni_attend1,
               Equity_group = equity_group) %>%  
        mutate(Category = paste0(equity_group)) %>% select(-Uni_attend0)
    } else {
      Table_C_PISA <- NA
      Table_C_PISA_quartile <- NA
    }
    
    
    Table_CU <- as_tibble(svyby(~get(equity_group), by = ~get(Uni_attend)+get(Uni_complete), design = design_CU,svytotal,
                                keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      rename_at(vars(contains('equity_group')), list(~str_remove(.,"equity_group"))) %>% 
      gather(key="Equity_group",value="Count",-c(Uni_attend,Uni_complete)) %>% 
      mutate(Uni_attend =  recode_factor(.$Uni_attend,`1`='Attended',`0`='Did not attend'),
             Uni_complete = factor(recode(.$Uni_complete,`Complete and undertaking further studies`='Postgraduate',
                                          `Currently undertaking`='Undertaking'),levels = c("Complete","Postgraduate","Undertaking","Dropped out","Never commenced")),
             Uni_complete_postgrad_complete = recode(Uni_complete, Postgraduate = "Complete"),
             Uni_complete_postgrad_undertaking = recode(Uni_complete, Postgraduate = "Undertaking"),
             type = as.factor(case_when(
               Uni_attend == ' Did not attend' ~ 'Never commenced', TRUE  ~ as.character(Uni_attend))),
             Category = paste0(equity_group))
    
    Table_MPC <- as_tibble(svyby(~get(equity_group), by = ~get(Uni_attend)+get(Uni_complete)+get(Outcome_variable), design = design_MPC,svytotal,
                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE,na.rm.all=TRUE)) %>%
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      rename_at(vars(contains('equity_group')), list(~str_remove(.,"equity_group"))) %>% 
      rename(Outcome=Outcome_variable) %>% 
      gather(key="Equity_group",value="Count",-c(Uni_attend,Uni_complete,Outcome)) %>% 
      mutate(Uni_attend =  recode_factor(.$Uni_attend,`1`='Attended',`0`='Did not attend'),
             Outcome = factor(Outcome,levels = c("Manager or professional","Other occupation","Not working")) ,
             Uni_complete = factor(recode(.$Uni_complete,`Complete and undertaking further studies`='Postgraduate',
                                          `Currently undertaking`='Undertaking'),levels = c("Complete","Postgraduate","Undertaking","Dropped out","Never commenced")),
             Uni_complete_postgrad_complete = recode(Uni_complete, Postgraduate = "Complete"),
             Uni_complete_postgrad_undertaking = recode(Uni_complete, Postgraduate = "Undertaking"),
             type = as.factor(case_when(
               Uni_attend == ' Did not attend' ~ 'Never commenced', TRUE  ~ as.character(Uni_attend))),
             Category = paste0(equity_group))
  }
  
  
  ####
  
  if(equity_group=="Parent_SES_quartile") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- Table_C_PISA %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
      
      
      Table_C_PISA_quartile <- Table_C_PISA_quartile %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    }
    
    Table_CU <- Table_CU %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    
    Table_MPC <- Table_MPC %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    
  }
  
  
  if(equity_group=="Low_SES") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES')) 
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- Table_C_PISA %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  
      
      Table_C_PISA_quartile <- Table_C_PISA_quartile %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))
      
    }
    
    Table_CU <- Table_CU %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES')) 
    
    Table_MPC <- Table_MPC %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  
    
  }
  
  if(equity_group=="Parent_edu") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family'))  
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- Table_C_PISA %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family')) 
      
      Table_C_PISA_quartile <- Table_C_PISA_quartile %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family')) 
    }
    
    Table_CU <- Table_CU %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family')) 
    
    Table_MPC <- Table_MPC %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family')) 
  }
  
  if(equity_group=="Location_RR_wave1") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote')) 
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- Table_C_PISA %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote'))  
      
      Table_C_PISA_quartile <- Table_C_PISA_quartile %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote')) 
    }
    
    Table_CU <- Table_CU %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote')) 
    
    Table_MPC <- Table_MPC %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote')) 
    
  }
  
  
  if(equity_group=="Equity_group") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))
    
    if(cohort %in% c(2003,2006,2009)) {
      Table_C_PISA <- Table_C_PISA %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group')) 
      
      Table_C_PISA_quartile <- Table_C_PISA_quartile %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))
    }
    
    Table_CU <- Table_CU %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))
    
    Table_MPC <- Table_MPC %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))
  }
  
  
  All_data_attend_weights <- Table_C %>% 
    group_by(Category,Equity_group,Uni_attend) %>% 
    summarise(Count=sum(Count)) %>% 
    mutate(Cohort = paste0(cohort),
           Year = paste0(Year)) %>% 
    ungroup() 
  
  if(cohort %in% c(2003,2006,2009)) {
    All_data_PISA_attend_weights <- Table_C_PISA %>% 
      group_by(Category,Equity_group,Uni_attend) %>% 
      summarise(PISA_score=mean(PISA_score)) %>% 
      mutate(Cohort = paste0(cohort),
             Year = paste0(Year)) %>% 
      ungroup() 
    
    
    All_data_PISA_quartile_attend_weights <- Table_C_PISA_quartile %>% 
      group_by(Category,Equity_group,PISA_quartile) %>% 
      summarise(Attendance_rate=mean(Attendance_rate) * 100) %>% 
      mutate(Cohort = paste0(cohort),
             Year = paste0(Year))  %>% 
      ungroup() 
  }
  
  All_data_competion_weights <- Table_CU %>% 
    group_by(Category,Equity_group,Uni_attend,Uni_complete,Uni_complete_postgrad_complete,Uni_complete_postgrad_undertaking) %>% 
    summarise(Count=sum(Count)) %>% 
    mutate(Cohort = paste0(cohort),
           Year = paste0(Year))  %>% 
    ungroup() 
  
  All_data_outcome_weights <- Table_MPC %>% 
    group_by(Category,Equity_group,Uni_attend,Uni_complete,Uni_complete_postgrad_complete,Uni_complete_postgrad_undertaking,Outcome) %>% 
    summarise(Count=sum(Count)) %>% 
    mutate(Cohort = paste0(cohort),
           Year = paste0(Year))  %>% 
    ungroup() 
  
  
  if(combine_undertaking=="Yes") {
    Table_CU <- Table_CU %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Undertaking"))
    
    Table_MPC <- Table_MPC %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Undertaking"))
    
  }
  
  if(combine_undertaking=="postgraduate") {
    Table_C <- Table_C 
    
    Table_CU <- Table_CU %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Complete")) %>% 
      group_by(Category,Equity_group,Uni_attend,Uni_complete) %>% summarise(Count=sum(Count)) %>% ungroup()
    
    Table_MPC <- Table_MPC %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Complete")) %>% 
      group_by(Category,Equity_group,Uni_complete,Uni_attend,Outcome) %>% summarise(Count=sum(Count)) %>% ungroup()
    
  }
  
  Uni_completion_share <- bind_cols(
    Table_CU %>% 
      filter(Uni_complete %ni% "Never commenced", Uni_attend %ni% "Did not attend") %>%  #previously did not include the uni_attend filter
      group_by(Equity_group,Uni_complete) %>% 
      summarise(Percent=sum(Count)) %>% 
      mutate_if(is.numeric, list(~./ sum(.) * 100)),
    Table_CU %>% 
      group_by(Equity_group,Uni_complete) %>% 
      filter(Uni_complete %ni% "Never commenced", Uni_attend %ni% "Did not attend") %>%  #previously did not include the uni_attend filter
      summarise(Count=sum(Count)) %>% 
      ungroup() %>% 
      select(Count)) %>% 
    mutate(Cohort = paste0(cohort),
           Year = paste0(Year)) 
  
  Uni_completion_outcome <- bind_cols(
    Table_MPC %>% 
      group_by(Equity_group,Uni_complete,Outcome) %>% 
      summarise(Percent=sum(Count)) %>% 
      mutate_if(is.numeric, list(~./ sum(.) * 100)),
    Table_MPC %>% 
      group_by(Equity_group,Uni_complete,Outcome) %>% 
      summarise(Count=sum(Count)) %>% 
      ungroup() %>% 
      select(Count))
  
  Uni_attend_share <- bind_cols(
    Table_C %>% 
      group_by(Category,Equity_group,Uni_attend) %>% 
      summarise(Attendance_rate=sum(Count)) %>% 
      mutate_if(is.numeric, list(~./ sum(.) * 100)),
    Table_C %>% 
      group_by(Category,Equity_group,Uni_attend) %>% 
      summarise(Count=sum(Count)) %>% 
      ungroup() %>% 
      select(Count)) %>% 
    left_join(.,    Table_C %>%  
                group_by(Category,Uni_attend) %>% 
                summarise(Population=sum(Count)) %>% 
                ungroup(), by = c("Category","Uni_attend")) %>% 
    mutate(Equity_group_share = Count / Population * 100,
           Cohort = paste0(cohort),
           Year = paste0(Year)) 
  
  if (age == 22) {
    All_data_attend_weights$Year = as.character(as.numeric(All_data_attend_weights$Year)-1)
    if(cohort %in% c(2003,2006,2009)) {
      All_data_PISA_attend_weights$Year = as.character(as.numeric(All_data_PISA_attend_weights$Year)-1)
      All_data_PISA_quartile_attend_weights$Year = as.character(as.numeric(All_data_PISA_quartile_attend_weights$Year)-1)
    }
    Uni_attend_share$Year = as.character(as.numeric(Uni_attend_share$Year)-1)
  }
  
  # Sankey data
  
  sankey_data_equity <- Table_C %>%  
    group_by(Equity_group) %>% 
    summarise(Equity_share=sum(Count)) %>% 
    mutate_if(is.numeric, list(~./ sum(.) * 100))
  
  if(Normalise == TRUE) { sankey_data_equity$Equity_share = 100}
  
  sankey_data_attend <- Table_C %>% 
    group_by(Category,Equity_group,Uni_attend) %>% 
    summarise(Uni_attend_count=sum(Count)) %>% 
    ungroup() %>% 
    left_join(.,    Table_C %>%  
                group_by(Equity_group) %>% 
                summarise(Uni_attend_population=sum(Count)) %>% 
                ungroup(), by = c("Equity_group")) %>% 
    mutate(Attend_share = Uni_attend_count / Uni_attend_population * 100) %>% 
    select(-Uni_attend_count,-Uni_attend_population)%>% 
    left_join(.,sankey_data_equity, by = "Equity_group") %>% 
    mutate(Attend_sankey = Attend_share / 100 * Equity_share)
  
  sankey_data_completion <- Table_CU %>%  
    group_by(Category,Equity_group,Uni_attend,Uni_complete) %>% 
    summarise(Uni_completion_count=sum(Count)) %>% 
    ungroup() %>% 
    left_join(.,    Table_CU %>%  
                group_by(Equity_group,Uni_attend) %>% 
                summarise(Uni_completion_population=sum(Count)) %>% 
                ungroup(), by = c("Equity_group","Uni_attend")) %>% 
    mutate(Completion_share = Uni_completion_count / Uni_completion_population * 100) %>% 
    select(-Uni_completion_count,-Uni_completion_population) %>% 
    left_join(.,sankey_data_attend, by = c("Category","Equity_group","Uni_attend")) %>% 
    mutate(Completion_sankey = Completion_share / 100 * Attend_sankey)
  
  sankey_data_outcome <- Table_MPC %>%  
    group_by(Category,Equity_group,Uni_attend,Uni_complete,Outcome) %>% 
    summarise(Outcome_count=sum(Count)) %>% 
    ungroup() %>% 
    left_join(.,    Table_MPC %>%  
                group_by(Equity_group,Uni_attend,Uni_complete) %>% 
                summarise(Outcome_population=sum(Count)) %>% 
                ungroup(), by = c("Equity_group","Uni_attend","Uni_complete")) %>% 
    mutate(Outcome_share = Outcome_count / Outcome_population * 100) %>% 
    select(-Outcome_count,-Outcome_population) %>% 
    left_join(.,sankey_data_completion, by = c("Category","Equity_group","Uni_attend","Uni_complete")) %>% 
    mutate(Outcome_sankey = Outcome_share / 100 * Completion_sankey)
  
  Sankey_data <- bind_rows(
    sankey_data_equity %>% 
      mutate(Result = "Equity group") %>% 
      spread(Equity_group,Equity_share) %>% 
      mutate(Stage = "Equity group") %>% 
      select(Stage,Result, everything()),
    sankey_data_attend %>% 
      select(Equity_group,Uni_attend,Attend_sankey) %>% 
      spread(Equity_group,Attend_sankey) %>% 
      rename(Result = Uni_attend) %>% 
      mutate(Result = as.character(Result),
             Stage = "University attend"),
    sankey_data_completion %>% 
      filter(Uni_attend %in% "Attended") %>%
      select(Equity_group,Uni_complete,Completion_sankey) %>% 
      spread(Equity_group,Completion_sankey) %>% 
      rename(Result = Uni_complete) %>% 
      mutate(Result = as.character(Result),
             Stage = "University outcome"),
    sankey_data_outcome %>% 
      filter(Uni_attend %in% "Attended",Uni_complete %in% "Complete") %>% 
      select(Equity_group,Outcome,Outcome_sankey) %>% 
      spread(Equity_group,Outcome_sankey) %>% 
      rename(Result = Outcome) %>% 
      mutate(Result = as.character(Result),
             Stage = "Job outcome")) %>% 
    mutate(Cohort = paste0(cohort),
           Year = paste0(Year)) %>% 
    select(Cohort, Year, Stage, Result, everything())
  
  if(cohort %in% c(2003,2006,2009)) {
    return(list(Uni_attend_share,Uni_completion_share,Uni_completion_outcome,All_data_attend_weights,All_data_competion_weights, 
                All_data_outcome_weights,Sankey_data,All_data_PISA_attend_weights,All_data_PISA_quartile_attend_weights)) #9
  } else {  
    return(list(Uni_attend_share,Uni_completion_share,Uni_completion_outcome,All_data_attend_weights,All_data_competion_weights, 
                All_data_outcome_weights,Sankey_data)) #7
  }
  
}


##############################################################################################

Summary_statistics_function_AS <- function(cohort=2009,equity_group="Low_SES",age=22,combine_undertaking="Yes",Normalise=TRUE) {
  
  if(age==19) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+5,cohort+4) # year at age 19
    Outcome_variable <- paste0("Outcome_age_19") 
    Uni_attend <- "Uni_attend_age_19" 
    Uni_complete <- "Uni_completion"  }
  
  if(age==22) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+9,cohort+8) # year at age 23
    Outcome_variable <- paste0("Outcome_age_23") 
    Uni_attend <- "Uni_attend_age_22" 
    Uni_complete <- "Uni_completion_age_23"  }
  
  if(age==23) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+9,cohort+8) # year at age 23
    Outcome_variable <- paste0("Outcome_age_23") 
    Uni_attend <- "Uni_attend_age_23" 
    Uni_complete <- "Uni_completion_age_23"  }
  
  if(age==25) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+11,cohort+10) # year at age 25
    Outcome_variable <- paste0("Outcome_age_25") 
    Uni_attend <- "Uni_attend_age_22" 
    Uni_complete <- "Uni_completion"  }
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data")) 
  
  # Additional students calcs (attendance)
  
  weights <- ifelse(cohort==2006,"WT2013CP",ifelse(cohort==2009,"WT2016CP",NA))
  design = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights])),]), 
                     weights = LSAY_data[which(!is.na(LSAY_data[,weights])),weights])
  
  AS <- as_tibble(svyby(~Additional_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Additional_students = statistic)
  
  AT <- as_tibble(svyby(~Other_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Other_students = statistic)
  
  NT <- as_tibble(svyby(~Never_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Never_students = statistic)
  
  L <- as_tibble(svyby(~Fewer_student, by = ~get(equity_group), design = design,svytotal,
                       keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students = statistic)
  
  AS_table <- bind_cols(AS,AT %>% select(Other_students),NT %>% select(Never_students),L %>% select(Fewer_students)) %>% 
    mutate(Never_students=Never_students+Fewer_students) %>% select(-Fewer_students) %>% # Combine Fewer_students & never takers
    bind_rows(.,select_if(., is.numeric) %>% colSums()) %>% # Calculate column totals
    rename(Equity_group = `get(equity_group)`) %>% 
    mutate(Equity_group=as.character(Equity_group),
           Equity_group=replace(Equity_group, is.na(Equity_group), "Total"),
           Population = rowSums(.[2:4]),
           Total_students = Population - Never_students,
           Additional_students = Additional_students / Additional_students[Equity_group == "Total"] * 100,  # Row shares of total
           Other_students = Other_students / Other_students[Equity_group == "Total"] * 100,  # Row shares of total
           Never_students = Never_students / Never_students[Equity_group == "Total"] * 100,
           Population = Population / Population[Equity_group == "Total"] * 100,
           Total_students = Total_students / Total_students[Equity_group == "Total"] * 100,
           Category=paste0(equity_group),
           Cohort=paste0(cohort)) %>% 
    filter(Equity_group %ni% "Total") %>% 
    select(Category,Equity_group,Cohort,Additional_students,Other_students,Total_students,Never_students,Population) %>% 
    rename(Other_students = Other_students,Not_students=Never_students) %>% 
    mutate_if(is.numeric, round, 1)
  
  
  LSAY_data_AS <- as.data.frame(LSAY_data)
  LSAY_data_AS$Uni_attend_numeric <- as.numeric(unlist(lapply(LSAY_data_AS$Uni_attend_age_22, as.character)))
  
  Variables <- c("Worked_first_year_university","Hours_worked_commencement","Part_time_first_year","No_ATAR","PISA")
  Student_types <- c("Uni_attend_numeric","Additional_student","Other_student","Never_student","Fewer_student")
  
  weights <- ifelse(cohort==2006,"WT2013PT",ifelse(cohort==2009,"WT2016PT",NA))
  
  
  for (i in c(Variables,Student_types)) {
    LSAY_data_AS[,i] <- ifelse(!is.na(LSAY_data_AS[,weights]) & LSAY_data_AS$Uni_attend_age_22 %in% 1, LSAY_data_AS[,i], NA)
  }
  
  Totals_by_type <- bind_rows(lapply(Student_types, function (y) {  
    LSAY_data_AS %>% 
      group_by(get(equity_group)) %>% summarise(sum(get(y),na.rm=TRUE)) %>% 
      rename(Equity_groups = `get(equity_group)`,
             Count = `sum(get(y), na.rm = TRUE)`) %>% 
      mutate(Type = paste0(y))})) %>% 
    spread(key = Type,value = Count)
  
  LSAY_data_AS_totals <- LSAY_data_AS %>% 
    rename(Equity_groups = one_of(equity_group)) %>% 
    select(-one_of(Student_types)) %>%
    left_join(.,Totals_by_type, by = "Equity_groups")
  
  New_cols <- bind_cols(lapply(Variables, function (x) { 
    sapply(Student_types, function (y) {  (LSAY_data_AS[,x] * LSAY_data_AS[,y])  }) %>% 
      as_tibble() %>% 
      rename_at(vars(one_of(Student_types)), list(~paste0(.,"__",x))) # concatenate column name with variable x name
  })) # this creates a bunch of new columns that are every combination of Variablesi * Student_typesj
  
  LSAY_data_AS[,colnames(New_cols)] <- NULL # delete the New_cols from the LSAY_data_AS if they already exist
  LSAY_data_AS <- bind_cols(LSAY_data_AS,New_cols) 
  
  
  design = svydesign(ids = ~0, data = as.data.frame(LSAY_data_AS[which(!is.na(LSAY_data_AS[,weights])),]), 
                     weights = LSAY_data_AS[which(!is.na(LSAY_data_AS[,weights])),weights])
  
  
  New_variables <- as.vector(outer(Student_types, Variables, paste, sep="__")) # A vector of each of the new variable names
  New_variables <- which(LSAY_data_AS[,New_variables] %>% colSums(na.rm=TRUE) >0) %>% names() # Only use columns that have data
  options(scipen=999)
  
  
  AS_equity_table <- bind_rows(lapply(New_variables, function (x) { as.data.frame(svyby(~get(x),  by = ~get(equity_group),
                                                                                        design = design,svytotal,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>%
      rename(Equity_group = `get(equity_group)`) %>% 
      mutate(Variable = x) %>% 
      mutate_if(is.factor, as.character)})) %>% 
    separate(col=Variable,into=c("Type","Variable"),sep="__") #%>% 
  
  AS_equity_tot_table <- bind_rows(lapply(Student_types, function (x) { 
    as.data.frame(svyby(~get(x),  by = ~get(equity_group),
                        design = design,svytotal,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>%
      rename(Equity_group = `get(equity_group)`) %>% 
      mutate(Type = x) %>% 
      mutate_if(is.factor, as.character)})) %>% 
    rename(Total = statistic)
  
  AS_equity_table <- AS_equity_table %>% left_join(.,AS_equity_tot_table, by=c("Equity_group","Type")) %>% 
    mutate(Result = statistic / Total,
           Result = ifelse(Variable %ni% c("Hours_worked_commencement","PISA"),Result * 100,Result),
           Type = replace(Type, Type == "Uni_attend_numeric", "All students"),
           Type = replace(Type, Type == "Other_student", "Other students"),
           Type = replace(Type, Type == "Additional_student", "Additional students"),
           #Type = str_replace_all(Type, "_", " "),
           Variable = str_replace_all(Variable, "_", " ")) %>% 
    select(-statistic,-Total) 
  
  ####
  
  # Additional students calcs (outcomes)
  
  weights <- ifelse(cohort==2006,"WT2014CPU",ifelse(cohort==2009,"WT2017CPU",NA))
  design = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights])),]), 
                     weights = LSAY_data[which(!is.na(LSAY_data[,weights])),weights])
  
  AS <- as_tibble(svyby(~Additional_student, by = ~get(equity_group)+get(Uni_complete), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Additional_students = statistic)
  
  AT <- as_tibble(svyby(~Other_student, by = ~get(equity_group)+get(Uni_complete), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Other_students = statistic)
  
  NT <- as_tibble(svyby(~Never_student, by = ~get(equity_group)+get(Uni_complete), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Never_students = statistic)
  
  L <- as_tibble(svyby(~Fewer_student, by = ~get(equity_group)+get(Uni_complete), design = design,svytotal,
                       keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students = statistic)
  
  AS_complete_table <- bind_cols(AS,AT %>% select(Other_students),NT %>% select(Never_students),L %>% select(Fewer_students)) %>% 
    mutate(Never_students=Never_students+Fewer_students) %>% select(-Fewer_students) %>% # Combine Fewer_students & never takers
    rename(Equity_group = `get(equity_group)`,
           Uni_complete = `get(Uni_complete)`) %>% 
    mutate(Uni_complete = factor(recode(.$Uni_complete,`Complete and undertaking further studies`='Postgraduate',
                                        `Currently undertaking`='Undertaking'),levels = c("Complete","Postgraduate","Undertaking","Dropped out","Never commenced")),
           Uni_complete_postgrad_complete = recode(Uni_complete, Postgraduate = "Complete"),
           Uni_complete_postgrad_undertaking = recode(Uni_complete, Postgraduate = "Undertaking"),
           Equity_group=as.character(Equity_group),
           Population = rowSums(select_if(., is.numeric)),
           Total_students = Population - Never_students) %>% 
    group_by(Equity_group) %>% 
    mutate_if(is.numeric, list(~./ sum(.) * 100)) %>% 
    mutate(Category=paste0(equity_group),
           Cohort=paste0(cohort)) %>% 
    select(Category,Equity_group,Uni_complete,Cohort,Additional_students,Other_students,Total_students,Never_students,Population) %>% 
    rename(Other_students = Other_students,Not_students=Never_students) %>% 
    mutate_if(is.numeric, round, 1) %>% 
    ungroup()
  
  if(combine_undertaking=="Yes") {
    AS_complete_table <- AS_complete_table %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Undertaking")) %>% 
      group_by(Category,Equity_group,Uni_complete,Cohort) %>% 
      summarise(Additional_students=sum(Additional_students),
                Other_students=sum(Other_students),
                Total_students=sum(Total_students),
                Not_students=sum(Not_students),
                Population=sum(Population)) %>% 
      ungroup()
  }
  
  if(combine_undertaking=="postgraduate") {
    AS_complete_table <- AS_complete_table %>%
      mutate(Uni_complete = recode(Uni_complete, Postgraduate = "Complete")) %>% 
      group_by(Category,Equity_group,Uni_complete,Cohort) %>% 
      summarise(Additional_students=sum(Additional_students),
                Other_students=sum(Other_students),
                Total_students=sum(Total_students),
                Not_students=sum(Not_students),
                Population=sum(Population)) %>% 
      ungroup()
  }
  
  
  
  if(equity_group=="Parent_SES_quartile") {
    
    AS_table <- AS_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    AS_complete_table <- AS_complete_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
    AS_equity_table <- AS_equity_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES')) 
  }
  
  
  if(equity_group=="Low_SES") {
    
    AS_table <- AS_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  
    AS_complete_table <- AS_complete_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  
    AS_equity_table <- AS_equity_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  
    
  }
  
  if(equity_group=="Parent_edu") {
    
    AS_table <- AS_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family'))
    AS_complete_table <- AS_complete_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family'))
    AS_equity_table <- AS_equity_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family'))
    
  }
  
  if(equity_group=="Location_RR_wave1") {
    
    if (cohort %in% c(2006,2009)) {
      AS_table <- AS_table %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote'))
      AS_complete_table <- AS_complete_table %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote'))
      AS_equity_table <- AS_equity_table %>% 
        mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote'))
      
    }
  }
  
  
  if(equity_group=="Equity_group") {
    
    AS_table <- AS_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))  
    AS_complete_table <- AS_complete_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))   
    AS_equity_table <- AS_equity_table %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))    
    
  }
  
  return(list(AS_table,AS_complete_table,AS_equity_table)) 
}


##############################################################################################

Participation_rates_function <- function(cohort=2006,equity_group="Low_SES",age=23) {
  
  if(age==19) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+5,cohort+4) # year at age 19
    Uni_attend <- "Uni_attend_age_19" }
  
  if(age==22) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+8,cohort+7) # year at age 22
    Uni_attend <- "Uni_attend_age_22"  }
  
  if(age==23) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+9,cohort+8) # year at age 23
    Uni_attend <- "Uni_attend_age_23" }
  
  if(age==25) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+11,cohort+10) # year at age 25
    Uni_attend <- "Uni_attend_all_years"  }
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  weights_C <- paste0("WT",Year,"C")
  
  design_C = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights_C])),]), 
                       weights = LSAY_data[which(!is.na(LSAY_data[,weights_C])),weights_C])
  
  Number_of_equity_groups <- sum(!is.na(unique(LSAY_data[,paste0(equity_group)])))
  
  if(Number_of_equity_groups == 1) {
    
    Table_C <- as.data.frame(svytotal(~get(Uni_attend), design = design_C,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
      rownames_to_column() %>% rename(Uni_attend = rowname, Count = total) %>% 
      mutate(Equity_group = paste0(equity_group),
             Category = paste0(equity_group),
             Uni_attend = recode(.$Uni_attend,`get(Uni_attend)0`='Did not attend',`get(Uni_attend)1`='Attended')) %>% 
      select(Uni_attend,Category,Equity_group,Count)
  }
  
  if(Number_of_equity_groups > 1) {
    
    Table_C <- as_tibble(svyby(~get(equity_group), by = ~get(Uni_attend), design = design_C,svytotal,
                               keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
      rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
      rename_at(vars(contains('equity_group')), list(~str_remove(.,"equity_group"))) %>%  
      gather(key="Equity_group",value="Count",-c(Uni_attend)) %>% 
      mutate(Uni_attend =  as.factor(recode(.$Uni_attend,`1`='Attended',`0`='Did not attend')),
             Category = paste0(equity_group))
  }
  
  ####
  
  if(equity_group=="Parent_SES_quartile") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`PQ4`='High SES',`PQ3`='Middle SES',`PQ2`='Middle SES',`PQ1`='Low SES'))  
  }
  
  if(equity_group=="Low_SES") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_Low_SES`='Not low SES', `Low_SES`='Low SES'))  }
  
  if(equity_group=="Parent_edu") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Uni`= 'Not first in family',`No_uni`='First in family')) }
  
  if(equity_group=="Location_RR_wave1") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Metropolitan`='Metropolitan',`Regional_remote`='Regional or remote')) }
  
  if(equity_group=="Equity_group") {
    Table_C <- Table_C %>% 
      mutate(Equity_group = recode_factor(.$Equity_group,`Not_disadvantaged`='No equity group',`Disadvantaged`='At least 1 equity group'))   }
  
  
  Participation_and_composition <- bind_cols( # composition
    Table_C %>% 
      group_by(Category, Equity_group, Uni_attend) %>% 
      summarise(Participation_rate = sum(Count)) %>% 
      mutate_if(is.numeric, list(~./ sum(.) * 100)),
    Table_C %>% 
      group_by(Category, Equity_group, Uni_attend) %>% 
      summarise(Count = sum(Count)) %>% 
      ungroup() %>% 
      select(Count)) %>% 
    left_join(., Table_C %>%  
                group_by(Category, Uni_attend) %>% 
                summarise(Population = sum(Count)) %>% 
                ungroup(), by = c("Category", "Uni_attend")) %>% 
    mutate(Composition = Count / Population * 100,
           Cohort = paste0(cohort),
           Year = paste0(Year),
           Age = paste0(age))
  
  return(Participation_and_composition)
}


###############################


Group_compositions_function <- function(cohort=2006) {
  age = 22
  
  if(age==22) { 
    Year <- ifelse(cohort %in% c(1995,1998), cohort+8,cohort+7) # year at age 22
    Outcome_variable <- paste0("Outcome_age_23") 
    Uni_attend <- "Uni_attend_age_22" 
    Uni_complete <- "Uni_completion_age_23"  }
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  LSAY_source <- get(paste0("LSAY_",cohort))
  
  
  if(age == 22) {
    weights_C <- ifelse(cohort==1995,paste0("WT",Year,"C"),
                        ifelse(cohort==1998,paste0("WT",Year,"C"),
                               ifelse(cohort==2003,paste0("WT",Year,"C"),
                                      ifelse(cohort==2006,paste0("WT",Year,"C"),
                                             ifelse(cohort==2009,paste0("WT",Year,"C"),NA)))))  #Consistent weights on uni attend
  } 
  
  LSAY_data$weights_P <- LSAY_source[,paste0("WT",cohort,"P")]
  LSAY_data$Weights <- LSAY_data[,paste0(weights_C)] * (sum(LSAY_data[,paste0("weights_P")],na.rm=TRUE) / sum(LSAY_data[,paste0(weights_C)],na.rm=TRUE) )
  
  design_C = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,"weights_P"])),]), 
                       weights = LSAY_data[which(!is.na(LSAY_data[,"weights_P"])),"weights_P"]) # Population weights, wave 1
  
  
  Table <- as_tibble(svyby(~Indigenous+Low_SES+Location_RR_wave1+Parent_edu, by = ~Indigenous+Low_SES+Location_RR_wave1+Parent_edu, 
                           design = design_C,svytotal,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
    rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
    rename_at(vars(contains('statistic.')), list(~str_remove(.,"statistic."))) %>%  
    mutate_if(is.factor, as.character) 
  
  Equity_groups <- c("Indigenous","Low_SES","Location_RR_wave1","Parent_edu")
  Compositions_data <- bind_rows(lapply(Equity_groups, function (x) { Table %>% group_by_at(x) %>% summarise_if(is.numeric, sum) %>% rename("Equity_group" = 1) }))
  
  return(Compositions_data)
}

##########----------------------------------------------------------------------------------------------------##########
#         Predict university attendance and identify the additional students
##########----------------------------------------------------------------------------------------------------##########


Correlation_matrix_bivariate <- function(Correlation=1) {matrix(c(1,Correlation,Correlation,1), nrow = 2, ncol = 2) }

Additional_students_function <- function(cohort, Bivariate_normal = TRUE, Correlation){
  if(cohort %ni% c(2006,2009))  stop("The cohort must be 2006 or 2009")  
  
  model1 <- Prediction_model_2003 
  
  if(cohort == 2006) {
    df2 <- LSAY_2006_data
    model2 <- Prediction_model_2006
  }
  
  if(cohort == 2009) {
    df2 <- LSAY_2009_data
    model2 <- Prediction_model_2009
  }
  
  Variables <- all.vars(as.formula(model2)) 
  dependent_variable <- paste(all.vars(as.formula(model2))[1])
  
  if(Bivariate_normal == FALSE) {
    Predictions <- df2 %>% select(ID,one_of(Variables)) %>% na.omit() %>% 
      bind_cols(.,enframe(predict(model1, newdata = ., type = "response"))) %>% rename(Prediction1 = value) %>%
      bind_cols(.,enframe(predict(model2, newdata = ., type = "response")))  %>% rename(Prediction2 = value) %>% select(ID,Prediction1,Prediction2)
    
    Predictions <- Predictions %>% right_join(.,df2 %>% select(ID,one_of(dependent_variable)), by="ID")
    
    Predictions <- Predictions %>% mutate(Other_student = ifelse(get(dependent_variable) == 1,
                                                                ifelse(Predictions$Prediction1 > Predictions$Prediction2,1,
                                                                       Predictions$Prediction1/Predictions$Prediction2),0),
                                          Additional_student =  ifelse(get(dependent_variable)  == 1,
                                                                       ifelse(Predictions$Prediction1 > Predictions$Prediction2,0,
                                                                              (Predictions$Prediction2-Predictions$Prediction1)/Predictions$Prediction2),0),
                                          
                                          Never_student = ifelse(get(dependent_variable)  == 0,1,0),
                                          Fewer_student = ifelse(get(dependent_variable) == 0,0,0)) 
  }
  
  if(Bivariate_normal == TRUE) {
    
    Predictions <- df2 %>% select(ID,one_of(Variables)) %>% na.omit() %>% 
      bind_cols(.,enframe(predict(model1, newdata = .))) %>% rename(Prediction1 = value) %>% 
      bind_cols(.,enframe(predict(model2, newdata = .))) %>% rename(Prediction2 = value) #%>% select(ID,Prediction1,Prediction2)
    
    Predictions$Prediction1 <- sapply(1:nrow(Predictions), 
                                      function (x) pmvnorm(lower=c(-Inf,-Inf), 
                                                           upper=as.numeric(Predictions[x,c("Prediction1","Prediction2")]), 
                                                           mean=c(0,0), corr=Correlation)) #Bivariate normal calculation
    
    Predictions <- Predictions %>% select(-Prediction2) %>% 
      bind_cols(.,enframe(predict(model2, newdata = ., type = "response")))  %>% 
      rename(Prediction2 = value) %>% select(ID,Prediction1,Prediction2) %>% 
      right_join(.,df2 %>% select(ID,one_of(dependent_variable)), by="ID") %>% 
      mutate(Other_student = ifelse(is.na(Prediction2),NA,
                                   ifelse(df2 %>% pull(paste0(dependent_variable)) == 1,(Prediction1/Prediction2),0)),
             Additional_student = ifelse(is.na(Prediction2),NA,
                                         ifelse(df2 %>% pull(paste0(dependent_variable)) == 1,1-(Prediction1/Prediction2),0)),
             Never_student = ifelse(is.na(Prediction2),NA,
                                  ifelse(df2 %>% pull(paste0(dependent_variable)) == 0,1,0)),
             Fewer_student = 0)
  }
  
  for (i in c("Other_student","Additional_student","Never_student","Fewer_student","Prediction1","Prediction2","Bivariate_normal")) { 
    if(i %in% (get(paste0("LSAY_",cohort,"_data")) %>% names(.))) {
      assign(paste0("LSAY_",cohort,"_data"), 
             get(paste0("LSAY_",cohort,"_data")) %>% select(-one_of(i)),.GlobalEnv)}
  }
  
  assign(paste0("LSAY_",cohort,"_data"), 
         get(paste0("LSAY_",cohort,"_data")) %>% bind_cols(.,Predictions),
         .GlobalEnv)
}


##########----------------------------------------------------------------------------------------------------##########
#         Marginal students table and plot
##########----------------------------------------------------------------------------------------------------##########

Additional_students_charts <- function(cohort=2009, groups_categorise,variable_names, university_network=FALSE){
  #variable names is a dataframe with 2 columns (Old_names ' New_names), which changes cateogry and variables names and puts the in the order specified (make sure you order them how you want them in the plot)
  
  if(cohort %ni% c(2006,2009))  stop("The cohort must be 2006 or 2009")  
  
  model1 <- Prediction_model_2003 
  df1 <- LSAY_2003_data
  
  if(cohort == 2006) {
    df2 <- LSAY_2006_data
    model2 <- Prediction_model_2006
  }
  
  if(cohort == 2009) {
    df2 <- LSAY_2009_data
    model2 <- Prediction_model_2009
  }
  
  Variables <- all.vars(as.formula(model2)) 
  dependent_variable <- paste(all.vars(as.formula(model2))[1])
  year = ifelse(cohort == 2006, 2013, 2016)
  Weights <- paste0("WT",year,"CP")
  
  design1 <- svydesign(ids = ~0, data = as.data.frame(df1 %>% drop_na(WT2010CP)), 
                       weights = df1 %>% drop_na(WT2010CP) %>% pull(WT2010CP))
  
  design2 <- svydesign(ids = ~0, data = as.data.frame(df2 %>% drop_na(Weights)), 
                       weights = df2 %>% drop_na(Weights) %>% pull(Weights))
  
  Additional_student <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(~Additional_student, by = ~get(x), design = design2,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% # use survey package to get the % of additional students by population group 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% # Create a column that contains the group name
      select(Category, Name, statistic))) %>% # only keep needed columns
    rename(Additional_students = statistic) 
  
  Other_student <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(~Other_student, by = ~get(x), design = design2,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% # use survey package to get the % of always takers by population group 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% # Create a column that contains the group name
      select(Category, Name, statistic))) %>% # only keep needed columns
    rename(Other_students = statistic)
  
  Never_student <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(~Never_student, by = ~get(x), design = design2,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% # use survey package to get the % of never takers by population group 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% # Create a column that contains the group name
      select(Category, Name, statistic))) %>% # only keep needed columns
    rename(Never_students = statistic)
  
  Fewer_student <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(~Fewer_student, by = ~get(x), design = design2,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% # use survey package to get the % of Fewer_students by population group 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% # Create a column that contains the group name
      select(Category, Name, statistic))) %>% # only keep needed columns
    rename(Fewer_students = statistic)
  
  data_model_only <- df2 %>% drop_na(Variables) # only include observations that are used in the model
  design2_data_model_only <-  design2[which(complete.cases(design2$variables[,Variables])==TRUE),]  # only include observations that are used in the model
  design1_data_model_only <-  design1[which(complete.cases(design1$variables[,Variables])==TRUE),] 
  
  Attendance_rate_actual <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(as.formula(paste("~", paste(all.vars(as.formula(model1))[1]))), by = ~get(x), drop.empty.groups = FALSE,
                    design = design2_data_model_only,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% 
      rename(`Non-attendance` = statistic.Uni_attend_age_220,
             !!(paste0("Actual_attendance_rate_",cohort)) := statistic.Uni_attend_age_221) %>% 
      select(Category, Name, one_of(paste0("Actual_attendance_rate_",cohort)))))
  
  
  Attendance_rate_actual_2003 <- bind_rows(lapply(groups_categorise, function(x) 
    as_tibble(svyby(as.formula(paste("~", paste(all.vars(as.formula(model1))[1]))), by = ~get(x), drop.empty.groups = FALSE,
                    design = design1_data_model_only,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
      mutate(Category=x,Name = as.character(`get(x)`)) %>% 
      rename(`Non-attendance_base_year` = statistic.Uni_attend_age_220,
             !!(paste0("Actual_attendance_rate_",2003)) := statistic.Uni_attend_age_221) %>% 
      select(Category, Name, one_of(paste0("Actual_attendance_rate_",2003)))))
  
  Attendance_rate_actual <- bind_cols(Attendance_rate_actual, Attendance_rate_actual_2003 %>% select(-c(Category,Name)))
  
  Marginal_students_BD <- bind_cols(Other_student,Never_student %>% select(Never_students),Additional_student %>% select(Additional_students),
                                    Fewer_student %>% select(Fewer_students),Attendance_rate_actual %>% select(-c(Category,Name)))
  
  if(university_network == TRUE) {
    #Group of 8 chart start#
    
    AS_type_of_uni <- as_tibble(svyby(~Additional_student, by = ~Type_of_uni_age_22, design = design2,svytotal,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Additional_students =statistic)
    
    AT_type_of_uni <- as_tibble(svyby(~Other_student, by = ~Type_of_uni_age_22, design = design2,svytotal,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Other_students =statistic)
    
    NT_type_of_uni <- as_tibble(svyby(~Never_student, by = ~Type_of_uni_age_22, design = design2,svytotal,
                                      keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Never_students =statistic)
    
    L_type_of_uni <- as_tibble(svyby(~Fewer_student, by = ~Type_of_uni_age_22, design = design2,svytotal,
                                     keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students =statistic)
    Attendance_2003 <- as_tibble(svymean(~Type_of_uni_age_22, design = design1)) %>% rename(Actual_attendance_rate_2003=mean)
    
    Type_of_uni <- bind_cols(AS_type_of_uni,AT_type_of_uni %>% select(Other_students),NT_type_of_uni %>% select(Never_students),
                             L_type_of_uni %>% select(Fewer_students)) %>% 
      mutate(Total = rowSums(.[2:5]),
             Total_percent = Total/sum(Total),
             Additional_students_percentage_points = Additional_students/Total * Total_percent,
             Other_students_percentage_points = Other_students/Total * Total_percent,
             Type_of_uni_age_22=replace(Type_of_uni_age_22, Type_of_uni_age_22 == "Go8", "Group of Eight")) %>% 
      select(Type_of_uni_age_22,Total_percent,Additional_students_percentage_points,Other_students_percentage_points) %>% 
      rename_at(vars(contains('_percentage_points')), list(~str_remove(.,'_percentage_points'))) %>%
      rename(Name = Type_of_uni_age_22) %>% 
      bind_cols(.,Attendance_2003 %>% select(Actual_attendance_rate_2003)) %>% 
      rename_at(vars(contains('Total_percent')), list(~str_replace(.,'Total_percent',paste0("Actual_attendance_rate_",cohort2)))) %>% 
      filter(Name != "No university") %>% 
      mutate(Category="University type",
             Never_students=0,
             Fewer_students=0)
    
    Marginal_students_BD <- bind_rows(Marginal_students_BD,Type_of_uni)
  }
  
  Marginal_students_BD <- Marginal_students_BD %>% mutate_if(is.numeric, list(~. * 100))
  rownames(Marginal_students_BD) <- c()
  
  Marginal_students_BD_plot <- Marginal_students_BD %>% gather(key = "Type", value = "Percent",-c(Category,Name))
  
  Marginal_students_BD_plot$Category <- ifelse(is.na(with(variable_names,New_names[match(Marginal_students_BD_plot$Category,Old_names)])), 
                                               Marginal_students_BD_plot$Category,
                                               with(variable_names,New_names[match(Marginal_students_BD_plot$Category,Old_names)]))
  
  Marginal_students_BD_plot$Variable <- ifelse(is.na(with(variable_names,New_names[match(Marginal_students_BD_plot$Name,Old_names)])), 
                                               Marginal_students_BD_plot$Name,
                                               with(variable_names,New_names[match(Marginal_students_BD_plot$Name,Old_names)]))
  
  Marginal_students_BD_plot$Type <- with(Marginal_students_BD_plot, str_replace_all(Type, "_", " "))
  Marginal_students_BD_plot$Type <- ifelse(Marginal_students_BD_plot$Type == "Additional_students", "Additional students",
                                           ifelse(Marginal_students_BD_plot$Type == "Always takers", "Other students",
                                                  ifelse(Marginal_students_BD_plot$Type == "Never takers", "Never takers",
                                                         ifelse(Marginal_students_BD_plot$Type == "Fewer_students", "Fewer_students",Marginal_students_BD_plot$Type))))
  
  Marginal_students_BD_plot <- Marginal_students_BD_plot[which(Marginal_students_BD_plot$Type %in% 
                                                                 c("Additional students","Other students", 
                                                                   paste0("Actual attendance rate ",cohort),
                                                                   paste0("Actual attendance rate ",2003))),]
  
  Marginal_students_BD_plot <- Marginal_students_BD_plot %>% 
    mutate(Type = factor(Type, levels = c("Additional students","Other students",
                                          paste0("Actual attendance rate ",cohort),
                                          paste0("Actual attendance rate ",2003))),
           Variable = factor(Variable, 
                             levels = unique(c(variable_names[which(variable_names$New_names %in% Marginal_students_BD_plot$Variable),
                                                              "New_names"],
                                               Marginal_students_BD_plot$Variable))),
           Category = factor(Category, 
                             levels = unique(c(variable_names[which(variable_names$New_names %in% Marginal_students_BD_plot$Category),
                                                              "New_names"],
                                               Marginal_students_BD_plot$Category))))  # Change the order for the histogram
  
  reordered_table <- Marginal_students_BD_plot[order(Marginal_students_BD_plot$Category),] 
  variables <- unique(reordered_table$Category)
  
  attendance_rate_1 <- paste0("Actual attendance rate ",2003)
  attendance_rate_2 <- paste0("Actual attendance rate ",cohort)
  
  Marginal_plots <- list() 
  for (i in variables) { 
    plot_data <- Marginal_students_BD_plot %>% filter(Category == i, Type %ni% c("attendance_rate_1","attendance_rate_2","Actual attendance rate 2003",
                                                                                 "Actual attendance rate 2006","Actual attendance rate 2009"))  
    assign(paste0("Marginal_",i),
           ggplot(data = plot_data,aes(x=Variable)) + 
             geom_bar(aes(y = Percent, fill = Type, group=Type),stat="identity", position = "stack") +  
             scale_y_continuous(name="Per cent",expand = c(0, 0.0), breaks = seq(0, 100, by=20), limits=c(0,100))+
             theme(legend.title=element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                   legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10),
                   plot.title = element_text(face="bold",size = 9,hjust=0.5), axis.text.x = element_text(size=9)) + 
             ggtitle(i))
    assign(paste0("Marginal_",i),get(paste0("Marginal_",i)) +
             scale_fill_manual(values = palette(),labels = get(paste0("Marginal_",i)))+
             scale_colour_manual(values = as.character(palette())[c(4,7)])) 
    
    Marginal_plots[[i]] <- get(paste0("Marginal_",i))
  }
  
  Marginal_plots <- ggarrange(plotlist=Marginal_plots,common.legend = TRUE, legend="bottom")
  
  
  return(list(Attendance_rate_actual,Marginal_plots,Marginal_students_BD,Marginal_students_BD_plot))
}



##########----------------------------------------------------------------------------------------------------##########
#         Decomposition chart functions
##########----------------------------------------------------------------------------------------------------##########

Decomp_table <- function(Cohort,Model,Groupings,Age) {
  
  MV_data <- rownames_to_column(as.data.frame(get(paste0("MV_",Cohort,"_",Age,"_",Groupings[1]))$decomposition), var = "Name")
  colnames(MV_data)[2] <- Groupings[1]
  
  if(length(Groupings)>1) {
    MV_data <- bind_cols(MV_data,
                         as_tibble(sapply(Groupings[2:length(Groupings)], function(x) x = get(paste0("MV_",Cohort,"_",Age,"_",x))$decomposition)))
  }
  # Add in variable and category names (uses the list of model variables and string split to achieve this)
  
  Variables <- all.vars(as.formula(Model))
  MV_data$Variable <- MV_data$Name   
  MV_data$Variable <- sapply(Variables,
                             function (x) MV_data$Variable  <<- {sub(x, "", MV_data$Variable)})[,length(Variables)]
  MV_data$Variable <- ifelse(MV_data$Variable == "",MV_data$Name,MV_data$Variable)
  
  Categories <- as_tibble(str_split_fixed(MV_data$Name, MV_data$Variable,2)) 
  Categories <- paste0(Categories$V1,Categories$V2)
  MV_data$Category <- Categories
  MV_data$Category <- ifelse(MV_data$Category == "", MV_data$Name,MV_data$Category)
  
  MV_data_participation_rate_group_2 <- as_tibble(t(as.data.frame(sapply(names(MV_data %>% select_if(., is.numeric)),function(x) {
    as.numeric(get(paste0("MV_",Cohort,"_",Age,"_",x))$group.means$mean.fit.y[2])}))))
  
  MV_data_participation_rate_group_1 <- as_tibble(t(as.data.frame(sapply(names(MV_data %>% select_if(., is.numeric)),function(x) {
    as.numeric(get(paste0("MV_",Cohort,"_",Age,"_",x))$group.means$mean.fit.y[1])}))))
  
  MV_data <- bind_rows(MV_data_participation_rate_group_2,MV_data,MV_data_participation_rate_group_1) 
  MV_data <- bind_cols(MV_data %>% select_if(., is.character),MV_data %>% select_if(., is.numeric) *100) 
  
  MV_data[1,"Variable"] <- "Group_2"
  MV_data[nrow(MV_data),"Variable"] <- "Group_1"
  
  MV_data$Variable <- factor(MV_data$Variable,levels = unique(MV_data$Variable))
  MV_data$Cohort <- Cohort
  
  return(MV_data)
}

Variable_names_decomp <- tibble(Old_names = c("PQ1", "PQ2", "PQ3", "PQ4", "Indigenous", "PISA", "Achievement",
                                            "Catholic", "Government", "Male","Female", "Under 40km", "40 to 80km", "Over 80km", 
                                            "Uni", "No_uni","English speaking background", "Non-English", "Regional_remote",
                                            "VIC","NSW","QLD","SA","WA","NT","ACT","TAS",
                                            "Not attended","Hours_worked_in_year_12","Hours_worked_age_of_yr_12_interaction_attend",
                                            "Group_2","Group_1","0-100","101-500","EQ1","EQ2","EQ3","SQ1","SQ2","SQ3","(Intercept)"),
                         New_names = c("Family characteristics", "Family characteristics", "Family characteristics", "Family characteristics", 
                                       "Other","PISA score","PISA score","School characteristics", "School characteristics","Other","Other",
                                       "Geographic characteristics", "Geographic characteristics", "Geographic characteristics", 
                                       "Family characteristics", "Family characteristics","Family characteristics", "Family characteristics", 
                                       "Geographic characteristics","Other","Other","Other","Other","Other","Other","Other","Other",
                                       "Other","Other","Other","Group_2","Group_1","Family characteristics",
                                       "Family characteristics","Neighbourhood characteristics","Neighbourhood characteristics",
                                       "Neighbourhood characteristics","School characteristics","School characteristics",
                                       "School characteristics","(Intercept)"),
                         Target_variable_name = c("Low_SES","Low_SES","Low_SES","Low_SES","Indigenous","Low_aptitude","Low_aptitude",
                                                  "Government","Government","Gender","Gender","Distance","Distance","Distance",
                                                  "Parent_edu","Parent_edu","NESB","NESB","Location_RR",
                                                  "State","State","State","State","State","State","State","State",
                                                  "Year_12","Hours_yr_12_10_or_more","Interaction_hours_attend",
                                                  "Group_2","Group_1","Books","Books","Low_SEIFA","Low_SEIFA","Low_SEIFA",
                                                  "Low_sch_SES","Low_sch_SES","Low_sch_SES","Intercept"),
                         Target_variable_long_name = c("Low socioeconomic status","Low socioeconomic status","Low socioeconomic status",
                                                       "Low socioeconomic status","Indigenous","Low_aptitude","Low_aptitude",
                                                       "Government","Government","Gender","Gender","Distance","Distance","Distance",
                                                       "First in family","First in family",
                                                       "English speaking background","English speaking background","Regional or remote location",
                                                       "State","State","State","State","State","State","State","State",
                                                       "Year_12","Hours_yr_12_10_or_more","Interaction_hours_attend",
                                                       "Group_2","Group_1","Books","Books","Low_SEIFA","Low_SEIFA","Low_SEIFA",
                                                       "Low_sch_SES","Low_sch_SES","Low_sch_SES","Intercept"),
                         Target_variable_long_name_2 = c("Top three SES quartiles","Top three SES quartiles","Top three SES quartiles","Top three SES quartiles",
                                                         "Non-indigenous","Low_aptitude","Low_aptitude",
                                                         "Non-government school","Non-government school","Gender","Gender","Distance","Distance","Distance",
                                                         "Parent with university degree","Parent with university degree",
                                                         "Non-English speaking background","Non-English speaking background","Metropolitan location",
                                                         "State","State","State","State","State","State","State","State",
                                                         "Year_12","Hours_yr_12_10_or_more","Interaction_hours_attend",
                                                         "Group_2","Group_1","Books","Books","Low_SEIFA","Low_SEIFA","Low_SEIFA",
                                                         "Low_sch_SES","Low_sch_SES","Low_sch_SES","Intercept"),
                         Target_variable_chart_title = c("Low socioeconomic status","Low socioeconomic status","Low socioeconomic status","Low socioeconomic status",
                                                         "Indigenous status","Low_aptitude","Low_aptitude",
                                                         "Government","Government","Gender","Gender","Distance","Distance","Distance",
                                                         "First in family","First in family","NESB","NESB","Regional or remote location",
                                                         "State","State","State","State","State","State","State","State",
                                                         "Year_12","Hours_yr_12_10_or_more","Interaction_hours_attend",
                                                         "Group_2","Group_1","Books","Books","Low_SEIFA","Low_SEIFA","Low_SEIFA",
                                                         "Low_sch_SES","Low_sch_SES","Low_sch_SES","Intercept"))


Decomp_chart <- function(Target_variable,Data) {
  
  
  MV_data_wf_chart <- Data %>%  
    mutate(Variable = sapply(Variable, as.character)) %>% 
    select(Variable,Target_variable) %>% 
    left_join(.,Variable_names_decomp,by = c("Variable"="Old_names")) %>% 
    mutate(New_names=replace(New_names, Target_variable_name==Target_variable, "Group effect")) %>% 
    dplyr::group_by(New_names) %>% 
    dplyr::summarise(Target_variable = sum(get(Target_variable))) %>% 
    mutate(New_names = factor(New_names,levels=c("(Intercept)","Group_1","PISA score","Gender","Indigenous",
                                                 "Hours worked in year 12","Family characteristics","School characteristics",
                                                 "Neighbourhood characteristics","Geographic characteristics","Other","Group effect","Group_2"))) %>% 
    arrange(New_names) %>% 
    #mutate(roll=cumsum(Target_variable)) %>% 
    #mutate(base=shift(roll, 1L, type="lag")) %>% 
    filter(New_names !="(Intercept)") %>% # , New_names != "Group_1") %>% 
    #mutate(roll = ifelse(New_names == "Group_2",0,roll)) %>% 
   # mutate(base = ifelse(New_names == "Group_2",Target_variable,base)) %>% 
    #mutate(roll_minus_base=roll-base) %>% 
   # mutate(roll_minus_base = ifelse(New_names == "Group_2",0,roll_minus_base)) %>% 
   # mutate(Colour = ifelse(roll_minus_base<0,"rgba(255, 0, 0, 1)","rgba(120, 162, 47, 1)")) %>% 
    #mutate(Colour_gg = ifelse(roll_minus_base<0,"Down","Up")) %>% 
    mutate(id = rev(1:n())) %>% 
    mutate(New_names = fct_rev(New_names)) %>% 
    droplevels() %>% 
    dplyr::arrange(New_names) %>% 
    mutate(New_names = sapply(New_names, as.character))
  
  colnames(MV_data_wf_chart)[2] <- "Decomp"
  MV_data_wf_chart$Target_variable <- Target_variable
  MV_data_wf_chart$Total <- sum(MV_data_wf_chart %>% filter(New_names %ni% c("Group_2","Group_1")) %>% select(Decomp))
  MV_data_wf_chart$Prop  <-  MV_data_wf_chart$Decomp /  MV_data_wf_chart$Total * 100 #

  return(MV_data_wf_chart)
}


Decomp_table2 <- function(Cohort,Data) {
  
  MV_data_wf_chart_all <- apply(as.matrix(Groupings),1,function (x) Decomp_chart(Target_variable=x,Data=Data)) %>% 
    lapply(., function(df) {
      df$Stack <- ifelse(as.numeric(df %>% filter(New_names=="Group_1") %>% select(Decomp)) <
                           as.numeric(df %>% filter(New_names=="Group_2") %>% select(Decomp)),-1,1)
      df
    }) %>% 
    lapply(., function(df) {
      df$Stack <- df$Stack * df$Decomp
      df # multiply decomp by -1 or 1
    })  %>% 
    lapply(., function(df) {
      df[which(df$New_names == "Group_1"),"Stack"] <- min(c(as.numeric(df %>% filter(New_names=="Group_1") %>% select(Decomp)),
                                                            as.numeric(df %>% filter(New_names=="Group_2") %>% select(Decomp))))
      df
    }) %>% 
    lapply(., function(df) {
      df[which(df$New_names == "Group_2"),"Stack"] <- max(c(as.numeric(df %>% filter(New_names=="Group_1") %>% select(Decomp)),
                                                            as.numeric(df %>% filter(New_names=="Group_2") %>% select(Decomp))))
      df
    }) %>% 
    bind_rows(.) %>% 
    mutate(Cohort=Cohort) 
  
  return(MV_data_wf_chart_all)
}

####

Detailed_decomp_table <- function(cohort = 2003) {
  
  Table <- get(paste0("MV_data_",cohort)) %>% 
    select(-c(Name,Category)) %>%  gather(key = "Target_variable", value = "Decomp",-c(Variable,Cohort)) %>% 
    mutate_if(is.factor, as.character) %>% 
    left_join(.,Variable_names_decomp %>% select(Target_variable_name,Old_names,Target_variable_long_name,New_names),by = c("Variable"="Old_names")) %>% 
    mutate(New_names=replace(New_names, Target_variable_name==Target_variable, "Group effect")) %>% 
    mutate(New_names = factor(New_names,levels=c("(Intercept)","Group_1","Group effect","PISA score","Gender","Indigenous",
                                                 "Hours worked in year 12","Family characteristics","School characteristics",
                                                 "Neighbourhood characteristics","Geographic characteristics","Other","Group_2")),
           New_names = recode(New_names, `(Intercept)` = "Gap")) %>% 
    arrange(Target_variable,New_names) %>%
    mutate(id = rev(1:n())) %>% 
    mutate(New_names = fct_rev(New_names)) %>% 
    droplevels() %>% 
    arrange(Target_variable,New_names) %>% 
    mutate(New_names = sapply(New_names, as.character)) %>% 
    group_by(Target_variable) %>% 
    mutate(Total = sum(Decomp[New_names %ni% c("Group_2","Group_1")]),
           Prop = Decomp / Total * 100) %>% 
    mutate(New_names = factor(New_names,levels=c("Group_1","Group_2","Gap","PISA score","Gender","Indigenous",
                                                 "Hours worked in year 12","Family characteristics","School characteristics",
                                                 "Neighbourhood characteristics","Geographic characteristics","Other","Group effect"))) %>% 
    droplevels() %>% 
    mutate(Part_rate = sum(Decomp[New_names %in% c("Group_2")]),
           Year = as.character(recode(Cohort,`2003`=2010,`2006`=2013,`2009`=2016))) %>% 
    group_by(Target_variable,New_names,Target_variable_long_name,Cohort,Year) %>% 
    summarise(Decomp = sum(Decomp),
              Total = mean(Total),
              Prop = sum(Prop),
              Part_rate = mean(Part_rate)) %>% 
    arrange(Cohort) %>% 
    group_by(Target_variable,Cohort,Year) %>% 
    mutate(Decomp = ifelse(New_names == "Gap",  sum(Decomp[New_names %ni% "Group_2"]) - sum(Decomp[New_names %ni% "Group_1"]),Decomp)) %>% 
    ungroup() %>%
    
    select(Cohort,Year,Target_variable, New_names,Target_variable_long_name, Percent_share = Prop,Percentage_points = Decomp) %>% 
    mutate(Target_variable_long_name = recode(Target_variable_long_name, Group_2 = "Participation rate: Advantaged group",
                                              Group_1 = "Participation rate: Disadvantaged group",
                                              Intercept = "Gap in university participation rate"),
           Percent_share = ifelse(New_names %in% c("Group_1","Group_2","Gap in university participation rate"),NA,Percent_share)) %>% 
    rename(Equity_group = Target_variable,
           Contribution_group = New_names,
           Contribution_detailed = Target_variable_long_name)
  return(Table)
  
}
