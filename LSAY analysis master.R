# This code can be used to replicate the findings in the Australian Productivity Commission's research paper titled 
# "The Demand Driven University System: A Mixed Report Card" (June 2019). 
# The report can be found here: https://www.pc.gov.au/research/completed/university-report-card

# These R files were written by Marco Hatt and Max Gillespie of the Productivity Commission. 

##########----------------------------------------------------------------------------------------------------##########
#         Important information
##########----------------------------------------------------------------------------------------------------##########

# Program code requirements:
# 1. Longitudinal Surveys of Austalian Youth (see https://www.lsay.edu.au/data), including confidentialised postcode data 
#    (individuals and schools). This is available by formal request to the Australian Data Archive at the Australian National 
#    University. 

# To replicate the findings of this research, it is recommended that the same versions of the LSAY data are used.
# The versions of each of the LSAY data files used are the following:
# a. LSAY 1995 Cohort Version 3 (Stata)
# b. LSAY 1998 Cohort Version 3.1 (Stata)
# c. LSAY 2003 Cohort Version 7 (Stata)
# d. LSAY 2006 Cohort Version 10 (Stata)
# e. LSAY 2009 Cohort Version 7 (Stata)
# f. LSAY 2015 Cohort Version 2 (Stata)

# 2. The R packages specified in "Packages.R" need to be installed. 

# 3. Other data files sourced mainly from the Australian Bureau of Statistics. As these data are publically available, they are
#    included in the data files provided.

##########----------------------------------------------------------------------------------------------------##########
#         Load packages
##########----------------------------------------------------------------------------------------------------##########

source("Packages.R") # installs (if not already installed) and loads into the library the packages required for this R program

# Session info from the most recent run of this R code in June 2019 

# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252    LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
# [5] LC_TIME=English_Australia.1252    
# 
# attached base packages:
#   [1] parallel  grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] PCcharts_3.0.0        mice_3.5.0            lattice_0.20-35       splitstackshape_1.4.8 readstata13_0.9.2     survey_3.36           survival_2.42-3      
# [8] lmtest_0.9-37         zoo_1.8-6             sandwich_2.5-1        usethis_1.5.0         devtools_2.0.2        lme4_1.1-21           Matrix_1.2-14        
# [15] stringi_1.4.3         lubridate_1.7.4       data.table_1.12.2     ggpubr_0.2.1          magrittr_1.5          stargazer_5.2.2       devEMF_3.6-2         
# [22] geosphere_1.5-10      ggmap_3.0.0.901       readxl_1.3.1          openxlsx_4.1.0.1      forcats_0.4.0         stringr_1.4.0         dplyr_0.8.1          
# [29] purrr_0.3.2           readr_1.3.1           tidyr_0.8.3           tibble_2.1.3          ggplot2_3.2.0         tidyverse_1.2.1     



##########----------------------------------------------------------------------------------------------------##########
#         Load LSAY data
##########----------------------------------------------------------------------------------------------------##########

#Load LSAY data (either in RData or dta (Stata) format. First convert, if necessary to RData format. Note: file names may differ, override if necessary


Load_stata_or_Rdata_function <- function(Name,RData_file,Stata_file){
  
  if (file.exists(paste0(RData_file))) { 
    env <- new.env() 
    load(RData_file, envir=env) 
    loadedObjects <- objects(env, all=TRUE) 
    stopifnot(length(loadedObjects)==1) 
    assign(Name,env[[loadedObjects]],.GlobalEnv)
    rm(env)}  else {
        
        if(!file.exists(paste0(Stata_file)))  { stop("Make sure you have the correct LSAY data") } else {
          assign(paste0(Name), read.dta13(paste0(Stata_file), convert.factors=FALSE),.GlobalEnv)
          save(list=paste0(Name),file=paste0(RData_file)) } 
      } 
}


LSAY_data <- "LSAY_1995"
RData_file <- "1. LSAY 1995 Cohort Version 3 (Stata).RData"
Stata_file <- "Y95_revised_1807.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_data <- "LSAY_1998"
RData_file <- "1. LSAY 1998 Cohort Version 3.1 (Stata).RData"
Stata_file <- "LSAY_1998.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_data <- "LSAY_2003"
RData_file <- "1. LSAY 2003 Cohort Version 7 (Stata).RData"
Stata_file <- "LSAY_2003.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_data <- "LSAY_2006"
RData_file <- "1. LSAY 2006 Cohort Version 10 (Stata).RData"
Stata_file <- "LSAY_2006.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_data <- "LSAY_2009"
RData_file <- "1. LSAY 2009 Cohort Version 7 (Stata).RData"
Stata_file <- "au.edu.anu.ada.ddi.30023v7S2_unrestricted.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_data <- "LSAY_2015"
RData_file <- "LSAY_2015.RData"
Stata_file <- "au.edu.anu.ada.ddi.01392-unrestricted.dta"
Load_stata_or_Rdata_function(Name = LSAY_data, RData_file = RData_file, Stata_file = Stata_file)

rm(LSAY_data,RData_file,Stata_file)

##### 
# Load restricted (postcodes) LSAY data


LSAY_restricted_data <- "Restricted_1995"
RData_file <- "Restricted_1995.RData"
Stata_file <- "lsay_y95_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_restricted_data <- "Restricted_1998"
RData_file <- "Restricted_1998.RData"
Stata_file <- "lsay_y98_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_restricted_data <- "Restricted_2003"
RData_file <- "Restricted_2003.RData"
Stata_file <- "lsay_y03_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_restricted_data <- "Restricted_2006"
RData_file <- "Restricted_2006.RData"
Stata_file <- "lsay_y06_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)

LSAY_restricted_data <- "Restricted_2009"
RData_file <- "Restricted_2009.RData"
Stata_file <- "lsay_y09_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)
Restricted_2009 <- Restricted_2009[order(match(Restricted_2009$STIDSTD, LSAY_2009$STIDSTD)), ] # put dataframe in same order as main dataset

LSAY_restricted_data <- "Restricted_2015"
RData_file <- "Restricted_2015.RData"
Stata_file <- "lsay_y15_cohort.dta"
Load_stata_or_Rdata_function(Name = LSAY_restricted_data, RData_file = RData_file, Stata_file = Stata_file)
Restricted_2015 <- Restricted_2015[order(match(Restricted_2015$STUDENID, LSAY_2015$STUDENID)), ] # put dataframe in same order as main dataset

Restricted_1995_missing <- read_excel("lsay_y95_y98_cohort_missing_postcodes.xlsx", col_names = TRUE, sheet = "Y95 postcodes")
Restricted_1998_missing <- read_excel("lsay_y95_y98_cohort_missing_postcodes.xlsx", col_names = TRUE, sheet = "Y98 postcodes")

rm(Stata_file,RData_file,LSAY_restricted_data)

##########----------------------------------------------------------------------------------------------------##########
#         Functions
##########----------------------------------------------------------------------------------------------------##########


# Age profile of survey participants by cohort and wave
Age_table <- tibble(Age = 14:25,Cohort_1995 = 1995:2006,Cohort_1998 = 1998:2009,
                    Cohort_2003 = c(NA,2003:2013),Cohort_2006 = c(NA,2006:2016),Cohort_2009 = c(NA,2009:2017,NA,NA)) %>% 
  gather(key="Cohort",value="Year",-Age) %>% 
  mutate(Cohort = as.numeric(str_remove(Cohort, "Cohort_"))) %>% 
  na.omit()

# load functions

source("Distance_function.R") # Function to geocode locations
source("svyMVdecomp_nocluster.R") # non-linear decomposition function
source("Summary statistics functions.R") # various functions to manipulate the LSAY data
source("Bootstrap_function.R") # Functions to bootstrap some of the results to esimate confidence intervals

# similar to Excel's vlookup
vlookup <- function(this, df, key, value) {
  m <- match(this, df[[key]])
  df[[value]][m]
} 

# 'not in'
`%ni%` = Negate(`%in%`) 

# calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


##########----------------------------------------------------------------------------------------------------##########
#         Extract LSAY data and derive new variables for analysis
##########----------------------------------------------------------------------------------------------------##########
# This takes around 10 minutes to extract the data on the first run. The data is saved to "LSAY_data.RData" for future runs.
# An 'if statement' reads in the LSAY data rather than running it if it has already been saved from a previous run.

# The raw data is not altered and is referred to as LSAY_1995, LSAY_1998 etc.
# There derived data, used for most of the analysis, is referred to as LSAY_1995_data, LSAY_1998_data etc.

if (file.exists("LSAY_data.RData")) { 
  cat("Derived LSAY variables were stored in a previous run. Loading results from RData file ... \n")
  load(file="LSAY_data.RData") } else {
    cat("Deriving LSAY variables for analysis. This may take 10 minutes ... \n")
    source("LSAY data extract.R") 
    save(LSAY_1995_data,LSAY_1998_data,LSAY_2003_data,LSAY_2006_data,LSAY_2009_data,LSAY_2015_data,file="LSAY_data.RData") 
  } 
  

##########----------------------------------------------------------------------------------------------------##########
#         Attrition weights
##########----------------------------------------------------------------------------------------------------##########
# Calculate consistent attrition weights. 

# It takes around half an hour to create all the weights on the first run. The weights are saved to RData files for future runs.
# An 'if statement' in the Weights.R file reads in weights rather than running them if they have already been saved from previous runs.

source("Weights.R") 

##########----------------------------------------------------------------------------------------------------##########
#         Regression models
##########----------------------------------------------------------------------------------------------------##########

# This estimates the probit regression models. 
# The dependent variable is either university attendance by age 22 years or type of university attended by age 22 years (Group of 8 or non-Group of 8)

# The independent variables are a range of characteristics associated with university attendance. 

# The 'inference model' is used to decompose the reasons for the under-representation of equity-groups at university
# The 'prediction model' is used to determine the 'additional' and 'other students'

# The main difference between the two types of models is that the prediction models includes ATAR scores and distance to a university campus

source("Regression_models.R")


##########----------------------------------------------------------------------------------------------------##########
#         Additional students
##########----------------------------------------------------------------------------------------------------##########

# Each individual who attended university by age 22 years is allocated some probability of being an additional or other student
# The `Additional_students_function()` code is in 'Summary statistics functions.r`


Additional_students_function(cohort = 2006,Bivariate_normal = FALSE, Correlation=Correlation_matrix_bivariate(1))
Additional_students_function(cohort = 2009,Bivariate_normal = FALSE, Correlation=Correlation_matrix_bivariate(1))


##########----------------------------------------------------------------------------------------------------##########
#         Decomposition data. 
##########----------------------------------------------------------------------------------------------------##########

# Calculates the non-linear decompositions of university participation and saves results to RData files. 
# Decompositions are estimated for university attendance by age 22 years and by type of university attended (Group of Eight or other university)
# This may take over 2 hours on the first run.
# An 'if statement' in the R file reads in the decomposition results rather than running them if they have been saved from previous runs.

source("Decomposition_results.R")


########################################################################################################################
##########----------------------------------------------------------------------------------------------------##########
#         Create charts and tables
##########----------------------------------------------------------------------------------------------------##########
########################################################################################################################

# The remainder of this R file re-creates the findings from the Productivity Commission's study
# This includes most of the figures and tables from Chapters 2, 3 and Appendix B. 


##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.2: Additional students have lower school achievement than other students, though distributions overlap considerably
##########----------------------------------------------------------------------------------------------------##########

PISA_density_function <- function(cohort=2009, equity_group="Total", Additional_students = TRUE) {
  
  year <- Age_table %>% filter(Cohort == cohort, Age == 22) %>% pull(Year)
  LSAY_data <- get(paste0("LSAY_",cohort,"_data")) 
  LSAY_source <- get(paste0("LSAY_",cohort))
  Weights_age_15 <- paste0("WT",cohort,"P") #population weights
  Weights_age_22_AS <- paste0("WT",year,"CP") # Probit model weights
  Weights_age_22 <- paste0("WT",year,"C") # Consistent weights
  Weights_age_23 <- paste0("WT",year+1,"CU") # Consistent weights for completion
  
  PISA_density_data <- as_tibble(LSAY_data) %>% bind_cols(.,LSAY_source %>% select(one_of(Weights_age_15))) %>% 
    mutate(Weights_age_22 = get(Weights_age_22)/sum(get(Weights_age_22),na.rm=TRUE) * sum(get(Weights_age_15))) %>%
    mutate(Weights_age_23 = get(Weights_age_23)/sum(get(Weights_age_23),na.rm=TRUE) * sum(get(Weights_age_15))) %>% 
    mutate(Weights_age_22_AS = get(Weights_age_22_AS)/sum(get(Weights_age_22_AS),na.rm=TRUE) * sum(get(Weights_age_15))) %>% 
    select(Uni_attend_age_22,Uni_completion_age_23,Weights_age_22,Weights_age_23,Weights_age_22_AS,PISA,ATAR_score_imputed,ATAR_group_imputed,one_of(equity_group),Additional_student,Other_student) %>% 
    mutate(Additional_student_weight = Weights_age_22_AS * Additional_student,
           Other_student_weight = Weights_age_22_AS *Other_student,
           Uni_attend_age_22 = recode(Uni_attend_age_22, `1` = "Attended", `0` = "Did not attend"),
           Uni_completion_age_23 = recode(Uni_completion_age_23, `Complete and undertaking further studies` = "Complete"),
           ATAR_score_imputed_with_no_ATAR = ifelse(ATAR_group_imputed == "No_ATAR", 0, ATAR_score_imputed)) %>% 
    select(-c(Additional_student,Other_student,ATAR_score_imputed,ATAR_group_imputed)) %>% 
    na.omit() 
  
  if(Additional_students == TRUE) {
    PISA_density_data <- 
      bind_rows(expandRows(PISA_density_data, "Additional_student_weight") %>% mutate(Type = "Additional student") %>% select(-Other_student_weight),
                expandRows(PISA_density_data, "Other_student_weight") %>% mutate(Type = "Other student") %>% select(-Additional_student_weight)) %>% 
      unite(Category, c("Type", paste0(equity_group)), sep = ", ")
  }
  
  if(Additional_students == FALSE) {
    PISA_density_data <- expandRows(PISA_density_data, "Weights_age_22") %>% unite(Category, c("Uni_attend_age_22", paste0(equity_group)), sep = ", ")
  }
  
  if(Additional_students == "Completion") {
    PISA_density_data <- expandRows(PISA_density_data, "Weights_age_23") %>% 
      filter(Uni_attend_age_22 %in% "Attended") %>% 
      filter(Uni_completion_age_23 %in% "Dropped out") %>% 
      unite(Category, c("Uni_completion_age_23", paste0(equity_group)), sep = ", ")
  }
  
  PISA_density_plot <- PISA_density_data %>% 
    ggplot(aes(x = PISA, colour = Category, fill = Category)) +
    geom_density(alpha = 0.3,adjust = 3)  +
    scale_y_continuous(expand = c(0, 0.0)) + 
    xlab("PISA score") +
    geom_vline(xintercept=500, linetype="dashed", color = "black", size=0.5)+
    theme(legend.margin = margin(0,0,0,0), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) + scale_colour_manual(values=palette()) 

  ATAR_density_plot <- PISA_density_data %>% 
    ggplot(aes(x = ATAR_score_imputed_with_no_ATAR, colour = Category, fill = Category)) +
    geom_density(alpha = 0.3,adjust = 1)  +
    scale_y_continuous(expand = c(0, 0.0)) + 
    xlab("ATAR score") +
    theme(legend.margin = margin(0,0,0,0), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) + scale_colour_manual(values=palette()) 
  

  
  
  return(list(PISA_density_plot,ATAR_density_plot))
  
}


Figure_2_2a <- PISA_density_function(cohort=2009, equity_group="Total", Additional_students = TRUE)[[1]]
Figure_2_2b <- PISA_density_function(cohort=2009, equity_group="Total", Additional_students = TRUE)[[2]]
ggarrange(Figure_2_2a,Figure_2_2b, common.legend = TRUE, legend = "bottom")

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_2.emf", pointsize=12, bg = "transparent")
ggarrange(Figure_2_2a,Figure_2_2b)
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure in Box 2.3: Additional student commencements by university network
##########----------------------------------------------------------------------------------------------------##########

Additional_students_table_2 <- function(cohort=2009,equity_group="Institution_network",Drop_others=FALSE) {
  
  year <- Age_table %>% filter(Cohort == cohort, Age == 22) %>% pull(Year)
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  weights <- ifelse(cohort==2006,"WT2013CP",ifelse(cohort==2009,"WT2016CP",NA))
  design = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights])),]), 
                     weights = LSAY_data[which(!is.na(LSAY_data[,weights])),weights])
  
  AS <- as_tibble(svyby(~Additional_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Additional_students =statistic)
  
  AT <- as_tibble(svyby(~Other_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Other_students =statistic)
  
  NT <- as_tibble(svyby(~Never_student, by = ~get(equity_group), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Never_students =statistic)
  
  L <- as_tibble(svyby(~Fewer_student, by = ~get(equity_group), design = design,svytotal,
                       keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students =statistic)
  
  Table <- bind_rows(
    bind_cols(AS,AT %>% select(Other_students)) %>% 
      mutate(Total = rowSums(select_if(., is.numeric)),
             Additional_students_percent = Additional_students/sum(Total) ,
             Other_students_percent = Other_students/sum(Total)) %>% 
      select(`get(equity_group)`,Additional_students_percent,Other_students_percent) %>% 
      rename(Equity_group = `get(equity_group)`) %>% 
      rename_at(vars(contains('_percent')), list(~str_remove(.,'_percent'))) %>% 
      gather(key = "Type", value = "Percent",-Equity_group) %>% 
      mutate(Type = recode(.$Type,Additional_students='Additional students',Other_students ='Other students'),
             Percent = Percent * 100,
             Chart = "Chart 1"),
    bind_cols(AS,AT %>% select(Other_students)) %>% 
      mutate(Total = rowSums(select_if(., is.numeric)),
             Additional_students_percent = Additional_students/Total ,
             Other_students_percent = Other_students/Total ) %>% 
      select(`get(equity_group)`,Additional_students_percent,Other_students_percent) %>% 
      rename(Equity_group = `get(equity_group)`) %>% 
      rename_at(vars(contains('_percent')), list(~str_remove(.,'_percent'))) %>% 
      gather(key = "Type", value = "Percent",-Equity_group) %>% 
      mutate(Type = recode(.$Type,Additional_students='Additional students',Other_students='Other students'),
             Percent = Percent * 100,
             Chart = "Chart 2"),
    bind_rows(as_tibble(svyby(~Additional_student, by = ~get(equity_group), design = design,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
                mutate(Type = "Additional students"),
              as_tibble(svyby(~Other_student, by = ~get(equity_group), design = design,svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
                mutate(Type = "Other students"))  %>%
      mutate(Percent = statistic * 100) %>% select(-statistic) %>% 
      arrange(Percent) %>% 
      mutate(Chart = "Chart 3") %>%  
      rename(Equity_group = `get(equity_group)`)) %>% 
    mutate(Equity_group = recode(.$Equity_group,`Group of Eight`='Go8',`Australian Technology Network`='ATN',
                                 `Innovative Research Universities`='IRU',`Regional Universities Network`='RUN',`Other universities` ='Other')) %>% 
    group_by(Chart,Type) %>% 
    arrange(Percent,.by_group=TRUE)
  
  Table_order <- Table %>% ungroup() %>% filter(Chart == "Chart 2") %>% pull(Equity_group) %>% unique()
  Table <- Table %>% 
    mutate(Equity_group=factor(Equity_group,levels=Table_order))
  
  if(Drop_others == TRUE) { Table <- Table %>% filter(Equity_group %ni% "Others") }
  
  Chart1 <- Table %>% filter(Chart == "Chart 1") %>% # Per cent of all students 
    ggplot(aes(x=Equity_group,y=Percent, fill=Type)) +
    geom_bar(stat="identity", position="stack") +
    scale_y_continuous(name="Per cent", expand = c(0, 0.0),limits=c(0, 50)) +
    theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
  
  Chart2 <- Table %>% filter(Type == "Additional students", Chart == "Chart 2") %>% # Per cent of students in that group
    ggplot(aes(x=Equity_group,y=Percent, fill=Type)) +
    geom_bar(stat="identity", position="dodge") + 
    scale_y_continuous(name="Per cent", expand = c(0, 0.0),limits=c(0, 15)) +
    theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
  
  Chart3 <- Table %>% filter(Chart == "Chart 3") %>% # Participation rate (like figure 4.1)
    ggplot(aes(x=Equity_group,y=Percent, fill=Type)) +
    geom_bar(stat="identity", position="stack") + 
    scale_y_continuous(name="Per cent", expand = c(0, 0.0),limits=c(0, 100))+
    theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 

  
  return(list(Chart1,Chart2,Chart3,Table))
}

Institution_network_results <- Additional_students_table_2(cohort=2009,equity_group="Institution_network")
Institution_network_chart1 <- Institution_network_results[[1]]
Institution_network_chart2 <- Institution_network_results[[2]]

Figure_box_2_3_data <- Institution_network_results[[4]] %>% filter(Chart %in% c("Chart 1", "Chart 2"))

ggarrange(Institution_network_chart1 + theme(plot.title = element_text(face="bold",size = 10,hjust=0.5)) + ggtitle("Per cent of all students"),
          Institution_network_chart2 + theme(plot.title = element_text(face="bold",size = 10,hjust=0.5)) + ggtitle("Per cent of university network"),
          common.legend = TRUE,legend = "bottom")

emf(width = 15/2.54, height = 8/2.54, file = "Figure_box_2_3.emf", pointsize=12, bg = "transparent")
ggarrange(Institution_network_chart1 + theme(plot.title = element_text(face="bold",size = 10,hjust=0.5)) + ggtitle("Per cent of all students"),
          Institution_network_chart2 + theme(plot.title = element_text(face="bold",size = 10,hjust=0.5)) + ggtitle("Per cent of university network"),
          common.legend = TRUE,legend = "bottom")
dev.off()

##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.4 Additional students are less likely to complete university and undertake further study
##########----------------------------------------------------------------------------------------------------##########

Figure_2_4_results <- bootstrap_tables_and_charts(Outcome_variables = c("Completed", "Undertaking", "Dropout", "Postgrad"),
                                                  Cohort = 2009, Age = 23, Graduates = FALSE)

Figure_2_4_data <- Figure_2_4_results %>% select(Outcome,University_attendance,Mean,CI_top,CI_bottom) %>% 
  filter(University_attendance %ni% c("Non-attenders","Students who dropped out")) 

Figure_2_4 <- Figure_2_4_data %>% 
  ggplot(aes(x=Outcome,y=Mean, fill= University_attendance)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=CI_bottom, ymax=CI_top),size=1,colour="black",position = position_dodge2(width = 0.2,padding=0.6)) + 
  ylab("Per cent") + scale_y_continuous(expand = c(0, 0.0)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
Figure_2_4

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_4.emf", pointsize=12, bg = "transparent")
Figure_2_4
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.6 
##########----------------------------------------------------------------------------------------------------##########

Figure_2_6_results <- bootstrap_tables_and_charts(Outcome_variables = c("Employed_FT", "Employed_PT", "Unemployed", "NILF"),
                                                  Cohort = 2009, Age = 23, Graduates = TRUE)

Figure_2_6_data <- Figure_2_6_results %>% select(Outcome,University_attendance,Mean,CI_top,CI_bottom) 

Figure_2_6 <- Figure_2_6_data %>% 
  ggplot(aes(x=Outcome,y=Mean, fill= University_attendance)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=CI_bottom, ymax=CI_top),size=1,colour="black",position = position_dodge2(width = 0.2,padding=0.6)) + 
  ylab("Per cent") + 
  scale_y_continuous(expand = c(0, 0.0)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
Figure_2_6

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_6.emf", pointsize=12, bg = "transparent")
Figure_2_6
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.7: Additional students earn somewhat less than other graduates
##########----------------------------------------------------------------------------------------------------##########

Figure_2_7_results <- bootstrap_tables_and_charts(Outcome_variables = c("Average_weekly_pay", "Average_hourly_pay", "Average_hours_worked"),
                                                  Cohort = 2009, Age = 23, Graduates = TRUE)

Figure_2_7_data <- Figure_2_7_results %>% select(Outcome,University_attendance,Mean,CI_top,CI_bottom) 

Figures <- tibble(Figure = c("Average weekly pay","Average hourly pay","Average hours worked"), ylab = c("Dollars", "$ per hour", "Weekly hours"))
#vlookup(Figure, Figures, "Figure", "ylab")
for (Figure in Figures$Figure) { 
  assign(paste0("Figure_",str_replace_all(Figure, " ", "_")), 
         Figure_2_7_data %>% filter(Outcome == Figure) %>% 
           ggplot(aes(x=University_attendance,y=Mean, fill= University_attendance)) +
           geom_bar(stat="identity", position="dodge") + 
           geom_errorbar(aes(ymin=CI_bottom, ymax=CI_top),size=1,colour="black",position = position_dodge2(width = 0.2,padding=0.6)) + 
           ylab(vlookup(Figure, Figures, "Figure", "ylab")) + 
           ggtitle(Figure) +
           scale_y_continuous(expand = c(0, 0.0)) +  
           theme(plot.title = element_text(face="bold",size = 10,hjust=0.5), 
                 legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), axis.text.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) +
           scale_fill_manual(values=palette()) 
  )
}

Figure_2_7 <- ggarrange(Figure_Average_weekly_pay,Figure_Average_hourly_pay,Figure_Average_hours_worked, common.legend = TRUE, legend="bottom", nrow = 1)   
Figure_2_7

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_7.emf", pointsize=12, bg = "transparent")
Figure_2_7
dev.off()

##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.8 Additional students are somewhat less likely to gain employment in a professional occupation than other graduates
##########----------------------------------------------------------------------------------------------------##########

Figure_2_8_results <- bootstrap_tables_and_charts(Outcome_variables = c("Community and personal serv","labourer","Machine opperator","Manager",
                                                                        "Not working (unemp or NILF)","Professionals","Sales","Tech and Trade"),
                                                  Cohort = 2009, Age = 23, Graduates = TRUE)

Figure_2_8_data <- Figure_2_8_results %>% select(Outcome,University_attendance,Mean,CI_top,CI_bottom) 

Figure_2_8 <- Figure_2_8_data %>% 
  ggplot(aes(x=Outcome,y=Mean, fill= University_attendance)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=CI_bottom, ymax=CI_top),size=1,colour="black",position = position_dodge2(width = 0.2,padding=0.6)) + 
  ylab("Per cent") + 
  scale_y_continuous(expand = c(0, 0.0)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
Figure_2_8

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_8.emf", pointsize=12, bg = "transparent")
Figure_2_8
dev.off()

##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.9 : Additional students are somewhat less satisfied with their career path than other graduates
##########----------------------------------------------------------------------------------------------------##########

Figure_2_9_results <- bootstrap_tables_and_charts(Outcome_variables = c("Job_does_not_utilise_skills", "Not_satisfied_with_work", 
                                                                        "Not_satisfied_with_career_path"),
                                                  Cohort = 2009, Age = 23, Graduates = TRUE)

Figure_2_9_data <- Figure_2_9_results %>% select(Outcome,University_attendance,Mean,CI_top,CI_bottom) 

Figure_2_9 <- Figure_2_9_data %>% 
  ggplot(aes(x=Outcome,y=Mean, fill= University_attendance)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=CI_bottom, ymax=CI_top),size=1,colour="black",position = position_dodge2(width = 0.2,padding=0.6)) + 
  ylab("Per cent") + 
  scale_y_continuous(expand = c(0, 0.0)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
Figure_2_9

emf(width = 15/2.54, height = 8/2.54, file = "Figure_2_9.emf", pointsize=12, bg = "transparent")
Figure_2_9
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure 2.10: Students with low school achievement or those who study part time have poorer outcomes
##########----------------------------------------------------------------------------------------------------##########


Equity_groups <- c("Ability","ATAR_group_imputed_3","Part_time_study_ever_23")
Cohorts <- c(1995,1998,2003,2006,2009)
Tables <- c(5,6)
Figure_2_10_age_23 <- lapply(Tables, function(y) { 
  bind_rows(lapply(Cohorts, function(z) {  
    bind_rows(lapply(Equity_groups, function(x) {
      Summary_statistics_function(cohort=z,equity_group=x,age=22,combine_undertaking=FALSE) } [[y]]  )) }) )})

Figure_2_10_age_23_dropouts <- Figure_2_10_age_23[[1]] %>% #select(-Uni_complete,-Uni_complete_postgrad_undertaking) %>% 
  group_by(Cohort,Year,Category,Equity_group,Uni_attend,Uni_complete) %>% 
  summarise(Percent=sum(Count)) %>% 
  mutate_at(vars("Percent"), list(~./ sum(.) * 100)) %>% 
  filter(Uni_attend %ni% "Did not attend",Uni_complete %in% "Dropped out") %>% ungroup() %>% 
  select(-Uni_attend,-Uni_complete) %>% mutate(Chart = "Drop-out rates (all university students)")

Figure_2_10_age_23_MP <- Figure_2_10_age_23[[2]] %>% #select(-Uni_complete_postgrad_complete,-Uni_complete_postgrad_undertaking) %>% 
  group_by(Cohort,Year,Category,Equity_group,Uni_attend,Uni_complete,Outcome) %>% 
  summarise(Percent=sum(Count)) %>% 
  mutate_at(vars("Percent"), list(~./ sum(.) * 100)) %>% 
  filter(Uni_attend %ni% "Did not attend",Uni_complete %in% "Complete",Outcome %in% "Manager or professional") %>% ungroup() %>% 
  select(-Uni_attend,-Uni_complete,-Outcome) %>% mutate(Chart = "Managerial or professional work (graduates)") 

Figure_2_10_data <- bind_rows(Figure_2_10_age_23_dropouts,Figure_2_10_age_23_MP) %>%
  mutate(Category = recode(Category, ATAR_group_imputed_3 = "ATAR", Part_time_study_ever_23 = "Study status"))

Figures <- c("Ability", "ATAR", "Study status")
for (Figure in Figures) { 
  assign(paste0("Figure_",str_replace_all(Figure, " ", "_")), 
         Figure_2_10_data %>% filter(Category == Figure) %>% 
           ggplot(aes(x=as.numeric(Year),y=Percent)) +
           geom_line(aes(colour=Equity_group)) +
           facet_wrap(~Chart, nrow = 1, scales = "free_y") +
           scale_x_continuous(breaks = c(2004,2006,2011,2014,2017)) +
           scale_y_continuous(name="Per cent", expand = c(0, 0.0)) + expand_limits(y = 0) +
           ggtitle(Figure) +
           theme(plot.title = element_text(face="bold",size = 10,hjust=0.5),
                 legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
  )
}

Figure_2_10 <- ggarrange(Figure_Ability,Figure_ATAR,Figure_Study_status, common.legend = FALSE, nrow = 3)   
Figure_2_10

Figure_2_10_data <- Figure_2_10_data %>% select(Chart, Category, Equity_group, Year, Percent) %>% spread(key = Year, value = Percent) %>% 
  arrange(Chart, Category, Equity_group)

emf(width = 15/2.54, height = 12/2.54, file = "Figure_2_10.emf", pointsize=12, bg = "transparent")
Figure_2_10
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure 3.2: University participation rates by equity group
##########----------------------------------------------------------------------------------------------------##########

Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group", "Total")
Cohorts <- c(2003,2006,2009)
Figure_3_2_data <- bind_rows(lapply(Cohorts, function(z) {  
  bind_rows(lapply(Equity_groups, function(x) {
    Participation_rates_function(cohort=z,equity_group=x,age=22) }  )) }) )

Equity_groups <- c("Indigenous", "Low SES", "Regional or remote","First in family", "No equity group", "Total")
Figure_3_2_data <- Figure_3_2_data %>% ungroup() %>% 
  filter(Uni_attend == "Attended",
         Equity_group %in% Equity_groups) %>% 
  select(-c(Cohort,Age,Uni_attend,Category,Count,Population,Composition)) %>% 
  mutate(Equity_group = factor(Equity_group, levels = Equity_groups))


Bar_Equity <- Figure_3_2_data %>% 
  ggplot(aes(x=Equity_group, y = Participation_rate, fill = Year)) + 
  geom_bar(stat="identity", position="dodge",width = 0.6)  + 
  ylab("Per cent")  + 
  scale_y_continuous(expand = c(0, 0.0), limits=c(0,100)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette()) 
Bar_Equity

Figure_3_2_data <- Figure_3_2_data %>% spread(key = Year, value = Participation_rate)

emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_2.emf", pointsize=12, bg = "transparent")
Bar_Equity
dev.off()

##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.4: Decomposition of differences in university attendance by equity group
##########----------------------------------------------------------------------------------------------------##########

MV_data_wf_chart_all <- bind_rows(Decomp_table2(Cohort=2003,Data=MV_data_2003),
                                  Decomp_table2(Cohort=2006,Data=MV_data_2006), 
                                  Decomp_table2(Cohort=2009,Data=MV_data_2009)) %>% 
  mutate(New_names = factor(New_names,levels=c("Group_1","PISA score","Gender","Indigenous",
                                               "Hours worked in year 12","Family characteristics","School characteristics",
                                               "Neighbourhood characteristics","Geographic characteristics","Other","Group effect","Group_2"))) %>% 
  mutate(New_names = fct_rev(New_names)) %>% droplevels() 

MV_data_wf_chart_part_rate <- MV_data_wf_chart_all %>% filter(New_names =="Group_2") %>% 
  select(Target_variable,Cohort,Stack) %>% rename(Part_rate=Stack)
MV_data_wf_chart_all <- left_join(MV_data_wf_chart_all,MV_data_wf_chart_part_rate,by=c("Cohort","Target_variable")) %>% 
  mutate(Target_variable_long_name = vlookup(Target_variable,Variable_names_decomp,"Target_variable_name","Target_variable_long_name"))

MV_data_wf_chart_all$Chart_title <- vlookup(MV_data_wf_chart_all$Target_variable, Variable_names_decomp, 
                                            "Target_variable_name", "Target_variable_chart_title")

palette_decomp <- palette()
names(palette_decomp) <- levels(fct_rev(MV_data_wf_chart_all$New_names))[levels(fct_rev(MV_data_wf_chart_all$New_names)) %ni% "Group_2"]

MV_data_wf_chart_all$Year <- recode(MV_data_wf_chart_all$Cohort,`2003`=2010,`2006`=2013,`2009`=2016)

Decomp_charts <- lapply(unique(MV_data_wf_chart_all$Target_variable),
                        function (x) {Legend_names <- MV_data_wf_chart_all %>% 
                          filter(Target_variable==x, New_names !="Group_2") %>% 
                          mutate(Legend_names = sapply(New_names,as.character)) %>%
                          mutate(Legend_names = replace(Legend_names, Legend_names=="Group_1", "Equity group participation rate")) %>% 
                          select(Legend_names) %>% unique()
                        
                        Group_2_name <- "Rest of the population participation rate"
                        
                        MV_data_wf_chart_all %>% 
                          filter(Target_variable==x, New_names !="Group_2",Cohort>1998) %>% 
                          
                          ggplot(aes(x=as.factor(Year))) + 
                          geom_bar(aes(y=Stack, fill = New_names, group=New_names),stat="identity",position="stack",width = 0.6) +
                          geom_point(aes(y=Part_rate, shape=Group_2_name),
                                     color = "black", size = 2, stat="identity", position = "identity") +
                          ggtitle(vlookup(x, Variable_names_decomp,"Target_variable_name", "Target_variable_chart_title")) +
                          scale_fill_manual(name="New_names", labels=Legend_names$Legend_names,values=palette_decomp,guide = "legend") +
                          scale_colour_manual(values=palette_decomp[1],guide = "legend") +
                          scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(-5,100),breaks = seq(0, 100, by = 20)) +
                          theme(plot.title = element_text(face="bold",size = 10,hjust=0.5),
                                legend.box = "vertical",legend.spacing.x = unit(0.1, 'cm'),
                                legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(0,0,0,0)) +
                          guides(fill=guide_legend(nrow=3)) 
                        })



names(Decomp_charts) <- unique(MV_data_wf_chart_all$Target_variable)

Figure_3_4 <- ggarrange(Decomp_charts[["Low_SES"]] +
                          theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank(),
                                plot.margin = margin(10,0,10,0)),
                        Decomp_charts[["Parent_edu"]] + 
                          theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank(),
                                axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank(),
                                plot.margin = margin(10,0,10,35)),
                        Decomp_charts[["Location_RR"]] + theme(plot.margin = margin(0,0,10,0)),
                        Decomp_charts[["Indigenous"]] + 
                          theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank(),
                                plot.margin = margin(0,0,10,35)),
                        common.legend = TRUE, legend = "bottom")

Figure_3_4

emf(width = 15/2.54, height = 18/2.54, file = "Figure_3_4.emf", pointsize=12, bg = "transparent")
Figure_3_4
dev.off()

Figure_3_4_data <- MV_data_wf_chart_all %>% select(Year,Chart_title = Target_variable,New_names,Decomp) %>% 
  spread(key = Year, value = Decomp)


##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.5: PISA scores by equity group
##########----------------------------------------------------------------------------------------------------##########

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003$WT2003P)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003P)),"WT2003P"])

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2006P)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2006P)),"WT2006P"])

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009$WT2009P)),]), 
                              weights = LSAY_2009[which(!is.na(LSAY_2009$WT2009P)),"WT2009P"])

LSAY_2015_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2015_data[which(!is.na(LSAY_2015$WT2015P)),]), 
                              weights = LSAY_2015[which(!is.na(LSAY_2015$WT2015P)),"WT2015P"])

PISA_table_function <- function(PISA_type="PISA",Equity_group="Low_SES"){
  PISA_equity_group <- bind_rows(as_tibble(svyby(~get(PISA_type), by = as.formula(paste0("~",Equity_group)), design = LSAY_2003_data.w,svymean,
                                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(PISA = statistic) %>% mutate(Cohort = "2003"),
                                 as_tibble(svyby(~get(PISA_type), by = as.formula(paste0("~",Equity_group)), design = LSAY_2006_data.w,svymean,
                                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(PISA = statistic) %>% mutate(Cohort = "2006"),
                                 as_tibble(svyby(~get(PISA_type), by = as.formula(paste0("~",Equity_group)), design = LSAY_2009_data.w,svymean,
                                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(PISA = statistic) %>% mutate(Cohort = "2009"),
                                 as_tibble(svyby(~get(PISA_type), by = as.formula(paste0("~",Equity_group)), design = LSAY_2015_data.w,svymean,
                                                 keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(PISA = statistic) %>% mutate(Cohort = "2015")) %>% 
    mutate(Cohort=as.factor(Cohort)) %>% 
    rename(Group=Equity_group) %>% 
    mutate(Group = as.character(Group))
  return(PISA_equity_group)
}

PISA_Low_SES <- PISA_table_function("PISA","Low_SES")
PISA_Indigenous <- PISA_table_function("PISA","Indigenous")
PISA_Location_RR <- PISA_table_function("PISA","Location_RR_wave1")
PISA_Parent_edu <- PISA_table_function("PISA","Parent_edu")
PISA_Equity_group <- PISA_table_function("PISA","Equity_group")

PISA_COB <- PISA_table_function("PISA","COB") 

PISA_tot <- bind_rows(
  as_tibble(svymean(~PISA, design = LSAY_2003_data.w, na.rm=TRUE)) %>% mutate(Cohort = "2003") %>% select(PISA = mean,Cohort),
  as_tibble(svymean(~PISA, design = LSAY_2006_data.w, na.rm=TRUE)) %>% mutate(Cohort = "2006") %>% select(PISA = mean,Cohort),
  as_tibble(svymean(~PISA, design = LSAY_2009_data.w, na.rm=TRUE)) %>% mutate(Cohort = "2009") %>% select(PISA = mean,Cohort),
  as_tibble(svymean(~PISA, design = LSAY_2015_data.w, na.rm=TRUE)) %>% mutate(Cohort = "2015") %>% select(PISA = mean,Cohort)) %>% 
  mutate(Cohort=as.factor(Cohort)) %>% 
  mutate(Group = "Total")

PISA_table <- bind_rows(PISA_tot,PISA_Location_RR,PISA_Indigenous,PISA_Parent_edu,PISA_Low_SES,PISA_Equity_group) %>% 
  filter(Group %in% c("Indigenous","Low_SES","No_uni","Regional_remote","Not_disadvantaged","Total")) %>% 
  mutate(Group=replace(Group, Group=="Regional_remote", "Regional or remote"),
         Group=replace(Group, Group=="Low_SES", "Low socioeconomic status"),
         Group=replace(Group, Group=="No_uni", "First in family"),
         Group=replace(Group, Group=="Not_disadvantaged", "No equity group")) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Indigenous","Low socioeconomic status", "First in family","Regional or remote","No equity group","Total")))


PISA_table %>% spread(key = Cohort,value = PISA)
write_csv(PISA_table %>% spread(key = Cohort,value = PISA),"pisa.csv")
Figure_3_5_data <- PISA_table 

PISA_equity_plot <- PISA_table %>% 
  ggplot(aes(x=Group, y = PISA, fill = Cohort)) + 
  geom_bar(stat="identity", position="dodge",width = 0.6) + 
  ylab("Score") + coord_cartesian(ylim=c(400,600)) + 
  geom_hline(yintercept=500, linetype="dashed", color = "black", size=0.5) +
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette())
PISA_equity_plot

emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_5.emf", pointsize=12, bg = "transparent")
PISA_equity_plot
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.6: University attendance by equity group and school achievement
##########----------------------------------------------------------------------------------------------------##########


Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group","Total")
Cohorts <- c(2003,2009)
Tables <- c(9)
PISA_equity_data <- bind_rows(lapply(Tables, function(y) { 
  bind_rows(lapply(Cohorts, function(z) {  
    bind_rows(lapply(Equity_groups, function(x) {
      Summary_statistics_function(cohort=z,equity_group=x,age=22,combine_undertaking=FALSE) } [[y]]  )) }) )}) )

Figure_3_6_data <- PISA_equity_data %>% 
  filter(Equity_group %in% c("Regional or remote","Low SES","Indigenous","First in family","No equity group","Total")) %>% 
  mutate(PISA_quartile = recode_factor(PISA_quartile,
                                       upper="High", `mid-upper`="Middle-high", 
                                       `mid-lower` = "Middle-low", lower = "Low"),
         Equity_group = factor(Equity_group, levels = c("Regional or remote", "Low SES", "First in family","Indigenous",
                                                        "No equity group", "Total"))) 

Figure_3_6_plot <- Figure_3_6_data %>% 
  ggplot() + 
  geom_bar(aes(x=Year, y = Attendance_rate, fill = Equity_group, group=Equity_group),stat="identity", position="dodge",width = 0.6) +
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 100)) +
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10))+
  facet_wrap(~PISA_quartile,ncol = 4) + scale_fill_manual(values=palette()) + scale_colour_manual(values=rep("black",6), labels=NULL)
Figure_3_6_plot

emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_6.emf", pointsize=12, bg = "transparent")
Figure_3_6_plot
dev.off()

##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.7: University attendance by distance and school achievement
##########----------------------------------------------------------------------------------------------------##########


Equity_groups <- c("Closest_uni_distance_over_40km")
Cohorts <- c(2003,2006,2009)
Tables <- c(1,9)
Figure_3_7_data <- bind_rows(lapply(Tables, function(y) { 
  bind_rows(lapply(Cohorts, function(z) {  
    bind_rows(lapply(Equity_groups, function(x) {
      Summary_statistics_function(cohort=z,equity_group=x,age=22,combine_undertaking=FALSE) } [[y]]  )) }) )}) )

Figure_3_7_data <- Figure_3_7_data %>% ungroup() %>% filter(Uni_attend %ni% "Did not attend") %>% 
  select(Equity_group,PISA_quartile,Attendance_rate,Cohort,Year) %>% 
  mutate(PISA_quartile = ifelse(is.na(PISA_quartile),"Total population", as.character(PISA_quartile)),
         PISA_quartile = replace(PISA_quartile, PISA_quartile =="upper", "High achievers"),
         Equity_group = str_replace(Equity_group, "_", " "),
         Equity_group = factor(Equity_group, levels = c("Under 40km","Over 40km"))) %>% 
  filter(PISA_quartile %in% c("Total population", "High achievers"))

Figure_3_7_plot <- Figure_3_7_data %>% 
  ggplot(aes(x=Year, y = Attendance_rate, fill = Equity_group)) + 
  geom_bar(stat="identity", position="dodge",width = 0.6) + 
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 100)) + 
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) +
  facet_wrap(~PISA_quartile) + scale_fill_manual(values=palette())
Figure_3_7_plot

Figure_3_7_data <- Figure_3_7_data %>% 
  select(PISA_quartile,Equity_group,Attendance_rate,Year) %>% 
  spread(key = Year, value = Attendance_rate)

emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_7.emf", pointsize=12, bg = "transparent")
Figure_3_7_plot
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.8: Decomposition of differences in university participation rates by equity group and type of university
##########----------------------------------------------------------------------------------------------------##########


MV_data_Go8_2009 <- Decomp_table(Cohort="2009",Model=Inference_model_2009_Go8,Groupings=Groupings,Age="Go8")
MV_data_non_Go8_2009 <- Decomp_table(Cohort="2009",Model=Inference_model_2009_Go8,Groupings=Groupings,Age="non_Go8")

MV_data_wf_chart_Go8_2009 <- Decomp_table2(Cohort=2009,Data=MV_data_Go8_2009) %>% mutate(Age = "Go8")
MV_data_wf_chart_non_go8_2009 <- Decomp_table2(Cohort=2009,Data=MV_data_non_Go8_2009)  %>% mutate(Age = "non_Go8")
MV_data_wf_chart_2009 <- Decomp_table2(Cohort=2009,Data=MV_data_2009)  %>% mutate(Age = "All unis")
MV_data_wf_chart_all_Go8 <- bind_rows(MV_data_wf_chart_non_go8_2009,MV_data_wf_chart_Go8_2009,MV_data_wf_chart_2009) %>% mutate(Age = as.factor(Age)) %>% 
  mutate(New_names = factor(New_names,levels=c("Group_1","PISA score","Gender","Indigenous",
                                               "Hours worked in year 12","Family characteristics","School characteristics",
                                               "Neighbourhood characteristics","Geographic characteristics","Other","Group effect","Group_2"))) %>% 
  mutate(New_names = fct_rev(New_names)) %>% droplevels() 

MV_data_wf_chart_part_rate_age <- MV_data_wf_chart_all_Go8 %>% filter(New_names =="Group_2") %>% 
  select(Target_variable,Cohort,Age,Stack) %>% rename(Part_rate=Stack)
MV_data_wf_chart_all_Go8 <- left_join(MV_data_wf_chart_all_Go8,MV_data_wf_chart_part_rate_age,by=c("Cohort","Target_variable","Age")) %>% 
  mutate(Age=sapply(Age,as.character)) %>% 
  mutate(University=recode_factor(Age,`All unis`='All universities',`non_Go8`='Non-Go8 universities',Go8 ='Go8 universities')) %>% 
  mutate(Target_variable_long_name = vlookup(Target_variable,Variable_names_decomp,"Target_variable_name","Target_variable_long_name"))

palette_decomp <- palette()
names(palette_decomp) <- levels(fct_rev(MV_data_wf_chart_all_Go8$New_names))[levels(fct_rev(MV_data_wf_chart_all_Go8$New_names)) %ni% "Group_2"]


Decomp_charts_Go8 <- lapply(unique(MV_data_wf_chart_all_Go8$Target_variable),
                            function (x) {Legend_names <- MV_data_wf_chart_all_Go8 %>% 
                              filter(Target_variable==x, New_names !="Group_2") %>% 
                              mutate(Legend_names = sapply(New_names,as.character)) %>%
                              mutate(Legend_names = replace(Legend_names, Legend_names=="Group_1", "Equity group participation rate")) %>% 
                              select(Legend_names) %>% unique()
                            
                            Group_2_name <- "Rest of the population participation rate"
                            
                            MV_data_wf_chart_all_Go8 %>% 
                              filter(Target_variable==x, New_names !="Group_2") %>% 
                              
                              ggplot(aes(x=as.factor(University))) + 
                              geom_bar(aes(y=Stack, fill = New_names, group=New_names),stat="identity",position="stack",width = 0.6) +
                              geom_point(aes(y=Part_rate, shape=Group_2_name),
                                         color = "black", size = 2, stat="identity", position = "identity") +
                              ggtitle(vlookup(x, Variable_names_decomp,"Target_variable_name", "Target_variable_chart_title")) +
                              scale_fill_manual(name="New_names", labels=Legend_names$Legend_names,values=palette_decomp,guide = "legend") +
                              scale_colour_manual(values=palette_decomp[1],guide = "legend") + 
                              scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(-5,100),breaks = seq(0, 100, by = 20)) +
                              theme(plot.title = element_text(face="bold",size = 10,hjust=0.5),
                                    legend.box = "vertical",legend.spacing.x = unit(0.1, 'cm'),
                                    legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(0,0,0,0)) +
                              guides(fill=guide_legend(nrow=3)) 
                            })

names(Decomp_charts_Go8) <- unique(MV_data_wf_chart_all_Go8$Target_variable)

Figure_3_8 <- ggarrange(Decomp_charts_Go8[["Low_SES"]] +
                          theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank(),
                                plot.margin = margin(10,0,10,0)),
                        Decomp_charts_Go8[["Parent_edu"]] + 
                          theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank(),
                                axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.line.x = element_blank(),
                                plot.margin = margin(10,0,10,35)),
                        Decomp_charts_Go8[["Location_RR"]] + theme(plot.margin = margin(0,0,10,0)),
                        Decomp_charts_Go8[["Indigenous"]] + 
                          theme(axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.line.y = element_blank(),
                                plot.margin = margin(0,0,10,35)),
                        common.legend = TRUE, legend = "bottom")

Figure_3_8

emf(width = 15/2.54, height = 19/2.54, file = "Figure_3_8.emf", pointsize=12, bg = "transparent")
Figure_3_8
dev.off()


Figure_3_8_data <- MV_data_wf_chart_all_Go8 %>% select(Chart_title = Target_variable,University,New_names,Decomp) %>% 
  spread(key = University, value = Decomp)


##########----------------------------------------------------------------------------------------------------##########
#         Figure in Box 3.2: Movements and costs faced by first year university students
##########----------------------------------------------------------------------------------------------------##########

Movement_function <- function(cohort=2003) {
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  year <- Age_table %>% filter(Cohort == cohort, Age == 22) %>% pull(Year)
  LSAY_data <- LSAY_data %>% rename(
    WT_Distance = paste0("WT",year,"_Distance"),
    WT_Movement = paste0("WT",year,"_Movement"))
  
  LSAY_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data$WT_Distance)),]), 
                           weights = LSAY_data[which(!is.na(LSAY_data$WT_Distance)),"WT_Distance"])
  
  
  LSAY_data_movement.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data$WT_Movement)),]), 
                                    weights = LSAY_data[which(!is.na(LSAY_data$WT_Movement)),"WT_Movement"])
  
  Distance <- as_tibble(svyby(~Distance_moved_to_uni, by = ~Location_RR, design = LSAY_data.w,svymean,
                              keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
    rename_at(vars(starts_with('statistic')), list(~paste0("Cohort_",cohort)))   # rename the column e.g. Cohort_2003
  
  Moved <- as_tibble(svyby(~Moved_to_uni, by = ~Location_RR, design = LSAY_data.w,svytotal,
                           keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
    rename_at(vars(starts_with('statistic')), list(~paste0("Cohort_",cohort)))
  
  Moved_not <- as_tibble(svyby(~Not_moved_to_uni, by = ~Location_RR, design = LSAY_data.w,svytotal,
                               keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% 
    rename_at(vars(starts_with('statistic')), list(~paste0("Cohort_",cohort,"_n")))
  
  Movement <- bind_cols(Moved,Moved_not[,2]) %>% 
    gather(key="Cohort",value="Count",-Location_RR) %>% 
    mutate(Moved = ifelse(endsWith(Cohort,"_n"),"Same postcode","Different postcode")) %>% 
    mutate(Cohort = gsub("_n", "", Cohort)) %>% 
    mutate(Cohort = as.character(gsub("Cohort_", "", Cohort))) %>% 
    mutate(Location_RR = as.factor(gsub("Regional_remote", "Regional or remote", Location_RR)))
  
  Movement <- left_join(Movement,Movement %>% group_by(Location_RR,Cohort) %>% summarise(Total = sum(Count)),by=c("Location_RR","Cohort")) %>% 
    mutate(Percent = Count/Total * 100) %>% 
    mutate(Year = as.character(year))
  
  Housing <- as_tibble(svyby(~Housing_cost_uni, by = ~Movement_detailed_4, design = LSAY_data_movement.w, svyquantile, quantiles=0.5, 
                             keep.var = FALSE, na.rm=TRUE)) %>% rename(Housing_cost = statistic) %>% 
    mutate(Cohort = as.character(cohort), Year = as.character(year),
           Movement_detailed_4=replace(Movement_detailed_4, Movement_detailed_4 == "Metro_moved", "Moved (metropolitan)"))
  
  return(list(Movement,Housing))
  
}

Movement <- bind_rows(Movement_function(2003)[[1]],Movement_function(2006)[[1]],Movement_function(2009)[[1]]) # will take a few minutes
Housing <- bind_rows(Movement_function(2003)[[2]],Movement_function(2006)[[2]],Movement_function(2009)[[2]]) 


Figure_box_3_2_a_data <- Movement %>% filter(Moved=="Different postcode") %>% 
  select(Cohort,Year,Location_RR,Moved,Total,Percent)

Movement_plot <-  Figure_box_3_2_a_data %>% 
  ggplot(aes(x=Location_RR, y = Percent, fill = Year)) + 
  geom_bar(stat="identity", position="dodge",width = 0.6) +
  ggtitle("Relocation rate") +
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 30)) +
  theme(plot.title = element_text(face="bold",size = 10,hjust=0.5), ##### New Phil bit
        legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette())


Figure_box_3_2_b_data <- Housing %>% filter(Movement_detailed_4!= "Regional_moved") %>% 
  rename(Movement = Movement_detailed_4) %>% select(Cohort,Year,Movement,Housing_cost)

Housing_plot <- Figure_box_3_2_b_data %>% 
  ggplot(aes(x=Movement, y = Housing_cost, fill = Year)) + 
  geom_bar(stat="identity", position="dodge",width = 0.6) + 
  ggtitle("Median housing costs") +
  scale_y_continuous(name="Dollars per month", expand = c(0, 0.0), limits=c(0, 800)) +
  theme(plot.title = element_text(face="bold",size = 10,hjust=0.5), ##### New Phil bit
        legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette())

ggarrange(Movement_plot,Housing_plot,common.legend = TRUE, legend="bottom")

emf(width = 15/2.54, height = 8/2.54, file = "Figure_box_3_2.emf", pointsize=12, bg = "transparent")
ggarrange(Movement_plot,Housing_plot,common.legend = TRUE, legend="bottom")
dev.off()




##########----------------------------------------------------------------------------------------------------##########
#        Figure 3.9: Drop-out rates by equity group
##########----------------------------------------------------------------------------------------------------##########

#dropouts are measured for people who attended by age 22 and dropped out by age 23

Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group")
Cohorts <- c(1995,1998,2003,2006,2009)
Tables <- c(2)
Figure_3_9_data <- bind_rows(lapply(Tables, function(y) { 
  bind_rows(lapply(Cohorts, function(z) {  
    bind_rows(lapply(Equity_groups, function(x) {
      Summary_statistics_function(cohort=z,equity_group=x,age=22,combine_undertaking=FALSE) } [[y]]  )) }) )}) )

Figure_3_9_data <- Figure_3_9_data %>% ungroup() %>% 
  filter(Equity_group %in% c("Low SES","First in family","Indigenous","Regional or remote","No equity group"), Uni_complete == "Dropped out") %>% 
  select(Cohort, Year, Equity_group, Dropout_rate = Percent) %>% 
  mutate(Equity_group = factor(Equity_group, levels=unique(Equity_group)),
         Year = as.numeric(Year),
         Cohort = as.numeric(Cohort))

Dropout_equity_chart <- Figure_3_9_data %>% 
  ggplot(aes(x=Year,y=Dropout_rate,linetype = Equity_group)) +
  geom_line(aes(colour=Equity_group)) +
  scale_x_continuous(breaks = c(2004,2007,2011,2014, 2017), expand = c(0, 0.0)) +
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), breaks = seq(0, 45, by = 5), limits=c(0,45)) + 
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10)) + scale_colour_manual(values=palette())
Dropout_equity_chart 

Figure_3_9_data <- Figure_3_9_data %>% select(-Cohort) %>% spread(key = Year, value = Dropout_rate)

emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_9.emf", pointsize=12, bg = "transparent")
Dropout_equity_chart
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure 3.10: Bachelor degree attainment by equity group
##########----------------------------------------------------------------------------------------------------##########

Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group")

Table_3_3 <- bind_rows(Reduce(full_join,lapply(Equity_groups, 
                                               function(x) Summary_statistics_function(cohort=2003,equity_group=x,age=22,combine_undertaking="postgraduate", Normalise=TRUE)[[7]])),
                       Reduce(full_join,lapply(Equity_groups, 
                                               function(x) Summary_statistics_function(cohort=2009,equity_group=x,age=22,combine_undertaking="postgraduate", Normalise=TRUE)[[7]])))

Figure_3_10_data <- Table_3_3 %>% filter(Stage %in% "University outcome", Result %ni% "Never commenced") 

# Using age 22 weights for attendance and age 23 weights for university result, but forcing the results at age 23 (complete + undertaking + dropout) to sum to attendance at age 22
# The previous version 
Equity_groups <- c("Indigenous", "Low SES", "First in family", "Regional or remote", "No equity group")
Figure_3_10 <- Figure_3_10_data %>% 
  gather(key="Equity_group", value = "Percent", -c(Cohort,Year,Stage,Result)) %>% 
  filter(Equity_group %in% Equity_groups) %>% 
  mutate(Equity_group = factor(Equity_group, levels = Equity_groups),
         Equity_group = recode(Equity_group, `Regional or remote` = "Regional \n or remote"),
         Result = factor(Result, levels = c("Dropped out", "Undertaking", "Complete"))) %>% 
  ggplot(aes(x = as.factor(Year), y = Percent, fill = Result)) + 
  geom_bar(stat="identity", position="stack",width = 0.6) + 
  ylab("Per cent") +
  facet_wrap(~Equity_group,nrow = 1) +
  scale_y_continuous(expand = c(0, 0.0), limits=c(0,100)) +  
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank()) + theme(plot.margin = margin(10,10,5,10)) + scale_fill_manual(values=palette())
Figure_3_10

Figure_3_10_data <- Figure_3_10_data %>% select(-Cohort,-Stage)
# gather(key = "Equity_group", value = "Per_cent", -c(Year,Result)) %>% 
# spread(key = Year, value = Per_cent)

#Chart title: Bachelor degree attainment status at age 23 years
emf(width = 15/2.54, height = 8/2.54, file = "Figure_3_10.emf", pointsize=12, bg = "transparent")
Figure_3_10
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure 3.11: Occupational outcomes by equity group
##########----------------------------------------------------------------------------------------------------##########

Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group")
Cohorts <- c(1995,1998,2003,2006,2009)
Tables <- c(6)
Outcome_equity_table <-lapply(Tables, function(y) { 
  bind_rows(lapply(Cohorts, function(z) {  
    bind_rows(lapply(Equity_groups, function(x) {
      Summary_statistics_function(cohort=z,equity_group=x,age=22,combine_undertaking=FALSE) } [[y]]  )) }) )})


Outcome_equity_table[[1]] <- Outcome_equity_table[[1]] %>% 
  mutate(Equity_group = str_replace(Equity_group, "SES", "socioeconomic status"))


Outcome_equity_table_for_chart <-  bind_rows(
  Outcome_equity_table[[1]] %>% group_by(Cohort,Year,Category,Equity_group,Outcome) %>% 
    summarise(Percent=sum(Count)) %>% 
    mutate_at(vars("Percent"), list(~./ sum(.) * 100)) %>% 
    filter(Outcome == "Manager or professional",
           Equity_group %in% c("Low socioeconomic status","First in family","Indigenous","Regional or remote","No equity group")) %>%
    ungroup() %>% 
    mutate(Equity_group = factor(Equity_group, levels = c("Indigenous","Low socioeconomic status","First in family","Regional or remote","No equity group")),
           Chart = "Total population (employment in managerial or professional occupation)"),
  Outcome_equity_table[[1]] %>% group_by(Cohort,Year,Category,Equity_group,Uni_attend,Uni_complete,Outcome) %>%  
    summarise(Percent=sum(Count)) %>% 
    mutate_at(vars("Percent"), list(~./ sum(.) * 100)) %>% 
    filter(Outcome == "Manager or professional",Uni_attend %in% "Attended", Uni_complete %in% "Complete",
           Equity_group %in% c("Low socioeconomic status","First in family","Indigenous","Regional or remote","No equity group")) %>%
    ungroup() %>% 
    mutate(Equity_group = factor(Equity_group, levels = c("Indigenous","Low socioeconomic status","First in family","Regional or remote","No equity group")),
           Chart = "University graduates (employment in managerial or professional occupation)")) %>% mutate(Ylab = "Per cent")



Figure_3_11_a_data <- Outcome_equity_table[[1]] %>% group_by(Cohort,Year,Category,Equity_group,Outcome) %>%
  summarise(Percent=sum(Count)) %>%
  mutate_at(vars("Percent"), list(~./ sum(.) * 100))%>%
  filter(Outcome == "Manager or professional",
         Equity_group %in% c("Low socioeconomic status","First in family","Indigenous","Regional or remote","No equity group")) %>%
  ungroup() %>%
  mutate(Equity_group = factor(Equity_group, levels = c("Indigenous","Low socioeconomic status","First in family","Regional or remote","No equity group")))

Outcome_equity_chart_tot <- Figure_3_11_a_data %>%
  ggplot(aes(x=Equity_group,y=Percent, fill=Year)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 100)) + 
  ggtitle("Total population (employment in managerial or professional occupation)") +
  theme(legend.margin = margin(5,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10),
        plot.title = element_text(size = 10,hjust=0.5)) + scale_fill_manual(values=palette())
 

Figure_3_11_b_data <- Outcome_equity_table[[1]] %>% group_by(Cohort,Year,Category,Equity_group,Uni_attend,Uni_complete,Outcome) %>%
  summarise(Percent=sum(Count)) %>%
  mutate_at(vars("Percent"), list(~./ sum(.) * 100)) %>%
  filter(Outcome == "Manager or professional",Uni_attend %in% "Attended", Uni_complete %in% "Complete",
         Equity_group %in% c("Low socioeconomic status","First in family","Indigenous","Regional or remote","No equity group")) %>%
  ungroup() %>%
  mutate(Equity_group = factor(Equity_group, levels = c("Indigenous","Low socioeconomic status","First in family","Regional or remote","No equity group")))

# Assuming postgrad is not completed
Outcome_equity_chart_uni <- Figure_3_11_b_data %>%
  ggplot(aes(x=Equity_group,y=Percent, fill=Year)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 100)) + 
  ggtitle("University graduates (employment in managerial or professional occupation)") +
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10),
        plot.title = element_text(size = 10,hjust=0.5)) + scale_fill_manual(values=palette())


ggarrange(Outcome_equity_chart_tot + theme(axis.title.x = element_blank(),
                                           axis.text.x = element_blank(),
                                           axis.ticks.x = element_blank(),
                                           axis.line.x = element_blank(),
                                           plot.margin = margin(10,10,25,10)),
          Outcome_equity_chart_uni,ncol=1,nrow=2,legend="bottom", common.legend = TRUE)


Figure_3_11_a_data <- Figure_3_11_a_data %>% 
  select(-Cohort,-Category) %>% 
  spread(key = Year, value = Percent)

Figure_3_11_b_data <- Figure_3_11_b_data %>% 
  select(-c(Cohort,Category,Uni_attend,Uni_complete)) %>% 
  spread(key = Year, value = Percent)

emf(width = 15/2.54, height = 13/2.54, file = "Figure_3_11.emf", pointsize=12, bg = "transparent")
ggarrange(Outcome_equity_chart_tot + theme(axis.title.x = element_blank(),
                                           axis.text.x = element_blank(),
                                           axis.ticks.x = element_blank(),
                                           axis.line.x = element_blank(),
                                           plot.margin = margin(10,10,25,10)),
          Outcome_equity_chart_uni,ncol=1,nrow=2,legend="bottom", common.legend = TRUE)
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Table 3.2: Student attributes and outcomes, by equity group status
##########----------------------------------------------------------------------------------------------------##########

Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group","Total")
Equity_groups_full_names <- c("Indigenous", "Low SES", "First in family", "Regional or remote", "At least 1 equity group","No equity group","Total")

AS_student_attributes_data <- bind_rows(lapply(Equity_groups, 
                                               function(x) Summary_statistics_function_AS(cohort=2009,equity_group=x,age=22,
                                                                                          combine_undertaking="postgraduate")[[3]] %>% 
                                                 ungroup() %>% 
                                                 mutate(Equity_group = as.character(Equity_group),Cohort=2009,Year=2016)))

Table_3_2a <- as_tibble(AS_student_attributes_data) %>% 
  filter(Equity_group %in% Equity_groups_full_names,
         Type %ni% "All students") %>% 
  spread(key = Type,value = Result) %>% 
  arrange(Variable,Equity_group) %>% 
  rename(Additional_students = `Additional students`, Other_students = `Other students`) %>% 
  filter(Equity_group %ni% c("Total", "Indigenous")) 


AS_completion_data <- bind_rows(lapply(Equity_groups, 
                                       function(x) Summary_statistics_function_AS(cohort=2009,equity_group=x,age=22,combine_undertaking="postgraduate")[[2]] %>% 
                                         ungroup() %>% mutate(Equity_group = as.character(Equity_group),Cohort=2009,Year=2017)))

Table_3_2b <- AS_completion_data %>% 
  filter(Equity_group %in% Equity_groups_full_names, Uni_complete %ni% "Never commenced")  %>% 
  mutate(Uni_complete = recode(Uni_complete, Complete = "Completion rate", `Dropped out` = "Drop-out rate", Undertaking = "Still undertaking"),
         Uni_complete = as.character(Uni_complete))

Table_3_2 <- bind_rows(Table_3_2a %>% select(Variable, Equity_group, Additional_students, Other_students),
                       Table_3_2b %>% select(Variable = Uni_complete, Equity_group, Additional_students, Other_students)) %>% 
  mutate(Variable = factor(Variable, levels = c("No ATAR","Part time first year","Worked first year university","PISA","Completion rate","Drop-out rate",
                                                "Still undertaking")),
         Equity_group = factor(Equity_group, levels = c("Low SES","First in family","Regional or remote","At least 1 equity group","No equity group"))) %>% 
  arrange(Variable, Equity_group)

Table_3_2

##########----------------------------------------------------------------------------------------------------##########
#         Table 3.3: University participation, completion and outcomes
##########----------------------------------------------------------------------------------------------------##########


Equity_groups <- c("Low_SES","Parent_edu","Indigenous","Location_RR_wave1","Equity_group")

Table_3_3 <- bind_rows(Reduce(full_join,lapply(Equity_groups, 
                                               function(x) Summary_statistics_function(cohort=2003,equity_group=x,age=22,combine_undertaking="postgraduate", Normalise=TRUE)[[7]])),
                       Reduce(full_join,lapply(Equity_groups, 
                                               function(x) Summary_statistics_function(cohort=2009,equity_group=x,age=22,combine_undertaking="postgraduate", Normalise=TRUE)[[7]])))
Table_3_3



##########----------------------------------------------------------------------------------------------------##########
#         Figure B.1: LSAY sample by school year level and state
##########----------------------------------------------------------------------------------------------------##########

Year_level_2006 <- LSAY_2006_data %>% select(STIDSTD,State)

Years_2006_cohort <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)

for (i in Years_2006_cohort) { 
  Year_level_2006[,paste0("wave_",i)] <- ifelse(LSAY_2006[,paste0("XCSL",i)] == 1, "Year 12",
                                                ifelse(LSAY_2006[,paste0("XCSL",i)] == 2, "Year 11",
                                                       ifelse(LSAY_2006[,paste0("XCSL",i)] == 3, "Year 10",
                                                              ifelse(LSAY_2006[,paste0("XCSL",i)] == 4, "Year 9 or below",
                                                                     ifelse(LSAY_2006[,paste0("XCSL",i)] == 5, "At school - year level unknown",
                                                                            ifelse(LSAY_2006[,paste0("XCSL",i)] == 6, "Not at school",NA))))))
}

Year_level_2006 <- Year_level_2006 %>% gather(key = "Wave", value = "Year_level", -c(STIDSTD, State)) %>% 
  mutate(Year_level = factor(Year_level, levels = c("At school - year level unknown","Year 9 or below","Year 10","Year 11","Year 12","Not at school")))

Figure_B_1 <- Year_level_2006 %>% filter(Wave == "wave_2006") %>% 
  ggplot(aes(x=State, y=Year_level, colour=Wave)) +  
  geom_point(size=0.5) + geom_jitter() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y=element_text(hjust = 0.95),
        legend.position = "none", plot.margin = margin(10,10,5,10)) + scale_colour_manual(values=palette())
Figure_B_1

# Note: this failed to save to EMF on some systems

# emf(width = 15/2.54, height = 8/2.54, file = "Figure_B_1.emf", pointsize=12, bg = "transparent")
# Figure_B_1
# dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure B.2 Sample attrition by equity group
##########----------------------------------------------------------------------------------------------------##########

#### 2003 ####
Attrition_table_2003 <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                               function (x) {  LSAY_2003_data %>% select(one_of(x),"Year_of_attrition") %>% 
                                   group_by_at(.vars = vars(one_of(x),Year_of_attrition)) %>% 
                                   summarise(Number=n()) %>% 
                                   mutate(Category=names(.)[1]) %>% 
                                   rename_at(vars(matches(x)), ~ "Variable") %>% 
                                   mutate(Attrition=cumsum(Number)) %>% 
                                   ungroup() %>% 
                                   mutate_if(is.factor, as.character)  %>% 
                                   mutate_at(vars(matches("Year_of_attrition")), list(as.character))}) %>% bind_rows() %>% na.omit() 

Attrition_table_2003  <- Attrition_table_2003 %>% group_by(Variable,Category) %>% 
  summarise(Year_of_attrition = "2003") %>% 
  mutate(Number = 0,Attrition=0) %>% 
  bind_rows(Attrition_table_2003,.)

Obs_table <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                    function (x) {  LSAY_2003_data %>% select(one_of(x)) %>% group_by_at(.vars = vars(one_of(x)))  %>% 
                        summarise(Number=n()) %>% 
                        mutate(Category=names(.)[1]) %>% 
                        rename_at(vars(matches(x)), ~ "Variable")  %>% 
                        ungroup() %>% 
                        mutate_if(is.factor, as.character)
                    }) %>% bind_rows() %>% na.omit()


Attrition_table_2003 <- Attrition_table_2003 %>% 
  mutate(Total = vlookup(Variable, Obs_table, "Variable", "Number")) %>% 
  filter(Year_of_attrition != 9999) %>% 
  mutate(Remaining_share = (1-Attrition / Total) *100) %>% 
  filter(Variable %in% c("Indigenous","Low_SES","No_uni","Regional_remote","Total")) %>% 
  mutate(Cohort = "2003 cohort")

#### 2006 ####
Attrition_table_2006 <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                               function (x) {  LSAY_2006_data %>% select(one_of(x),"Year_of_attrition") %>% 
                                   group_by_at(.vars = vars(one_of(x),Year_of_attrition)) %>% 
                                   summarise(Number=n()) %>% 
                                   mutate(Category=names(.)[1]) %>% 
                                   rename_at(vars(matches(x)), ~ "Variable") %>% 
                                   mutate(Attrition=cumsum(Number)) %>% 
                                   ungroup() %>% 
                                   mutate_if(is.factor, as.character)  %>% 
                                   mutate_at(vars(matches("Year_of_attrition")), list(as.character))}) %>% bind_rows() %>% na.omit() 

Attrition_table_2006  <- Attrition_table_2006 %>% group_by(Variable,Category) %>% 
  summarise(Year_of_attrition = "2006") %>% 
  mutate(Number = 0,Attrition=0) %>% 
  bind_rows(Attrition_table_2006,.)

Obs_table <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                    function (x) {  LSAY_2006_data %>% select(one_of(x)) %>% group_by_at(.vars = vars(one_of(x)))  %>% 
                        summarise(Number=n()) %>% 
                        mutate(Category=names(.)[1]) %>% 
                        rename_at(vars(matches(x)), ~ "Variable")  %>% 
                        ungroup() %>% 
                        mutate_if(is.factor, as.character)
                    }) %>% bind_rows() %>% na.omit()


Attrition_table_2006 <- Attrition_table_2006 %>% 
  mutate(Total = vlookup(Variable, Obs_table, "Variable", "Number")) %>% 
  filter(Year_of_attrition != 9999) %>% 
  mutate(Remaining_share = (1-Attrition / Total) *100) %>% 
  filter(Variable %in% c("Indigenous","Low_SES","No_uni","Regional_remote","Total")) %>% 
  mutate(Cohort = "2006 cohort")

#### 2009 ####
Attrition_table_2009 <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                               function (x) {  LSAY_2009_data %>% select(one_of(x),"Year_of_attrition") %>% 
                                   group_by_at(.vars = vars(one_of(x),Year_of_attrition)) %>% 
                                   summarise(Number=n()) %>% 
                                   mutate(Category=names(.)[1]) %>% 
                                   rename_at(vars(matches(x)), ~ "Variable") %>% 
                                   mutate(Attrition=cumsum(Number)) %>% 
                                   ungroup() %>% 
                                   mutate_if(is.factor, as.character) %>% 
                                   mutate_at(vars(matches("Year_of_attrition")), list(as.character))
                               }) %>% bind_rows() %>% na.omit() 

Attrition_table_2009  <- Attrition_table_2009 %>% group_by(Variable,Category) %>% 
  summarise(Year_of_attrition = "2009") %>% 
  mutate(Number = 0,Attrition=0) %>% 
  bind_rows(Attrition_table_2009,.)

Obs_table <- lapply(c("Indigenous","NESB","Low_SES","Parent_edu","Location_RR_wave1","Total"),
                    function (x) {  LSAY_2009_data %>% select(one_of(x)) %>% group_by_at(.vars = vars(one_of(x))) %>% 
                        summarise(Number=n()) %>% 
                        mutate(Category=names(.)[1]) %>% 
                        rename_at(vars(matches(x)), ~ "Variable") %>% 
                        ungroup() %>% 
                        mutate_at(vars(matches("Year_of_attrition")), list(as.character))
                    }) %>% bind_rows() %>% na.omit()


Attrition_table_2009 <- Attrition_table_2009 %>% 
  mutate(Total = vlookup(Variable, Obs_table, "Variable", "Number")) %>% 
  filter(Year_of_attrition != 9999) %>% 
  mutate(Remaining_share = (1-Attrition / Total) *100) %>% 
  filter(Variable %in% c("Indigenous","Low_SES","No_uni","Regional_remote","Total")) %>% 
  mutate(Cohort = "2009 cohort")

Figure_B_2_data <- bind_rows(Attrition_table_2003,Attrition_table_2006,Attrition_table_2009) %>% 
  mutate(Variable = replace(Variable, Variable=="Low_SES",  "Low SES")) %>% 
  mutate(Variable = replace(Variable, Variable=="No_uni",  "Parents not university educated")) %>% 
  mutate(Variable = replace(Variable, Variable=="Regional_remote",  "Regional or remote location"))

Figure_B_2 <- Figure_B_2_data %>%
  ggplot(aes(x=as.numeric(Year_of_attrition), y = Remaining_share, colour=Variable)) +
  geom_line(size=0.8) +
  facet_wrap(~Cohort,nrow=3,ncol = 1) + 
  scale_x_continuous(breaks = c(2003:2016)) +
  scale_y_continuous(name="Per cent", expand = c(0, 0.0), limits=c(0, 100)) +
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10),
        strip.text.x = element_text(face="bold",size = 10,hjust=0.5),legend.position="bottom") + # change format of facet text 
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) + # put legend on 2 lines
  scale_colour_manual(values=palette())
Figure_B_2

Figure_B_2_data <- Figure_B_2_data %>% select(Cohort, Variable, Year_of_attrition, Remaining_share) %>% 
  spread(key = Year_of_attrition, value = Remaining_share)


emf(width = 15/2.54, height = 12/2.54, file = "Figure_B_2.emf", pointsize=12, bg = "transparent")
Figure_B_2
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure B.3: Example: the impact of weights
##########----------------------------------------------------------------------------------------------------##########


LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2016MP)),]), 
                              weights = LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2016MP)),"WT2016MP"])

LSAY_2006_data$WT2016_NCVER <- LSAY_2006$WT2016
LSAY_2006_data_NCVER.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2016_NCVER)),]), 
                                    weights = LSAY_2006_data[which(!is.na(LSAY_2006_data$WT2016_NCVER)),"WT2016_NCVER"])

Equity_groups <- c("Low_SES","Parent_edu","Location_RR_wave1","Equity_group")

Figure_B_3_data <- tibble()
for (equity_group in Equity_groups) { 
  Table_no_weights <- LSAY_2006_data %>% select(Manager_prof2016,one_of(equity_group)) %>% 
    rename(equity_group = paste0(equity_group)) %>% 
    group_by(equity_group) %>% 
    summarise(Outcome = mean(Manager_prof2016, na.rm=TRUE) * 100) %>% 
    ungroup() %>% 
    mutate(equity_group = as.character(equity_group),
           Weights = "Unweighted")
  
  Table_NCVER <- as_tibble(svyby(~Manager_prof2016, by = ~get(equity_group), design = LSAY_2006_data_NCVER.w, svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
    rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
    rename(Outcome = statistic) %>% 
    mutate(Outcome = Outcome * 100,
           equity_group = as.character(equity_group),
           Weights = "NCVER weights")
  
  Table_MPC <- as_tibble(svyby(~Manager_prof2016, by = ~get(equity_group), design = LSAY_2006_data.w, svymean,keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>%
    rename_at(vars(contains('get')), list(~str_replace(.,"^.*?\\((.*)\\)", "\\1"))) %>% 
    rename(Outcome = statistic) %>% 
    mutate(Outcome = Outcome * 100,
           equity_group = as.character(equity_group),
           Weights = "New weights")
  
  Figure_B_3_data <- bind_rows(Figure_B_3_data, Table_no_weights, Table_NCVER, Table_MPC)
}

Figure_B_3_data <- Figure_B_3_data %>% 
  filter(equity_group %in% c("Low_SES","No_uni","Regional_remote","Not_disadvantaged")) %>% 
  mutate(equity_group = recode_factor(equity_group, Low_SES = "Low SES", No_uni = "First in family",
                                      Regional_remote = "Regional or remote",Not_disadvantaged = "No equity group")) %>%
  mutate(Weights = factor(Weights, levels = c("Unweighted","NCVER weights","New weights")))

Figure_B_3 <- Figure_B_3_data %>%
  ggplot(aes(x=equity_group,y=Outcome, fill=Weights)) +
  geom_bar(stat="identity", position="dodge") + 
  scale_y_continuous(name="Per cent", expand = c(0, 0.0),limits=c(0, 60)) + 
  theme(legend.margin = margin(0,0,0,0), axis.title.x = element_blank(), plot.margin = margin(10,10,5,10), legend.position="bottom") +
  scale_fill_manual(values=palette())
Figure_B_3 

Figure_B_3_data <- Figure_B_3_data %>% spread(key = Weights, value = Outcome)

emf(width = 15/2.54, height = 8/2.54, file = "Figure_B_3.emf", pointsize=12, bg = "transparent")
Figure_B_3
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure B.4: University attendance by age 22 years in 2013 
##########----------------------------------------------------------------------------------------------------##########

variable_names <- data.frame()
variable_names <- as.data.frame(c("Total","University type","Group of Eight","Other university","Equity_group","Gender",
                                  "Parent_SES_quartile","PQ1", "PQ2", "PQ3", "PQ4", "Educ_occupation_quartile","Sector",
                                  "Location_RR_wave1",
                                  "Indigenous","Not indigenous",  "Government", 
                                  "Catholic", "Independent", "No_uni","Uni",  "Not university", "University", "Male", 
                                  "Female", "Under 40km", "40 to 80km", "Over 80km", 
                                  "English speaking background", "Non-English", "lower", "mid-lower", "Regional_remote",
                                  "mid-upper", "upper", "Disadvantaged", "Not_disadvantaged",
                                  "EQ1", "EQ2", "EQ3", "EQ4","Study_plans",
                                  "Closest_uni_distance_g2","Parent_edu","ATAR_group_imputed","PISA_quartile","NESB",
                                  "Non-traditional_areas",
                                  "No_ATAR","0-50","50-60","60-70","70-80","80-90","90-100"))
colnames(variable_names)[1] <- "Old_names"

variable_names$New_names <- c("Total","University network","Group of Eight","Other university","Equity group","Gender",
                              "Parents' socioeconomic status","Q1", "Q2", "Q3", "Q4", "Neighbourhood socioeconomic status","School sector",
                              "Location",
                              "Indigenous","Non-Indigenous", "Public", 
                              "Catholic", "Independent", "Below university","University", "Not university", "University", "Male", 
                              "Female", "Under 40km", "40 to 80km", "Over 80km", 
                              "English speaking", "NESB", "Q1", "Q2", "Regional or remote",
                              "Q3", "Q4", "Disadvantaged", "Not disadvantaged", 
                              "Q1", "Q2", "Q3", "Q4","Aspiration",
                              "Distance to a university","Parents' education","ATAR score","PISA score","Ethnicity",
                              "Non-traditonal areas of study",
                              "No ATAR","0-50","50-60","60-70","70-80","80-90","90-100")

groups <- c("Parent_SES_quartile","Indigenous","Sector","Educ_occupation_quartile",
            "Gender","Location_RR_wave1","Parent_edu","PISA_quartile","ATAR_group_imputed")  


Marginal_students_new_2006 <- Additional_students_charts(cohort = 2006, groups_categorise=groups,
                                                         variable_names=variable_names,university_network=FALSE)

Marginal_students_new_2006[[2]]
Figure_B_4_data <- Marginal_students_new_2006[[3]]

emf(width = 29/2.54, height = 13/2.54, file = "Figure_B_4.emf", pointsize=6, bg = "transparent")
Marginal_students_new_2006[[2]]
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Figure B.5: University attendance by age 22 years in 2016 
##########----------------------------------------------------------------------------------------------------##########



groups <- c("Parent_SES_quartile","Indigenous","Sector","Educ_occupation_quartile",
            "Gender","Location_RR_wave1","Parent_edu","PISA_quartile","ATAR_group_imputed")  

#groups <- c("ATAR_group_imputed","PISA_quartile")  

variable_names <- data.frame()
variable_names <- as.data.frame(c("Total","University type","Group of Eight","Other university","Equity_group","Gender",
                                  "Parent_SES_quartile","PQ1", "PQ2", "PQ3", "PQ4", "Educ_occupation_quartile","Sector",
                                  "Location_RR_wave1",
                                  "Indigenous","Not indigenous",  "Government", 
                                  "Catholic", "Independent", "No_uni","Uni",  "Not university", "University", "Male", 
                                  "Female", "Under 40km", "40 to 80km", "Over 80km", 
                                  "English speaking background", "Non-English", "lower", "mid-lower", "Regional_remote",
                                  "mid-upper", "upper", "Disadvantaged", "Not_disadvantaged",
                                  "EQ1", "EQ2", "EQ3", "EQ4","Study_plans",
                                  "Closest_uni_distance_g2","Parent_edu","ATAR_group_imputed","PISA_quartile","NESB",
                                  "Non-traditional_areas",
                                  "No_ATAR","0-50","50-60","60-70","70-80","80-90","90-100"))
colnames(variable_names)[1] <- "Old_names"

variable_names$New_names <- c("Total","University network","Group of Eight","Other university","Equity group","Gender",
                              "Parents' socioeconomic status","Q1", "Q2", "Q3", "Q4", "Neighbourhood socioeconomic status","School sector",
                              "Location",
                              "Indigenous","Non-Indigenous", "Public", 
                              "Catholic", "Independent", "Below university","University", "Not university", "University", "Male", 
                              "Female", "Under 40km", "40 to 80km", "Over 80km", 
                              "English speaking", "NESB", "Q1", "Q2", "Regional or remote",
                              "Q3", "Q4", "Disadvantaged", "Not disadvantaged", 
                              "Q1", "Q2", "Q3", "Q4","Aspiration",
                              "Distance to a university","Parents' education","ATAR score","PISA score","Ethnicity",
                              "Non-traditonal areas of study",
                              "No ATAR","0-50","50-60","60-70","70-80","80-90","90-100")


Marginal_students_new_2009 <- Additional_students_charts(cohort = 2009, groups_categorise=groups,
                                                         variable_names=variable_names,university_network=FALSE)
Marginal_students_new_2009[[2]] 
Figure_B_5_data <- Marginal_students_new_2009[[3]] 


emf(width = 29/2.54, height = 13/2.54, file = "Figure_B_5.emf", pointsize=6, bg = "transparent")
Marginal_students_new_2009[[2]]
dev.off()


##########----------------------------------------------------------------------------------------------------##########
#         Table B.6: Detailed decomposition results: Indigenous
##########----------------------------------------------------------------------------------------------------##########


Table_B_6 <- bind_rows(Detailed_decomp_table(2003),Detailed_decomp_table(2006),Detailed_decomp_table(2009)) %>% 
  filter(Equity_group == "Indigenous")

##########----------------------------------------------------------------------------------------------------##########
#         Table B.7: Detailed decomposition results: Regional or remote location
##########----------------------------------------------------------------------------------------------------##########

Table_B_7 <- bind_rows(Detailed_decomp_table(2003),Detailed_decomp_table(2006),Detailed_decomp_table(2009)) %>% 
  filter(Equity_group == "Location_RR")

##########----------------------------------------------------------------------------------------------------##########
#         Table B.8: Detailed decomposition results: Low socioeconomic status
##########----------------------------------------------------------------------------------------------------##########

Table_B_8 <- bind_rows(Detailed_decomp_table(2003),Detailed_decomp_table(2006),Detailed_decomp_table(2009)) %>% 
  filter(Equity_group == "Low_SES")

##########----------------------------------------------------------------------------------------------------##########
#         Table B.9: Detailed decomposition results: First in family
##########----------------------------------------------------------------------------------------------------##########

Table_B_9 <- bind_rows(Detailed_decomp_table(2003),Detailed_decomp_table(2006),Detailed_decomp_table(2009)) %>% 
  filter(Equity_group == "Parent_edu")

##########----------------------------------------------------------------------------------------------------##########
#         Table B.11: Who are the additional students
##########----------------------------------------------------------------------------------------------------##########

Additional_students_alternative <- function(cohort=2009,variable="Institution_network") {
  
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  weights <- ifelse(cohort==2006,"WT2013CP",ifelse(cohort==2009,"WT2016CP",NA))
  design = svydesign(ids = ~0, data = as.data.frame(LSAY_data[which(!is.na(LSAY_data[,weights])),]), 
                     weights = LSAY_data[which(!is.na(LSAY_data[,weights])),weights])
  
  AS <- as_tibble(svyby(~Additional_student, by = ~get(variable), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Additional_students = statistic)
  
  AT <- as_tibble(svyby(~Other_student, by = ~get(variable), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Other_students = statistic)
  
  NT <- as_tibble(svyby(~Never_student, by = ~get(variable), design = design,svytotal,
                        keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Never_students = statistic)
  
  L <- as_tibble(svyby(~Fewer_student, by = ~get(variable), design = design,svytotal,
                       keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students = statistic)
  
  L <- as_tibble(svyby(~Fewer_student, by = ~get(variable), design = design,svytotal,
                       keep.names = TRUE,keep.var = FALSE, na.rm=TRUE)) %>% rename(Fewer_students = statistic)
  
  Table <- bind_cols(AS,AT %>% select(Other_students),NT %>% select(Never_students),L %>% select(Fewer_students)) %>% 
    mutate(Never_students=Never_students+Fewer_students) %>% select(-Fewer_students) %>% # Combine Fewer_students & never takers
    bind_rows(.,select_if(., is.numeric) %>% colSums()) %>% # Calculate column totals
    rename(Name = `get(variable)`) %>% 
    mutate(Name=as.character(Name),
           Name=replace(Name, is.na(Name), "Total"),
           Population = rowSums(.[2:4]),
           Total_students = Population - Never_students,
           Additional_students = Additional_students / Additional_students[Name == "Total"] * 100,  # Row shares of total
           Other_students = Other_students / Other_students[Name == "Total"] * 100,  # Row shares of total
           Never_students = Never_students / Never_students[Name == "Total"] * 100,
           Population = Population / Population[Name == "Total"] * 100,
           Total_students = Total_students / Total_students[Name == "Total"] * 100,
           Category=paste0(variable),
           Cohort=paste0(cohort)) %>% 
    select(Category,Name,Cohort,Additional_students,Other_students,Total_students,Never_students,Population) %>% 
    rename(Other_students = Other_students,Not_students=Never_students) %>% 
    mutate_if(is.numeric, round, 1)
  
  return(Table)
}

Equity_groups <- c("Parent_SES_quartile","Parent_edu","Indigenous","Location_RR_wave1","Equity_group","Educ_occupation_quartile",
                   "Institution_network","Sector","Gender","ATAR_group_imputed")

Table_B_11 <- bind_rows(lapply(Equity_groups, function(x) Additional_students_alternative(cohort=2009,variable=x))) 


##########----------------------------------------------------------------------------------------------------##########
#        Table B.14  Some convergence of outcomes by age 25 years
##########----------------------------------------------------------------------------------------------------##########


Table_B_14a1_results <- bootstrap_tables_and_charts(Outcome_variables = c("Completed", "Dropout"),
                                                    Cohort = 2006, Age = 23, Graduates = FALSE)

Table_B_14a2_results <- bootstrap_tables_and_charts(Outcome_variables = c("Employed_FT", "Manager", "Professionals", "Average_weekly_pay"),
                                                    Cohort = 2006, Age = 23, Graduates = TRUE)

Table_B_14b1_results <- bootstrap_tables_and_charts(Outcome_variables = c("Completed", "Dropout"),
                                                    Cohort = 2006, Age = 25, Graduates = FALSE)

Table_B_14b2_results <- bootstrap_tables_and_charts(Outcome_variables = c("Employed_FT", "Manager", "Professionals", "Average_weekly_pay"),
                                                    Cohort = 2006, Age = 25, Graduates = TRUE)

Table_B_14 <- bind_rows(Table_B_14a1_results %>% mutate(Year = "2014", Age = "23") %>% mutate_if(is.factor, list(~as.character(.))) %>% 
                          filter(University_attendance %in% c("Additional students","Other students")),
                        Table_B_14a2_results %>% mutate(Year = "2014", Age = "23") %>% mutate_if(is.factor, list(~as.character(.))) ,
                        Table_B_14b1_results %>% mutate(Year = "2016", Age = "25") %>% mutate_if(is.factor, list(~as.character(.)))  %>% 
                          filter(University_attendance %in% c("Additional students","Other students")),
                        Table_B_14b2_results %>% mutate(Year = "2016", Age = "25") %>% mutate_if(is.factor, list(~as.character(.))) ) %>% 
  select(Year, Age, `Outcome measure` = Outcome, `Student type` = University_attendance, Mean, 
         `95 % confidence interval: upper` = CI_top, `95 % confidence interval: lower` = CI_bottom )


##########----------------------------------------------------------------------------------------------------##########
#         Table B.15 and B.16: Regression results (output is in html format)
##########----------------------------------------------------------------------------------------------------##########

Variable_names <- tibble(Old_names = c("PQ1", "PQ2", "PQ3", "PQ4", "Not indigenous", "Indigenous", "Independent", 
                                  "Catholic", "Government", "Not university", "University", "Male", 
                                  "Female", "Under 40km", "40 to 80km", "Over 80km", "Uni", "No_uni", 
                                  "English speaking background", "lower", "mid-lower", "Regional_remote",
                                  "mid-upper", "upper", "Disadvantaged", "Not_disadvantaged",
                                  "Aspiration","Parent_SES_quartile","Closest_uni_distance_g2","Parent_edu","PISA_quartile","NESB","Sector",
                                  "Location_RR_wave1","Gender","Non-traditional_areas","SQ3","SQ2","SQ1","EQ3","EQ2","EQ1"),
                         New_names = c("Q1", "Q2", "Q3", "Q4", "Non-Indigenous", "Indigenous", "Independent", 
                              "Catholic", "Public", "No aspiration", "Aspiration", "Male", 
                              "Female", "Under 40km", "40 to 80km", "Over 80km", "University", 
                              "Below university", "English speaking", "Q1", "Q2", "Regional or remote",
                              "Q3", "Q4", "Disadvantaged", "Not disadvantaged", 
                              "Aspiration","Socioeconomic status","Distance to a university","Parent's education","PISA score","Ethnicity","School sector",
                              "Location","Gender","Non-traditonal areas of study","Q3","Q2","Q1","Q3","Q2","Q1"))


Category_names <- tibble(Old_names = c("NESB","Parent_SES_quartile","Parent_edu","School_SES_quartile","Log_closest_uni",
                                  "Educ_occupation_quartile","Attend_year_12","Hours_worked_in_year_12",
                                  "Hours_worked_age_of_yr_12_interaction_attend","Location_RR_wave1","ATAR_group","ATAR_group_imputed","ATAR_missing_dummy"), 
                         New_names = c("Ethnic background","Parent's occupation","Parent's education","SES of the school","Distance","Neighbourhood SES",
                              "Whether attended year 12","Hours worked in year 12","Interaction of hours worked with year 12 attendance",
                              "Location","ATAR score","ATAR score","Missing ATAR"))


Age_22_Go8 <- list(Prediction_model_1995_Go8,Inference_model_1995_Go8,Prediction_model_1998_Go8,Inference_model_1998_Go8,
                   Prediction_model_2003_Go8,Inference_model_2003_Go8,Prediction_model_2006_Go8,Inference_model_2006_Go8,
                   Prediction_model_2009_Go8,Inference_model_2009_Go8)

Age_22 <- list(Prediction_model_1995,Inference_model_1995,Prediction_model_1998,Inference_model_1998,Prediction_model_2003,Inference_model_2003,
               Prediction_model_2006,Inference_model_2006,Prediction_model_2009,Inference_model_2009)
names(Age_22) <- c("Prediction", "Inference", "Prediction", "Inference","Prediction", "Inference","Prediction", "Inference","Prediction", "Inference")
####### Function
Regression_tables <- function(Model_list=Age_22, File_name = "Models_age_22.html"){
  
  Variables <- unique(unlist(lapply(Model_list, function(x) attr(x$terms,"term.labels")))) %>% 
    as_tibble() %>% rename(Variables = value)
  Names <- unique(unlist(sapply(Model_list, function(x) names(x$coefficients)))) %>% 
    as_tibble() %>% rename(Names = value)%>% 
    mutate(Variable = Names)
  
  for (i in Variables$Variables) {
    Names$Variable <- sub(i, "", Names$Variable) 
  }
  
  Category_order <- c("Gender","Indigenous","Achievement","PISA","ATAR score","Missing ATAR","Hours worked in year 12","Whether attended year 12",
                      "Interaction of hours worked with year 12 attendance","Ethnic background","Books","Parent's occupation", "Parent's education",
                      "SES of the school","Sector","Neighbourhood SES","Distance", "Location","State","Grade","Constant")
  
  Names <- Names %>% mutate(Variable = ifelse(Variable == "", Names,Variable),
                            Category = str_remove(Names,Variable),
                            Category = ifelse(Category == "", Names,Category),
                            Variable = ifelse(Variable == Category | Variable == "(Intercept)", "", Variable),
                            Category = ifelse(Names =="(Intercept)", "Constant",Category),
                            Category_new = vlookup(Category,Category_names,"Old_names","New_names"),
                            Variable_new = vlookup(Variable,Variable_names,"Old_names","New_names"),
                            Category_new = ifelse(is.na(Category_new),Category,Category_new),
                            Variable_new = ifelse(is.na(Variable_new),Variable,Variable_new),
                            Category_new = str_replace_all(Category_new, "_", " "),
                            Variable_new = str_replace_all(Variable_new, "_", " "),
                            Full_names = paste0(Category_new,": ",Variable_new),
                            Full_names = ifelse(substr(Full_names, nchar(Full_names)-2+1, nchar(Full_names)) == ": ",
                                                substr(Full_names, 1, nchar(Full_names)-2),Full_names),
                            Category_new = factor(Category_new, levels = Category_order)) %>% 
    
    arrange(Category_new)
  
  Order_of_output <- as.character(Names$Names) 
  Order_of_labels <- as.character(Names$Full_names) 
  
  stargazer(Model_list,out=File_name,order = Order_of_output,covariate.labels = Order_of_labels, 
            column.labels = c("1995","1998","2003","2006","2009"),column.separate = c(2,2,2,2,2),
            type = "html",style = "aer",model.numbers=TRUE, dep.var.labels = "", 
            notes = "", notes.append = FALSE, notes.label = "")
}

# Produce the regression output
Regression_tables(Model_list=Age_22, File_name = "Models_age_22.html")

Regression_tables(Model_list=Age_22_Go8, File_name = "Models_age_22_Go8.html")


##########----------------------------------------------------------------------------------------------------##########
#         Export chart data and tables to Excel
##########----------------------------------------------------------------------------------------------------##########

#Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
# Chapter 2

list_of_datasets <- list_of_datasets <- list("Figure 2.4" = Figure_2_4_data, "Figure 2.6" = Figure_2_6_data,
                                             "Figure 2.7" = Figure_2_7_data, "Figure 2.8" = Figure_2_8_data,
                                             "Figure 2.9" = Figure_2_9_data, "Figure 2.10" = Figure_2_10_data,
                                             "Box 3" = Figure_box_2_3_data)

write.xlsx(list_of_datasets, file = "Chapter_2_chart_data.xlsx")


# Chapter 3

list_of_datasets <- list_of_datasets <- list("Figure 3.2" = Figure_3_2_data, "Figure 3.4" = Figure_3_4_data,
                                             "Figure 3.5" = Figure_3_5_data, "Figure 3.6" = Figure_3_6_data,
                                             "Figure 3.7" = Figure_3_7_data, "Figure 3.8" = Figure_3_8_data,
                                             "Box 3 Figure a" = Figure_box_3_2_a_data, "Box 3 Figure b" = Figure_box_3_2_b_data,
                                             "Figure 3.9" = Figure_3_9_data, "Figure 3.10" = Figure_3_10_data,
                                             "Figure 3.11a" = Figure_3_11_a_data, "Figure 3.11b" = Figure_3_11_b_data,
                                             "Table 3.2" = Table_3_2, "Table 3.3" = Table_3_3)

write.xlsx(list_of_datasets, file = "Chapter_3_chart_data.xlsx")

# Appendix B

list_of_datasets <- list_of_datasets <- list("Figure B.2" = Figure_B_2_data, "Figure B.3" = Figure_B_3_data,
                                             "Figure B.4" = Figure_B_4_data, "Figure B.5" = Figure_B_5_data,
                                             "Table B.6" = Table_B_6, "Table B.7" = Table_B_7,"Table B.8" = Table_B_8, 
                                             "Table B.9" = Table_B_9, "Table B.11" = Table_B_11, "Table B.14" = Table_B_14)

write.xlsx(list_of_datasets, file = "Appendix_B_chart_data.xlsx")


