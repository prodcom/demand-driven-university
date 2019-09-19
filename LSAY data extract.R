

University_locations <- read_excel("University_locations_2.xlsx", col_names = TRUE)


SEIFA_1995 <- read.csv("SEIFA_1995_interpolation_updated.csv", header = TRUE)
SEIFA_1998 <- read.csv("SEIFA_1998_interpolation_updated.csv", header = TRUE)
SEIFA_2003 <- read.csv("SEIFA_2003_interpolation_updated.csv", header = TRUE)
SEIFA_2006 <- read.csv("SEIFA_2006_interpolation_updated.csv", header = TRUE)
SEIFA_2009 <- read.csv("SEIFA_2009_interpolation_updated.csv", header = TRUE)


Institution_table_pre_2005 <- read_excel("University_codes.xlsx", col_names = TRUE, sheet = "University_codes_pre_2005_frame")
Institution_table_2005_onwards <- read_excel("University_codes.xlsx", col_names = TRUE, sheet = "University_codes_2005_frame")
Institution_variables <- read_csv("Institution_LSAY_variables.csv", col_names = TRUE) %>% 
  left_join(.,Age_table, by = c("Cohort","Year"))

Australian_Technology_Network <- c("University of Technology, Sydney","Rmit University Tafe","University of South Australia",
                                   "Curtin University of Technology")
Group_of_Eight <- c("University of Sydney","University of Melbourne","University of Queensland","University of Adelaide",
                    "University of Western Australia","Australian National University","Monash University","University of New South Wales")

Innovative_Research_Universities <- c("Charles Darwin University","James Cook University","Griffith University","La Trobe University",
                                      "Flinders University of South Australia","Murdoch University","University of Western Sydney")

Regional_Universities_Network <- c("Central Queensland University","Southern Cross University","University Of Ballarat", # University Of Ballarat is Federation University (name change in 2014)
                                   "University of Southern Queensland","University of The Sunshine Coast","University of New England")

Institution_networks <- bind_rows(enframe(Australian_Technology_Network,name = NULL) %>% mutate(Network = "Australian_Technology_Network"),
                                  enframe(Group_of_Eight,name = NULL) %>% mutate(Network = "Group_of_Eight"),
                                  enframe(Innovative_Research_Universities,name = NULL) %>% mutate(Network = "Innovative_Research_Universities"),
                                  enframe(Regional_Universities_Network,name = NULL) %>% mutate(Network = "Regional_Universities_Network")) %>% 
  rename(Institution = value)

Other_universities_pre_2005 <- Institution_table_pre_2005 %>% 
  filter(!duplicated(Institution), 
         University == 1,
         Institution %ni% (Institution_networks %>% pull(Institution)))

Other_universities_from_2005 <- Institution_table_2005_onwards %>% 
  filter(!duplicated(Institution), 
         University == 1,
         Institution %ni% (Institution_networks %>% pull(Institution)))

Other_universities <- bind_rows(Other_universities_pre_2005,Other_universities_from_2005) %>% 
  filter(!duplicated(Institution)) %>% select(Institution) %>% mutate(Network = "Other_university")

Institution_networks <- bind_rows(Institution_networks,Other_universities)

University_networks_function <- function(cohort = 1995, max_age = 22) {
  
  ID_column <- ifelse(cohort %in% c(1995,1998), "stuidno", "STIDSTD")
  year <- Age_table %>% filter(Cohort == cohort, Age == max_age) %>% pull(Year)
  
  LSAY <- get(paste0("LSAY_",cohort)) %>% 
    mutate_at(vars(ID_column), list(as.numeric)) 
  LSAY_data <- get(paste0("LSAY_",cohort,"_data")) %>% select(one_of(ID_column)) %>% 
    mutate_at(vars(ID_column), list(as.numeric)) 
  
Variables_pre_2005 <- Institution_variables %>% filter(Cohort %in% cohort, Year < 2005) %>% pull(Study_institution)
Variables_2005_onwards <- Institution_variables %>% filter(Cohort %in% cohort, Year >= 2005) %>% pull(Study_institution)


#All institutions attended
Institutions_and_networks_pre_2005 <- bind_rows(lapply(Variables_pre_2005, function (x) LSAY %>% select(one_of(ID_column,x)) %>% 
                                                     rename(Code = 2) %>% mutate(Code = as.numeric(Code)) %>% 
                                                     left_join(.,Institution_table_pre_2005 %>% select(Code, Institution)) %>% 
                                                     left_join(.,Institution_networks) %>% 
                                                     mutate(Variable = x, 
                                                            Age = vlookup(x, Institution_variables, "Study_institution", "Age"))))

Institutions_and_networks_2005_onwards <- bind_rows(lapply(Variables_2005_onwards, function (x) LSAY %>% select(one_of(ID_column,x)) %>% 
                                                              rename(Code = 2) %>% mutate(Code = as.numeric(Code)) %>% 
                                                              left_join(.,Institution_table_2005_onwards %>% select(Code, Institution)) %>% 
                                                              left_join(.,Institution_networks) %>% 
                                                              mutate(Variable = x, 
                                                                     Age = vlookup(x, Institution_variables, "Study_institution", "Age"))))


Institutions_and_networks_all <- bind_rows(Institutions_and_networks_pre_2005,Institutions_and_networks_2005_onwards) %>% 
  left_join(.,LSAY %>% select(one_of(paste0("XBAC",year),ID_column)), by = ID_column) 

# Find which students EVER attended a Go8 (they may have also attended another institution)
Go8_students <- Institutions_and_networks_all %>% filter(Network %in% "Group_of_Eight") %>% 
  filter(Age <= max_age, !is.na(get(paste0("XBAC",year)))) %>% # drop observation if it is above max age and if we don't know if that person went to uni that year (i.e. they either attrited or didn't respond to those questions)
  filter(!duplicated(get(ID_column))) %>% select(one_of(ID_column),Network) %>% rename(Uni_attend_age_22_Go8 = Network)

# Find which students NEVER attended a Go8 (all students excluding the Go8 ones already identified)
Other_students <- Institutions_and_networks_all %>% filter(Network != "Group_of_Eight") %>% 
  filter(Age <= max_age, !is.na(get(paste0("XBAC",year)))) %>% # drop observation if it is above max age and if we don't know if that person went to uni that year (i.e. they either attrited or didn't respond to those questions)
  filter(get(ID_column) %ni% (Go8_students %>% pull(ID_column))) %>% # ignore ID's if they went to Go8 
  filter(!duplicated(get(ID_column))) %>% select(one_of(ID_column)) %>% mutate(Uni_attend_age_22_Go8 = "Other_university_only")

Uni_network_data <- bind_rows(Go8_students,Other_students) %>% 
  left_join(LSAY_data,., by = ID_column) %>% pull(Uni_attend_age_22_Go8)


return(Uni_network_data)
}


Postcode_location <- read_csv("postcode_location.csv", col_names = TRUE) %>% na.omit() %>% 
  mutate(postcode = as.character(postcode), 
         postcode_4digit = ifelse(nchar(postcode) <4,
                                  paste0("0",postcode),postcode), 
         Location = ifelse(metro==1,"Metropolitan", 
                                     ifelse(regional ==1 | remote == 1, "Regional and remote",NA)),
         Regional_remote = regional+remote)

# Recode locations to 2006 ASGS

ASGS_correspondance <- read.csv("CP2006RA_2006POA.csv", header = TRUE) # population weighted areas. Downloaded from https://data.gov.au/dataset/ds-dga-c9c33a59-f716-4c5a-b519-56ab178fc50b/details?q=ASGS%20%20correspondences%202006

ASGS_correspondance <- ASGS_correspondance %>% 
  mutate(Location = ifelse(RA_2006_NAME %in% "Major Cities of Australia","Metropolitan",
                           ifelse(RA_2006_NAME %in% 
                                    c("Inner Regional Australia","Outer Regional Australia","Remote Australia","Very Remote Australia"),
                                  "Regional_remote",NA))) %>% 
  group_by(POA_2006_CODE,Location) %>% summarise(PERCENT=sum(PERCENT)) %>% group_by(POA_2006_CODE) %>% 
  filter(PERCENT == max(PERCENT))



##########----------------------------------------------------------------------------------------------------##########
#         VARIABLE CREATION
##########----------------------------------------------------------------------------------------------------##########

# Initiate cohort_data with student IDs
LSAY_1995_data <- as_tibble(LSAY_1995) %>% select(stuidno)
LSAY_1998_data <- as_tibble(LSAY_1998) %>% select(stuidno)
LSAY_2003_data <- as_tibble(LSAY_2003) %>% select(STIDSTD) 
LSAY_2006_data <- as_tibble(LSAY_2006) %>% select(STIDSTD) 
LSAY_2009_data <- as_tibble(LSAY_2009) %>% select(STIDSTD) %>% mutate(STIDSTD = as.numeric(STIDSTD)) 
LSAY_2015_data <- as_tibble(LSAY_2015) %>% select(STUDENID) %>% mutate(STUDENID = as.numeric(STUDENID)) 

# Create a numeric ID field
Cohorts <- c(1995,1998,2003,2006,2009,2015)
for (Cohort in Cohorts) { 
ID <- if (Cohort %in% c(1995,1998)) "stuidno" else if (Cohort %in% c(2003,2006,2009)) "STIDSTD" else if (Cohort %in% c(2015)) "STUDENID"
assign(paste0("LSAY_",Cohort,"_data"), get(paste0("LSAY_",Cohort,"_data")) %>% mutate(ID = as.numeric(get(ID))))
}       

# School IDs
LSAY_1995_data$schno <- LSAY_1995$schno
LSAY_1998_data$SCHNO_R <- LSAY_1998$SCHNO_R
LSAY_2003_data$SCHID_R <- LSAY_2003$SCHID_R
LSAY_2006_data$SCHOOLID <- LSAY_2006$SCHOOLID
LSAY_2009_data$SCHOOLID <- LSAY_2009$SCHOOLID
LSAY_2015_data$CNTSCHID <- LSAY_2015$CNTSCHID

LSAY_1995_data$Cohort <- 1995
LSAY_1998_data$Cohort <- 1998
LSAY_2003_data$Cohort <- 2003
LSAY_2006_data$Cohort <- 2006
LSAY_2009_data$Cohort <- 2009
LSAY_2015_data$Cohort <- 2015


LSAY_2003_data$Sample_weight <- LSAY_2003$W_FSTUWT
LSAY_2006_data$Sample_weight <- LSAY_2006$W_FSTUWT
LSAY_2009_data$Sample_weight <- LSAY_2009$W_FSTUWT

First_year_function <- function(cohort = 1995, years = c(1996:2006), variable = "IN", missing = NA) {
  #Variable names must be in the format "variableyyyy"
  LSAY <- get(paste0("LSAY_",cohort))
  LSAY_data <- get(paste0("LSAY_",cohort,"_data"))
  
  First_year <- sapply(c(1:nrow(LSAY)), 
                       function(x) min(which(LSAY[x,paste0(variable,years)] %ni% 1))) 
  First_year <- years[First_year]
  First_year <- ifelse(is.na(First_year),missing,First_year)
  return(First_year)
}
  
LSAY_1995_data$Year_of_attrition <- First_year_function(cohort = 1995, years = c(1996:2006), variable = "IN", missing = 9999)
LSAY_1998_data$Year_of_attrition <- First_year_function(cohort = 1998, years = c(1998:2009), variable = "IN", missing = 9999)
LSAY_2003_data$Year_of_attrition <- First_year_function(cohort = 2003, years = c(2003:2013), variable = "IN", missing = 9999)
LSAY_2006_data$Year_of_attrition <- First_year_function(cohort = 2006, years = c(2006:2016), variable = "IN", missing = 9999)
LSAY_2009_data$Year_of_attrition <- First_year_function(cohort = 2009, years = c(2009:2017), variable = "IN", missing = 9999)


# Fix problem with Uni degree attendance for 1995 cohort. Note: prior to 2002 the code doesn't split category 5 (completed and undertaking higher studies)
LSAY_1995_Fixed <-  as.data.frame(LSAY_1995$stuidno)
colnames(LSAY_1995_Fixed)[1] <- "stuidno"
LSAY_1995_Fixed$XBAC1999 <- ifelse(LSAY_1995$IN1999 %in% 1 & ((LSAY_1995$EC010 %in% c(1,2) & LSAY_1995$XBAC1998 %in% 1) | LSAY_1995$EC013 %in% 1), 1,
                                   ifelse(LSAY_1995$IN1999 %in% 1 & ((LSAY_1995$XBAC1998 %in% 1 & LSAY_1995$EC011 %in% 1) | LSAY_1995$XBAC1998 %in% 2), 2,
                                          ifelse(LSAY_1995$IN1999 %in% 1 & ((LSAY_1995$XBAC1998 %in% 1 & LSAY_1995$EC011 %in% c(2,3,5,6)) |
                                                                              LSAY_1995$XBAC1998 %in% 3),3,
                                                 ifelse(LSAY_1995$IN1999 %in% 1 & (LSAY_1995$EA001 %in% 1 | LSAY_1995$EA002 %in% 2 |
                                                                                     LSAY_1995$EC010 %in% 6 | LSAY_1995$EC013 %in% 9), 4,
                                                        ifelse(LSAY_1995$IN1999 %in% 0 | is.na(LSAY_1995$IN1999) %in% TRUE,NA,LSAY_1995$XBAC1998)))))


LSAY_1995_Fixed$XBAC2000 <- ifelse(LSAY_1995$IN2000 %in% 1 & (LSAY_1995$FC018 %in% c(12,13) | LSAY_1995$XBAC1999 %in%  1 & LSAY_1995$FC011 %in% c(1,2)),1,
                                   ifelse(LSAY_1995$IN2000 %in%1 & (((LSAY_1995$FC001 %in%  1 | LSAY_1995$FC002 %in%  1) & LSAY_1995$FC005 %in% c(12,13)) | 
                                                                      LSAY_1995$XBAC1999 %in%  2),2,
                                          ifelse(LSAY_1995$IN2000 %in%  1 & (LSAY_1995$FC012 %in%  1 &  LSAY_1995$XBAC1999 %in%  1),2,
                                                 ifelse(LSAY_1995$IN2000 %in%  1 & ((LSAY_1995$FB003 %in%  2 & LSAY_1995$FB005 %in% c(12,13)) | 
                                                                                      (LSAY_1995$FC012 %in%  2 & LSAY_1995$XBAC1999 %in%  1) | LSAY_1995$XBAC1999 %in%  3),3,
                                                        ifelse(LSAY_1995$IN2000 %in%1 & (LSAY_1995$FA001 %in%  1 | LSAY_1995$FC015 %in%  4),4,
                                                               ifelse(LSAY_1995$IN2000 %in% 0 | is.na(LSAY_1995$IN2000) %in% TRUE,NA,LSAY_1995_Fixed$XBAC1999))))))


LSAY_1995_Fixed$XBAC2001 <-  ifelse(LSAY_1995$IN2001 %in% 1 & ((LSAY_1995$GCA004 %in% c(11,12,13) & LSAY_1995$GCA005 %in% 1) |
                                                                 (LSAY_1995$GCA004 %in% c(11,12,13) & LSAY_1995$GCA011 %in% 1) |
                                                                 (LSAY_1995$GCC005 %in% c(11,12,13) & LSAY_1995$GCC014 %in% 1) |
                                                                 (LSAY_1995$GCC033 %in% c(11,12,13) & LSAY_1995$GCC042 %in% 1) |
                                                                 (LSAY_1995$GC2A004 %in% c(11,12,13) & LSAY_1995$GC2A005 %in% 1) |
                                                                 (LSAY_1995$GC2A004 %in% c(11,12,13) & LSAY_1995$GC2A011 %in% 1) |
                                                                 (LSAY_1995$GC2C005 %in% c(11,12,13) & LSAY_1995$GC2C014 %in% 1) |
                                                                 (LSAY_1995$GC3A004 %in% c(11,12,13) & LSAY_1995$GC3A005 %in% 1) |
                                                                 (LSAY_1995$GC3A004 %in% c(11,12,13) & LSAY_1995$GC3A011 %in% 1) |
                                                                 (LSAY_1995$GC3C005 %in% c(11,12,13) & LSAY_1995$GC3C014 %in% 1) |
                                                                 (LSAY_1995$GC084 %in% c(11,12,13))),1,
                                    ifelse(LSAY_1995$IN2001 %in% 1 & ((LSAY_1995$GCA004 %in% c(11,12,13) & LSAY_1995$GCA013 %in% 1) |
                                                                        (LSAY_1995$GCC005 %in% c(11,12,13) & LSAY_1995$GCC021 %in% 1) |
                                                                        (LSAY_1995$GCC033 %in% c(11,12,13) & LSAY_1995$GCC049 %in% 1) |
                                                                        (LSAY_1995$GC2A004 %in% c(11,12,13) & LSAY_1995$GC2A013 %in% 1) |
                                                                        (LSAY_1995$GC2C005 %in% c(11,12,13) & LSAY_1995$GC2C021 %in% 1) |
                                                                        (LSAY_1995$GC3A004 %in% c(11,12,13) & LSAY_1995$GC3A013A %in% 1) |
                                                                        (LSAY_1995$GC3C005 %in% c(11,12,13) & LSAY_1995$GC3C021 %in% 1) | 
                                                                        LSAY_1995$XBAC2000 %in% 2),2,
                                           ifelse(LSAY_1995$IN2001 %in% 1 & ((LSAY_1995$GCA004 %in% c(11,12,13) & LSAY_1995$GCA013 %in% c(2,3,4)) |
                                                                               (LSAY_1995$GCC005 %in% c(11,12,13) & LSAY_1995$GCC021 %in% c(2,3,4)) |
                                                                               (LSAY_1995$GCC033 %in% c(11,12,13) & LSAY_1995$GCC049 %in% c(2,3,4)) |
                                                                               (LSAY_1995$GC2A004 %in% c(11,12,13) & LSAY_1995$GC2A013 %in% c(2,3,4)) |
                                                                               (LSAY_1995$GC2C005 %in% c(11,12,13) & LSAY_1995$GC2C021 %in% c(2,3,4)) |
                                                                               (LSAY_1995$GC3A004 %in% c(11,12,13) & LSAY_1995$GC3A013A %in% c(2,3,4)) |
                                                                               (LSAY_1995$GC3C005 %in% c(11,12,13) & LSAY_1995$GC3C021 %in% c(2,3,4)) |
                                                                               LSAY_1995$XBAC2000 %in% 3),3,
                                                  ifelse(LSAY_1995$IN2001 %in% 1 & (LSAY_1995$GA001 %in% 1 | LSAY_1995$GCA001 %in% 0),4,
                                                         ifelse(LSAY_1995$IN2001 %in% 0 | is.na(LSAY_1995$IN2001) %in% TRUE,NA,LSAY_1995$XBAC2000)))))

LSAY_1995_Fixed$XBAC2002 <- ifelse(LSAY_1995$IN2002 %in% 1 & LSAY_1995$XCEL2002 %in% c(9,10,11) & !(LSAY_1995$XHEL2002 %in% c(9,10,11)), 1,
                                   ifelse(LSAY_1995$IN2002 %in% 1 & LSAY_1995$XHEL2002 %in% c(9,10,11) & !(LSAY_1995$XCEL2002 %in% c(9,10,11)), 2,
                                          ifelse(LSAY_1995$IN2002 %in% 1 & ((LSAY_1995$HCA010 %in% c(11,12,13) & LSAY_1995$HCA020 %in% c(2,3,4) & !(LSAY_1995$XHEL2002 %in% c(9,10,11)))
                                                                            | LSAY_1995$XBAC2001 %in% 3 | (LSAY_1995$HCA020 %in% c(2,3,4) & LSAY_1995$XCEL2001 %in% c(9,10,11))
                                                                            & !(LSAY_1995$XCEL2002 %in% c(9,10,11))),3,
                                                 ifelse(LSAY_1995$IN2002 %in% 1 & (LSAY_1995$XCEL2002 %in% c(9,10,11) & LSAY_1995$XHEL2002 %in% c(9,10,11)), 5,
                                                        ifelse(LSAY_1995$HCA007 %in% 0 & LSAY_1995$XBAC2001 %in% 4 & !(LSAY_1995$XCEL2002 %in% c(9,10,11)),4,
                                                               ifelse(LSAY_1995$IN2002 %in% 0 | is.na(LSAY_1995$IN2002) %in% TRUE,NA,LSAY_1995_Fixed$XBAC2001))))))


LSAY_1995_Fixed$XBAC2003 <- ifelse(LSAY_1995$IN2003 %in% 1 & LSAY_1995$XCEL2003 %in% c(9,10,11) & !(LSAY_1995$XHEL2003 %in% c(9,10,11)), 1,
                                   ifelse(LSAY_1995$IN2003 %in% 1 & LSAY_1995$XHEL2003 %in% c(9,10,11) & !(LSAY_1995$XCEL2003 %in% c(9,10,11)), 2,
                                          ifelse(LSAY_1995$IN2003 %in% 1 & ((LSAY_1995$ICA010 %in% c(11,12,13) & LSAY_1995$ICA020 %in% c(2,3,4) & !(LSAY_1995$XHEL2003 %in% c(9,10,11)))
                                                                            | LSAY_1995$XBAC2002 %in% 3 | (LSAY_1995$ICA020 %in% c(2,3,4) & LSAY_1995$XCEL2002 %in% c(9,10,11))
                                                                            & !(LSAY_1995$XCEL2003 %in% c(9,10,11))),3,
                                                 ifelse(LSAY_1995$IN2003 %in% 1 & (LSAY_1995$XCEL2003 %in% c(9,10,11) & LSAY_1995$XHEL2003 %in% c(9,10,11)), 5,
                                                        ifelse(LSAY_1995$ICA007A %in% 0 & LSAY_1995$XBAC2002 %in% 4 & !(LSAY_1995$XCEL2003 %in% c(9,10,11)),4,
                                                               ifelse(LSAY_1995$IN2003 %in% 0 | is.na(LSAY_1995$IN2003) %in% TRUE,NA,LSAY_1995_Fixed$XBAC2002))))))

LSAY_1995_Fixed$XBAC2004 <- ifelse(LSAY_1995$IN2004 %in% 1 & LSAY_1995$XCEL2004 %in% c(9,10,11) & !(LSAY_1995$XHEL2004 %in% c(9,10,11)), 1,
                                   ifelse(LSAY_1995$IN2004 %in% 1 & LSAY_1995$XHEL2004 %in% c(9,10,11) & !(LSAY_1995$XCEL2004 %in% c(9,10,11)), 2,
                                          ifelse(LSAY_1995$IN2004 %in% 1 & ((LSAY_1995$JCA010 %in% c(11,12,13) & LSAY_1995$JCA020 %in% c(2,3,4) & !(LSAY_1995$XHEL2004 %in% c(9,10,11)))
                                                                            | LSAY_1995$XBAC2003 %in% 3 | (LSAY_1995$JCA020 %in% c(2,3,4) & LSAY_1995$XCEL2003 %in% c(9,10,11))
                                                                            & !(LSAY_1995$XCEL2004 %in% c(9,10,11))),3,
                                                 ifelse(LSAY_1995$IN2004 %in% 1 & (LSAY_1995$XCEL2004 %in% c(9,10,11) & LSAY_1995$XHEL2004 %in% c(9,10,11)), 5,
                                                        ifelse(LSAY_1995$ICA007A %in% 0 & LSAY_1995$XBAC2003 %in% 4 & !(LSAY_1995$XCEL2004 %in% c(9,10,11)),4,
                                                               ifelse(LSAY_1995$IN2004 %in% 0 | is.na(LSAY_1995$IN2004) %in% TRUE,NA,LSAY_1995_Fixed$XBAC2003))))))

LSAY_1995_Fixed$XBAC2005 <- ifelse(LSAY_1995$IN2005 %in% 1 & LSAY_1995$XCEL2005 %in% c(9,10,11) & !(LSAY_1995$XHEL2005 %in% c(9,10,11)), 1,
                                   ifelse(LSAY_1995$IN2005 %in% 1 & LSAY_1995$XHEL2005 %in% c(9,10,11) & !(LSAY_1995$XCEL2005 %in% c(9,10,11)), 2,
                                          ifelse(LSAY_1995$IN2005 %in% 1 & ((LSAY_1995$KCA010 %in% c(11,12,13) & LSAY_1995$KCA020 %in% c(2,3,4) & !(LSAY_1995$XHEL2005 %in% c(9,10,11)))
                                                                            | LSAY_1995$XBAC2004 %in% 3 | (LSAY_1995$KCA020 %in% c(2,3,4) & LSAY_1995$XCEL2004 %in% c(9,10,11))
                                                                            & !(LSAY_1995$XCEL2005 %in% c(9,10,11))),3,
                                                 ifelse(LSAY_1995$IN2005 %in% 1 & (LSAY_1995$XCEL2005 %in% c(9,10,11) & LSAY_1995$XHEL2005 %in% c(9,10,11)), 5,
                                                        ifelse(LSAY_1995$ICA007A %in% 0 & LSAY_1995$XBAC2004 %in% 4 & !(LSAY_1995$XCEL2005 %in% c(9,10,11)),4,
                                                               ifelse(LSAY_1995$IN2005 %in% 0 | is.na(LSAY_1995$IN2005) %in% TRUE,NA,LSAY_1995_Fixed$XBAC2004))))))


LSAY_1995_Fixed$XBAC2006 <- ifelse(LSAY_1995$IN2006 %in% 1 & LSAY_1995$XCEL2006 %in% c(9,10,11) & !(LSAY_1995$XHEL2006 %in% c(9,10,11)), 1,
                                   ifelse(LSAY_1995$IN2006 %in% 1 & LSAY_1995$XHEL2006 %in% c(9,10,11) & !(LSAY_1995$XCEL2006 %in% c(9,10,11)), 2,
                                          ifelse(LSAY_1995$IN2006 %in% 1 & ((LSAY_1995$LCA010 %in% c(11,12,13) & LSAY_1995$LCA020 %in% c(2,3,4) & !(LSAY_1995$XHEL2006 %in% c(9,10,11)))
                                                                            | LSAY_1995$XBAC2005 %in% 3 | (LSAY_1995$LCA020 %in% c(2,3,4) & LSAY_1995$XCEL2005 %in% c(9,10,11))
                                                                            & !(LSAY_1995$XCEL2006 %in% c(9,10,11))),3,
                                                 ifelse(LSAY_1995$IN2006 %in% 1 & (LSAY_1995$XCEL2006 %in% c(9,10,11) & LSAY_1995$XHEL2006 %in% c(9,10,11)), 5,
                                                        ifelse(LSAY_1995$ICA007A %in% 0 & LSAY_1995$XBAC2005 %in% 4 & !(LSAY_1995$XCEL2006 %in% c(9,10,11)),4,
                                                               ifelse(LSAY_1995$IN2006 %in% 0 | is.na(LSAY_1995$IN2006) %in% TRUE,NA,LSAY_1995_Fixed$XBAC2005))))))



Year_age_22 <- 2003
Year_age_19 <- 2000
Final_year <- 2006

LSAY_1995_data <- left_join(LSAY_1995_data, 
                            LSAY_1995_Fixed %>% 
                              mutate(Uni_attend_age_22 = ifelse(get(paste0("XBAC",Year_age_22)) %in% 4,0,
                                                                ifelse(get(paste0("XBAC",Year_age_22)) <=5,1,NA)),
                                     Uni_attend_all_years = ifelse(get(paste0("XBAC",Final_year)) %in% 4,0,
                                                                   ifelse(get(paste0("XBAC",Final_year)) <=5,1,NA)),
                                     Uni_attend_age_19 = ifelse(get(paste0("XBAC",Year_age_19)) %in% 4,0,
                                                                ifelse(get(paste0("XBAC",Year_age_19)) <=5,1,NA))) %>% 
                            select(stuidno,Uni_attend_age_22,Uni_attend_all_years,Uni_attend_age_19), by = "stuidno")


Year_age_22 <- 2006
Year_age_19 <- 2003
Final_year <- 2009

LSAY_1998_data <- left_join(LSAY_1998_data, 
                            LSAY_1998 %>% 
  mutate(Uni_attend_age_22 = ifelse(get(paste0("XBAC",Year_age_22)) %in% 5,0,
                                    ifelse(get(paste0("XBAC",Year_age_22)) < 5,1,NA)),
         Uni_attend_all_years = ifelse(get(paste0("XBAC",Final_year)) %in% 5,0,
                                       ifelse(get(paste0("XBAC",Final_year)) <5,1,NA)),
         Uni_attend_age_19 = ifelse(get(paste0("XBAC",Year_age_19)) %in% 5,0,
                                    ifelse(get(paste0("XBAC",Year_age_19)) < 5,1,NA))) %>% 
    select(stuidno,Uni_attend_age_22,Uni_attend_all_years,Uni_attend_age_19), by = "stuidno")



Year_age_22 <- 2010
Year_age_19 <- 2007
Year_age_23 <- 2011
Final_year <- 2013

LSAY_2003_data <- left_join(LSAY_2003_data, 
                            LSAY_2003 %>% 
                              mutate(Uni_attend_age_22 = ifelse(get(paste0("XBAC",Year_age_22)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_22)) < 5,1,NA)),
                                     Uni_attend_all_years = ifelse(get(paste0("XBAC",Final_year)) %in% 5,0,
                                                                   ifelse(get(paste0("XBAC",Final_year)) <5,1,NA)),
                                     Uni_attend_age_19 = ifelse(get(paste0("XBAC",Year_age_19)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_19)) < 5,1,NA)),
                                     Uni_attend_age_23 = ifelse(get(paste0("XBAC",Year_age_23)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_23)) < 5,1,NA))) %>% 
                              select(STIDSTD,Uni_attend_age_22,Uni_attend_all_years,Uni_attend_age_19,Uni_attend_age_23), by = "STIDSTD")


Year_age_22 <- 2013
Year_age_23 <- 2014
Year_age_19 <- 2010
Final_year <- 2016

LSAY_2006_data <- left_join(LSAY_2006_data, 
                            LSAY_2006 %>% 
                              mutate(Uni_attend_age_22 = ifelse(get(paste0("XBAC",Year_age_22)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_22)) < 5,1,NA)),
                                     Uni_attend_all_years = ifelse(get(paste0("XBAC",Final_year)) %in% 5,0,
                                                                   ifelse(get(paste0("XBAC",Final_year)) <5,1,NA)),
                                     Uni_attend_age_19 = ifelse(get(paste0("XBAC",Year_age_19)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_19)) < 5,1,NA)),
                                     Uni_attend_age_23 = ifelse(get(paste0("XBAC",Year_age_23)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_23)) < 5,1,NA))) %>% 
                              select(STIDSTD,Uni_attend_age_22,Uni_attend_all_years,Uni_attend_age_19,Uni_attend_age_23), by = "STIDSTD")

Year_age_22 <- 2016
Year_age_23 <- 2017
Year_age_19 <- 2013
Final_year <- 2017

LSAY_2009_data <- left_join(LSAY_2009_data, 
                            LSAY_2009 %>% 
                              mutate(STIDSTD = as.numeric(STIDSTD),
                                Uni_attend_age_22 = ifelse(get(paste0("XBAC",Year_age_22)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_22)) < 5,1,NA)),
                                     Uni_attend_all_years = ifelse(get(paste0("XBAC",Final_year)) %in% 5,0,
                                                                   ifelse(get(paste0("XBAC",Final_year)) <5,1,NA)),
                                     Uni_attend_age_19 = ifelse(get(paste0("XBAC",Year_age_19)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_19)) < 5,1,NA)),
                                     Uni_attend_age_23 = ifelse(get(paste0("XBAC",Year_age_23)) %in% 5,0,
                                                                ifelse(get(paste0("XBAC",Year_age_23)) < 5,1,NA))) %>% 
                              select(STIDSTD,Uni_attend_age_22,Uni_attend_all_years,Uni_attend_age_19,Uni_attend_age_23), by = "STIDSTD")

LSAY_1995_data$Uni_attend_age_22_Go8 <- University_networks_function(cohort = 1995, max_age = 22)
LSAY_1998_data$Uni_attend_age_22_Go8 <- University_networks_function(cohort = 1998, max_age = 22)
LSAY_2003_data$Uni_attend_age_22_Go8 <- University_networks_function(cohort = 2003, max_age = 22)
LSAY_2006_data$Uni_attend_age_22_Go8 <- University_networks_function(cohort = 2006, max_age = 22)
LSAY_2009_data$Uni_attend_age_22_Go8 <- University_networks_function(cohort = 2009, max_age = 22)

LSAY_1995_data$Uni_attend_age_19_Go8 <- University_networks_function(cohort = 1995, max_age = 19)
LSAY_1998_data$Uni_attend_age_19_Go8 <- University_networks_function(cohort = 1998, max_age = 19)
LSAY_2003_data$Uni_attend_age_19_Go8 <- University_networks_function(cohort = 2003, max_age = 19)
LSAY_2006_data$Uni_attend_age_19_Go8 <- University_networks_function(cohort = 2006, max_age = 19)
LSAY_2009_data$Uni_attend_age_19_Go8 <- University_networks_function(cohort = 2009, max_age = 19)

cols <- c("Uni_attend_age_22","Uni_attend_all_years","Uni_attend_age_19","Uni_attend_age_22_Go8","Uni_attend_age_19_Go8")
LSAY_1995_data[,cols] <- lapply(LSAY_1995_data[,cols], factor)
LSAY_1998_data[,cols] <- lapply(LSAY_1998_data[,cols], factor)
cols <- c("Uni_attend_age_22","Uni_attend_all_years","Uni_attend_age_19","Uni_attend_age_22_Go8","Uni_attend_age_19_Go8","Uni_attend_age_23")
LSAY_2003_data[,cols] <- lapply(LSAY_2003_data[,cols], factor)
LSAY_2006_data[,cols] <- lapply(LSAY_2006_data[,cols], factor)
LSAY_2009_data[,cols] <- lapply(LSAY_2009_data[,cols], factor)

# 
LSAY_1995_data$Uni_complete_age_23 <- ifelse(LSAY_1995$XBAC2004 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_1995_data$Uni_complete_age_25 <- ifelse(LSAY_1995$XBAC2006 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_1998_data$Uni_complete_age_23 <- ifelse(LSAY_1998$XBAC2007 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_1998_data$Uni_complete_age_25 <- ifelse(LSAY_1998$XBAC2009 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2003_data$Uni_complete_age_23 <- ifelse(LSAY_2003$XBAC2011 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2003_data$Uni_complete_age_25 <- ifelse(LSAY_2003$XBAC2013 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2006_data$Uni_complete_age_23 <- ifelse(LSAY_2006$XBAC2014 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2006_data$Uni_complete_age_25 <- ifelse(LSAY_2006$XBAC2016 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2009_data$Uni_complete_age_23 <- ifelse(LSAY_2009$XBAC2017 == 2, 1,0) # does not include complete and undertaking postgraduate
LSAY_2009_data$Uni_complete_age_25 <- NA

LSAY_1995_data$Uni_dropout_age_22 <- ifelse(LSAY_1995$XBAC2003 == 3, 1,0)
LSAY_1995_data$Uni_dropout_age_25 <- ifelse(LSAY_1995$XBAC2006 == 3, 1,0)

LSAY_1998_data$Uni_dropout_age_22 <- ifelse(LSAY_1998$XBAC2006 == 4, 1,0)
LSAY_1998_data$Uni_dropout_age_25 <- ifelse(LSAY_1998$XBAC2009 == 4, 1,0)

LSAY_2003_data$Uni_dropout_age_22 <- ifelse(LSAY_2003$XBAC2010 == 4, 1,0)
LSAY_2003_data$Uni_dropout_age_23 <- ifelse(LSAY_2003$XBAC2011 == 4, 1,0)
LSAY_2003_data$Uni_dropout_age_25 <- ifelse(LSAY_2003$XBAC2013 == 4, 1,0)

LSAY_2006_data$Uni_dropout_age_22 <- ifelse(LSAY_2006$XBAC2013 == 4, 1,0)
LSAY_2006_data$Uni_dropout_age_23 <- ifelse(LSAY_2006$XBAC2014 == 4, 1,0)
LSAY_2006_data$Uni_dropout_age_25 <- ifelse(LSAY_2006$XBAC2016 == 4, 1,0)

LSAY_2009_data$Uni_dropout_age_22 <- ifelse(LSAY_2009$XBAC2016 == 4, 1,0)
LSAY_2009_data$Uni_dropout_age_23 <- ifelse(LSAY_2009$XBAC2017 == 4, 1,0)
LSAY_2009_data$Uni_dropout_age_25 <- NA


Years_2009_cohort <- c(2009:2017)
Years_2003_cohort <- c(2003:2011)
Years_2006_cohort <- c(2009:2014)
Years_2009_cohort <- c(2009:2017)

LSAY_2003_data$Futher_studies_age_23 <-  sapply(c(1:nrow(LSAY_2003)), 
                                                function(x) min(which(LSAY_2003[x,paste0("XBAC",Years_2003_cohort)] == 3))) 
LSAY_2003_data$Futher_studies_age_23 <- Years_2003_cohort[LSAY_2003_data$Futher_studies_age_23] 

LSAY_2006_data$Futher_studies_age_23 <-  sapply(c(1:nrow(LSAY_2006)), 
                                                function(x) min(which(LSAY_2006[x,paste0("XBAC",Years_2006_cohort)] == 3))) 
LSAY_2006_data$Futher_studies_age_23 <- Years_2006_cohort[LSAY_2006_data$Futher_studies_age_23] 

LSAY_2009_data$Futher_studies_age_23 <-  sapply(c(1:nrow(LSAY_2009)), 
                                                function(x) min(which(LSAY_2009[x,paste0("XBAC",Years_2009_cohort)] == 3)))
LSAY_2009_data$Futher_studies_age_23 <- Years_2009_cohort[LSAY_2009_data$Futher_studies_age_23] 


# University completion at age 25 (age 24 in 2009 cohort)
LSAY_1995_data$Uni_completion <- as.factor(sapply(LSAY_1995$XBAC2006,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 5, "Complete and undertaking further studies",
                       ifelse(x == 3, "Dropped out",
                              ifelse(x == 4, "Never commenced",NA)))))}))
  
LSAY_1998_data$Uni_completion <- as.factor(sapply(LSAY_1998$XBAC2009,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2003_data$Uni_completion <- as.factor(sapply(LSAY_2003$XBAC2013,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2006_data$Uni_completion <- as.factor(sapply(LSAY_2006$XBAC2016,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2009_data$Uni_completion <- as.factor(sapply(LSAY_2009$XBAC2017,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))


LSAY_2003_data$Uni_completion_age_22 <- as.factor(sapply(LSAY_2003$XBAC2010,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2006_data$Uni_completion_age_22 <- as.factor(sapply(LSAY_2006$XBAC2013,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2009_data$Uni_completion_age_22 <- as.factor(sapply(LSAY_2009$XBAC2016,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

#
LSAY_1995_data$Uni_completion_age_23 <- as.factor(sapply(LSAY_1995$XBAC2004,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 5, "Complete and undertaking further studies",
                       ifelse(x == 3, "Dropped out",
                              ifelse(x == 4, "Never commenced",NA)))))}))

LSAY_1998_data$Uni_completion_age_23 <- as.factor(sapply(LSAY_1998$XBAC2007,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))


LSAY_2003_data$Uni_completion_age_23 <- as.factor(sapply(LSAY_2003$XBAC2011,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2003_data$Uni_completion_age_25 <- as.factor(sapply(LSAY_2003$XBAC2013,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2006_data$Uni_completion_age_23 <- as.factor(sapply(LSAY_2006$XBAC2014,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2006_data$Uni_completion_age_25 <- as.factor(sapply(LSAY_2006$XBAC2016,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_2009_data$Uni_completion_age_23 <- as.factor(sapply(LSAY_2009$XBAC2017,  function(x) {
  ifelse(x == 1, "Currently undertaking",
         ifelse(x == 2, "Complete",
                ifelse(x == 3, "Complete and undertaking further studies",
                       ifelse(x == 4, "Dropped out",
                              ifelse(x == 5, "Never commenced",NA)))))}))

LSAY_1995_data$Aspiration <- ifelse(LSAY_1995$AB010 %in% 0,"Not university", 
                                    ifelse(LSAY_1995$AB011 %in% 1, "University", 
                                           ifelse(LSAY_1995$AB011 %in% 0,NA,
                                                  ifelse(LSAY_1995$AB011 <5, "Not university",NA))))

LSAY_1998_data$Aspiration <- ifelse(LSAY_1998$AB003A %in% 0,"Not university", 
                                    ifelse(LSAY_1998$AB003B %in% 1, "University", 
                                           ifelse(LSAY_1998$AB003B %in% 0,NA,
                                                  ifelse(LSAY_1998$AB003B <5, "Not university",NA))))

LSAY_2003_data$Aspiration <- as.factor(ifelse(LSAY_2003$LAA005 %in% 1 | LSAY_2003$LAA009 %in% 1, "University", 
                                              ifelse(LSAY_2003$LAA005 < 12, "Not university",NA)))

LSAY_2006_data$Aspiration <- as.factor(ifelse(LSAY_2006$ST48N01 %in% 1 | LSAY_2006$ST49N01 %in% 2, "University", 
                                              ifelse(LSAY_2006$ST48N01 < 10, "Not university",NA)))

LSAY_2009_data$Aspiration <- as.factor(ifelse(LSAY_2009$ST65N01 %in% 1 | LSAY_2009$ST66N01 %in% 2, "University", 
                                              ifelse(LSAY_2009$ST65N01 < 13, "Not university",NA)))


LSAY_2006_data$Highest_school <- LSAY_2006[,paste0("XHSL",2010)]


LSAY_2006_data$School_completed <- ifelse(LSAY_2006_data$Highest_school == 1, "School_complete",
                                          ifelse(LSAY_2006_data$Highest_school == 2, "School_incomplete",
                                                 ifelse(LSAY_2006_data$Highest_school == 3, "School_incomplete",
                                                        ifelse(LSAY_2006_data$Highest_school == 4, "School_incomplete",NA))))

LSAY_2006_data$Highest_school <- ifelse(LSAY_2006_data$Highest_school == 1, "Year 12",
                                        ifelse(LSAY_2006_data$Highest_school == 2, "Year 11",
                                               ifelse(LSAY_2006_data$Highest_school == 3, "Year 10",
                                                      ifelse(LSAY_2006_data$Highest_school == 4, "Year 9 or lower",NA))))


##########----------------------------------------------------------------------------------------------------##########
#         Weighting variables
##########----------------------------------------------------------------------------------------------------##########

LSAY_2003_data$Grade <- LSAY_2003$GRADE # Individuals school grade, relative to modal school year
LSAY_2006_data$Grade <- LSAY_2006$ST01Q01 - getmode(LSAY_2006$ST01Q01)
LSAY_2009_data$Grade <- LSAY_2009$ST01Q01 - getmode(LSAY_2009$ST01Q01)


LSAY_1995_data$HISCED <- sapply(1:nrow(LSAY_1995), function (x) {max(LSAY_1995[x,"EDUC_M"],LSAY_1995[x,"EDUC_F"]) })
LSAY_1995_data$HISCED <- as.factor(ifelse(is.na(LSAY_1995_data$HISCED),9999,LSAY_1995_data$HISCED))


LSAY_1998_data$EDUC_F <- ifelse(LSAY_1998$EDU_FQ4 %in% 1,6, #Uni
                                ifelse(LSAY_1998$EDU_FQ3 %in% 1,5, #Tafe
                                       ifelse(LSAY_1998$EDU_FQ2 %in% 1,4, #Apprentice
                                              ifelse(LSAY_1998$EDU_FSEC %in% 3,3, #Finshed School
                                                     ifelse(LSAY_1998$EDU_FSEC %in% 2,2, #Some School
                                                            ifelse(LSAY_1998$EDU_FSEC %in% 1,1,NA)))))) #No School

LSAY_1998_data$EDUC_M <- ifelse(LSAY_1998$EDU_MQ4 %in% 1,6, #Uni
                                ifelse(LSAY_1998$EDU_MQ3 %in% 1,5, #Tafe
                                       ifelse(LSAY_1998$EDU_MQ2 %in% 1,4, #Apprentice
                                              ifelse(LSAY_1998$EDU_MSEC %in% 3,3, #Finshed School
                                                     ifelse(LSAY_1998$EDU_MSEC %in% 2,2, #Some School
                                                            ifelse(LSAY_1998$EDU_MSEC %in% 1,1,NA)))))) #No School

LSAY_1998_data$HISCED <- sapply(1:nrow(LSAY_1998_data), function (x) {max(LSAY_1998_data[x,"EDUC_M"],LSAY_1998_data[x,"EDUC_F"]) })
LSAY_1998_data$HISCED <- as.factor(ifelse(is.na(LSAY_1998_data$HISCED),9999,LSAY_1998_data$HISCED))

LSAY_1995_data$IMMIG <- as.factor(ifelse(LSAY_1995$COB_MAU %in% 1 | LSAY_1995$COB_FAU %in% 1,"Native", # Parent born in Australia
                                         ifelse(LSAY_1995$COB_SAU %in% 2,"First generation", # student not born in Australia
                                                ifelse(LSAY_1995$COB_SAU %in% 1,"Second generation",NA)))) # student born in Australia

LSAY_1998_data$IMMIG <- as.factor(ifelse(LSAY_1998$COB_MAU %in% 1 | LSAY_1998$COB_FAU %in% 1,"Native", # Parent born in Australia
                                         ifelse(LSAY_1998$COB_SAU %in% 2,"First generation", # student not born in Australia
                                                ifelse(LSAY_1998$COB_SAU %in% 1,"Second generation",NA)))) # student born in Australia




# Hours worked while studying. 
##Hours worked is zero for unemployed and NILF. The 1998 cohort is different in that it doesn't treat these as NA (998) for hours worked, instead we have to get the LFS data and make those guys zero

Data_table <- "LSAY_1995"
Years_1995_cohort <- c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("XHRS",i)] <- ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 998,0, 
                                              ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 999,NA,get(Data_table)[,paste0("XHRS",i)]))
  LSAY_1995_data[,paste0("Hours_worked_at_uni_",i)] <- ifelse(get(Data_table)[,paste0("XBAC",i)] %in% 1,LSAY_1995_data[,paste0("XHRS",i)],NA)
  LSAY_1995_data[,paste0("XHRS",i)] <- NULL
}   

Data_table <- "LSAY_1998"
Years_1998_cohort <- c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("XHRS",i)] <- ifelse(get(Data_table)[,paste0("XLFS",i)] %in% 2 | get(Data_table)[,paste0("XLFS",i)] %in% 3,0, 
                                              ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 999,NA,get(Data_table)[,paste0("XHRS",i)]))
  LSAY_1998_data[,paste0("Hours_worked_at_uni_",i)] <- ifelse(get(Data_table)[,paste0("XBAC",i)] %in% 1,LSAY_1998_data[,paste0("XHRS",i)],NA)
  LSAY_1998_data[,paste0("XHRS",i)] <- NULL
}   

Data_table <- "LSAY_2003"
Years_2003_cohort <- c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("XHRS",i)] <- ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 998,0, 
                                              ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 999,NA,get(Data_table)[,paste0("XHRS",i)]))
  LSAY_2003_data[,paste0("Hours_worked_at_uni_",i)] <- ifelse(get(Data_table)[,paste0("XBAC",i)] %in% 1,LSAY_2003_data[,paste0("XHRS",i)],NA)
  LSAY_2003_data[,paste0("XHRS",i)] <- NULL
}   

Data_table <- "LSAY_2006"
Years_2006_cohort <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("XHRS",i)] <- ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 998,0, 
                                              ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 999,NA,get(Data_table)[,paste0("XHRS",i)]))
  LSAY_2006_data[,paste0("Hours_worked_at_uni_",i)] <- ifelse(get(Data_table)[,paste0("XBAC",i)] %in% 1,LSAY_2006_data[,paste0("XHRS",i)],NA)
  LSAY_2006_data[,paste0("XHRS",i)] <- NULL
}   



Data_table <- "LSAY_2009"
Years_2009_cohort <- c(2009,2010,2011,2012,2013,2014,2015,2016)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("XHRS",i)] <- ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 998,0, 
                                              ifelse(get(Data_table)[,paste0("XHRS",i)] %in% 999,NA,get(Data_table)[,paste0("XHRS",i)]))
  LSAY_2009_data[,paste0("Hours_worked_at_uni_",i)] <- ifelse(get(Data_table)[,paste0("XBAC",i)] %in% 1,LSAY_2009_data[,paste0("XHRS",i)],NA)
  LSAY_2009_data[,paste0("XHRS",i)] <- NULL
}   


Cohort <- 1995
Year_age_22 <- 2003
Data_table <- paste("LSAY_",Cohort,"_data",sep = "")
Data_source <- paste0("LSAY_",Cohort)

LSAY_1995_data$SES <- apply(LSAY_1995[,c("ISEI_MUM","ISEI_F")], 1, max,na.rm = TRUE) # SES score is the max of mother & father
LSAY_1995_data$SES <- as.numeric(ifelse(LSAY_1995_data$SES == "-Inf",NA,LSAY_1995_data$SES))

LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"SES"])),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)[,"SES"])),"WT95GEN"])

LSAY_1995_data$Parent_SES_quartile <- ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1995_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1995_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(get(Data_table)[,"SES"]<svyquantile(~SES, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))


LSAY_1995_data <-  as_tibble(svyby(~SES, by = ~schno, design = LSAY_1995_data.w,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
  rename(School_SES = statistic) %>% 
  right_join(.,LSAY_1995_data,by="schno")


LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"SES"])),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)[,"SES"])),"WT95GEN"])

LSAY_1995_data$School_SES_quartile <- ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1995_data.w, 0.75, ci=FALSE)[1,1],"SQ4",
                                             ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1995_data.w, 0.5, ci=FALSE)[1,1],"SQ3",
                                                    ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"SQ2",
                                                           ifelse(get(Data_table)[,"School_SES"]<svyquantile(~School_SES, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"SQ1",NA))))


LSAY_1995_data$Indigenous <- ifelse(LSAY_1995$indig == 0, "Not indigenous", ifelse(LSAY_1995$indig == 1, "Indigenous",NA))

LSAY_1995_data$Sector <- ifelse(LSAY_1995$schtyp == 1, "Government", 
                                ifelse(LSAY_1995$schtyp == 2, "Catholic",
                                       ifelse(LSAY_1995$schtyp == 3, "Independent",NA)))

LSAY_1995_data$Gender <- ifelse(LSAY_1995$sex == 1, "Male", ifelse(LSAY_1995$sex == 2, "Female",NA))

LSAY_1995_data$Location <- as.factor(ifelse(get(Data_source)$size == 10,"Metropolitan",ifelse(get(Data_source)$size == 20,"Provincial",
                                                                                              ifelse(get(Data_source)$size == 30,"Remote",NA))))

LSAY_1995_data$Achievement <- (LSAY_1995$TOT_MATH + LSAY_1995$TOT_READ )/2

LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)[,"Achievement"])),"WT95GEN"])

LSAY_1995_data$Achievement_quartile <- ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1995_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                              ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1995_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                                     ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                            ifelse(get(Data_table)[,"Achievement"]<svyquantile(~Achievement, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))


LSAY_1995_data$Parent_edu <- ifelse(LSAY_1995$EDUCP_3 == 1, "Uni", ifelse(LSAY_1995$EDUCP_3 == 2 | LSAY_1995$EDUCP_3 == 3 , "No_uni",NA))

# NESB if they arrived <5 years ago and come from a home where a language other than English is spoken. 
# This is the definition used by Department of Education. I have assumed they mean that they are also NESB if they speak multiple languages at home
LSAY_1995_data$NESB <- ifelse(LSAY_1995$COB_SAU %in% 1, "English speaking background", 
                              ifelse(LSAY_1995$COB_ARR %in% 0,NA,
                                     ifelse(LSAY_1995$COB_ARR <= 90 & !is.na(LSAY_1995$COB_ARR),"English speaking background",
                                            ifelse(LSAY_1995$LANG_ENG %in% 1,"English speaking background",
                                                   ifelse(LSAY_1995$LANG_ENG > 1,"Non-English",NA))))) 

LSAY_1995_data$Disability <- as.factor(ifelse(LSAY_1995$disab == 1, "Disability", ifelse(LSAY_1995$disab == 0, "No disability",NA)))

LSAY_1995_data$COB <-  recode(LSAY_1995$COB_SAU,`0`='Other',`1`='Australia') 

Cohort <- 1998
Year_age_22 <- 2006
Data_table <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)

LSAY_1998_data$SES <- apply(LSAY_1998[,c("ANU3MUMR","ANU3DADR")], 1, max,na.rm = TRUE) # SES score is the max of mother & father. For 1998 it is based on ANU3 score
LSAY_1998_data$SES <- as.numeric(ifelse(LSAY_1998_data$SES == "-Inf",NA,LSAY_1998_data$SES))


LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"SES"])),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)[,"SES"])),"WT1998"])

LSAY_1998_data$Parent_SES_quartile <- ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1998_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1998_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(get(Data_table)[,"SES"]<svyquantile(~SES, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))

LSAY_1998_data <-  as_tibble(svyby(~SES, by = ~SCHNO_R, design = LSAY_1998_data.w,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
  rename(School_SES = statistic) %>% 
  right_join(.,LSAY_1998_data,by="SCHNO_R")

LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"SES"])),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)[,"SES"])),"WT1998"])

LSAY_1998_data$School_SES_quartile <- ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1998_data.w, 0.75, ci=FALSE)[1,1],"SQ4",
                                             ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1998_data.w, 0.5, ci=FALSE)[1,1],"SQ3",
                                                    ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"SQ2",
                                                           ifelse(get(Data_table)[,"School_SES"]<svyquantile(~School_SES, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"SQ1",NA))))


LSAY_1998_data$Indigenous <- ifelse(LSAY_1998$indig == 0, "Not indigenous", ifelse(LSAY_1998$indig == 1, "Indigenous",NA))

LSAY_1998_data$Sector <- ifelse(LSAY_1998$schtyp == 1, "Government", 
                                ifelse(LSAY_1998$schtyp == 2, "Catholic",
                                       ifelse(LSAY_1998$schtyp == 3, "Independent",NA)))

LSAY_1998_data$Gender <- ifelse(LSAY_1998$sex == 1, "Male", ifelse(LSAY_1998$sex == 2, "Female",NA))

LSAY_1998_data$Location <- as.factor(ifelse(get(Data_source)$size == 10,"Metropolitan",ifelse(get(Data_source)$size == 20,"Provincial",
                                                                                              ifelse(get(Data_source)$size == 30,"Remote",NA))))

LSAY_1998_data$Achievement <- (LSAY_1998$TOT_MATH + LSAY_1998$TOT_READ )/2


LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)[,"Achievement"])),"WT1998"])

LSAY_1998_data$Achievement_quartile <- ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1998_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                              ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1998_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                                     ifelse(get(Data_table)[,"Achievement"]>=svyquantile(~Achievement, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                            ifelse(get(Data_table)[,"Achievement"]<svyquantile(~Achievement, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))


LSAY_1998_data$Parent_edu <- ifelse(LSAY_1998$EDU_FQ4 == 1, "Uni", 
                                    ifelse(LSAY_1998$EDU_MQ4 == 1,"Uni",
                                           ifelse(is.na(LSAY_1998$EDU_FSEC) == FALSE & is.na(LSAY_1998$EDU_MSEC) == FALSE,"No_uni",NA)))

# NESB if they arrived <5 years ago and come from a home where a language other than English is spoken
# This is the definition used by Department of Education. I have assumed they mean that they are also NESB if they speak multiple languages at home
LSAY_1998_data$NESB <- ifelse(LSAY_1998$COB_SAU %in% 1, "English speaking background", 
                              ifelse(LSAY_1998$COB_ARR %in% 0,NA,
                                     ifelse(LSAY_1998$COB_ARR <= 93 & !is.na(LSAY_1998$COB_ARR),"English speaking background",
                                            ifelse(LSAY_1998$LANG_ENG %in% 1,"English speaking background",
                                                   ifelse(LSAY_1998$LANG_ENG > 1,"Non-English",NA))))) 

LSAY_1998_data$COB <-  recode(LSAY_1998$COB_SAU,`2`='Other',`1`='Australia', `8` = as.character(NA)) 

LSAY_1998_data$Disability <- as.factor(ifelse(LSAY_1998$disab == 1, "Disability", ifelse(LSAY_1998$disab == 0, "No disability",NA)))


Cohort <- 2003
Year_age_22 <- 2010
Data_table <- paste("LSAY_",Cohort,"_data",sep = "")
Data_source <- paste0("LSAY_",Cohort)

LSAY_2003_data$SES <- ifelse(LSAY_2003$HISEI %in% c(97,98,99),NA,LSAY_2003$HISEI)


LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003$WT2003) & !is.na(LSAY_2003_data$SES)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003) & !is.na(LSAY_2003_data$SES)),"WT2003"])

LSAY_2003_data$Parent_SES_quartile <- ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2003_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2003_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(get(Data_table)[,"SES"]<svyquantile(~SES, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))


LSAY_2003_data$PISA <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                                                   "PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)

LSAY_2003_data$PISA_Math <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH")],na.rm=TRUE)
LSAY_2003_data$PISA_Read <- rowMeans(get(Data_source)[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)
LSAY_2003_data$PISA_Science <- rowMeans(get(Data_source)[c("PV1SCIE","PV2SCIE","PV3SCIE","PV4SCIE","PV5SCIE")],na.rm=TRUE)

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003$WT2003P)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003P)),"WT2003P"])

LSAY_2003_data$PISA_quartile <- ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2003_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                       ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2003_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                              ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                     ifelse(get(Data_table)[,"PISA"]<svyquantile(~PISA, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))


LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003$WT2003P) & !is.na(LSAY_2003_data$SES)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003P) & !is.na(LSAY_2003_data$SES)),"WT2003P"]) #There appears to be at least 1 school in 2003 where no student supplied an SES response

LSAY_2003_data <-  as_tibble(svyby(~SES, by = ~SCHID_R, design = LSAY_2003_data.w,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
  rename(School_SES = statistic) %>% 
  right_join(.,LSAY_2003_data,by="SCHID_R")

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2003_data[which(!is.na(LSAY_2003$WT2003) & !is.na(LSAY_2003_data$SES)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003) & !is.na(LSAY_2003_data$SES)),"WT2003"])

LSAY_2003_data$School_SES_quartile <- ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2003_data.w, 0.75, ci=FALSE)[1,1],"SQ4",
                                             ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2003_data.w, 0.5, ci=FALSE)[1,1],"SQ3",
                                                    ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"SQ2",
                                                           ifelse(get(Data_table)[,"School_SES"]<svyquantile(~School_SES, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1],"SQ1",NA))))


LSAY_2003_data$Parent_edu <- as.factor(ifelse(LSAY_2003$HISCED < 6, "No_uni", 
                                              ifelse(LSAY_2003$HISCED == 6, "Uni", NA)))

LSAY_2003_data$Language <- ifelse(LSAY_2003$LANGHOME == 110, "English", ifelse(LSAY_2003$LANGHOME !=110,"NES",NA))

#It seems they cannot choose multiple languages as an option this time
LSAY_2003_data$NESB <- ifelse(LSAY_2003$COB_STD %in% 1101, "English speaking background", # If born in Australia, then ESB
                              ifelse(LSAY_2003$ST15Q04 >= 10 & LSAY_2003$ST15Q04 < 20 & !is.na(LSAY_2003$ST15Q04),"English speaking background", # If came to Australia after age 10, then ESB
                                     ifelse(LSAY_2003$ST16Q01 %in% 1,"English speaking background", # If language spoken at home is English, then ESB
                                            ifelse(LSAY_2003$ST16Q01 <=4,"Non-English",NA)))) # If none of the above and speaks language other than English at home, then NESB

LSAY_2003_data$COB <-  ifelse(LSAY_2003$COB_STD == 1101, "Australia","Other")


LSAY_2003_data$Remote <- ifelse(LSAY_2003$LOC > 6, "Remote", ifelse(LSAY_2003$LOC <=6,"Not_remote",NA))

Cohort <- 2006
Year_age_22 <- 2013
Data_table <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)

LSAY_2006_data$SES <- ifelse(LSAY_2006$HISEI %in% c(97,98,99),NA,LSAY_2006$HISEI)

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2006) & 
                                                                                    !is.na(LSAY_2006_data$SES)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2006) & !is.na(LSAY_2006_data$SES)),"WT2006"])

LSAY_2006_data$Parent_SES_quartile <- ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2006_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2006_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(get(Data_table)[,"SES"]<svyquantile(~SES, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))

LSAY_2006_data$PISA <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                                                   "PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)

LSAY_2006_data$PISA_Math <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH")],na.rm=TRUE)
LSAY_2006_data$PISA_Read <- rowMeans(get(Data_source)[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)
LSAY_2006_data$PISA_Science <- rowMeans(get(Data_source)[c("PV1SCIE","PV2SCIE","PV3SCIE","PV4SCIE","PV5SCIE")],na.rm=TRUE)

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2006P)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2006P)),"WT2006P"])

LSAY_2006_data$PISA_quartile <- ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2006_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                       ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2006_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                              ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                     ifelse(get(Data_table)[,"PISA"]<svyquantile(~PISA, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))

LSAY_2006_data <-  as_tibble(svyby(~SES, by = ~SCHOOLID, design = LSAY_2006_data.w,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
  rename(School_SES = statistic) %>% 
  right_join(.,LSAY_2006_data,by="SCHOOLID")

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2006) & 
                                                                                    !is.na(LSAY_2006_data$SES)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2006) & !is.na(LSAY_2006_data$SES)),"WT2006"])

LSAY_2006_data$School_SES_quartile <- ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2006_data.w, 0.75, ci=FALSE)[1,1],"SQ4",
                                             ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2006_data.w, 0.5, ci=FALSE)[1,1],"SQ3",
                                                    ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"SQ2",
                                                           ifelse(get(Data_table)[,"School_SES"]<svyquantile(~School_SES, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1],"SQ1",NA))))



LSAY_2006_data$Parent_edu <- as.factor(ifelse(LSAY_2006$HISCED < 6, "No_uni", 
                                              ifelse(LSAY_2006$HISCED == 6, "Uni", NA)))
LSAY_2006_data$Language <- ifelse(LSAY_2006$HOMELANG == 1, "English", ifelse(LSAY_2006$HOMELANG ==2,"NES",NA))

#It seems they cannot choose multiple languages as an option this time
LSAY_2006_data$NESB <- ifelse(LSAY_2006$ST11Q01 %in% 1, "English speaking background", # If born in Australia, then ESB
                              ifelse(LSAY_2006$ST11Q04 >=10 & LSAY_2006$ST11Q04 < 20 & !is.na(LSAY_2006$ST11Q04),"English speaking background", # If came to Australia after age 10, then ESB
                                     ifelse(LSAY_2006$ST12Q01 %in% 1,"English speaking background", # If language spoken at home is English, then ESB
                                            ifelse(LSAY_2006$ST16Q01 <=3,"Non-English",NA)))) # If none of the above and speaks language other than English at home, then NESB

LSAY_2006_data$COB <- recode(LSAY_2006$ST11Q01,`2`='Other',`1`='Australia', `7` = as.character(NA),`8` = as.character(NA),`9` = as.character(NA)) 

LSAY_2006_data$Remote <- ifelse(LSAY_2006$GEOLOC_3 ==3, "Remote", ifelse(LSAY_2006$GEOLOC_3 <3,"Not_remote",NA))


LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2015) & 
                                                                                    !is.na(LSAY_2006_data$Real_student_rent)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2015) & !is.na(LSAY_2006_data$Real_student_rent)),"WT2015"])

Cohort <- 2009
Year_age_22 <- 2016
Data_table <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)

LSAY_2009_data$SES  <- ifelse(LSAY_2009$HISEI %in% c(97,98,99),NA,LSAY_2009$HISEI)

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009$WT2009) & 
                                                                                    !is.na(LSAY_2009_data$SES)),]), 
                              weights = LSAY_2009[which(!is.na(LSAY_2009$WT2009) & !is.na(LSAY_2009_data$SES)),"WT2009"])

LSAY_2009_data$Parent_SES_quartile <- ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2009_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2009_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(get(Data_table)[,"SES"]>=svyquantile(~SES, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(get(Data_table)[,"SES"]<svyquantile(~SES, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))

LSAY_2009_data$PISA <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                                                   "PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)

LSAY_2009_data$PISA_Math <- rowMeans(get(Data_source)[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH")],na.rm=TRUE)
LSAY_2009_data$PISA_Read <- rowMeans(get(Data_source)[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ")],na.rm=TRUE)
LSAY_2009_data$PISA_Science <- rowMeans(get(Data_source)[c("PV1SCIE","PV2SCIE","PV3SCIE","PV4SCIE","PV5SCIE")],na.rm=TRUE)

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009$WT2009P)),]), 
                              weights = LSAY_2009[which(!is.na(LSAY_2009$WT2009P)),"WT2009P"])

LSAY_2009_data$PISA_quartile <- ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2009_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                       ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2009_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                              ifelse(get(Data_table)[,"PISA"]>=svyquantile(~PISA, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                     ifelse(get(Data_table)[,"PISA"]<svyquantile(~PISA, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))

LSAY_2009_data <-  as_tibble(svyby(~SES, by = ~SCHOOLID, design = LSAY_2009_data.w,svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE)) %>% 
  rename(School_SES = statistic) %>% 
  right_join(.,LSAY_2009_data,by="SCHOOLID")

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2009_data[which(!is.na(LSAY_2009$WT2009) & 
                                                                                    !is.na(LSAY_2009$HISEI)),]), 
                              weights = LSAY_2009[which(!is.na(LSAY_2009$WT2009) & !is.na(LSAY_2009$HISEI)),"WT2009"])

LSAY_2009_data$School_SES_quartile <- ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2009_data.w, 0.75, ci=FALSE)[1,1],"SQ4",
                                             ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2009_data.w, 0.5, ci=FALSE)[1,1],"SQ3",
                                                    ifelse(get(Data_table)[,"School_SES"]>=svyquantile(~School_SES, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"SQ2",
                                                           ifelse(get(Data_table)[,"School_SES"]<svyquantile(~School_SES, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1],"SQ1",NA))))

LSAY_2009_data$Parent_edu <- ifelse(LSAY_2009$HISCED < 6, "No_uni", ifelse(LSAY_2009$HISCED ==6,"Uni",NA))
LSAY_2009_data$Language <- ifelse(LSAY_2009$LANGHOME == 1, "English", ifelse(LSAY_2009$LANGHOME ==2,"NES",NA))

#It seems they cannot choose multiple languages as an option this time
LSAY_2009_data$NESB <- ifelse(LSAY_2009$ST17Q01 == 1, "English speaking background", # If born in Australia, then ESB
                              ifelse(LSAY_2009$ST18Q01 >=10 & LSAY_2009$ST18Q01 < 20 & !is.na(LSAY_2009$ST18Q01),"English speaking background", # If came to Australia after age 10, then ESB
                                     ifelse(LSAY_2009$ST19Q01 ==1,"English speaking background", # If language spoken at home is English, then ESB
                                            ifelse(LSAY_2009$ST19Q01 <=3,"Non-English",NA)))) # If none of the above and speaks language other than English at home, then NESB

LSAY_2009_data$COB <- recode(LSAY_2009$ST17Q01,`2`='Other',`1`='Australia', `7` = as.character(NA),`8` = as.character(NA),`9` = as.character(NA)) 
  
LSAY_2009_data$Remote <- ifelse(LSAY_2009$GEOLOC ==3, "Remote", ifelse(LSAY_2009$GEOLOC <3,"Not_remote",NA))


#### 2015 cohort

LSAY_2015_data$SES <- ifelse(LSAY_2015$HISEI %in% c(9995,9996,9997,9998,9999),NA,LSAY_2015$HISEI)

LSAY_2015_data$Indigenous <- ifelse(LSAY_2015$INDIG == 0, "Not indigenous", ifelse(LSAY_2015$INDIG == 1, "Indigenous",NA))

LSAY_2015_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2015_data[which(!is.na(LSAY_2015$WT2015P) & 
                                                                                    !is.na(LSAY_2015_data$SES)),]), 
                              weights = LSAY_2015[which(!is.na(LSAY_2015$WT2015P) & !is.na(LSAY_2015_data$SES)),"WT2015P"])

LSAY_2015_data$Parent_SES_quartile <- ifelse(LSAY_2015_data[,"SES"]>=svyquantile(~SES, LSAY_2015_data.w, 0.75, ci=FALSE)[1,1],"PQ4",
                                             ifelse(LSAY_2015_data[,"SES"]>=svyquantile(~SES, LSAY_2015_data.w, 0.5, ci=FALSE)[1,1],"PQ3",
                                                    ifelse(LSAY_2015_data[,"SES"]>=svyquantile(~SES, LSAY_2015_data.w, 0.25, ci=FALSE)[1,1],"PQ2",
                                                           ifelse(LSAY_2015_data[,"SES"]<svyquantile(~SES, LSAY_2015_data.w, 0.25, ci=FALSE)[1,1],"PQ1",NA))))

LSAY_2015_data$PISA <- rowMeans(LSAY_2015[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                                            "PV6MATH","PV7MATH","PV8MATH","PV9MATH","PV10MATH",
                                            "PV1READ","PV2READ","PV3READ","PV4READ","PV5READ",
                                            "PV6READ","PV7READ","PV8READ","PV9READ","PV10READ")],na.rm=TRUE)

LSAY_2015_data$PISA_Math <- rowMeans(LSAY_2015[c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH",
                                                 "PV6MATH","PV7MATH","PV8MATH","PV9MATH","PV10MATH")],na.rm=TRUE)
LSAY_2015_data$PISA_Read <- rowMeans(LSAY_2015[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ",
                                                 "PV6READ","PV7READ","PV8READ","PV9READ","PV10READ")],na.rm=TRUE)
LSAY_2015_data$PISA_Science <- rowMeans(LSAY_2015[c("PV1SCIE","PV2SCIE","PV3SCIE","PV4SCIE","PV5SCIE",
                                                    "PV6SCIE","PV7SCIE","PV8SCIE","PV9SCIE","PV10SCIE")],na.rm=TRUE)

LSAY_2015_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2015_data[which(!is.na(LSAY_2015$WT2015P)),]), 
                              weights = LSAY_2015[which(!is.na(LSAY_2015$WT2015P)),"WT2015P"])

svymean(~PISA_Math,design=LSAY_2015_data.w)
svymean(~PISA_Read,design=LSAY_2015_data.w)
svymean(~PISA_Science,design=LSAY_2015_data.w)

LSAY_2015_data$PISA_quartile <- ifelse(LSAY_2015_data[,"PISA"]>=svyquantile(~PISA, LSAY_2015_data.w, 0.75, ci=FALSE)[1,1],"upper",
                                       ifelse(LSAY_2015_data[,"PISA"]>=svyquantile(~PISA, LSAY_2015_data.w, 0.5, ci=FALSE)[1,1],"mid-upper",
                                              ifelse(LSAY_2015_data[,"PISA"]>=svyquantile(~PISA, LSAY_2015_data.w, 0.25, ci=FALSE)[1,1],"mid-lower",
                                                     ifelse(LSAY_2015_data[,"PISA"]<svyquantile(~PISA, LSAY_2015_data.w, 0.25, ci=FALSE)[1,1],"lower",NA))))
rm(LSAY_2015_data.w)
LSAY_2015_data$Parent_edu <- ifelse(LSAY_2015$HISCED < 6, "No_uni", ifelse(LSAY_2015$HISCED ==6,"Uni",NA))

LSAY_2015_data$Parent_edu <- factor(LSAY_2015_data %>% pull(Parent_edu), levels = c("Uni","No_uni")) 
LSAY_2015_data$Indigenous <- factor(LSAY_2015_data %>% pull(Indigenous), levels = c("Not indigenous","Indigenous" )) 


LSAY_2015_data$Sector <- ifelse(LSAY_2015$SECTOR == 1, "Catholic", 
                                ifelse(LSAY_2015$SECTOR == 2, "Government",
                                       ifelse(LSAY_2015$SECTOR == 3, "Independent",NA)))

LSAY_2015_data$COB <-  recode(LSAY_2015$ST019AQ01T,`1`='Australia',`2`='Other')

# Math and reading PISA quartiles, unweighted

# Quartiles for 1995 already exist in the main LSAY dataset

# 1998
Q4 <- quantile(LSAY_1998$TOT_M_C, c(0.75), type = 1, na.rm = TRUE) # Math quartile
Q3 <- quantile(LSAY_1998$TOT_M_C, c(0.50), type = 1, na.rm = TRUE) # Math quartile
Q2 <- quantile(LSAY_1998$TOT_M_C, c(0.25), type = 1, na.rm = TRUE) # Math quartile

LSAY_1998_data$Achievement_Math_quartile <- as.factor(sapply(LSAY_1998$TOT_M_C,  
                                                             function(x) {ifelse(x >= Q4, "AM_Q4",
                                                                                 ifelse(x >= Q3, "AM_Q3",
                                                                                        ifelse(x >= Q2, "AM_Q2",
                                                                                               ifelse(x < Q2, "AM_Q1",NA))))}))

Q4 <- quantile(LSAY_1998$TOT_R_C, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_1998$TOT_R_C, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_1998$TOT_R_C, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_1998_data$Achievement_Read_quartile <- as.factor(sapply(LSAY_1998$TOT_R_C,  
                                                             function(x) {ifelse(x >= Q4, "AR_Q4",
                                                                                 ifelse(x >= Q3, "AR_Q3",
                                                                                        ifelse(x >= Q2, "AR_Q2",
                                                                                               ifelse(x < Q2, "AR_Q1",NA))))}))


# 2003
Q4 <- quantile(LSAY_2003_data$PISA_Math, c(0.75), type = 1, na.rm = TRUE) # Math quartile
Q3 <- quantile(LSAY_2003_data$PISA_Math, c(0.50), type = 1, na.rm = TRUE) # Math quartile
Q2 <- quantile(LSAY_2003_data$PISA_Math, c(0.25), type = 1, na.rm = TRUE) # Math quartile

LSAY_2003_data$PISA_Math_quartile <- as.factor(sapply(LSAY_2003_data$PISA_Math,  
                                                      function(x) {ifelse(x >= Q4, "PM_Q4",
                                                                          ifelse(x >= Q3, "PM_Q3",
                                                                                 ifelse(x >= Q2, "PM_Q2",
                                                                                        ifelse(x < Q2, "PM_Q1",NA))))}))

Q4 <- quantile(LSAY_2003_data$PISA_Read, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2003_data$PISA_Read, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2003_data$PISA_Read, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2003_data$PISA_Read_quartile <- as.factor(sapply(LSAY_2003_data$PISA_Read,  
                                                      function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                          ifelse(x >= Q3, "PR_Q3",
                                                                                 ifelse(x >= Q2, "PR_Q2",
                                                                                        ifelse(x < Q2, "PR_Q1",NA))))}))

Q4 <- quantile(LSAY_2003_data$PISA_Science, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2003_data$PISA_Science, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2003_data$PISA_Science, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2003_data$PISA_Science_quartile <- as.factor(sapply(LSAY_2003_data$PISA_Science,  
                                                         function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                             ifelse(x >= Q3, "PR_Q3",
                                                                                    ifelse(x >= Q2, "PR_Q2",
                                                                                           ifelse(x < Q2, "PR_Q1",NA))))}))


# 2006
Q4 <- quantile(LSAY_2006_data$PISA_Math, c(0.75), type = 1, na.rm = TRUE) # Math quartile
Q3 <- quantile(LSAY_2006_data$PISA_Math, c(0.50), type = 1, na.rm = TRUE) # Math quartile
Q2 <- quantile(LSAY_2006_data$PISA_Math, c(0.25), type = 1, na.rm = TRUE) # Math quartile

LSAY_2006_data$PISA_Math_quartile <- as.factor(sapply(LSAY_2006_data$PISA_Math,  
                                                      function(x) {ifelse(x >= Q4, "PM_Q4",
                                                                          ifelse(x >= Q3, "PM_Q3",
                                                                                 ifelse(x >= Q2, "PM_Q2",
                                                                                        ifelse(x < Q2, "PM_Q1",NA))))}))

Q4 <- quantile(LSAY_2006_data$PISA_Read, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2006_data$PISA_Read, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2006_data$PISA_Read, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2006_data$PISA_Read_quartile <- as.factor(sapply(LSAY_2006_data$PISA_Read,  
                                                      function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                          ifelse(x >= Q3, "PR_Q3",
                                                                                 ifelse(x >= Q2, "PR_Q2",
                                                                                        ifelse(x < Q2, "PR_Q1",NA))))}))

Q4 <- quantile(LSAY_2006_data$PISA_Science, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2006_data$PISA_Science, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2006_data$PISA_Science, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2006_data$PISA_Science_quartile <- as.factor(sapply(LSAY_2006_data$PISA_Science,  
                                                         function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                             ifelse(x >= Q3, "PR_Q3",
                                                                                    ifelse(x >= Q2, "PR_Q2",
                                                                                           ifelse(x < Q2, "PR_Q1",NA))))}))


# 2009
Q4 <- quantile(LSAY_2009_data$PISA_Math, c(0.75), type = 1, na.rm = TRUE) # Math quartile
Q3 <- quantile(LSAY_2009_data$PISA_Math, c(0.50), type = 1, na.rm = TRUE) # Math quartile
Q2 <- quantile(LSAY_2009_data$PISA_Math, c(0.25), type = 1, na.rm = TRUE) # Math quartile

LSAY_2009_data$PISA_Math_quartile <- as.factor(sapply(LSAY_2009_data$PISA_Math,  
                                                      function(x) {ifelse(x >= Q4, "PM_Q4",
                                                                          ifelse(x >= Q3, "PM_Q3",
                                                                                 ifelse(x >= Q2, "PM_Q2",
                                                                                        ifelse(x < Q2, "PM_Q1",NA))))}))

Q4 <- quantile(LSAY_2009_data$PISA_Read, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2009_data$PISA_Read, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2009_data$PISA_Read, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2009_data$PISA_Read_quartile <- as.factor(sapply(LSAY_2009_data$PISA_Read,  
                                                      function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                          ifelse(x >= Q3, "PR_Q3",
                                                                                 ifelse(x >= Q2, "PR_Q2",
                                                                                        ifelse(x < Q2, "PR_Q1",NA))))}))

Q4 <- quantile(LSAY_2009_data$PISA_Science, c(0.75), type = 1, na.rm = TRUE) # Reading quartile
Q3 <- quantile(LSAY_2009_data$PISA_Science, c(0.50), type = 1, na.rm = TRUE) # Reading quartile
Q2 <- quantile(LSAY_2009_data$PISA_Science, c(0.25), type = 1, na.rm = TRUE) # Reading quartile

LSAY_2009_data$PISA_Science_quartile <- as.factor(sapply(LSAY_2009_data$PISA_Science,  
                                                         function(x) {ifelse(x >= Q4, "PR_Q4",
                                                                             ifelse(x >= Q3, "PR_Q3",
                                                                                    ifelse(x >= Q2, "PR_Q2",
                                                                                           ifelse(x < Q2, "PR_Q1",NA))))}))



Cohort <- 1995
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)
Data_table.w <- paste0("LSAY_",Cohort,"_data.w")

Sector_order <- c("Independent","Catholic","Government") 
LSAY_1995_data$Sector <- factor(get(Data_table) %>% pull(Sector), levels = Sector_order)   # Change the order for regression

Gender_order <- c("Male","Female" ) 
LSAY_1995_data$Gender <- factor(get(Data_table) %>% pull(Gender), levels = Gender_order)   # Change the order for regression

Indigenous_order <- c("Not indigenous","Indigenous" ) 
LSAY_1995_data$Indigenous <- factor(get(Data_table) %>% pull(Indigenous), levels = Indigenous_order)   # Change the order for regression

SES_quartile_order <- c("PQ4","PQ3", "PQ2","PQ1") 
LSAY_1995_data$Parent_SES_quartile <- factor(get(Data_table) %>% pull(Parent_SES_quartile), levels = SES_quartile_order)   # Change the order for regression

SES_school_order <- c("SQ4","SQ3", "SQ2","SQ1") 
LSAY_1995_data$School_SES_quartile <- factor(get(Data_table) %>% pull(School_SES_quartile), levels = SES_school_order)   # Change the order for regression

Parent_edu_order <- c("Uni","No_uni") 
LSAY_1995_data$Parent_edu <- factor(get(Data_table) %>% pull(Parent_edu), levels = Parent_edu_order)   # Change the order for regression

Disability_order <- c("No disability","Disability") 
LSAY_1995_data$Disability <- factor(get(Data_table) %>% pull(Disability), levels = Disability_order)   # Change the order for regression


Cohort <- 1998
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)
Data_table.w <- paste0("LSAY_",Cohort,"_data.w")

Sector_order <- c("Independent","Catholic","Government") 
LSAY_1998_data$Sector <- factor(get(Data_table) %>% pull(Sector), levels = Sector_order)   # Change the order for regression

Gender_order <- c("Male","Female" ) 
LSAY_1998_data$Gender <- factor(get(Data_table) %>% pull(Gender), levels = Gender_order)   # Change the order for regression

Indigenous_order <- c("Not indigenous","Indigenous" ) 
LSAY_1998_data$Indigenous <- factor(get(Data_table) %>% pull(Indigenous), levels = Indigenous_order)   # Change the order for regression

SES_quartile_order <- c("PQ4","PQ3", "PQ2","PQ1") 
LSAY_1998_data$Parent_SES_quartile <- factor(get(Data_table) %>% pull(Parent_SES_quartile), levels = SES_quartile_order)   # Change the order for regression

SES_school_order <- c("SQ4","SQ3", "SQ2","SQ1") 
LSAY_1998_data$School_SES_quartile <- factor(get(Data_table) %>% pull(School_SES_quartile), levels = SES_school_order)     # Change the order for regression

Parent_edu_order <- c("Uni","No_uni") 
LSAY_1998_data$Parent_edu <- factor(get(Data_table) %>% pull(Parent_edu), levels = Parent_edu_order)   # Change the order for regression

Disability_order <- c("No disability","Disability") 
LSAY_1998_data$Disability <- factor(get(Data_table) %>% pull(Disability), levels = Disability_order)   # Change the order for regression


Cohort <- 2003
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)
Data_table.w <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2003_data[,"HISEI"] <- get(Data_source)[,"HISEI"]
LSAY_2003_data[,"SCHOOLID"] <- get(Data_source)[,"SCHOOLID"]
LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(LSAY_2003$WT2003) & 
                                                                                     !is.na(get(Data_source)$HISEI)),]), 
                              weights = LSAY_2003[which(!is.na(LSAY_2003$WT2003) & !is.na(get(Data_source)$HISEI)),"WT2003"])

LSAY_2003_data$Indigenous <- ifelse(get(Data_source)$INDIG == 0, "Not indigenous", ifelse(get(Data_source)$INDIG == 1, "Indigenous", NA))
LSAY_2003_data$Sector <- ifelse(get(Data_source)$SECTOR == 1, "Government", ifelse(get(Data_source)$SECTOR == 2, "Catholic", 
                                                                                   ifelse(get(Data_source)$SECTOR == 3, "Independent",NA)))
LSAY_2003_data$Gender <- ifelse(get(Data_source)$SEX == 1, "Male", ifelse(get(Data_source)$SEX == 2, "Female", NA))
LSAY_2003_data$Location <- ifelse(get(Data_source)$LOC <3,"Metropolitan",ifelse(get(Data_source)$LOC <7,"Provincial",ifelse(get(Data_source)$LOC<9,"Remote",NA))) # Make categories consistent with 2006 & 2009 cohorts
LSAY_2003_data$Family_structure <- ifelse(get(Data_source)$FAMSTRUC == 1,"Single parent",ifelse(get(Data_source)$FAMSTRUC == 2,"2 parent HH",
                                                                                                ifelse(get(Data_source)$FAMSTRUC == 3,"2 parent HH",
                                                                                                       ifelse(get(Data_source)$FAMSTRUC == 4,NA,NA))))

LSAY_2003_data$Student_plans <- ifelse(get(Data_source)$LAA005 == 1, "University", ifelse(get(Data_source)$LAA005 < 6, "Other_training", 
                                                                                          ifelse(get(Data_source)$LAA005 < 10, "Other",
                                                                                                 ifelse(get(Data_source)$LAA005 == 10, "Dont know", NA))))

cols <- c("Uni_attend_age_22", "Uni_attend_age_19","Parent_SES_quartile","Indigenous", "Sector","Location","NESB",
          "Family_structure","Parent_edu","Language","Gender","PISA_quartile")
LSAY_2003_data[cols] <- lapply(LSAY_2003_data[cols], factor)

Sector_order <- c("Independent","Catholic","Government") 
LSAY_2003_data$Sector <- factor(get(Data_table) %>% pull(Sector), levels = Sector_order)   # Change the order for regression

Gender_order <- c("Male","Female" ) 
LSAY_2003_data$Gender <- factor(get(Data_table) %>% pull(Gender), levels = Gender_order)   # Change the order for regression

Indigenous_order <- c("Not indigenous","Indigenous" ) 
LSAY_2003_data$Indigenous <- factor(get(Data_table) %>% pull(Indigenous), levels = Indigenous_order)   # Change the order for regression

SES_quartile_order <- c("PQ4","PQ3", "PQ2","PQ1")
LSAY_2003_data$Parent_SES_quartile <- factor(get(Data_table) %>% pull(Parent_SES_quartile), levels = SES_quartile_order)   # Change the order for regression

SES_school_order <- c("SQ4","SQ3", "SQ2","SQ1") 
LSAY_2003_data$School_SES_quartile <- factor(get(Data_table) %>% pull(School_SES_quartile), levels = SES_school_order)    # Change the order for regression

Parent_edu_order <- c("Uni","No_uni") 
LSAY_2003_data$Parent_edu <- factor(get(Data_table) %>% pull(Parent_edu), levels = Parent_edu_order)   # Change the order for regression


Cohort <- 2006
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)
Data_table.w <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2006_data[,"HISEI"] <- get(Data_source)[,"HISEI"]
LSAY_2006_data[,"SCHOOLID"] <- get(Data_source)[,"SCHOOLID"]

LSAY_2006_data$Indigenous <- ifelse(get(Data_source)$INDIG == 0, "Not indigenous", ifelse(get(Data_source)$INDIG == 1, "Indigenous", NA))
LSAY_2006_data$Sector <- ifelse(get(Data_source)$SECTOR == 1, "Catholic", ifelse(get(Data_source)$SECTOR == 2, "Government", 
                                                                                 ifelse(get(Data_source)$SECTOR == 3, "Independent",NA)))
LSAY_2006_data$Gender <- sapply(get(Data_source)$ST04Q01,  function(x) {ifelse(x == 1, "Female",
                                                                               ifelse(x == 2, "Male",NA))})
LSAY_2006_data$Location <- ifelse(get(Data_source)$GEOLOC_3 == 1,"Metropolitan",ifelse(get(Data_source)$GEOLOC_3 == 2,"Provincial",
                                                                                       ifelse(get(Data_source)$GEOLOC_3 == 3,"Remote",NA))) # Make categories consistent with 2006 & 2009 cohorts

LSAY_2006_data$Student_plans <- ifelse(get(Data_source)$ST48N01 == 1, "University", ifelse(get(Data_source)$ST48N01 < 6, "Other_training", 
                                                                                           ifelse(get(Data_source)$ST48N01 < 9, "Other",
                                                                                                  ifelse(get(Data_source)$ST48N01 == 9, "Dont know",NA))))


LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(LSAY_2006_data[which(!is.na(LSAY_2006$WT2006) & 
                                                                                    !is.na(LSAY_2006_data$HISEI)),]), 
                              weights = LSAY_2006[which(!is.na(LSAY_2006$WT2006) & !is.na(LSAY_2006_data$HISEI)),"WT2006"])

cols <- c("Uni_attend_age_22", "Uni_attend_age_19","Parent_SES_quartile","Indigenous", "Sector","Location","NESB",
          "Parent_edu","Language","Gender","PISA_quartile")
LSAY_2006_data[cols] <- lapply(LSAY_2006_data[cols], factor)

Sector_order <- c("Independent","Catholic","Government") 
LSAY_2006_data$Sector <- factor(get(Data_table) %>% pull(Sector), levels = Sector_order)   # Change the order for regression

Gender_order <- c("Male","Female" ) 
LSAY_2006_data$Gender <- factor(get(Data_table) %>% pull(Gender), levels = Gender_order)   # Change the order for regression

Indigenous_order <- c("Not indigenous","Indigenous" ) 
LSAY_2006_data$Indigenous <- factor(get(Data_table) %>% pull(Indigenous), levels = Indigenous_order)   # Change the order for regression

SES_quartile_order <- c("PQ4","PQ3","PQ2","PQ1")
LSAY_2006_data$Parent_SES_quartile <- factor(get(Data_table) %>% pull(Parent_SES_quartile), levels = SES_quartile_order)   # Change the order for regression

SES_school_order <- c("SQ4","SQ3", "SQ2","SQ1") 
LSAY_2006_data$School_SES_quartile <- factor(get(Data_table) %>% pull(School_SES_quartile), levels = SES_school_order)    # Change the order for regression

Parent_edu_order <- c("Uni","No_uni") 
LSAY_2006_data$Parent_edu <- factor(get(Data_table) %>% pull(Parent_edu), levels = Parent_edu_order)   # Change the order for regression

Cohort <- 2009
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Data_source <- paste0("LSAY_",Cohort)
Data_table.w <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2009_data[,"HISEI"] <- get(Data_source)[,"HISEI"]
LSAY_2009_data[,"SCHOOLID"] <- get(Data_source)[,"SCHOOLID"]

LSAY_2009_data$Indigenous <- ifelse(get(Data_source)$INDIG == 0, "Not indigenous", ifelse(get(Data_source)$INDIG == 1, "Indigenous", NA))
LSAY_2009_data$Sector <- ifelse(get(Data_source)$SECTOR == 1, "Catholic", ifelse(get(Data_source)$SECTOR == 2, "Government", 
                                                                                 ifelse(get(Data_source)$SECTOR == 3, "Independent",NA)))
LSAY_2009_data$Gender <- ifelse(get(Data_source)$ST04Q01 == 1, "Female", ifelse(get(Data_source)$ST04Q01 == 2, "Male", NA))
LSAY_2009_data$Location <- ifelse(get(Data_source)$GEOLOC == 1,"Metropolitan",ifelse(get(Data_source)$GEOLOC == 2,"Provincial",
                                                                                     ifelse(get(Data_source)$GEOLOC == 3,"Remote",NA))) # Make categories consistent with 2006 & 2009 cohorts
LSAY_2009_data$Family_structure <- ifelse(get(Data_source)$FAMSTRUC == 1,"Single parent",ifelse(get(Data_source)$FAMSTRUC == 2,"2 parent HH",
                                                                                                ifelse(get(Data_source)$FAMSTRUC == 3,"2 parent HH",NA)))

LSAY_2009_data$Student_plans <- ifelse(get(Data_source)$ST65N01 == 1, "University", ifelse(get(Data_source)$ST65N01 < 6, "Other_training", 
                                                                                           ifelse(get(Data_source)$ST65N01 == 8, "Dont know",
                                                                                                  ifelse(get(Data_source)$ST65N01 < 13, "Other",NA))))

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(LSAY_2009$WT2009) & 
                                                                                     !is.na(get(Data_table)$HISEI)),]), 
                              weights = LSAY_2009[which(!is.na(LSAY_2009$WT2009) & !is.na(get(Data_table)$HISEI)),"WT2009"])

cols <- c("Uni_attend_age_22", "Uni_attend_age_19","Parent_SES_quartile","Indigenous", "Sector","Location","NESB",
          "Family_structure","Parent_edu","Language","Gender","PISA_quartile")
LSAY_2009_data[cols] <- lapply(LSAY_2009_data[cols], factor)

Sector_order <- c("Independent","Catholic","Government") 
LSAY_2009_data$Sector <- factor(get(Data_table) %>% pull(Sector), levels = Sector_order)   # Change the order for regression

Gender_order <- c("Male","Female" ) 
LSAY_2009_data$Gender <- factor(get(Data_table) %>% pull(Gender), levels = Gender_order)   # Change the order for regression

Indigenous_order <- c("Not indigenous","Indigenous" ) 
LSAY_2009_data$Indigenous <- factor(get(Data_table) %>% pull(Indigenous), levels = Indigenous_order)   # Change the order for regression

SES_quartile_order <- c("PQ4","PQ3","PQ2","PQ1")
LSAY_2009_data$Parent_SES_quartile <- factor(get(Data_table) %>% pull(Parent_SES_quartile), levels = SES_quartile_order)   # Change the order for regression

SES_school_order <- c("SQ4","SQ3", "SQ2","SQ1") 
LSAY_2009_data$School_SES_quartile <- factor(get(Data_table) %>% pull(School_SES_quartile), levels = SES_school_order)    # Change the order for regression

Parent_edu_order <- c("Uni","No_uni") 
LSAY_2009_data$Parent_edu <- factor(get(Data_table) %>% pull(Parent_edu), levels = Parent_edu_order)   # Change the order for regression

rm(Sector_order,Gender_order,Indigenous_order,SES_quartile_order,SES_school_order,Parent_edu_order)

#### State ####

LSAY_1995_data$State <- recode_factor(LSAY_1995$state, `2`='NSW',`3`='VIC',`4`='QLD',`5`='SA',`6`='WA',`7`='TAS',`8`='NT', `1`='ACT')
LSAY_1998_data$State <- recode_factor(LSAY_1998$state, `2`='NSW',`3`='VIC',`4`='QLD',`5`='SA',`6`='WA',`7`='TAS',`8`='NT', `1`='ACT')   
LSAY_2003_data$State <- recode_factor(LSAY_2003$STATEID, `2`='NSW',`3`='VIC',`4`='QLD',`5`='SA',`6`='WA',`7`='TAS',`8`='NT', `1`='ACT')  
LSAY_2006_data$State <- recode_factor(LSAY_2006$STATE, `2`='NSW',`3`='VIC',`4`='QLD',`5`='SA',`6`='WA',`7`='TAS',`8`='NT', `1`='ACT')
LSAY_2009_data$State <- recode_factor(LSAY_2009$STATE, `2`='NSW',`3`='VIC',`4`='QLD',`5`='SA',`6`='WA',`7`='TAS',`8`='NT', `1`='ACT')


#################################### New Variables #######

# New aspirations variable (based on advice from NCVER)

LSAY_2003_data$Study_plans <- recode_factor(LSAY_2003$ST23Q06,`1`='University',`2`='Not university',`7`=as.character(NA),`8`=as.character(NA))
LSAY_2009_data$Study_plans <- recode_factor(LSAY_2009$EC05Q01F,`1`='University',`2`='Not university',`7`=as.character(NA),`8`=as.character(NA),
                                            `9`=as.character(NA)) 

LSAY_1995_data$Plan_complete_yr_12 <- recode_factor(LSAY_1995$CA030,`1`='Yes',`0`='No',`3`='Not sure')
LSAY_1998_data$Plan_complete_yr_12 <- recode_factor(LSAY_1998$CA025,`1`='Yes',`0`='No',`3`='Not sure') # plan to go on to year 12
LSAY_2003_data$Plan_complete_yr_12 <- recode_factor(LSAY_2003$ST23Q03,`1`='Yes',`2`='No',`7`=as.character(NA),`8`=as.character(NA))
LSAY_2006_data$Plan_complete_yr_12 <- recode_factor(LSAY_2006$ST47N01,`1`='Yes',`2`='No',`3`='Not sure',`7`=as.character(NA),
                                                    `8`=as.character(NA),`9`=as.character(NA)) 
LSAY_2009_data$Plan_complete_yr_12 <- recode_factor(LSAY_2009$EC05Q01C,`1`='Yes',`2`='No',`7`=as.character(NA),`8`=as.character(NA),`9`=as.character(NA)) 


##### Books at home at age 15

LSAY_1995_data$Books <- recode(LSAY_1995$CJ003,`1`='0-100',`2`='0-100',`3`='101-500',`4`='>500',`5`='>500',`6`=as.character(NA)) # measured in 1997
LSAY_1998_data$Books <- recode(LSAY_1998$CJ003,`1`='0-100',`2`='0-100',`3`='101-500',`4`='>500',`5`='>500',`6`=as.character(NA)) # measured in 2000
LSAY_2003_data$Books <- recode(LSAY_2003$ST19Q01,`1`='0-100',`2`='0-100',`3`='0-100',`4`='101-500',`5`='101-500',`6`='>500',`7`=as.character(NA),
                               `8`=as.character(NA),`9`=as.character(NA))
LSAY_2006_data$Books <- recode(LSAY_2006$ST15Q01,`1`='0-100',`2`='0-100',`3`='0-100',`4`='101-500',`5`='101-500',`6`='>500',`7`=as.character(NA),
                               `8`=as.character(NA),`9`=as.character(NA))
LSAY_2009_data$Books <- recode(LSAY_2009$ST22Q01,`1`='0-100',`2`='0-100',`3`='0-100',`4`='101-500',`5`='101-500',`6`='>500',`7`=as.character(NA),
                               `8`=as.character(NA),`9`=as.character(NA))

Books_order <- c(">500","101-500","0-100")
LSAY_1995_data$Books <- factor(LSAY_1995_data %>% pull(Books), levels = Books_order) 
LSAY_1998_data$Books <- factor(LSAY_1998_data %>% pull(Books), levels = Books_order)
LSAY_2003_data$Books <- factor(LSAY_2003_data %>% pull(Books), levels = Books_order) 
LSAY_2006_data$Books <- factor(LSAY_2006_data %>% pull(Books), levels = Books_order)
LSAY_2009_data$Books <- factor(LSAY_2009_data %>% pull(Books), levels = Books_order)

rm(Books_order)

## average school pisa in year 12 ##

Cohort <- 1995 
Data_table <- paste0("LSAY_",Cohort,"_data")
Data_source <- paste0("LSAY_",Cohort)
Data_design <- paste0("LSAY_",Cohort,"_data.w")

LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)[,"Achievement"])),"WT95GEN"])

School_achievement_1995 <-  as.data.frame(svyby(~Achievement, by = ~schno, design = get(Data_design),svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE))
colnames(School_achievement_1995)[2] <- "School_Achievement"

LSAY_1995_data$School_Achievement <- with(School_achievement_1995,School_Achievement[match(get(Data_table)$schno,schno)])  

LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)[,"Achievement"])),"WT95GEN"])


LSAY_1995_data$School_achievement_quartile <- ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.75, ci=FALSE)[1,1],"SPQ4",
                                                     ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.5, ci=FALSE)[1,1],"SPQ3",
                                                            ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ2",
                                                                   ifelse(get(Data_table)[,"School_Achievement"]<svyquantile(~School_Achievement, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ1",NA))))

Cohort <- 1998 
Data_table <- paste0("LSAY_",Cohort,"_data")
Data_source <- paste0("LSAY_",Cohort)
Data_design <- paste0("LSAY_",Cohort,"_data.w")

LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)[,"Achievement"])),"WT1998"])

School_achievement_1998 <-  as.data.frame(svyby(~Achievement, by = ~SCHNO_R, design = get(Data_design),svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE))
colnames(School_achievement_1998)[2] <- "School_Achievement"

LSAY_1998_data$School_Achievement <- with(School_achievement_1998,School_Achievement[match(get(Data_table)$SCHNO_R,SCHNO_R)])  

LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)[,"Achievement"])),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)[,"Achievement"])),"WT1998"])


LSAY_1998_data$School_achievement_quartile <- ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.75, ci=FALSE)[1,1],"SPQ4",
                                                     ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.5, ci=FALSE)[1,1],"SPQ3",
                                                            ifelse(get(Data_table)[,"School_Achievement"]>=svyquantile(~School_Achievement, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ2",
                                                                   ifelse(get(Data_table)[,"School_Achievement"]<svyquantile(~School_Achievement, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ1",NA))))


Cohort <- 2003
Data_table <- paste("LSAY_",Cohort,"_data",sep = "")
Data_source <- paste0("LSAY_",Cohort)
Data_design <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2003) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2003) & !is.na(get(Data_table)$PISA)),"WT2003"])

School_PISA_2003 <-  as.data.frame(svyby(~PISA, by = ~SCHID_R, design = get(Data_design),svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE))
colnames(School_PISA_2003)[2] <- "School_PISA"

LSAY_2003_data$School_PISA <- with(School_PISA_2003,School_PISA[match(get(Data_table)$SCHID_R,SCHID_R)]) 
LSAY_2003_data$School_aptitude <- LSAY_2003_data$School_PISA

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2003) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2003) & !is.na(get(Data_table)$PISA)),"WT2003"])

LSAY_2003_data$School_PISA_quartile <- ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.75, ci=FALSE)[1,1],"SPQ4",
                                              ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.5, ci=FALSE)[1,1],"SPQ3",
                                                     ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ2",
                                                            ifelse(get(Data_table)[,"School_PISA"]<svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ1",NA))))


Cohort <- 2006
Data_table <- paste("LSAY_",Cohort,"_data",sep = "")
Data_source <- paste0("LSAY_",Cohort)
Data_design <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2006) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2006) & !is.na(get(Data_table)$PISA)),"WT2006"])

School_PISA_2006 <-  as.data.frame(svyby(~PISA, by = ~SCHOOLID, design = get(Data_design),svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE))
colnames(School_PISA_2006)[2] <- "School_PISA"

LSAY_2006_data$School_PISA <- with(School_PISA_2006,School_PISA[match(get(Data_table)$SCHOOLID,SCHOOLID)]) 
LSAY_2006_data$School_aptitude <- LSAY_2006_data$School_PISA

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2006) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2006) & !is.na(get(Data_table)$PISA)),"WT2006"])

LSAY_2006_data$School_PISA_quartile <- ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.75, ci=FALSE)[1,1],"SPQ4",
                                              ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.5, ci=FALSE)[1,1],"SPQ3",
                                                     ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ2",
                                                            ifelse(get(Data_table)[,"School_PISA"]<svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ1",NA))))


Cohort <- 2009
Data_table <- paste("LSAY_",Cohort,"_data",sep = "")
Data_source <- paste0("LSAY_",Cohort)
Data_design <- paste0("LSAY_",Cohort,"_data.w")

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2009) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2009) & !is.na(get(Data_table)$PISA)),"WT2009"])

School_PISA_2009 <-  as.data.frame(svyby(~PISA, by = ~SCHOOLID, design = get(Data_design),svymean,keep.var=FALSE,keep.names = TRUE,na.rm=TRUE))
colnames(School_PISA_2009)[2] <- "School_PISA"

LSAY_2009_data$School_PISA <- with(School_PISA_2009,School_PISA[match(get(Data_table)$SCHOOLID,SCHOOLID)])  
LSAY_2009_data$School_aptitude <- LSAY_2009_data$School_PISA

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_source)$WT2009) & !is.na(get(Data_table)$PISA)),]), 
                              weights = get(Data_source)[which(!is.na(get(Data_source)$WT2009) & !is.na(get(Data_table)$PISA)),"WT2009"])

LSAY_2009_data$School_PISA_quartile <- ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.75, ci=FALSE)[1,1],"SPQ4",
                                              ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.5, ci=FALSE)[1,1],"SPQ3",
                                                     ifelse(get(Data_table)[,"School_PISA"]>=svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ2",
                                                            ifelse(get(Data_table)[,"School_PISA"]<svyquantile(~School_PISA, get(Data_design), 0.25, ci=FALSE)[1,1],"SPQ1",NA))))


School_PISA_quartile_order <- c("SPQ4","SPQ3","SPQ2","SPQ1") 
LSAY_1995_data$School_achievement_quartile <- factor(LSAY_1995_data %>% pull(School_achievement_quartile), levels = School_PISA_quartile_order) 
LSAY_1998_data$School_achievement_quartile <- factor(LSAY_1998_data %>% pull(School_achievement_quartile), levels = School_PISA_quartile_order) 
LSAY_2003_data$School_PISA_quartile <- factor(LSAY_2003_data %>% pull(School_PISA_quartile), levels = School_PISA_quartile_order) 
LSAY_2006_data$School_PISA_quartile <- factor(LSAY_2006_data %>% pull(School_PISA_quartile), levels = School_PISA_quartile_order) 
LSAY_2009_data$School_PISA_quartile <- factor(LSAY_2009_data %>% pull(School_PISA_quartile), levels = School_PISA_quartile_order) 

rm(School_achievement_1995,School_achievement_1998,School_PISA_2003,School_PISA_2006,School_PISA_2009)


# Identify which year they were in year 12. Note this does not pick up everyone who attended or even completed year 12, only those people
# who said they were in year 12 at the time of the interview (e.g. they may have finished or dropped out before the interview took place)

Data_source <- "LSAY_1995"
Data_table <- "LSAY_1995_data"
Years_1995_cohort <- c(1998:2003)
LSAY_1995_data$Year_of_year_12 <- sapply(c(1:nrow(get(Data_source))), 
                                         function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_1995_cohort)] == 1))) 
LSAY_1995_data$Year_of_year_12 <- Years_1995_cohort[get(Data_table)$Year_of_year_12] - 1
LSAY_1995_data$Year_of_year_12 <- ifelse(is.na(LSAY_1995_data$Year_of_year_12),
                                         ifelse(LSAY_1995$XCSL1995 == 4, 1998,
                                                ifelse(LSAY_1995$XCSL1995 == 3, 1997,
                                                       ifelse(LSAY_1995$XCSL1995 == 2, 1996,
                                                              ifelse(LSAY_1995$XCSL1995 == 1, 1995,NA)))),LSAY_1995_data$Year_of_year_12)
LSAY_1995_data$Attend_year_12 <-  sapply(c(1:nrow(get(Data_source))), 
                                         function(x) any(which(get(Data_source)[x,paste0("XCSL",Years_1995_cohort)] == 1)))
LSAY_1995_data$Attend_year_12 <- ifelse(LSAY_1995_data$Attend_year_12 == TRUE,"Attended","Not attended")

LSAY_1995_data$Completed_year_12 <- LSAY_1995$XHSL2003
LSAY_1995_data$Completed_year_12 <- ifelse(LSAY_1995_data$Completed_year_12 %in% 1, "Completed",
                                           ifelse(LSAY_1995_data$Completed_year_12 %in% 2,"Not_completed",NA))

Data_source <- "LSAY_1998"
Data_table <- "LSAY_1998_data"
Years_1998_cohort <- c(1998:2006)
LSAY_1998_data$Year_of_year_12 <- sapply(c(1:nrow(get(Data_source))), 
                                         function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_1998_cohort)] == 1))) 
LSAY_1998_data$Year_of_year_12 <-  Years_1998_cohort[get(Data_table)$Year_of_year_12]
LSAY_1998_data$Year_of_year_12 <- ifelse(is.na(LSAY_1998_data$Year_of_year_12),
                                         ifelse(LSAY_1998$XCSL1998 == 4, 2001,
                                                ifelse(LSAY_1998$XCSL1998 == 3, 2000,
                                                       ifelse(LSAY_1998$XCSL1998 == 2, 1999,
                                                              ifelse(LSAY_1998$XCSL1998 == 1, 1998,NA)))),LSAY_1998_data$Year_of_year_12)

LSAY_1998_data$Attend_year_12 <-  sapply(c(1:nrow(get(Data_source))), 
                                         function(x) any(which(get(Data_source)[x,paste0("XCSL",Years_1998_cohort)] == 1)))
LSAY_1998_data$Attend_year_12 <- ifelse(LSAY_1998_data$Attend_year_12 == TRUE,"Attended","Not attended")

LSAY_1998_data$Completed_year_12 <- LSAY_1998$XHSL2006
LSAY_1998_data$Completed_year_12 <- ifelse(LSAY_1998_data$Completed_year_12 %in% 1, "Completed",
                                           ifelse(LSAY_1998_data$Completed_year_12 %in% 2,"Not_completed",NA))

Data_source <- "LSAY_2003"
Data_table <- "LSAY_2003_data"
Years_2003_cohort <- c(2003:2013)
LSAY_2003_data$Year_of_year_12 <- sapply(c(1:nrow(get(Data_source))), 
                                         function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2003_cohort)] == 1))) 
LSAY_2003_data$Year_of_year_12 <-  Years_2003_cohort[get(Data_table)$Year_of_year_12]
LSAY_2003_data$Year_of_year_12 <- ifelse(is.na(LSAY_2003_data$Year_of_year_12),
                                         ifelse(LSAY_2003$XCSL2003 == 4, 2006,
                                                ifelse(LSAY_2003$XCSL2003 == 3, 2005,
                                                       ifelse(LSAY_2003$XCSL2003 == 2, 2004,
                                                              ifelse(LSAY_2003$XCSL2003 == 1, 2003,NA)))),LSAY_2003_data$Year_of_year_12)

LSAY_2003_data$Attend_year_12 <-  sapply(c(1:nrow(get(Data_source))), 
                                         function(x) any(which(get(Data_source)[x,paste0("XCSL",Years_2003_cohort)] == 1)))
LSAY_2003_data$Attend_year_12 <- ifelse(LSAY_2003_data$Attend_year_12 == TRUE,"Attended","Not attended")

LSAY_2003_data$Completed_year_12 <- LSAY_2003$XHSL2010
LSAY_2003_data$Completed_year_12 <- ifelse(LSAY_2003_data$Completed_year_12 %in% 1, "Completed",
                                           ifelse(LSAY_2003_data$Completed_year_12 %in% 2,"Not_completed",NA))

Data_source <- "LSAY_2006"
Data_table <- "LSAY_2006_data"
Years_2006_cohort <- c(2006:2016)
LSAY_2006_data$Year_of_year_12 <- sapply(c(1:nrow(get(Data_source))), 
                                         function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2006_cohort)] == 1))) 
LSAY_2006_data$Year_of_year_12 <-  Years_2006_cohort[get(Data_table)$Year_of_year_12]
LSAY_2006_data$Year_of_year_12 <- ifelse(is.na(LSAY_2006_data$Year_of_year_12),
                                         ifelse(LSAY_2006$XCSL2006 == 4, 2009,
                                                ifelse(LSAY_2006$XCSL2006 == 3, 2008,
                                                       ifelse(LSAY_2006$XCSL2006 == 2, 2007,
                                                              ifelse(LSAY_2006$XCSL2006 == 1, 2006,NA)))),LSAY_2006_data$Year_of_year_12)

LSAY_2006_data$Attend_year_12 <-  sapply(c(1:nrow(get(Data_source))), 
                                         function(x) any(which(get(Data_source)[x,paste0("XCSL",Years_2006_cohort)] == 1)))
LSAY_2006_data$Attend_year_12 <- ifelse(LSAY_2006_data$Attend_year_12 == TRUE,"Attended","Not attended")

LSAY_2006_data$Completed_year_12 <- LSAY_2006$XHSL2013
LSAY_2006_data$Completed_year_12 <- ifelse(LSAY_2006_data$Completed_year_12 %in% 1, "Completed",
                                           ifelse(LSAY_2006_data$Completed_year_12 %in% 2,"Not_completed",NA))

Data_source <- "LSAY_2009"
Data_table <- "LSAY_2009_data"
Years_2009_cohort <- c(2009:2016)
LSAY_2009_data$Year_of_year_12 <- sapply(c(1:nrow(get(Data_source))), 
                                         function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2009_cohort)] == 1))) 
LSAY_2009_data$Year_of_year_12 <-  Years_2009_cohort[get(Data_table)$Year_of_year_12]
LSAY_2009_data$Year_of_year_12 <- ifelse(is.na(LSAY_2009_data$Year_of_year_12),
                                         ifelse(LSAY_2009$XCSL2009 == 4, 2012,
                                                ifelse(LSAY_2009$XCSL2009 == 3, 2011,
                                                       ifelse(LSAY_2009$XCSL2009 == 2, 2010,
                                                              ifelse(LSAY_2009$XCSL2009 == 1, 2009,NA)))),LSAY_2009_data$Year_of_year_12)

LSAY_2009_data$Attend_year_12 <-  sapply(c(1:nrow(get(Data_source))), 
                                         function(x) any(which(get(Data_source)[x,paste0("XCSL",Years_2009_cohort)] == 1)))
LSAY_2009_data$Attend_year_12 <- ifelse(LSAY_2009_data$Attend_year_12 == TRUE,"Attended","Not attended")


LSAY_2009_data$Completed_year_12 <- LSAY_2009$XHSL2016
LSAY_2009_data$Completed_year_12 <- ifelse(LSAY_2009_data$Completed_year_12 %in% 1, "Completed",
                                           ifelse(LSAY_2009_data$Completed_year_12 %in% 2,"Not_completed",NA))

Attend_year_12_order <- c("Attended","Not attended") 
LSAY_1995_data$Attend_year_12 <- factor(LSAY_1995_data %>% pull(Attend_year_12), levels = Attend_year_12_order)
LSAY_1998_data$Attend_year_12 <- factor(LSAY_1998_data %>% pull(Attend_year_12), levels = Attend_year_12_order)
LSAY_2003_data$Attend_year_12 <- factor(LSAY_2003_data %>% pull(Attend_year_12), levels = Attend_year_12_order)
LSAY_2006_data$Attend_year_12 <- factor(LSAY_2006_data %>% pull(Attend_year_12), levels = Attend_year_12_order)
LSAY_2009_data$Attend_year_12 <- factor(LSAY_2009_data %>% pull(Attend_year_12), levels = Attend_year_12_order)

Completed_year_12_order <- c("Not_completed","Completed")
LSAY_1995_data$Completed_year_12 <- factor(LSAY_1995_data %>% pull(Completed_year_12), levels = Completed_year_12_order) 
LSAY_1998_data$Completed_year_12 <- factor(LSAY_1998_data %>% pull(Completed_year_12), levels = Completed_year_12_order) 
LSAY_2003_data$Completed_year_12 <- factor(LSAY_2003_data %>% pull(Completed_year_12), levels = Completed_year_12_order) 
LSAY_2006_data$Completed_year_12 <- factor(LSAY_2006_data %>% pull(Completed_year_12), levels = Completed_year_12_order) 
LSAY_2009_data$Completed_year_12 <- factor(LSAY_2009_data %>% pull(Completed_year_12), levels = Completed_year_12_order) 

rm(Completed_year_12_order,Attend_year_12_order)


LSAY_1995_data$Hours_worked_in_year_12 <- mapply(function (x,y) LSAY_1995[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_1995_data), y = LSAY_1995_data$Year_of_year_12)
LSAY_1998_data$Hours_worked_in_year_12 <- mapply(function (x,y) LSAY_1998[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_1998_data), y = LSAY_1998_data$Year_of_year_12)
LSAY_2003_data$Hours_worked_in_year_12 <- mapply(function (x,y) LSAY_2003[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$Year_of_year_12)
LSAY_2006_data$Hours_worked_in_year_12 <- mapply(function (x,y) LSAY_2006[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$Year_of_year_12)
LSAY_2009_data$Hours_worked_in_year_12 <- mapply(function (x,y) LSAY_2009[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$Year_of_year_12)

# Hours worked unknown is coded as NA (XHRS=999 is NA). XHRS=998 means not working, so coded as zero
LSAY_1995_data$Hours_worked_in_year_12 <- ifelse(LSAY_1995_data$Hours_worked_in_year_12 ==998,0,
                                                 ifelse(LSAY_1995_data$Hours_worked_in_year_12 == 999,NA,
                                                        LSAY_1995_data$Hours_worked_in_year_12)) 
LSAY_1998_data$Hours_worked_in_year_12 <- ifelse(LSAY_1998_data$Hours_worked_in_year_12 ==998,0,
                                                 ifelse(LSAY_1998_data$Hours_worked_in_year_12 == 999,NA,
                                                        LSAY_1998_data$Hours_worked_in_year_12)) 
LSAY_2003_data$Hours_worked_in_year_12 <- ifelse(LSAY_2003_data$Hours_worked_in_year_12 ==998,0,
                                                 ifelse(LSAY_2003_data$Hours_worked_in_year_12 == 999,NA,
                                                        LSAY_2003_data$Hours_worked_in_year_12)) 
LSAY_2006_data$Hours_worked_in_year_12 <- ifelse(LSAY_2006_data$Hours_worked_in_year_12 ==998,0,
                                                 ifelse(LSAY_2006_data$Hours_worked_in_year_12 == 999,NA,
                                                        LSAY_2006_data$Hours_worked_in_year_12)) 
LSAY_2009_data$Hours_worked_in_year_12 <- ifelse(LSAY_2009_data$Hours_worked_in_year_12 ==998,0,
                                                 ifelse(LSAY_2009_data$Hours_worked_in_year_12 == 999,NA,
                                                        LSAY_2009_data$Hours_worked_in_year_12)) 


##### find the people who, when surveyed, had already completed yr 12 in the year of the wave in which they attended year 12. We will use their yr 11 hrs for hours worked in yr 12

LSAY_1995_data$Already_completed <- mapply(function (x,y) LSAY_1995[x,paste0("XHSL",y)], 
                                                 x = 1:nrow(LSAY_1995_data), y = LSAY_1995_data$Year_of_year_12) 

LSAY_1995_data$Hours_worked_in_year_11 <- mapply(function (x,y) LSAY_1995[x,paste0("XHRS",y)], 
                                           x = 1:nrow(LSAY_1995_data), y = LSAY_1995_data$Year_of_year_12-1)

LSAY_1995_data <- LSAY_1995_data %>% 
  mutate(Already_completed = ifelse(Attend_year_12 == "Not attended" & Already_completed == 1,"Already completed",NA),
         Hours_worked_in_year_11 = ifelse(Hours_worked_in_year_11 == 998,0,
                                          ifelse(Hours_worked_in_year_11 == 999,NA,Hours_worked_in_year_11)),
         Hours_worked_in_year_12 = ifelse(Already_completed %in% "Already completed",Hours_worked_in_year_11,
                                          Hours_worked_in_year_12),
         Attend_year_12 = ifelse(Attend_year_12 %in% "Attended","Attended",
                ifelse(Already_completed %in% "Already completed","Attended",
                       ifelse(Attend_year_12 %in% "Not attended","Not attended",NA))))

LSAY_1998_data$Already_completed <- mapply(function (x,y) LSAY_1998[x,paste0("XHSL",y)], 
                                           x = 1:nrow(LSAY_1998_data), y = LSAY_1998_data$Year_of_year_12) 

LSAY_1998_data$Hours_worked_in_year_11 <- mapply(function (x,y) LSAY_1998[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_1998_data), y = LSAY_1998_data$Year_of_year_12-1)

LSAY_1998_data <- LSAY_1998_data %>% 
  mutate(Already_completed = ifelse(Attend_year_12 == "Not attended" & Already_completed == 1,"Already completed",NA),
         Hours_worked_in_year_11 = ifelse(Hours_worked_in_year_11 == 998,0,
                                          ifelse(Hours_worked_in_year_11 == 999,NA,Hours_worked_in_year_11)),
         Hours_worked_in_year_12 = ifelse(Already_completed %in% "Already completed",Hours_worked_in_year_11,
                                          Hours_worked_in_year_12),
         Attend_year_12 = ifelse(Attend_year_12 %in% "Attended","Attended",
                                 ifelse(Already_completed %in% "Already completed","Attended",
                                        ifelse(Attend_year_12 %in% "Not attended","Not attended",NA))))

LSAY_2003_data$Already_completed <- mapply(function (x,y) LSAY_2003[x,paste0("XHSL",y)], 
                                           x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$Year_of_year_12) 

LSAY_2003_data$Hours_worked_in_year_11 <- mapply(function (x,y) LSAY_2003[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$Year_of_year_12-1) %>% 
  as.character(.) %>% as.numeric(.)


LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Already_completed = ifelse(Attend_year_12 == "Not attended" & Already_completed == 1,"Already completed",NA),
         Hours_worked_in_year_11 = ifelse(Hours_worked_in_year_11 == 998,0,
                                          ifelse(Hours_worked_in_year_11 == 999,NA,Hours_worked_in_year_11)),
         Hours_worked_in_year_12 = ifelse(Already_completed %in% "Already completed",Hours_worked_in_year_11,
                                          Hours_worked_in_year_12),
         Attend_year_12 = ifelse(Attend_year_12 %in% "Attended","Attended",
                                 ifelse(Already_completed %in% "Already completed","Attended",
                                        ifelse(Attend_year_12 %in% "Not attended","Not attended",NA))))


LSAY_2006_data$Already_completed <- mapply(function (x,y) LSAY_2006[x,paste0("XHSL",y)], 
                                           x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$Year_of_year_12) 

LSAY_2006_data$Hours_worked_in_year_11 <- mapply(function (x,y) LSAY_2006[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$Year_of_year_12-1) %>% 
  as.character(.) %>% as.numeric(.)


LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Already_completed = ifelse(Attend_year_12 == "Not attended" & Already_completed == 1,"Already completed",NA),
         Hours_worked_in_year_11 = ifelse(Hours_worked_in_year_11 == 998,0,
                                          ifelse(Hours_worked_in_year_11 == 999,NA,Hours_worked_in_year_11)),
         Hours_worked_in_year_12 = ifelse(Already_completed %in% "Already completed",Hours_worked_in_year_11,
                                          Hours_worked_in_year_12),
         Attend_year_12 = ifelse(Attend_year_12 %in% "Attended","Attended",
                                 ifelse(Already_completed %in% "Already completed","Attended",
                                        ifelse(Attend_year_12 %in% "Not attended","Not attended",NA))))

LSAY_2009_data$Already_completed <- mapply(function (x,y) LSAY_2009[x,paste0("XHSL",y)], 
                                           x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$Year_of_year_12) 

LSAY_2009_data$Hours_worked_in_year_11 <- mapply(function (x,y) LSAY_2009[x,paste0("XHRS",y)], 
                                                 x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$Year_of_year_12-1) %>% 
  as.character(.) %>% as.numeric(.)


LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Already_completed = ifelse(Attend_year_12 == "Not attended" & Already_completed == 1,"Already completed",NA),
         Hours_worked_in_year_11 = ifelse(Hours_worked_in_year_11 == 998,0,
                                          ifelse(Hours_worked_in_year_11 == 999,NA,Hours_worked_in_year_11)),
         Hours_worked_in_year_12 = ifelse(Already_completed %in% "Already completed",Hours_worked_in_year_11,
                                          Hours_worked_in_year_12),
         Attend_year_12 = ifelse(Attend_year_12 %in% "Attended","Attended",
                                 ifelse(Already_completed %in% "Already completed","Attended",
                                        ifelse(Attend_year_12 %in% "Not attended","Not attended",NA))))

Attend_year_12_order <- c("Attended","Not attended") 
LSAY_1995_data$Attend_year_12 <- factor(LSAY_1995_data %>% pull(Attend_year_12), levels = Attend_year_12_order)
LSAY_1998_data$Attend_year_12 <- factor(LSAY_1998_data %>% pull(Attend_year_12), levels = Attend_year_12_order) 
LSAY_2003_data$Attend_year_12 <- factor(LSAY_2003_data %>% pull(Attend_year_12), levels = Attend_year_12_order) 
LSAY_2006_data$Attend_year_12 <- factor(LSAY_2006_data %>% pull(Attend_year_12), levels = Attend_year_12_order) 
LSAY_2009_data$Attend_year_12 <- factor(LSAY_2009_data %>% pull(Attend_year_12), levels = Attend_year_12_order)

##### ##### 

#### Interaction term attend_year_12 * hrs worked

LSAY_1995_data$Hours_worked_age_of_yr_12_interaction_attend <- ifelse(LSAY_1995_data$Attend_year_12 %in% "Attended",
                                                                      0*LSAY_1995_data$Hours_worked_in_year_12,LSAY_1995_data$Hours_worked_in_year_12)

LSAY_1998_data$Hours_worked_age_of_yr_12_interaction_attend <- ifelse(LSAY_1998_data$Attend_year_12 %in% "Attended",
                                                                      0*LSAY_1998_data$Hours_worked_in_year_12,LSAY_1998_data$Hours_worked_in_year_12)

LSAY_2003_data$Hours_worked_age_of_yr_12_interaction_attend <- ifelse(LSAY_2003_data$Attend_year_12 %in% "Attended",
                                                                      0*LSAY_2003_data$Hours_worked_in_year_12,LSAY_2003_data$Hours_worked_in_year_12)

LSAY_2006_data$Hours_worked_age_of_yr_12_interaction_attend <- ifelse(LSAY_2006_data$Attend_year_12 %in% "Attended",
                                                                      0*LSAY_2006_data$Hours_worked_in_year_12,LSAY_2006_data$Hours_worked_in_year_12)

LSAY_2009_data$Hours_worked_age_of_yr_12_interaction_attend <- ifelse(LSAY_2009_data$Attend_year_12 %in% "Attended",
                                                                      0*LSAY_2009_data$Hours_worked_in_year_12,LSAY_2009_data$Hours_worked_in_year_12)

##### Parent's plans #####
LSAY_1998_data$Parent_aspiration <- ifelse(LSAY_1998$BC012B %in% c(4,6,7), "Go_to_university", 
                                           ifelse(!is.na(LSAY_1998$BC012B),"Other",NA))

LSAY_2003_data$Parent_aspiration <- ifelse(LSAY_2003$LAA006 == 1, "Go_to_university", 
                                           ifelse(!is.na(LSAY_2003$LAA006),"Other",NA))

LSAY_2006_data$Parent_aspiration <- ifelse(LSAY_2006$ST48N02 == 1, "Go_to_university", 
                                           ifelse(LSAY_2006$ST48N02 > 9,NA,
                                                  ifelse(!is.na(LSAY_2006$ST48N02),"Other",NA)))

LSAY_2009_data$Parent_aspiration <- ifelse(LSAY_2009$ST65N02 == 1, "Go_to_university", 
                                           ifelse(LSAY_2009$ST65N02 >= 97,NA,
                                                  ifelse(!is.na(LSAY_2009$ST65N02),"Other",NA)))

Parent_aspiration_order <- c("Other","Go_to_university") 
LSAY_1998_data$Parent_aspiration <- factor(LSAY_1998_data %>% pull(Parent_aspiration), levels = Parent_aspiration_order)   # Change the order for regression
LSAY_2003_data$Parent_aspiration <- factor(LSAY_2003_data %>% pull(Parent_aspiration), levels = Parent_aspiration_order) 
LSAY_2006_data$Parent_aspiration <- factor(LSAY_2006_data %>% pull(Parent_aspiration), levels = Parent_aspiration_order) 
LSAY_2009_data$Parent_aspiration <- factor(LSAY_2009_data %>% pull(Parent_aspiration), levels = Parent_aspiration_order) 

## Last year at school

Data_source <- "LSAY_1995"
Data_table <- "LSAY_1995_data"
LSAY_1995_data$Last_year_at_school <- sapply(c(1:nrow(get(Data_source))), 
                                             function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_1995_cohort)] == 6))) 
LSAY_1995_data$Last_year_at_school <-  Years_1995_cohort[get(Data_table)$Last_year_at_school] - 1

Data_source <- "LSAY_1998"
Data_table <- "LSAY_1998_data"
LSAY_1998_data$Last_year_at_school <- sapply(c(1:nrow(get(Data_source))), 
                                             function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_1998_cohort)] == 6))) 
LSAY_1998_data$Last_year_at_school <-  Years_1998_cohort[get(Data_table)$Last_year_at_school] - 1

Data_source <- "LSAY_2003"
Data_table <- "LSAY_2003_data"
LSAY_2003_data$Last_year_at_school <- sapply(c(1:nrow(get(Data_source))), 
                                             function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2003_cohort)] == 6))) 
LSAY_2003_data$Last_year_at_school <-  Years_2003_cohort[get(Data_table)$Last_year_at_school] - 1


Data_source <- "LSAY_2006"
Data_table <- "LSAY_2006_data"
LSAY_2006_data$Last_year_at_school <- sapply(c(1:nrow(get(Data_source))), 
                                             function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2006_cohort)] == 6))) 
LSAY_2006_data$Last_year_at_school <-  Years_2006_cohort[get(Data_table)$Last_year_at_school] - 1

Data_source <- "LSAY_2009"
Data_table <- "LSAY_2009_data"
LSAY_2009_data$Last_year_at_school <- sapply(c(1:nrow(get(Data_source))), 
                                             function(x) min(which(get(Data_source)[x,paste0("XCSL",Years_2009_cohort)] == 6))) 
LSAY_2009_data$Last_year_at_school <-  Years_2009_cohort[get(Data_table)$Last_year_at_school] - 1


##########----------------------------------------------------------------------------------------------------##########
#         Estimate distance to closest university using restricted LSAY data
##########----------------------------------------------------------------------------------------------------##########

#register_google(key = "Insert API key") # You can obtain a Google API key from https://cloud.google.com/maps-platform/. Note this is not necessary if you have the coordinates CSV files I have already created


# postcode at last year of school
Postcodes_1995 <- data.frame(Restricted_1995$stuidno)
colnames(Postcodes_1995)[1] <- "stuidno"
Postcodes_1995$PC1995 <- Restricted_1995$postcode
Postcodes_1995$PC1996 <- Restricted_1995$postcode
Postcodes_1995$PC1997 <- with(Restricted_1995_missing,`1997`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC1998 <- with(Restricted_1995_missing,`1998`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC1999 <- with(Restricted_1995_missing,`1999`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2000 <- with(Restricted_1995_missing,`2000`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2001 <- with(Restricted_1995_missing,`2001`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2002 <- with(Restricted_1995_missing,`2002`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2003 <- with(Restricted_1995_missing,`2003`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2004 <- with(Restricted_1995_missing,`2004`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2005 <- with(Restricted_1995_missing,`2005`[match(Postcodes_1995$stuidno,STUIDNO)])
Postcodes_1995$PC2006 <- with(Restricted_1995_missing,`2006`[match(Postcodes_1995$stuidno,STUIDNO)])

Postcodes_1995 <- data.frame(lapply(Postcodes_1995, function(x) {gsub("not intv'd", NA, x)}))
Postcodes_1995[1:ncol(Postcodes_1995)] <- unlist(lapply(Postcodes_1995[1:ncol(Postcodes_1995)], as.character))
Postcodes_1995[Postcodes_1995=="9999"]<-NA
Postcodes_1995[Postcodes_1995=="0"]<-NA


LSAY_1995_data$Location_at_end_of_school <- mapply(function (x,y) Postcodes_1995[x,paste0("PC",y)], 
                                                 x = 1:nrow(LSAY_1995_data), y = LSAY_1995_data$Last_year_at_school) %>% 
  as.character(.) %>% as.numeric(.)

Postcodes_1998 <- data.frame(Restricted_1998$stuidno)
colnames(Postcodes_1998)[1] <- "stuidno"
Postcodes_1998$PC1998 <- Restricted_1998$postcode
Postcodes_1998$PC1999 <- with(Restricted_1998_missing,`1999`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2000 <- with(Restricted_1998_missing,`2000`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2001 <- with(Restricted_1998_missing,`2001`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2002 <- with(Restricted_1998_missing,`2002`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2003 <- with(Restricted_1998_missing,`2003`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2004 <- with(Restricted_1998_missing,`2004`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2005 <- with(Restricted_1998_missing,`2005`[match(Postcodes_1998$stuidno,STUIDNO)])
Postcodes_1998$PC2006 <- with(Restricted_1998_missing,`2006`[match(Postcodes_1998$stuidno,STUIDNO)])

Postcodes_1998 <- data.frame(lapply(Postcodes_1998, function(x) {gsub("not intv'd", NA, x)}))
Postcodes_1998[1:ncol(Postcodes_1998)] <- unlist(lapply(Postcodes_1998[1:ncol(Postcodes_1998)], as.character))
Postcodes_1998[Postcodes_1998=="9999"]<-NA
Postcodes_1998[Postcodes_1998=="0"]<-NA

LSAY_1998_data$Location_at_end_of_school <- mapply(function (x,y) Postcodes_1998[x,paste0("PC",y)], 
                                                   x = 1:nrow(LSAY_1998_data), y = LSAY_1998_data$Last_year_at_school) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2003_data$Location_at_end_of_school <- mapply(function (x,y) Restricted_2003[x,paste0("PC",y)], 
                                                   x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$Last_year_at_school) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2003_data$Location_at_end_of_school <- ifelse(LSAY_2003_data$Location_at_end_of_school %in% c("9999","0"),NA,
                                                   LSAY_2003_data$Location_at_end_of_school)

LSAY_2006_data$Postcode <- ifelse(Restricted_2006$PC2007 == 0 & is.na(Restricted_2006$PC2008) == TRUE,Restricted_2006$POSTCODE,
                                  ifelse(Restricted_2006$PC2007 == 0,Restricted_2006$PC2008,Restricted_2006$PC2007)) # if they don't know their postcode, then use the postcode provided next year and failing that use the school postcode
LSAY_2006_data$Postcode <- ifelse(LSAY_2006_data$Postcode == 0,NA,LSAY_2006_data$Postcode)
Restricted_2006$PC2006 <- LSAY_2006_data$Postcode

LSAY_2006_data$Location_at_end_of_school <- mapply(function (x,y) Restricted_2006[x,paste0("PC",y)], 
                                                   x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$Last_year_at_school) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2006_data$Location_at_end_of_school <- ifelse(LSAY_2006_data$Location_at_end_of_school %in% c("9999","0"),NA,
                                                   LSAY_2006_data$Location_at_end_of_school)

LSAY_2009_data$Postcode <- ifelse(Restricted_2009$PC2010 == 0 & is.na(Restricted_2009$PC2011) == TRUE,Restricted_2009$SCHPCODE,
                                  ifelse(Restricted_2009$PC2010 == 0,Restricted_2009$PC2011,Restricted_2009$PC2010)) # if they don't know their postcode, then use the postcode provided next year and failing that use the school postcode
LSAY_2009_data$Postcode <- ifelse(LSAY_2009_data$Postcode == 0,NA,LSAY_2009_data$Postcode)
Restricted_2009$PC2009 <- LSAY_2009_data$Postcode
LSAY_2009_data$Location_at_end_of_school <- mapply(function (x,y) Restricted_2009[x,paste0("PC",y)], 
                                                   x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$Last_year_at_school) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2009_data$Location_at_end_of_school <- as.numeric(ifelse(LSAY_2009_data$Location_at_end_of_school %in% c("9999","0"),NA,
                                                              LSAY_2009_data$Location_at_end_of_school))
LSAY_2015_data$Postcode <- ifelse(Restricted_2015$PC2016 == 0 | is.na(Restricted_2015$PC2016),Restricted_2015$SCHPCODE,
                                  Restricted_2015$PC2016)
LSAY_2015_data$Postcode <- ifelse(LSAY_2015_data$Postcode == 0,NA,LSAY_2015_data$Postcode)
LSAY_2015_data$Postcode <- ifelse(nchar(LSAY_2015_data$Postcode) <4,paste0("0",LSAY_2015_data$Postcode),LSAY_2015_data$Postcode)



Years_1995 <- c(1995:2003)
LSAY_1995_data$First_year_uni <- Years_1995[sapply(1:nrow(LSAY_1995), function (x) {min(which(LSAY_1995[x,paste0("XBAC",Years_1995)] %in% 1))})]

Years_1998 <- c(1998:2006)
LSAY_1998_data$First_year_uni <- Years_1998[sapply(1:nrow(LSAY_1998), function (x) {min(which(LSAY_1998[x,paste0("XBAC",Years_1998)] %in% 1))})]

Years_2003 <- c(2004,2005,2006,2007,2008,2009,2010)
LSAY_2003_data$First_year_uni <- Years_2003[sapply(1:nrow(LSAY_2003), function (x) {min(which(LSAY_2003[x,paste0("XBAC",Years_2003)] %in% 1))})]

Years_2006 <- c(2007,2008,2009,2010,2011,2012,2013)
LSAY_2006_data$First_year_uni <- Years_2006[sapply(1:nrow(LSAY_2006), function (x) {min(which(LSAY_2006[x,paste0("XBAC",Years_2006)] %in% 1))})]

Years_2009 <-  c(2010,2011,2012,2013,2014,2015,2016)
LSAY_2009_data$First_year_uni <- Years_2009[sapply(1:nrow(LSAY_2009), function (x) {min(which(LSAY_2009[x,paste0("XBAC",Years_2009)] %in% 1))})]

LSAY_2003_data$Location_uni <- mapply(function (x,y) Restricted_2003[x,paste0("PC",y)], 
                                      x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)

LSAY_2006_data$Location_uni <- mapply(function (x,y) Restricted_2006[x,paste0("PC",y)], 
                                      x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)

LSAY_2009_data$Location_uni <- mapply(function (x,y) Restricted_2009[x,paste0("PC",y)], 
                                      x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)


LSAY_2003_data <- LSAY_2003_data %>%
  mutate(Freq_housing_payments_2006 = ifelse(LSAY_2003$LDH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LDH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LDH009== 3,1, 
                                                           ifelse(LSAY_2003$LDH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2007 = ifelse(LSAY_2003$LEH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LEH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LEH009== 3,1, 
                                                           ifelse(LSAY_2003$LEH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2008 = ifelse(LSAY_2003$LFH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LFH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LFH009== 3,1, 
                                                           ifelse(LSAY_2003$LFH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2009 = ifelse(LSAY_2003$LGH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LGH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LGH009== 3,1, 
                                                           ifelse(LSAY_2003$LGH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2010 = ifelse(LSAY_2003$LHH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LHH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LHH009== 3,1, 
                                                           ifelse(LSAY_2003$LHH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2011 = ifelse(LSAY_2003$LIH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LIH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LIH009== 3,1, 
                                                           ifelse(LSAY_2003$LIH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2012 = ifelse(LSAY_2003$LJH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LJH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LJH009== 3,1, 
                                                           ifelse(LSAY_2003$LJH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2013 = ifelse(LSAY_2003$LKH009== 1,365/7/12,
                                             ifelse(LSAY_2003$LKH009== 2,365/14/12, 
                                                    ifelse(LSAY_2003$LKH009== 3,1, 
                                                           ifelse(LSAY_2003$LKH009== 5,0,NA)))))

LSAY_2006_data <- LSAY_2006_data %>%
  mutate(Freq_housing_payments_2009 = ifelse(LSAY_2006$LDH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LDH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LDH009== 3,1, 
                                                           ifelse(LSAY_2006$LDH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2010 = ifelse(LSAY_2006$LEH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LEH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LEH009== 3,1, 
                                                           ifelse(LSAY_2006$LEH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2011 = ifelse(LSAY_2006$LFH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LFH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LFH009== 3,1, 
                                                           ifelse(LSAY_2006$LFH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2012 = ifelse(LSAY_2006$LGH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LGH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LGH009== 3,1, 
                                                           ifelse(LSAY_2006$LGH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2013 = ifelse(LSAY_2006$LHH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LHH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LHH009== 3,1, 
                                                           ifelse(LSAY_2006$LHH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2014 = ifelse(LSAY_2006$LIH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LIH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LIH009== 3,1, 
                                                           ifelse(LSAY_2006$LIH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2015 = ifelse(LSAY_2006$LJH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LJH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LJH009== 3,1, 
                                                           ifelse(LSAY_2006$LJH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2016 = ifelse(LSAY_2006$LKH009== 1,365/7/12,
                                             ifelse(LSAY_2006$LKH009== 2,365/14/12, 
                                                    ifelse(LSAY_2006$LKH009== 3,1, 
                                                           ifelse(LSAY_2006$LKH009== 5,0,NA)))))

LSAY_2009_data <- LSAY_2009_data %>%
  mutate(Freq_housing_payments_2012 = ifelse(LSAY_2009$LDH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LDH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LDH009== 3,1, 
                                                           ifelse(LSAY_2009$LDH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2013 = ifelse(LSAY_2009$LEH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LEH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LEH009== 3,1, 
                                                           ifelse(LSAY_2009$LEH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2014 = ifelse(LSAY_2009$LFH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LFH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LFH009== 3,1, 
                                                           ifelse(LSAY_2009$LFH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2015 = ifelse(LSAY_2009$LGH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LGH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LGH009== 3,1, 
                                                           ifelse(LSAY_2009$LGH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2016 = ifelse(LSAY_2009$LHH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LHH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LHH009== 3,1, 
                                                           ifelse(LSAY_2009$LHH009== 5,0,NA))))) %>% 
  mutate(Freq_housing_payments_2017 = ifelse(LSAY_2009$LIH009== 1,365/7/12,
                                             ifelse(LSAY_2009$LIH009== 2,365/14/12, 
                                                    ifelse(LSAY_2009$LIH009== 3,1, 
                                                           ifelse(LSAY_2009$LIH009== 5,0,NA))))) 


LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Housing_payments_2006 = ifelse(LSAY_2003$LDH010==9999,NA,LSAY_2003$LDH010)) %>% 
  mutate(Housing_payments_2007 = ifelse(LSAY_2003$LEH010==9999,NA,LSAY_2003$LEH010)) %>% 
  mutate(Housing_payments_2008 = ifelse(LSAY_2003$LFH010==9999,NA,LSAY_2003$LFH010)) %>% 
  mutate(Housing_payments_2009 = ifelse(LSAY_2003$LGH010==9999,NA,LSAY_2003$LGH010)) %>% 
  mutate(Housing_payments_2010 = ifelse(LSAY_2003$LHH010==9999,NA,LSAY_2003$LHH010)) %>% 
  mutate(Housing_payments_2011 = ifelse(LSAY_2003$LIH010==9999,NA,LSAY_2003$LIH010)) %>% 
  mutate(Housing_payments_2012 = ifelse(LSAY_2003$LJH010==9999,NA,LSAY_2003$LJH010)) %>% 
  mutate(Housing_payments_2013 = ifelse(LSAY_2003$LKH010==9999,NA,LSAY_2003$LKH010))

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Housing_payments_2009 = ifelse(LSAY_2006$LDH010==9999,NA,LSAY_2006$LDH010)) %>% 
  mutate(Housing_payments_2010 = ifelse(LSAY_2006$LEH010==9999,NA,LSAY_2006$LEH010)) %>% 
  mutate(Housing_payments_2011 = ifelse(LSAY_2006$LFH010==9999,NA,LSAY_2006$LFH010)) %>% 
  mutate(Housing_payments_2012 = ifelse(LSAY_2006$LGH010==9999,NA,LSAY_2006$LGH010)) %>% 
  mutate(Housing_payments_2013 = ifelse(LSAY_2006$LHH010==9999,NA,LSAY_2006$LHH010)) %>% 
  mutate(Housing_payments_2014 = ifelse(LSAY_2006$LIH010==9999,NA,LSAY_2006$LIH010)) %>% 
  mutate(Housing_payments_2015 = ifelse(LSAY_2006$LJH010==9999,NA,LSAY_2006$LJH010)) %>% 
  mutate(Housing_payments_2016 = ifelse(LSAY_2006$LKH010==9999,NA,LSAY_2006$LKH010)) 

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Housing_payments_2012 = ifelse(LSAY_2009$LDH010==9999,NA,LSAY_2009$LDH010)) %>% 
  mutate(Housing_payments_2013 = ifelse(LSAY_2009$LEH010==9999,NA,LSAY_2009$LEH010)) %>% 
  mutate(Housing_payments_2014 = ifelse(LSAY_2009$LFH010==9999,NA,LSAY_2009$LFH010)) %>% 
  mutate(Housing_payments_2015 = ifelse(LSAY_2009$LGH010==9999,NA,LSAY_2009$LGH010)) %>% 
  mutate(Housing_payments_2016 = ifelse(LSAY_2009$LHH010==9999,NA,LSAY_2009$LHH010)) %>% 
  mutate(Housing_payments_2017 = ifelse(LSAY_2009$LIH010==9999,NA,LSAY_2009$LIH010))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Housing_payments_2006 = ifelse(Freq_housing_payments_2006 == 0,0, Housing_payments_2006 * Freq_housing_payments_2006)) %>% 
  mutate(Housing_payments_2007 = ifelse(Freq_housing_payments_2007 == 0,0, Housing_payments_2007 * Freq_housing_payments_2007)) %>% 
  mutate(Housing_payments_2008 = ifelse(Freq_housing_payments_2008 == 0,0, Housing_payments_2008 * Freq_housing_payments_2008)) %>% 
  mutate(Housing_payments_2009 = ifelse(Freq_housing_payments_2009 == 0,0, Housing_payments_2009 * Freq_housing_payments_2009)) %>% 
  mutate(Housing_payments_2010 = ifelse(Freq_housing_payments_2010 == 0,0, Housing_payments_2010 * Freq_housing_payments_2010)) %>% 
  mutate(Housing_payments_2011 = ifelse(Freq_housing_payments_2011 == 0,0, Housing_payments_2011 * Freq_housing_payments_2011)) %>% 
  mutate(Housing_payments_2012 = ifelse(Freq_housing_payments_2012 == 0,0, Housing_payments_2012 * Freq_housing_payments_2012)) %>% 
  mutate(Housing_payments_2013 = ifelse(Freq_housing_payments_2013 == 0,0, Housing_payments_2013 * Freq_housing_payments_2013)) 

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Housing_payments_2009 = ifelse(Freq_housing_payments_2009 == 0,0, Housing_payments_2009 * Freq_housing_payments_2009)) %>% 
  mutate(Housing_payments_2010 = ifelse(Freq_housing_payments_2010 == 0,0, Housing_payments_2010 * Freq_housing_payments_2010)) %>% 
  mutate(Housing_payments_2011 = ifelse(Freq_housing_payments_2011 == 0,0, Housing_payments_2011 * Freq_housing_payments_2011)) %>% 
  mutate(Housing_payments_2012 = ifelse(Freq_housing_payments_2012 == 0,0, Housing_payments_2012 * Freq_housing_payments_2012)) %>% 
  mutate(Housing_payments_2013 = ifelse(Freq_housing_payments_2013 == 0,0, Housing_payments_2013 * Freq_housing_payments_2013)) %>% 
  mutate(Housing_payments_2014 = ifelse(Freq_housing_payments_2014 == 0,0, Housing_payments_2014 * Freq_housing_payments_2014)) %>% 
  mutate(Housing_payments_2015 = ifelse(Freq_housing_payments_2015 == 0,0, Housing_payments_2015 * Freq_housing_payments_2015)) %>% 
  mutate(Housing_payments_2016 = ifelse(Freq_housing_payments_2016 == 0,0, Housing_payments_2016 * Freq_housing_payments_2016)) 

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Housing_payments_2012 = ifelse(Freq_housing_payments_2012 == 0,0, Housing_payments_2012 * Freq_housing_payments_2012)) %>% 
  mutate(Housing_payments_2013 = ifelse(Freq_housing_payments_2013 == 0,0, Housing_payments_2013 * Freq_housing_payments_2013)) %>% 
  mutate(Housing_payments_2014 = ifelse(Freq_housing_payments_2014 == 0,0, Housing_payments_2014 * Freq_housing_payments_2014)) %>% 
  mutate(Housing_payments_2015 = ifelse(Freq_housing_payments_2015 == 0,0, Housing_payments_2015 * Freq_housing_payments_2015)) %>% 
  mutate(Housing_payments_2016 = ifelse(Freq_housing_payments_2016 == 0,0, Housing_payments_2016 * Freq_housing_payments_2016)) %>% 
  mutate(Housing_payments_2017 = ifelse(Freq_housing_payments_2017 == 0,0, Housing_payments_2017 * Freq_housing_payments_2017)) 

# Cost of housing in first year uni. Note, cost of housing isn't available in waves 1-3 (up to age 17), which may disproportionally affect QLD because they finish school younger

LSAY_2003_data$Housing_payments_NA <- NA
LSAY_2003_data$Housing_payments_2004 <- NA
LSAY_2003_data$Housing_payments_2005 <- NA
LSAY_2003_data$Housing_cost_uni <- mapply(function (x,y) LSAY_2003_data[x,paste0("Housing_payments_",y)], 
                                          x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)
LSAY_2003_data$Housing_payments_NA <- NULL
LSAY_2003_data$Housing_payments_2004 <- NULL
LSAY_2003_data$Housing_payments_2005 <- NULL

LSAY_2006_data$Housing_payments_NA <- NA
LSAY_2006_data$Housing_payments_2007 <- NA
LSAY_2006_data$Housing_payments_2008 <- NA
LSAY_2006_data$Housing_cost_uni <- mapply(function (x,y) LSAY_2006_data[x,paste0("Housing_payments_",y)], 
                                          x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)
LSAY_2006_data$Housing_payments_NA <- NULL
LSAY_2006_data$Housing_payments_2007 <- NULL
LSAY_2006_data$Housing_payments_2008 <- NULL

LSAY_2009_data$Housing_payments_NA <- NA
LSAY_2009_data$Housing_payments_2010 <- NA
LSAY_2009_data$Housing_payments_2011 <- NA
LSAY_2009_data$Housing_cost_uni <- mapply(function (x,y) LSAY_2009_data[x,paste0("Housing_payments_",y)], 
                                          x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$First_year_uni) %>% as.character(.) %>% as.numeric(.)
LSAY_2009_data$Housing_payments_NA <- NULL
LSAY_2009_data$Housing_payments_2010 <- NULL
LSAY_2009_data$Housing_payments_2011 <- NULL


# UNIVERSITY LOCATIONS #

University_locations_city <- as.character(as.matrix(unique(University_locations[which(is.na(University_locations$Include)==FALSE),"Location"])))

if (file.exists("University_locations_Coord.csv")) { 
  University_locations_Coord <- read_csv("University_locations_Coord.csv", col_names = TRUE, col_types = "cnn") } else {
    
    University_locations_Coord <- geocode(paste(University_locations_city,"Australia"), key = "Enter API key")
    University_locations_Coord <- cbind(University_locations_city,University_locations_Coord)
    colnames(University_locations_Coord)[1] <- "City" 
    write_csv(University_locations_Coord,"University_locations_Coord.csv") }



### Group of Eight universities ###

Group_of_Eight_location_names <- c("Australian National University Acton","Monash University Clayton","University of Adelaide North Terrace",
                                   "University of Melbourne Parkville","University of New South Wales Kensington","University of Queensland St Lucia",
                                   "University of Sydney Camperdown","35 Stirling Hwy, Crawley WA 6009")

University_locations_Go8_Coord <- as.character(as.matrix(unique(University_locations[which(is.na(University_locations$Go8)==FALSE),"Location"]))) %>% 
  as_tibble(Group_of_Eight_location_names) %>% rename(Go8 = value) %>% 
  left_join(.,University_locations_Coord, by = c("Go8" = "City"))

### 1995 ###

LSAY_1995_data$Postcode <- LSAY_1995_data$Location_at_end_of_school

LSAY_1995_data$Closest_uni <- distance(origin_postcodes=LSAY_1995_data$Postcode, 
                                       destination_coordinates=University_locations_Coord, 
                                       csv_name="Schools_1995_Coord.csv", API_key="")


LSAY_1995_data$Closest_uni_distance_g1 <- as.factor(sapply(LSAY_1995_data$Closest_uni,  function(x) {ifelse(x < 20000, "Under 20km",
                                                                                                            ifelse(x < 200000, "20 to 200km",
                                                                                                                   ifelse(x > 200000, "Over 200km",NA)))}))

LSAY_1995_data$Closest_uni_distance_g2 <- as.factor(sapply(LSAY_1995_data$Closest_uni,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                            ifelse(x <= 80000, "40 to 80km",
                                                                                                                   ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_1995_data$Closest_uni_Go8 <- distance(origin_postcodes=LSAY_1995_data$Postcode, destination_coordinates=University_locations_Go8_Coord, 
                                           csv_name="Schools_1995_Coord.csv", API_key="")

LSAY_1995_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_1995_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

### 1998 ###

LSAY_1998_data$Postcode <- LSAY_1998_data$Location_at_end_of_school

LSAY_1998_data$Closest_uni <- distance(origin_postcodes=LSAY_1998_data$Postcode, destination_coordinates=University_locations_Coord, 
                                       csv_name="Schools_1998_Coord.csv", API_key="")

LSAY_1998_data$Closest_uni_distance_g1 <- as.factor(sapply(LSAY_1998_data$Closest_uni,  function(x) {ifelse(x < 20000, "Under 20km",
                                                                                                            ifelse(x < 200000, "20 to 200km",
                                                                                                                   ifelse(x > 200000, "Over 200km",NA)))}))

LSAY_1998_data$Closest_uni_distance_g2 <- as.factor(sapply(LSAY_1998_data$Closest_uni,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                            ifelse(x <= 80000, "40 to 80km",
                                                                                                                   ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_1998_data$Closest_uni_Go8 <- distance(origin_postcodes=LSAY_1998_data$Postcode, destination_coordinates=University_locations_Go8_Coord, 
                                           csv_name="Schools_1998_Coord.csv", API_key="")

LSAY_1998_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_1998_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

### 2003 ###
LSAY_2003_data$Postcode <- LSAY_2003_data$Location_at_end_of_school
LSAY_2003_data$Closest_uni <- distance(origin_postcodes=LSAY_2003_data$Postcode, destination_coordinates=University_locations_Coord, 
                                       csv_name="Schools_2003_Coord.csv", API_key="")

LSAY_2003_data$Closest_uni_distance_g1 <- as.factor(sapply(LSAY_2003_data$Closest_uni,  function(x) {ifelse(x < 20000, "Under 20km",
                                                                                                            ifelse(x < 200000, "20 to 200km",
                                                                                                                   ifelse(x > 200000, "Over 200km",NA)))}))

LSAY_2003_data$Closest_uni_distance_g2 <- as.factor(sapply(LSAY_2003_data$Closest_uni,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                            ifelse(x <= 80000, "40 to 80km",
                                                                                                                   ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_2003_data$Closest_uni_Go8 <- distance(origin_postcodes=LSAY_2003_data$Postcode, destination_coordinates=University_locations_Go8_Coord, 
                                           csv_name="Schools_2003_Coord.csv", API_key="")

LSAY_2003_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_2003_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

### 2006 ###

LSAY_2006_data$Postcode <- LSAY_2006_data$Location_at_end_of_school

LSAY_2006_data$Closest_uni <- distance(origin_postcodes=LSAY_2006_data$Postcode, destination_coordinates=University_locations_Coord, 
                                       csv_name="Schools_2006_Coord.csv", API_key="")

LSAY_2006_data$Closest_uni_distance_g1 <- as.factor(sapply(LSAY_2006_data$Closest_uni,  function(x) {ifelse(x < 20000, "Under 20km",
                                                                                                            ifelse(x < 200000, "20 to 200km",
                                                                                                                   ifelse(x > 200000, "Over 200km",NA)))}))

LSAY_2006_data$Closest_uni_distance_g2 <- as.factor(sapply(LSAY_2006_data$Closest_uni,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                            ifelse(x <= 80000, "40 to 80km",
                                                                                                                   ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_2006_data$Closest_uni_Go8 <- distance(origin_postcodes=LSAY_2006_data$Postcode, destination_coordinates=University_locations_Go8_Coord, 
                                           csv_name="Schools_2006_Coord.csv", API_key="")

LSAY_2006_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_2006_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_2006_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_2006_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

### 2009 ###

LSAY_2009_data$Postcode <- LSAY_2009_data$Location_at_end_of_school

LSAY_2009_data$Closest_uni <- distance(origin_postcodes=LSAY_2009_data$Postcode, destination_coordinates=University_locations_Coord, 
                                       csv_name="Schools_2009_Coord.csv", API_key="")

LSAY_2009_data$Closest_uni_distance_g1 <- as.factor(sapply(LSAY_2009_data$Closest_uni,  function(x) {ifelse(x < 20000, "Under 20km",
                                                                                                            ifelse(x < 200000, "20 to 200km",
                                                                                                                   ifelse(x > 200000, "Over 200km",NA)))}))

LSAY_2009_data$Closest_uni_distance_g2 <- as.factor(sapply(LSAY_2009_data$Closest_uni,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                            ifelse(x <= 80000, "40 to 80km",
                                                                                                                   ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_2009_data$Closest_uni_Go8 <- distance(origin_postcodes=LSAY_2009_data$Postcode, destination_coordinates=University_locations_Go8_Coord, 
                                           csv_name="Schools_2009_Coord.csv", API_key="")

LSAY_2009_data$Closest_uni_Go8_distance_g2 <- as.factor(sapply(LSAY_2009_data$Closest_uni_Go8,  function(x) {ifelse(x < 40000, "Under 40km",
                                                                                                                    ifelse(x <= 80000, "40 to 80km",
                                                                                                                           ifelse(x > 80000, "Over 80km",NA)))}))

LSAY_2009_data$Moving_cost <- as.factor(ifelse(is.na(LSAY_2009_data$Parent_SES_quartile) == TRUE | is.na(LSAY_2009_data$Closest_uni) == TRUE,NA,
                                               ifelse(LSAY_2009_data$Parent_SES_quartile == "Lower" & LSAY_2009_data$Closest_uni >= 100000,"Strong_barrier",
                                                      ifelse((LSAY_2009_data$Parent_SES_quartile == "Mid-lower" | LSAY_2009_data$Parent_SES_quartile == "Lower") &
                                                               LSAY_2009_data$Closest_uni >= 80000,"Moderate_barrier","Low_barrier"))))


rm(University_locations_Coord,University_locations_Go8_Coord)

Cohort <- 1995
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Closest_uni_distance_order <- c("Under 20km","20 to 200km","Over 200km") 
LSAY_1995_data$Closest_uni_distance_g1 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g1), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
Closest_uni_distance_order <- c("Under 40km","40 to 80km","Over 80km")
LSAY_1995_data$Closest_uni_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g2), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_1995_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression

Cohort <- 1998
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Closest_uni_distance_order <- c("Under 20km","20 to 200km","Over 200km") 
LSAY_1998_data$Closest_uni_distance_g1 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g1), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
Closest_uni_distance_order <- c("Under 40km","40 to 80km","Over 80km")
LSAY_1998_data$Closest_uni_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g2), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_1998_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression

Cohort <- 2003
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Closest_uni_distance_order <- c("Under 20km","20 to 200km","Over 200km") 
LSAY_2003_data$Closest_uni_distance_g1 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g1), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
Closest_uni_distance_order <- c("Under 40km","40 to 80km","Over 80km")
LSAY_2003_data$Closest_uni_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g2), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_2003_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression

Cohort <- 2006
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Closest_uni_distance_order <- c("Under 20km","20 to 200km","Over 200km") 
LSAY_2006_data$Closest_uni_distance_g1 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g1), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
Closest_uni_distance_order <- c("Under 40km","40 to 80km","Over 80km")
LSAY_2006_data$Closest_uni_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g2), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_2006_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_2006_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression

Cohort <- 2009
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 
Closest_uni_distance_order <- c("Under 20km","20 to 200km","Over 200km")  
LSAY_2009_data$Closest_uni_distance_g1 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g1), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
Closest_uni_distance_order <- c("Under 40km","40 to 80km","Over 80km")
LSAY_2009_data$Closest_uni_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_distance_g2), 
                                                 levels = Closest_uni_distance_order)   # Change the order for regression
LSAY_2009_data$Closest_uni_Go8_distance_g2 <- factor(get(Data_table) %>% pull(Closest_uni_Go8_distance_g2), 
                                                     levels = Closest_uni_distance_order)   # Change the order for regression

cols <- c("Parent_SES_quartile","Indigenous", "Sector", "Gender","Achievement_quartile","Parent_edu","NESB","Closest_uni_distance_g1","Aspiration")
LSAY_1995_data[cols] <- lapply(LSAY_1995_data[cols], factor)

cols <- c("Parent_SES_quartile", "Indigenous", "Sector", "Gender","Achievement_quartile","Parent_edu","NESB","Closest_uni_distance_g1","Aspiration")
LSAY_1998_data[cols] <- lapply(LSAY_1998_data[cols], factor)

rm(University_locations)

##########----------------------------------------------------------------------------------------------------##########
#        Uni commencemment variables
##########----------------------------------------------------------------------------------------------------##########


#### Year of bachelor degree commencement
Data_source <- "LSAY_1995"
Data_table <- "LSAY_1995_data"

LSAY_1995_data$Year_of_commencement <- sapply(c(1:nrow(get(Data_source))), 
                                              function(x) min(which(get(Data_source)[x,paste0("XBAC",Years_1995_cohort)] %in% c(1:3,5)))) 
LSAY_1995_data$Year_of_commencement <-  Years_1995_cohort[get(Data_table)$Year_of_commencement] 

Data_source <- "LSAY_1998"
Data_table <- "LSAY_1998_data"
LSAY_1998_data$Year_of_commencement <- sapply(c(1:nrow(get(Data_source))), 
                                              function(x) min(which(get(Data_source)[x,paste0("XBAC",Years_1998_cohort)] %in% c(1:4)))) 
LSAY_1998_data$Year_of_commencement <-  Years_1998_cohort[get(Data_table)$Year_of_commencement] 

Data_source <- "LSAY_2003"
Data_table <- "LSAY_2003_data"
LSAY_2003_data$Year_of_commencement <- sapply(c(1:nrow(get(Data_source))), 
                                              function(x) min(which(get(Data_source)[x,paste0("XBAC",Years_2003_cohort)] %in% c(1:4)))) 
LSAY_2003_data$Year_of_commencement <-  Years_2003_cohort[get(Data_table)$Year_of_commencement] 

Data_source <- "LSAY_2006"
Data_table <- "LSAY_2006_data"
LSAY_2006_data$Year_of_commencement <- sapply(c(1:nrow(get(Data_source))), 
                                              function(x) min(which(get(Data_source)[x,paste0("XBAC",Years_2006_cohort)] %in% c(1:4)))) 
LSAY_2006_data$Year_of_commencement <-  Years_2006_cohort[get(Data_table)$Year_of_commencement] 

Data_source <- "LSAY_2009"
Data_table <- "LSAY_2009_data"
LSAY_2009_data$Year_of_commencement <- sapply(c(1:nrow(get(Data_source))), 
                                              function(x) min(which(get(Data_source)[x,paste0("XBAC",Years_2009_cohort)] %in% c(1:4)))) 
LSAY_2009_data$Year_of_commencement <-  Years_2009_cohort[get(Data_table)$Year_of_commencement] 


##########----------------------------------------------------------------------------------------------------##########
#        Study status
##########----------------------------------------------------------------------------------------------------##########

# Study status (part time or full time) in first year uni (does not pick up < 1 year of uni)

Variable_lookup <- paste0("XFTS",LSAY_1995_data$Year_of_commencement)
LSAY_1995_data$Study_status_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_1995_data), function (x) (LSAY_1995[x,Variable_lookup[x]]))))

LSAY_1995_data$Study_status_commencement <- ifelse(LSAY_1995_data$Study_status_commencement == 1,"Full-time",
                                                   ifelse(LSAY_1995_data$Study_status_commencement == 2,"Part-time",NA))

Variable_lookup <- paste0("XFTS",LSAY_1998_data$Year_of_commencement)
LSAY_1998_data$Study_status_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_1998_data), function (x) (LSAY_1998[x,Variable_lookup[x]]))))

LSAY_1998_data$Study_status_commencement <- ifelse(LSAY_1998_data$Study_status_commencement == 1,"Full-time",
                                                   ifelse(LSAY_1998_data$Study_status_commencement == 2,"Part-time",NA))

Variable_lookup <- paste0("XFTS",LSAY_2003_data$Year_of_commencement)
LSAY_2003_data$Study_status_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2003_data), function (x) (LSAY_2003[x,Variable_lookup[x]]))))

LSAY_2003_data$Study_status_commencement <- ifelse(LSAY_2003_data$Study_status_commencement == 1,"Full-time",
                                                   ifelse(LSAY_2003_data$Study_status_commencement == 2,"Part-time",NA))
  
Variable_lookup <- paste0("XFTS",LSAY_2006_data$Year_of_commencement)
LSAY_2006_data$Study_status_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2006_data), function (x) (LSAY_2006[x,Variable_lookup[x]]))))

LSAY_2006_data$Study_status_commencement <- ifelse(LSAY_2006_data$Study_status_commencement == 1,"Full-time",
                                                   ifelse(LSAY_2006_data$Study_status_commencement == 2,"Part-time",NA))

Variable_lookup <- paste0("XFTS",LSAY_2009_data$Year_of_commencement)
LSAY_2009_data$Study_status_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2009_data), function (x) (LSAY_2009[x,Variable_lookup[x]]))))

LSAY_2009_data$Study_status_commencement <- ifelse(LSAY_2009_data$Study_status_commencement == 1,"Full-time",
                                                   ifelse(LSAY_2009_data$Study_status_commencement == 2,"Part-time",NA))


LSAY_1995_data$Part_time_study_ever_23 <- LSAY_1995 %>% 
  select(XFTS1995:XFTS2004) %>% 
  apply(1, function(x) any(x == 2)) %>% unlist() %>% ifelse(.,"Part-time","Full-time") %>% as.factor()

LSAY_1998_data$Part_time_study_ever_23 <- LSAY_1998 %>% 
  select(XFTS1998:XFTS2007) %>% 
  apply(1, function(x) any(x == 2)) %>% unlist() %>% ifelse(.,"Part-time","Full-time") %>% as.factor()

LSAY_2003_data$Part_time_study_ever_23 <- LSAY_2003 %>% 
  select(XFTS2003:XFTS2011) %>% 
  apply(1, function(x) any(x == 2)) %>% unlist() %>% ifelse(.,"Part-time","Full-time") %>% as.factor()

LSAY_2006_data$Part_time_study_ever_23 <- LSAY_2006 %>% 
  select(XFTS2006:XFTS2014) %>% 
  apply(1, function(x) any(x == 2)) %>% unlist() %>% ifelse(.,"Part-time","Full-time") %>% as.factor()

LSAY_2009_data$Part_time_study_ever_23 <- LSAY_2009 %>% 
  select(XFTS2009:XFTS2017) %>% 
  apply(1, function(x) any(x == 2)) %>% unlist() %>% ifelse(.,"Part-time","Full-time") %>% as.factor()


#### Location at commencement


LSAY_1995_data$Location_at_commencement <- mapply(function (x,y) Postcodes_1995[x,paste0("PC",y)], 
                                                  x = 1:nrow(LSAY_1995_data), y = LSAY_1995_data$Year_of_commencement) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_1998_data$Location_at_commencement <- mapply(function (x,y) Postcodes_1998[x,paste0("PC",y)], 
                                                  x = 1:nrow(LSAY_1998_data), y = LSAY_1998_data$Year_of_commencement) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2003_data$Location_at_commencement <- mapply(function (x,y) Restricted_2003[x,paste0("PC",y)], 
                                                  x = 1:nrow(LSAY_2003_data), y = LSAY_2003_data$Year_of_commencement) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2006_data$Location_at_commencement <- mapply(function (x,y) Restricted_2006[x,paste0("PC",y)], 
                                                  x = 1:nrow(LSAY_2006_data), y = LSAY_2006_data$Year_of_commencement) %>% 
  as.character(.) %>% as.numeric(.)

LSAY_2009_data$Location_at_commencement <- mapply(function (x,y) Restricted_2009[x,paste0("PC",y)], 
                                                  x = 1:nrow(LSAY_2009_data), y = LSAY_2009_data$Year_of_commencement) %>% 
  as.character(.) %>% as.numeric(.)


##########----------------------------------------------------------------------------------------------------##########
#        Hours worked at university commencement
##########----------------------------------------------------------------------------------------------------##########

Variable_lookup <- paste0("XHRS",LSAY_1995_data$Year_of_commencement)
LSAY_1995_data$Hours_worked_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_1995_data), function (x) (LSAY_1995[x,Variable_lookup[x]]))))

LSAY_1995_data$Hours_worked_commencement <- ifelse(LSAY_1995_data$Hours_worked_commencement == 998,0,
                                                   ifelse(LSAY_1995_data$Hours_worked_commencement == 999,NA,LSAY_1995_data$Hours_worked_commencement))

Variable_lookup <- paste0("XHRS",LSAY_1998_data$Year_of_commencement)
LSAY_1998_data$Hours_worked_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_1998_data), function (x) (LSAY_1998[x,Variable_lookup[x]]))))

LSAY_1998_data$Hours_worked_commencement <- ifelse(LSAY_1998_data$Hours_worked_commencement == 998,0,
                                                   ifelse(LSAY_1998_data$Hours_worked_commencement == 999,NA,LSAY_1998_data$Hours_worked_commencement))

Variable_lookup <- paste0("XHRS",LSAY_2003_data$Year_of_commencement)
LSAY_2003_data$Hours_worked_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2003_data), function (x) (LSAY_2003[x,Variable_lookup[x]]))))

LSAY_2003_data$Hours_worked_commencement <- ifelse(LSAY_2003_data$Hours_worked_commencement == 998,0,
                                                   ifelse(LSAY_2003_data$Hours_worked_commencement == 999,NA,LSAY_2003_data$Hours_worked_commencement))

Variable_lookup <- paste0("XHRS",LSAY_2006_data$Year_of_commencement)
LSAY_2006_data$Hours_worked_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2006_data), function (x) (LSAY_2006[x,Variable_lookup[x]]))))

LSAY_2006_data$Hours_worked_commencement <- ifelse(LSAY_2006_data$Hours_worked_commencement == 998,0,
                                                   ifelse(LSAY_2006_data$Hours_worked_commencement == 999,NA,LSAY_2006_data$Hours_worked_commencement))

Variable_lookup <- paste0("XHRS",LSAY_2009_data$Year_of_commencement)
LSAY_2009_data$Hours_worked_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2009_data), function (x) (LSAY_2009[x,Variable_lookup[x]]))))

LSAY_2009_data$Hours_worked_commencement <- ifelse(LSAY_2009_data$Hours_worked_commencement == 998,0,
                                                   ifelse(LSAY_2009_data$Hours_worked_commencement == 999,NA,LSAY_2009_data$Hours_worked_commencement))



#### Institution at commencement ####


#2003
Variable_lookup <- sapply(1:nrow(LSAY_2003_data), function(x) {
  vlookup(LSAY_2003_data[x,"Year_of_commencement"],
          Institution_variables %>% filter(Cohort == 2003), "Year","Study_institution")})

LSAY_2003_data$Institution_code_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2003_data), function (x) (LSAY_2003[x,Variable_lookup[x]]))))

LSAY_2003_data$Institution_at_commencement <- sapply(1:nrow(LSAY_2003_data), function(x) {
  ifelse(LSAY_2003_data[x,"Year_of_commencement"] < 2005, 
         vlookup(LSAY_2003_data[x,"Institution_code_commencement"],Institution_table_pre_2005, "Code","Institution"),
         vlookup(LSAY_2003_data[x,"Institution_code_commencement"],Institution_table_2005_onwards, "Code","Institution"))})

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Institution_network = ifelse(Institution_at_commencement %in% Australian_Technology_Network,"Australian Technology Network",
                                      ifelse(Institution_at_commencement %in% Group_of_Eight, "Group of Eight", 
                                             ifelse(Institution_at_commencement %in% Innovative_Research_Universities,"Innovative Research Universities",
                                                    ifelse(Institution_at_commencement %in% Regional_Universities_Network, "Regional Universities Network",
                                                           ifelse(!is.na(Institution_at_commencement),"Other universities",NA))))))
#2006
Variable_lookup <- sapply(1:nrow(LSAY_2006_data), function(x) 
  vlookup(LSAY_2006_data[x,"Year_of_commencement"],Institution_variables %>% filter(Cohort == 2006), "Year","Study_institution"))

LSAY_2006_data$Institution_code_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2006_data), function (x) (LSAY_2006[x,Variable_lookup[x]]))))

LSAY_2006_data$Institution_at_commencement <- sapply(1:nrow(LSAY_2006_data), function(x) {
  ifelse(LSAY_2006_data[x,"Year_of_commencement"] < 2005, 
         vlookup(LSAY_2006_data[x,"Institution_code_commencement"],Institution_table_pre_2005, "Code","Institution"),
         vlookup(LSAY_2006_data[x,"Institution_code_commencement"],Institution_table_2005_onwards, "Code","Institution"))})

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Institution_network = ifelse(Institution_at_commencement %in% Australian_Technology_Network,"Australian Technology Network",
                                      ifelse(Institution_at_commencement %in% Group_of_Eight, "Group of Eight", 
                                             ifelse(Institution_at_commencement %in% Innovative_Research_Universities,"Innovative Research Universities",
                                                    ifelse(Institution_at_commencement %in% Regional_Universities_Network, "Regional Universities Network",
                                                           ifelse(!is.na(Institution_at_commencement),"Other universities",NA))))))


#2009
Variable_lookup <- sapply(1:nrow(LSAY_2009_data), function(x) 
  vlookup(LSAY_2009_data[x,"Year_of_commencement"],Institution_variables %>% filter(Cohort == 2009), "Year","Study_institution"))

LSAY_2009_data$Institution_code_commencement <- as.numeric(as.character(sapply(1:nrow(LSAY_2009_data), function (x) (LSAY_2009[x,Variable_lookup[x]]))))

LSAY_2009_data$Institution_at_commencement <- sapply(1:nrow(LSAY_2009_data), function(x) {
  ifelse(LSAY_2009_data[x,"Year_of_commencement"] < 2005, 
         vlookup(LSAY_2009_data[x,"Institution_code_commencement"],Institution_table_pre_2005, "Code","Institution"),
         vlookup(LSAY_2009_data[x,"Institution_code_commencement"],Institution_table_2005_onwards, "Code","Institution"))})

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Institution_network = ifelse(Institution_at_commencement %in% Australian_Technology_Network,"Australian Technology Network",
                                      ifelse(Institution_at_commencement %in% Group_of_Eight, "Group of Eight", 
                                             ifelse(Institution_at_commencement %in% Innovative_Research_Universities,"Innovative Research Universities",
                                                    ifelse(Institution_at_commencement %in% Regional_Universities_Network, "Regional Universities Network",
                                                           ifelse(!is.na(Institution_at_commencement),"Other universities",NA))))))

rm(Variable_lookup)

# New location variable
Postcode_location$Location_Max <- colnames(Postcode_location[,c(2:4)])[apply(Postcode_location[,c(2:4)],1,which.max)]

Postcode_location$Location  <- sapply(Postcode_location$Location_Max,  function(x) {
  ifelse(x == "metro", "Metropolitan",
         ifelse(x == "regional", "Regional",
                ifelse(x == "remote", "Remote",x)))})


LSAY_1995_data$Location <- as.factor(with(Postcode_location,Location[match(LSAY_1995_data$Location_at_end_of_school,postcode)]))  
LSAY_1998_data$Location <- as.factor(with(Postcode_location,Location[match(LSAY_1998_data$Location_at_end_of_school,postcode)]))  
LSAY_2003_data$Location <- as.factor(with(Postcode_location,Location[match(LSAY_2003_data$Location_at_end_of_school,postcode)]))  
LSAY_2006_data$Location <- as.factor(with(Postcode_location,Location[match(LSAY_2006_data$Location_at_end_of_school,postcode)]))  
LSAY_2009_data$Location <- as.factor(with(Postcode_location,Location[match(LSAY_2009_data$Location_at_end_of_school,postcode)]))  


# New location variable2: combining regional and remote

Postcode_location$Location_RR  <- sapply(Postcode_location$Location_Max,  function(x) {
  ifelse(x == "metro", "Metropolitan",
         ifelse(x == "regional", "Regional_remote",
                ifelse(x == "remote", "Regional_remote",x)))})

# New location variable3: 2006 ASGS

Postcode_location$Location_RR_3  <- vlookup(Postcode_location$postcode,ASGS_correspondance,"POA_2006_CODE","Location")
Postcode_location$Location_RR <- ifelse(is.na(Postcode_location$Location_RR_3),Postcode_location$Location_RR,Postcode_location$Location_RR_3) # if missing, then leave it as it was

Postcode_location$Location_RR_3 <- NULL

LSAY_1995_data$Location_at_first_wave <- Restricted_1995$postcode
LSAY_1998_data$Location_at_first_wave <- Restricted_1998$postcode
LSAY_2003_data$Location_at_first_wave <- Restricted_2003$PC2003
LSAY_2006_data$Location_at_first_wave <- ifelse(is.na(LSAY_2006_data$Postcode),Restricted_2006$POSTCODE,LSAY_2006_data$Postcode)
LSAY_2009_data$Location_at_first_wave <- ifelse(is.na(LSAY_2009_data$Postcode),Restricted_2009$SCHPCODE,LSAY_2009_data$Postcode)

LSAY_1995_data$Location_at_first_wave <- as.numeric(LSAY_1995_data$Location_at_first_wave)
LSAY_1998_data$Location_at_first_wave <- as.numeric(LSAY_1998_data$Location_at_first_wave)
LSAY_2003_data$Location_at_first_wave <- as.numeric(LSAY_2003_data$Location_at_first_wave)
LSAY_2006_data$Location_at_first_wave <- as.numeric(LSAY_2006_data$Location_at_first_wave)
LSAY_2009_data$Location_at_first_wave <- as.numeric(LSAY_2009_data$Location_at_first_wave)

LSAY_1995_data$School_postcode_guess <- sapply(LSAY_1995$schno, 
                                               function(x) {getmode(Restricted_1995[which(Restricted_1995$schno == x),"postcode"])}) # get the most common postcode for kids at that school
LSAY_1995_data$Location_at_first_wave <- ifelse(LSAY_1995_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                LSAY_1995_data$School_postcode_guess,LSAY_1995_data$Location_at_first_wave)       


LSAY_1998_data$School_postcode_guess <- sapply(LSAY_1998$schoolno, 
                                               function(x) {getmode(Restricted_1998[which(Restricted_1998$schoolno == x),"postcode"])}) # get the most common postcode for kids at that school
LSAY_1998_data$Location_at_first_wave <- ifelse(LSAY_1998_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                LSAY_1998_data$School_postcode_guess,LSAY_1998_data$Location_at_first_wave)       

LSAY_2003_data$School_postcode_guess <- sapply(LSAY_2003$SCHOOLID, 
                                               function(x) {getmode(Restricted_2003[which(LSAY_2003$SCHOOLID == x),"PC2003"])}) # get the most common postcode for kids at that school
LSAY_2003_data$Location_at_first_wave <- ifelse(LSAY_2003_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                LSAY_2003_data$School_postcode_guess,LSAY_2003_data$Location_at_first_wave) 

LSAY_2006_data$Location_at_first_wave <- ifelse(LSAY_2006_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                Restricted_2006$POSTCODE,LSAY_2006_data$Location_at_first_wave)  

Restricted_2006_no_na <- Restricted_2006
Restricted_2006_no_na$SCHOOLID <- LSAY_2006$SCHOOLID
Restricted_2006_no_na <- Restricted_2006_no_na[which(!is.na(Restricted_2006$PC2007)),]

LSAY_2006_data$School_postcode_guess_wave2 <- sapply(LSAY_2006$SCHOOLID, 
                                                     function(x) {getmode(Restricted_2006_no_na[which(Restricted_2006_no_na$SCHOOLID == x),"PC2007"])}) # get the most common postcode for kids at that school

LSAY_2006_data$Location_at_first_wave <- ifelse(LSAY_2006_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                LSAY_2006_data$School_postcode_guess_wave2,LSAY_2006_data$Location_at_first_wave) 

LSAY_2006_data$School_postcode_guess_wave2 <- NULL
rm(Restricted_2006_no_na)

LSAY_2009_data$Location_at_first_wave <- ifelse(LSAY_2009_data$Location_at_first_wave %ni% Postcode_location$postcode,
                                                Restricted_2009$SCHPCODE,LSAY_2009_data$Location_at_first_wave)  

LSAY_2015_data$Location_at_first_wave <- ifelse(LSAY_2015_data$Postcode %ni% Postcode_location$postcode,
                                                Restricted_2015$SCHPCODE,LSAY_2015_data$Postcode)  

LSAY_1995_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_1995_data$Location_at_first_wave,postcode)]))  
LSAY_1998_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_1998_data$Location_at_first_wave,postcode)]))  
LSAY_2003_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2003_data$Location_at_first_wave,postcode)]))  
LSAY_2006_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2006_data$Location_at_first_wave,postcode)]))  
LSAY_2009_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2009_data$Location_at_first_wave,postcode)]))  
LSAY_2015_data$Location_RR_wave1 <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2015_data$Location_at_first_wave,postcode_4digit)]))  

LSAY_1995_data$Location_RR <- as.factor(with(Postcode_location,Location_RR[match(LSAY_1995_data$Location_at_end_of_school,postcode)]))  
LSAY_1998_data$Location_RR <- as.factor(with(Postcode_location,Location_RR[match(LSAY_1998_data$Location_at_end_of_school,postcode)]))  
LSAY_2003_data$Location_RR <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2003_data$Location_at_end_of_school,postcode)]))  
LSAY_2006_data$Location_RR <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2006_data$Location_at_end_of_school,postcode)]))  
LSAY_2009_data$Location_RR <- as.factor(with(Postcode_location,Location_RR[match(LSAY_2009_data$Location_at_end_of_school,postcode)]))  


##########----------------------------------------------------------------------------------------------------##########
#        Distance moved to university
##########----------------------------------------------------------------------------------------------------##########
cohort <- NULL
Cohorts <- c(2003,2006,2009)
for (cohort in Cohorts) { 

Strata_variables_derived <- c("PISA_Math_quartile","PISA_Read_quartile","PISA_Science_quartile",
                              "Parent_SES_quartile","Location_RR_wave1")
Survey_weights <- "W_FSTUWT"

if(cohort==2003) { Strata_variables <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST03Q01","IMMIG") }
if(cohort %in% c(2006,2009)) { Strata_variables <- c("SCHOOLID","INDIG","HISCED","ST01Q01","ST04Q01","IMMIG") }

assign(paste0("LSAY_",cohort,"_data"), get(paste0("LSAY_",cohort,"_data")) %>% 
         mutate(lon = NULL,
                lat = NULL,
                Origin_lon = NULL,
                Origin_lat = NULL,
                Destination_lon = NULL,
                Destination_lat = NULL,
                Remoteness_uni = NULL))

assign(paste0("LSAY_",cohort,"_data"), get(paste0("LSAY_",cohort,"_data")) %>% 
         left_join(., Postcode_location %>% select(postcode,lon,lat) %>% mutate(postcode = as.numeric(postcode)),
                                     by=c("Location_at_end_of_school" = "postcode")) %>% 
  rename(Origin_lon = lon,Origin_lat=lat) %>% 
    left_join(., Postcode_location %>% select(postcode,lon,lat,Location_RR) %>% 
                                       rename(Remoteness_uni=Location_RR) %>% 
                                       mutate(postcode = as.numeric(postcode)),
                                     by=c("Location_uni" = "postcode")) %>% 
  rename(Destination_lon = lon,Destination_lat=lat) %>% 
    left_join(., Postcode_location %>% select(postcode,lon,lat) %>% mutate(postcode = as.numeric(postcode)),
                                     by=c("Location_at_end_of_school" = "postcode")) %>% 
    mutate(Remoteness_uni = as.factor(gsub("Regional_remote", "Regional or remote", Remoteness_uni))) %>% 
    mutate_at(vars(c("Origin_lon","Origin_lat","Destination_lon","Destination_lat")), list(as.numeric)) %>% 
    mutate(Distance_moved_to_uni = distGeo(matrix(c(Origin_lon, Origin_lat), ncol = 2),
                                           matrix(c(Destination_lon, Destination_lat), ncol = 2)),
           Moved_to_uni = ifelse(Distance_moved_to_uni>0,1,0),
           Not_moved_to_uni = ifelse(Distance_moved_to_uni==0,1,0),
           Remoteness_end_school = as.factor(gsub("Regional_remote", "Regional or remote", Location_RR)),
           Movement = ifelse(Remoteness_end_school == Remoteness_uni, paste0("Unchanged_",Remoteness_end_school),
                             ifelse(Remoteness_end_school == "Metropolitan" & Remoteness_uni == "Regional or remote","Metro_to_RR",
                                    ifelse(Remoteness_end_school == "Regional or remote" & Remoteness_uni == "Metropolitan","RR_to_metro",NA))),
           Movement_detailed = ifelse(Movement == "Metro_to_RR","Metro_to_RR",
                                      ifelse(Movement == "RR_to_metro","RR_to_metro",
                                             ifelse(Moved_to_uni == 1 & Remoteness_end_school == "Metropolitan","Moved_within_metro",
                                                    ifelse(Moved_to_uni == 1 & Remoteness_end_school == "Regional or remote","Moved_within_regional",
                                                           ifelse(Moved_to_uni == 0, paste0("Unchanged_",Remoteness_end_school),NA))))),
           Movement_detailed_4 = ifelse(Movement_detailed=="Metro_to_RR" | Movement_detailed=="Moved_within_regional","Regional_moved",
                                        ifelse(Movement_detailed=="Moved_within_metro" | Movement_detailed=="RR_to_metro","Metro_moved",
                                               ifelse(Movement_detailed=="Unchanged_Metropolitan","Didn't move",
                                                      ifelse(Movement_detailed=="Unchanged_Regional or remote","Didn't move",NA)))),
           Movement_data = ifelse(!is.na(Movement_detailed) & !is.na(Housing_cost_uni),1,NA)))
}



##########----------------------------------------------------------------------------------------------------##########
#        Uni completion variables
##########----------------------------------------------------------------------------------------------------##########

#### Year of completion is only for bachelor degree and NOT undertaking further study at a higher level.

Years_1995_cohort <- c(1995:2003)
Years_1998_cohort <- c(1998:2009)
Years_2003_cohort <- c(2003:2013)
Years_2006_cohort <- c(2006:2016)
Years_2009_cohort <- c(2009:2017)
LSAY_1995_data$Year_of_completion <- sapply(c(1:nrow(LSAY_1995)), 
                                                   function(x) min(which(LSAY_1995[x,paste0("XBAC",Years_1995_cohort)] %in% 2)))
LSAY_1995_data$Year_of_completion <-  Years_1995_cohort[LSAY_1995_data$Year_of_completion] 

LSAY_1998_data$Year_of_completion <- sapply(c(1:nrow(LSAY_1998)), 
                                            function(x) min(which(LSAY_1998[x,paste0("XBAC",Years_1998_cohort)] %in% 2)))
LSAY_1998_data$Year_of_completion <-  Years_1998_cohort[LSAY_1998_data$Year_of_completion] 

LSAY_2003_data$Year_of_completion <- sapply(c(1:nrow(LSAY_2003)), 
                                            function(x) min(which(LSAY_2003[x,paste0("XBAC",Years_2003_cohort)] %in% 2)))
LSAY_2003_data$Year_of_completion <-  Years_2003_cohort[LSAY_2003_data$Year_of_completion] 

LSAY_2006_data$Year_of_completion <- sapply(c(1:nrow(LSAY_2006)), 
                                            function(x) min(which(LSAY_2006[x,paste0("XBAC",Years_2006_cohort)] %in% 2)))
LSAY_2006_data$Year_of_completion <-  Years_2006_cohort[LSAY_2006_data$Year_of_completion] 

LSAY_2009_data$Year_of_completion <- sapply(c(1:nrow(LSAY_2009)), 
                                            function(x) min(which(LSAY_2009[x,paste0("XBAC",Years_2009_cohort)] %in% 2)))
LSAY_2009_data$Year_of_completion <-  Years_2009_cohort[LSAY_2009_data$Year_of_completion] 


LSAY_1995_data$Year_of_completion_inc_pg <- sapply(c(1:nrow(LSAY_1995)), 
                                                   function(x) min(which(LSAY_1995[x,paste0("XBAC",Years_1995_cohort)] %in% c(2,5)))) 
LSAY_1995_data$Year_of_completion_inc_pg <-  Years_1995_cohort[LSAY_1995_data$Year_of_completion_inc_pg] 

LSAY_1998_data$Year_of_completion_inc_pg <- sapply(c(1:nrow(LSAY_1998)), 
                                              function(x) min(which(LSAY_1998[x,paste0("XBAC",Years_1998_cohort)] %in% c(2:3)))) 
LSAY_1998_data$Year_of_completion_inc_pg <-  Years_1998_cohort[LSAY_1998_data$Year_of_completion_inc_pg] 

LSAY_2003_data$Year_of_completion_inc_pg <- sapply(c(1:nrow(LSAY_2003)), 
                                                   function(x) min(which(LSAY_2003[x,paste0("XBAC",Years_2003_cohort)] %in% c(2:3)))) 
LSAY_2003_data$Year_of_completion_inc_pg <-  Years_2003_cohort[LSAY_2003_data$Year_of_completion_inc_pg] 

LSAY_2006_data$Year_of_completion_inc_pg <- sapply(c(1:nrow(LSAY_2006)), 
                                                   function(x) min(which(LSAY_2006[x,paste0("XBAC",Years_2006_cohort)] %in% c(2:3)))) 
LSAY_2006_data$Year_of_completion_inc_pg <-  Years_2006_cohort[LSAY_2006_data$Year_of_completion_inc_pg] 

LSAY_2009_data$Year_of_completion_inc_pg <- sapply(c(1:nrow(LSAY_2009)), 
                                                   function(x) min(which(LSAY_2009[x,paste0("XBAC",Years_2009_cohort)] %in% c(2:3)))) 
LSAY_2009_data$Year_of_completion_inc_pg <-  Years_2009_cohort[LSAY_2009_data$Year_of_completion_inc_pg] 


  
LSAY_2003_data$Age_of_commencement <- vlookup(LSAY_2003_data$Year_of_commencement,Age_table %>% filter(Cohort == 2003),"Year","Age")
LSAY_2006_data$Age_of_commencement <- vlookup(LSAY_2006_data$Year_of_commencement,Age_table %>% filter(Cohort == 2006),"Year","Age")
LSAY_2009_data$Age_of_commencement <- vlookup(LSAY_2009_data$Year_of_commencement,Age_table %>% filter(Cohort == 2009),"Year","Age")

Year_age_22_1995 <- 2003
Year_age_22_1998 <- 2006
Year_age_22_2003 <- 2010
Year_age_22_2006 <- 2013
Year_age_22_2009 <- 2016

LSAY_1995_data <- LSAY_1995_data %>% 
  mutate(Years_to_complete_22 = ifelse(Year_of_completion_inc_pg <= Year_age_22_1995,Year_of_completion_inc_pg-Year_of_commencement,NA))

LSAY_1998_data <- LSAY_1998_data %>% 
  mutate(Years_to_complete_22 = ifelse(Year_of_completion_inc_pg <= Year_age_22_1998,Year_of_completion_inc_pg-Year_of_commencement,NA))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Years_to_complete_22 = ifelse(Year_of_completion_inc_pg <= Year_age_22_2003,Year_of_completion_inc_pg-Year_of_commencement,NA))

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Years_to_complete_22 = ifelse(Year_of_completion_inc_pg <= Year_age_22_2006,Year_of_completion_inc_pg-Year_of_commencement,NA))

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Years_to_complete_22 = ifelse(Year_of_completion_inc_pg <= Year_age_22_2009,Year_of_completion_inc_pg-Year_of_commencement,NA))


#####

rm(LSAY_1995_Fixed)

##########----------------------------------------------------------------------------------------------------##########
#        Outcome variables
##########----------------------------------------------------------------------------------------------------##########

################ average weekly pay, hours worked and hourly pay ################

Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("Average_weekly_pay_",i)] <- LSAY_1995[,paste0("XWKP",i)]
  LSAY_1995_data[,paste0("Average_weekly_hours_worked_",i)] <- LSAY_1995[,paste0("XHRS",i)]
  LSAY_1995_data[,paste0("Average_hourly_pay_",i)] <- LSAY_1995[,paste0("XHRP",i)]
  
  LSAY_1995_data <- LSAY_1995_data %>% 
    mutate(!!paste0("Average_weekly_pay_",i) := replace (get(paste0("Average_weekly_pay_",i)) ,get(paste0("Average_weekly_pay_",i))  >9997,NA),
           !!paste0("Average_weekly_hours_worked_",i) := replace(get(paste0("Average_weekly_hours_worked_",i)),
                                                                 get(paste0("Average_weekly_hours_worked_",i)) > 997,NA),
           !!paste0("Average_hourly_pay_",i) := replace (get(paste0("Average_hourly_pay_",i)) ,get(paste0("Average_hourly_pay_",i))  >9997,NA))
}

Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("Average_weekly_pay_",i)] <- LSAY_1998[,paste0("XWKP",i)]
  LSAY_1998_data[,paste0("Average_weekly_hours_worked_",i)] <- LSAY_1998[,paste0("XHRS",i)]
  LSAY_1998_data[,paste0("Average_hourly_pay_",i)] <- LSAY_1998[,paste0("XHRP",i)]
  
  LSAY_1998_data <- LSAY_1998_data %>% 
    mutate(!!paste0("Average_weekly_pay_",i) := replace (get(paste0("Average_weekly_pay_",i)) ,get(paste0("Average_weekly_pay_",i))  >9997,NA),
           !!paste0("Average_weekly_hours_worked_",i) := replace(get(paste0("Average_weekly_hours_worked_",i)),
                                                                 get(paste0("Average_weekly_hours_worked_",i)) > 997,NA),
           !!paste0("Average_hourly_pay_",i) := replace (get(paste0("Average_hourly_pay_",i)) ,get(paste0("Average_hourly_pay_",i))  >9997,NA))
}

Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("Average_weekly_pay_",i)] <- LSAY_2003[,paste0("XWKP",i)]
  LSAY_2003_data[,paste0("Average_weekly_hours_worked_",i)] <- LSAY_2003[,paste0("XHRS",i)]
  LSAY_2003_data[,paste0("Average_hourly_pay_",i)] <- LSAY_2003[,paste0("XHRP",i)]
  
  LSAY_2003_data <- LSAY_2003_data %>% 
    mutate(!!paste0("Average_weekly_pay_",i) := replace (get(paste0("Average_weekly_pay_",i)) ,get(paste0("Average_weekly_pay_",i))  >9997,NA),
           !!paste0("Average_weekly_hours_worked_",i) := replace(get(paste0("Average_weekly_hours_worked_",i)),
                                                                 get(paste0("Average_weekly_hours_worked_",i)) > 997,NA),
           !!paste0("Average_hourly_pay_",i) := replace (get(paste0("Average_hourly_pay_",i)) ,get(paste0("Average_hourly_pay_",i))  >9997,NA))
}

Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("Average_weekly_pay_",i)] <- LSAY_2006[,paste0("XWKP",i)]
  LSAY_2006_data[,paste0("Average_weekly_hours_worked_",i)] <- LSAY_2006[,paste0("XHRS",i)]
  LSAY_2006_data[,paste0("Average_hourly_pay_",i)] <- LSAY_2006[,paste0("XHRP",i)]
  
  LSAY_2006_data <- LSAY_2006_data %>% 
    mutate(!!paste0("Average_weekly_pay_",i) := replace (get(paste0("Average_weekly_pay_",i)) ,get(paste0("Average_weekly_pay_",i))  >9997,NA),
           !!paste0("Average_weekly_hours_worked_",i) := replace(get(paste0("Average_weekly_hours_worked_",i)),
                                                                 get(paste0("Average_weekly_hours_worked_",i)) > 997,NA),
           !!paste0("Average_hourly_pay_",i) := replace (get(paste0("Average_hourly_pay_",i)) ,get(paste0("Average_hourly_pay_",i))  >9997,NA))
}

Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("Average_weekly_pay_",i)] <- LSAY_2009[,paste0("XWKP",i)]
  LSAY_2009_data[,paste0("Average_weekly_hours_worked_",i)] <- LSAY_2009[,paste0("XHRS",i)]
  LSAY_2009_data[,paste0("Average_hourly_pay_",i)] <- LSAY_2009[,paste0("XHRP",i)]
  
  LSAY_2009_data <- LSAY_2009_data %>% 
    mutate(!!paste0("Average_weekly_pay_",i) := replace (get(paste0("Average_weekly_pay_",i)) ,get(paste0("Average_weekly_pay_",i))  >9997,NA),
           !!paste0("Average_weekly_hours_worked_",i) := replace(get(paste0("Average_weekly_hours_worked_",i)),
                                                                 get(paste0("Average_weekly_hours_worked_",i)) > 997,NA),
           !!paste0("Average_hourly_pay_",i) := replace (get(paste0("Average_hourly_pay_",i)) ,get(paste0("Average_hourly_pay_",i))  >9997,NA))
}




################################## ANZSCO ##############################

ANZSCO_table <- tibble(XOCC_number = c(1:10),
                       XOCC_variable=c("Manager","Professionals","Tech and Trade","Community and personal serv",
                                       "clerical and admin","Sales","Machine opperator","labourer",NA,"Not working (unemp or NILF)"),
                       Manager_prof = c(1,1,0,0,0,0,0,0,NA,0))
ANZSCO_table


Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data <- LSAY_1995_data %>% 
    mutate(!!paste0("ANZSCO",i) :=  vlookup(LSAY_1995 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","XOCC_variable"),
           !!paste0("Manager_prof",i) := vlookup(LSAY_1995 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","Manager_prof"))
}

Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data <- LSAY_1998_data %>% 
    mutate(!!paste0("ANZSCO",i) :=  vlookup(LSAY_1998 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","XOCC_variable"),
           !!paste0("Manager_prof",i) := vlookup(LSAY_1998 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","Manager_prof"))
}

Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data <- LSAY_2003_data %>% 
    mutate(!!paste0("ANZSCO",i) :=  vlookup(LSAY_2003 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","XOCC_variable"),
           !!paste0("Manager_prof",i) := vlookup(LSAY_2003 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","Manager_prof"))
}

Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data <- LSAY_2006_data %>% 
    mutate(!!paste0("ANZSCO",i) :=  vlookup(LSAY_2006 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","XOCC_variable"),
           !!paste0("Manager_prof",i) := vlookup(LSAY_2006 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","Manager_prof"))
}

Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data <- LSAY_2009_data %>% 
    mutate(!!paste0("ANZSCO",i) :=  vlookup(LSAY_2009 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","XOCC_variable"),
           !!paste0("Manager_prof",i) := vlookup(LSAY_2009 %>% pull(paste0("XOCC",i)),ANZSCO_table, "XOCC_number","Manager_prof"))
}


################################## In full time employment or full time education ##############################


Data_source <- "LSAY_1995"
Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("Full_time_emp_study",i)] <- ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 1,"FT emp or edu",
                                                             ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 0,"Not in FT emp or edu",NA))
  LSAY_1995_data[,paste0("Spell_of_unemploymen_last_year",i)] <- ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 1,1,
                                                                        ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 0,0,NA))
}
Data_source <- "LSAY_1998"
Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("Full_time_emp_study",i)] <- ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 1,"FT emp or edu",
                                                             ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 0,"Not in FT emp or edu",NA))
  LSAY_1998_data[,paste0("Spell_of_unemploymen_last_year",i)] <- ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 1,1,
                                                                        ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 0,0,NA))
}

Data_source <- "LSAY_2003"
Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("Full_time_emp_study",i)] <- ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 1,"FT emp or edu",
                                                             ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 0,"Not in FT emp or edu",NA))
  LSAY_2003_data[,paste0("Spell_of_unemploymen_last_year",i)] <- ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 1,1,
                                                                        ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 0,0,NA))
}

Data_source <- "LSAY_2006"
Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("Full_time_emp_study",i)] <- ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 1,1,
                                                             ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 0,0,NA))
  LSAY_2006_data[,paste0("Spell_of_unemploymen_last_year",i)] <- ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 1,1,
                                                                        ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 0,0,NA))
}

Data_source <- "LSAY_2009"
Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("Full_time_emp_study",i)] <- ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 1,1,
                                                             ifelse(get(Data_source)[,paste0("XFTE",i)] %in% 0,0,NA))
  LSAY_2009_data[,paste0("Spell_of_unemploymen_last_year",i)] <- ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 1,1,
                                                                        ifelse(get(Data_source)[,paste0("XUNE",i)] %in% 0,0,NA))
}


#### Employment status by year

Data_source <- "LSAY_1995"
Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) {
  LSAY_1995_data[,paste0("Employment_status_",i)] <- ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 3,"NILF",
                                                            ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 2,"Unemployed",
                                                                   ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 1,"Full_time",
                                                                          ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 2,"Part_time",NA))))
}

Data_source <- "LSAY_1998"
Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) {
  LSAY_1998_data[,paste0("Employment_status_",i)] <- ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 3,"NILF",
                                                            ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 2,"Unemployed",
                                                                   ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 1,"Full_time",
                                                                          ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 2,"Part_time",NA))))
}

Data_source <- "LSAY_2003"
Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) {
  LSAY_2003_data[,paste0("Employment_status_",i)] <- ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 3,"NILF",
                                                            ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 2,"Unemployed",
                                                                   ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 1,"Full_time",
                                                                          ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 2,"Part_time",NA))))
}

Data_source <- "LSAY_2006"
Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) {
  LSAY_2006_data[,paste0("Employment_status_",i)] <- ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 3,"NILF",
                                                            ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 2,"Unemployed",
                                                                   ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 1,"Full_time",
                                                                          ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 2,"Part_time",NA))))
}

Data_source <- "LSAY_2009"
Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) {
  LSAY_2009_data[,paste0("Employment_status_",i)] <- ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 3,"NILF",
                                                            ifelse(get(Data_source)[,paste0("XLFS",i)] %in% 2,"Unemployed",
                                                                   ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 1,"Full_time",
                                                                          ifelse(get(Data_source)[,paste0("XFTP",i)] %in% 2,"Part_time",NA))))
}


# Drop out year
# 1995
Data_source <- "LSAY_1995"
Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("drop_out_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                   ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"No",
                                                          ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"Yes",
                                                                 ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                        ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_1995"
Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("Completed_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"Yes",
                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"No",
                                                                  ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                         ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"Yes", NA)))))
  
  
}



Data_source <- "LSAY_1995"
Years_1995_cohort <- c(1995:2006)
for (i in Years_1995_cohort) { 
  LSAY_1995_data[,paste0("undertaking_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,1,
                                                      ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,0,
                                                             ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,0,
                                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,0,
                                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,0, NA)))))
  
  
}
## 1998 

Data_source <- "LSAY_1998"
Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("drop_out_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                   ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"No",
                                                          ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"No",
                                                                 ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"Yes",
                                                                        ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_1998"
Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("Completed_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"Yes",
                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"Yes",
                                                                  ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                         ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_1998"
Years_1998_cohort <- c(1998:2009)
for (i in Years_1998_cohort) { 
  LSAY_1998_data[,paste0("undertaking_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,1,
                                                      ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,0,
                                                             ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,0,
                                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,0,
                                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,0, NA)))))
  
  
}

## 2003 

Data_source <- "LSAY_2003"
Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("drop_out_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                   ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"No",
                                                          ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"No",
                                                                 ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"Yes",
                                                                        ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_2003"
Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("Completed_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"Yes",
                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"Yes",
                                                                  ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                         ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_2003"
Years_2003_cohort <- c(2003:2013)
for (i in Years_2003_cohort) { 
  LSAY_2003_data[,paste0("undertaking_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,1,
                                                      ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,0,
                                                             ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,0,
                                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,0,
                                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,0, NA)))))
  
  
}

#2006

Data_source <- "LSAY_2006"
Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("drop_out_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                   ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"No",
                                                          ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"No",
                                                                 ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"Yes",
                                                                        ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_2006"
Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("Completed_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"Yes",
                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"Yes",
                                                                  ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                         ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}
Data_source <- "LSAY_2006"
Years_2006_cohort <- c(2006:2016)
for (i in Years_2006_cohort) { 
  LSAY_2006_data[,paste0("undertaking_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,1,
                                                      ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,0,
                                                             ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,0,
                                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,0,
                                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,0, NA)))))
  
  
}
##2009 drop out

Data_source <- "LSAY_2009"
Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("drop_out_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                   ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"No",
                                                          ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"No",
                                                                 ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"Yes",
                                                                        ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_2009"
Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("Completed_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,"No",
                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,"Yes",
                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,"Yes",
                                                                  ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,"No",
                                                                         ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,"No", NA)))))
  
  
}

Data_source <- "LSAY_2009"
Years_2009_cohort <- c(2009:2017)
for (i in Years_2009_cohort) { 
  LSAY_2009_data[,paste0("undertaking_",i)] <- ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 1,1,
                                                      ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 2,0,
                                                             ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 3,0,
                                                                    ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 4,0,
                                                                           ifelse(get(Data_source)[,paste0("XBAC",i)] %in% 5,0, NA)))))
  
  
}


################################## Years before drop-out ##############################

myfun <- function(series,value){
  tmp <- rle(series); runs <- tmp$lengths[tmp$values == value]
  if (length(runs)==0) return(0)
  else return(max(runs))
}


# 2003
years <- as.character(c(2003:2011))
variables <- c()

for (year in years) { 
  variables[year] <- paste0("undertaking_",year) }

Dropout_2003 <- LSAY_2003_data %>% select(one_of(variables),ID) %>%
  gather(key = "Variable",value = "Year",-ID) %>%  
  group_by(ID) %>% 
  summarise(Years_prior_to_dropout=myfun(tail(Year,9),1))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Years_prior_to_dropout = ifelse(undertaking_2011 %in% 1, NA, Dropout_2003$Years_prior_to_dropout))


# 2006
years <- as.character(c(2006:2014))
variables <- c()

for (year in years) { 
  variables[year] <- paste0("undertaking_",year) }

Dropout_2006 <- LSAY_2006_data %>% select(one_of(variables),ID) %>%
  gather(key = "Variable",value = "Year",-ID) %>%  
  group_by(ID) %>% 
  summarise(Years_prior_to_dropout=myfun(tail(Year,9),1))

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Years_prior_to_dropout = ifelse(undertaking_2014 %in% 1, NA, Dropout_2006$Years_prior_to_dropout))


# 2009
years <- as.character(c(2009:2017))
variables <- c()
  
for (year in years) { 
  variables[year] <- paste0("undertaking_",year) }
  
Dropout_2009 <- LSAY_2009_data %>% select(one_of(variables),ID) %>%
  gather(key = "Variable",value = "Year",-ID) %>%  
  group_by(ID) %>% 
  summarise(Years_prior_to_dropout=myfun(tail(Year,9),1))

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Years_prior_to_dropout = ifelse(undertaking_2017 %in% 1, NA, Dropout_2009$Years_prior_to_dropout))


################################## Job satisfaction ##############################


# 2003

LSAY_2003_data$Job_does_not_utilise_skills_2011 <- cut(LSAY_2003$LID028B, 
                                          breaks=c(0, 2, 4,6), 
                                          labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2003_data$Not_satisfied_with_work_2011 <- cut(LSAY_2003$LID028A, 
                                                  breaks=c(0, 2, 4,6), 
                                                  labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2003_data$Not_satisfied_with_career_path_2011 <- recode(LSAY_2003$LID026, `1` = 0, `0` = 1, `3` = 0)

LSAY_2003_data$Job_does_not_utilise_skills_2013 <- cut(LSAY_2003$LKD028B, 
                                          breaks=c(0, 2, 4,6), 
                                          labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2003_data$Not_satisfied_with_work_2013 <- cut(LSAY_2003$LKD028A, 
                                                  breaks=c(0, 2, 4,6), 
                                                  labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2003_data$Not_satisfied_with_career_path_2013 <- recode(LSAY_2003$LKD026, `1` = 0, `0` = 1, `3` = 0)


# 2006

LSAY_2006_data$Job_does_not_utilise_skills_2014 <- cut(LSAY_2006$LID028B, 
                                          breaks=c(0, 2, 4,6), 
                                          labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2006_data$Not_satisfied_with_work_2014 <- cut(LSAY_2006$LID028A, 
                                                  breaks=c(0, 2, 4,6), 
                                                  labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2006_data$Not_satisfied_with_career_path_2014 <- recode(LSAY_2006$LID026, `1` = 0, `0` = 1, `3` = 0)

LSAY_2006_data$Job_does_not_utilise_skills_2016 <- cut(LSAY_2006$LKD028B, 
                                                       breaks=c(0, 2, 4,6), 
                                                       labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2006_data$Not_satisfied_with_work_2016 <- cut(LSAY_2006$LKD028A, 
                                                   breaks=c(0, 2, 4,6), 
                                                   labels=c(0,1,0)) %>% as.character() %>% as.numeric()

LSAY_2006_data$Not_satisfied_with_career_path_2016 <- recode(LSAY_2006$LKD026, `1` = 0, `0` = 1, `3` = 0)


# 2009
LSAY_2009_data$Job_does_not_utilise_skills_2017 <- cut(LSAY_2009$LID028B, 
                                                       breaks=c(-Inf, 5, 10), 
                                                       labels=c(1,0)) %>% as.character() %>% as.numeric()

LSAY_2009_data$Not_satisfied_with_work_2017 <- cut(LSAY_2009$LID028A, 
                                                   breaks=c(-Inf, 5, 10), 
                                                   labels=c(1,0)) %>% as.character() %>% as.numeric()

LSAY_2009_data$Not_satisfied_with_career_path_2017 <- recode(LSAY_2009$LID026, `1` = 0, `0` = 1, `3` = 0)


rm(Year_age_19,Year_age_22,Year_age_22_1995,Year_age_22_1998,Year_age_22_2003,Year_age_22_2006,Year_age_22_2009,Year_age_23,
   Years_1995,Years_1995_cohort,Years_1998,Years_1998_cohort,Years_2003,Years_2003_cohort,Years_2006,Years_2006_cohort,Years_2009,Years_2009_cohort)


##########----------------------------------------------------------------------------------------------------##########
#         Census SEIFA data - age 15
##########----------------------------------------------------------------------------------------------------##########

LSAY_1995_data$Postcode <- Restricted_1995$postcode
LSAY_1995_data$educ_occupation <- with(SEIFA_1995,educ_occupation[match(LSAY_1995_data$Postcode,postcode)]) 

LSAY_1998_data$Postcode <- Restricted_1998$postcode
LSAY_1998_data$educ_occupation <- with(SEIFA_1998,educ_occupation[match(LSAY_1998_data$Postcode,postcode)]) 

LSAY_2003_data$Postcode <- Restricted_2003$PC2003
LSAY_2003_data$educ_occupation <- with(SEIFA_2003,educ_occupation[match(LSAY_2003_data$Postcode,postcode)]) 

LSAY_2006_data$Postcode <- ifelse(Restricted_2006$PC2007 == 0 & is.na(Restricted_2006$PC2008) == TRUE,Restricted_2006$POSTCODE,
                                  ifelse(Restricted_2006$PC2007 == 0,Restricted_2006$PC2008,Restricted_2006$PC2007)) # if they don't know their postcode, then use the postcode provided next year and failing that use the school postcode
LSAY_2006_data$Postcode <- ifelse(LSAY_2006_data$Postcode == 0,NA,LSAY_2006_data$Postcode)

LSAY_2006_data$educ_occupation <- with(SEIFA_2006,educ_occupation[match(LSAY_2006_data$Postcode,postcode)]) 

LSAY_2009_data$Postcode <- ifelse(Restricted_2009$PC2010 == 0 & is.na(Restricted_2009$PC2011) == TRUE,Restricted_2009$SCHPCODE,
                                  ifelse(Restricted_2009$PC2010 == 0,Restricted_2009$PC2011,Restricted_2009$PC2010)) # if they don't know their postcode, then use the postcode provided next year and failing that use the school postcode
LSAY_2009_data$Postcode <- ifelse(LSAY_2009_data$Postcode == 0,NA,LSAY_2009_data$Postcode)
LSAY_2009_data$educ_occupation <- with(SEIFA_2009,educ_occupation[match(LSAY_2009_data$Postcode,postcode)]) 

rm(SEIFA_1995,SEIFA_1998,SEIFA_2003,SEIFA_2006,SEIFA_2009)

#1995 SEIFA quartiles

Cohort <- 1995
Data_table  <- paste0("LSAY_",Cohort,"_data") 

LSAY_1995_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)$educ_occupation)),]), 
                              weights = LSAY_1995[which(!is.na(get(Data_table)$educ_occupation)),"WT1995"])

Quartile_4 <- svyquantile(~educ_occupation, LSAY_1995_data.w, 0.75, ci=FALSE)[1,1] # 
Quartile_3 <- svyquantile(~educ_occupation, LSAY_1995_data.w, 0.5, ci=FALSE)[1,1] #
Quartile_2 <- svyquantile(~educ_occupation, LSAY_1995_data.w, 0.25, ci=FALSE)[1,1] #

LSAY_1995_data$Educ_occupation_quartile <- as.factor(sapply(get(Data_table)$educ_occupation,  
                                                            function(x) {ifelse(x >= Quartile_4, "EQ4",
                                                                                ifelse(x >= Quartile_3, "EQ3",
                                                                                       ifelse(x >= Quartile_2, "EQ2",
                                                                                              ifelse(x < Quartile_2, "EQ1",NA))))}))

#1998 SEIFA quartiles
Cohort <- 1998
Data_table  <- paste0("LSAY_",Cohort,"_data") 

LSAY_1998_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)$educ_occupation)),]), 
                              weights = LSAY_1998[which(!is.na(get(Data_table)$educ_occupation)),"WT1998"])


Quartile_4 <- svyquantile(~educ_occupation, LSAY_1998_data.w, 0.75, ci=FALSE)[1,1] # 
Quartile_3 <- svyquantile(~educ_occupation, LSAY_1998_data.w, 0.5, ci=FALSE)[1,1] #
Quartile_2 <- svyquantile(~educ_occupation, LSAY_1998_data.w, 0.25, ci=FALSE)[1,1] #

LSAY_1998_data$Educ_occupation_quartile <- as.factor(sapply(get(Data_table)$educ_occupation,  
                                                            function(x) {ifelse(x >= Quartile_4, "EQ4",
                                                                                ifelse(x >= Quartile_3, "EQ3",
                                                                                       ifelse(x >= Quartile_2, "EQ2",
                                                                                              ifelse(x < Quartile_2, "EQ1",NA))))}))

#2003 SEIFA quartiles
Cohort <- 2003
Data_table  <- paste0("LSAY_",Cohort,"_data") 

LSAY_2003_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)$educ_occupation)),]), 
                              weights = LSAY_2003[which(!is.na(get(Data_table)$educ_occupation)),"WT2003"])

Quartile_4 <- svyquantile(~educ_occupation, LSAY_2003_data.w, 0.75, ci=FALSE)[1,1] # 
Quartile_3 <- svyquantile(~educ_occupation, LSAY_2003_data.w, 0.5, ci=FALSE)[1,1] #
Quartile_2 <- svyquantile(~educ_occupation, LSAY_2003_data.w, 0.25, ci=FALSE)[1,1] #

LSAY_2003_data$Educ_occupation_quartile <- as.factor(sapply(get(Data_table)$educ_occupation,  
                                                            function(x) {ifelse(x >= Quartile_4, "EQ4",
                                                                                ifelse(x >= Quartile_3, "EQ3",
                                                                                       ifelse(x >= Quartile_2, "EQ2",
                                                                                              ifelse(x < Quartile_2, "EQ1",NA))))}))

#2006 SEIFA quartiles
Cohort <- 2006
Data_table  <- paste0("LSAY_",Cohort,"_data") 

LSAY_2006_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)$educ_occupation)),]), 
                              weights = LSAY_2006[which(!is.na(get(Data_table)$educ_occupation)),"WT2006"])

Quartile_4 <- svyquantile(~educ_occupation, LSAY_2006_data.w, 0.75, ci=FALSE)[1,1] # 
Quartile_3 <- svyquantile(~educ_occupation, LSAY_2006_data.w, 0.5, ci=FALSE)[1,1] #
Quartile_2 <- svyquantile(~educ_occupation, LSAY_2006_data.w, 0.25, ci=FALSE)[1,1] #

LSAY_2006_data$Educ_occupation_quartile <- as.factor(sapply(get(Data_table)$educ_occupation,  
                                                            function(x) {ifelse(x >= Quartile_4, "EQ4",
                                                                                ifelse(x >= Quartile_3, "EQ3",
                                                                                       ifelse(x >= Quartile_2, "EQ2",
                                                                                              ifelse(x < Quartile_2, "EQ1",NA))))}))


#2009 SEIFA quartiles
Cohort <- 2009
Data_table  <- paste0("LSAY_",Cohort,"_data") 

LSAY_2009_data.w <- svydesign(ids = ~0, data = as.data.frame(get(Data_table)[which(!is.na(get(Data_table)$educ_occupation)),]), 
                              weights = LSAY_2009[which(!is.na(get(Data_table)$educ_occupation)),"WT2009"])

Quartile_4 <- svyquantile(~educ_occupation, LSAY_2009_data.w, 0.75, ci=FALSE)[1,1] # 
Quartile_3 <- svyquantile(~educ_occupation, LSAY_2009_data.w, 0.5, ci=FALSE)[1,1] #
Quartile_2 <- svyquantile(~educ_occupation, LSAY_2009_data.w, 0.25, ci=FALSE)[1,1] #

LSAY_2009_data$Educ_occupation_quartile <- as.factor(sapply(get(Data_table)$educ_occupation,  
                                                            function(x) {ifelse(x >= Quartile_4, "EQ4",
                                                                                ifelse(x >= Quartile_3, "EQ3",
                                                                                       ifelse(x >= Quartile_2, "EQ2",
                                                                                              ifelse(x < Quartile_2, "EQ1",NA))))}))


rm(Quartile_2,Quartile_3,Quartile_4)


Educ_occupation_order <- c("EQ4","EQ3", "EQ2","EQ1") 
LSAY_1995_data$Educ_occupation_quartile <- factor(LSAY_1995_data %>% pull(Educ_occupation_quartile), levels = Educ_occupation_order) 
LSAY_1998_data$Educ_occupation_quartile <- factor(LSAY_1998_data %>% pull(Educ_occupation_quartile), levels = Educ_occupation_order) 
LSAY_2003_data$Educ_occupation_quartile <- factor(LSAY_2003_data %>% pull(Educ_occupation_quartile), levels = Educ_occupation_order) 
LSAY_2006_data$Educ_occupation_quartile <- factor(LSAY_2006_data %>% pull(Educ_occupation_quartile), levels = Educ_occupation_order) 
LSAY_2009_data$Educ_occupation_quartile <- factor(LSAY_2009_data %>% pull(Educ_occupation_quartile), levels = Educ_occupation_order) 


Cohorts <- c(1995,1998,2003,2006,2009)

for (Cohort in Cohorts) { 
  assign(paste0("LSAY_",Cohort,"_data"), 
         get(paste0("LSAY_",Cohort,"_data")) %>% 
           mutate(top_8_university_age_22 = as.factor(ifelse(as.character(Uni_attend_age_22) == 0,0,
                                                             recode(Uni_attend_age_22_Go8, Group_of_Eight=1, Other_university_only=0))) ))
  assign(paste0("LSAY_",Cohort,"_data"), 
         get(paste0("LSAY_",Cohort,"_data")) %>% 
           mutate(top_8_university_age_19 = as.factor(ifelse(as.character(Uni_attend_age_19) == 0,0,
                                                             recode(Uni_attend_age_19_Go8, Group_of_Eight=1, Other_university_only=0))) ))
}

rm(LSAY_1995_data.w,LSAY_1998_data.w,LSAY_2003_data.w,LSAY_2006_data.w,LSAY_2009_data.w)

# Attendance at a non-Go8 university
LSAY_2003_data$Other_university_age_22 <- as.factor(ifelse(LSAY_2003_data$top_8_university_age_22 %in% 1,0,
                                                           as.numeric(sapply(LSAY_2003_data$Uni_attend_age_22, as.character))))
LSAY_2006_data$Other_university_age_22 <- as.factor(ifelse(LSAY_2006_data$top_8_university_age_22 %in% 1,0,
                                                           as.numeric(sapply(LSAY_2006_data$Uni_attend_age_22, as.character))))
LSAY_2009_data$Other_university_age_22 <- as.factor(ifelse(LSAY_2009_data$top_8_university_age_22 %in% 1,0,
                                                           as.numeric(sapply(LSAY_2009_data$Uni_attend_age_22, as.character))))


LSAY_2003_data$Type_of_uni_age_22 <- ifelse(LSAY_2003_data$top_8_university_age_22 %in% 1,"Go8",
                                            ifelse(LSAY_2003_data$Uni_attend_age_22 %in% 1, "Other university",
                                                   ifelse(LSAY_2003_data$Uni_attend_age_22 %in% 0,"No university",NA)))

LSAY_2006_data$Type_of_uni_age_22 <- ifelse(LSAY_2006_data$top_8_university_age_22 %in% 1,"Go8",
                                            ifelse(LSAY_2006_data$Uni_attend_age_22 %in% 1, "Other university",
                                                   ifelse(LSAY_2006_data$Uni_attend_age_22 %in% 0,"No university",NA)))

LSAY_2009_data$Type_of_uni_age_22 <- ifelse(LSAY_2009_data$top_8_university_age_22 %in% 1,"Go8",
                                            ifelse(LSAY_2009_data$Uni_attend_age_22 %in% 1, "Other university",
                                                   ifelse(LSAY_2009_data$Uni_attend_age_22 %in% 0,"No university",NA)))

##########----------------------------------------------------------------------------------------------------##########
#         Equity group membership
##########----------------------------------------------------------------------------------------------------##########

#1995 weighted
Cohort <- 1995
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 

LSAY_1995_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))

#1998 weighted
Cohort <- 1998
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 

LSAY_1998_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))

# 2003 weighted
Cohort <- 2003
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 


LSAY_2003_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))

#2006 weighted
Cohort <- 2006
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 

LSAY_2006_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))


#2009 weighted
Cohort <- 2009
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 

LSAY_2009_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))

Cohort <- 2015
Equity <- paste0("Equity_",Cohort)
Data_table  <- paste("LSAY_",Cohort,"_data",sep = "") 

LSAY_2015_data$Equity_group <- as.factor(ifelse(get(Data_table)$Indigenous == "Not indigenous" & 
                                                  (get(Data_table)$Parent_SES_quartile == "PQ4" | 
                                                     get(Data_table)$Parent_SES_quartile == "PQ3" |
                                                     get(Data_table)$Parent_SES_quartile == "PQ2") &
                                                  (get(Data_table)$Parent_edu == "Uni") & 
                                                  #(get(Data_table)$Closest_uni_distance_g2 == "Under 40km" |
                                                  (get(Data_table)$Location_RR_wave1 == "Metropolitan"),"Not_disadvantaged","Disadvantaged"))


Equity_group_order <- c("Not_disadvantaged","Disadvantaged") 
LSAY_1995_data$Equity_group <- factor(LSAY_1995_data$Equity_group, levels = Equity_group_order)   # Change the order for regression
LSAY_1998_data$Equity_group <- factor(LSAY_1998_data$Equity_group, levels = Equity_group_order)   # Change the order for regression
LSAY_2003_data$Equity_group <- factor(LSAY_2003_data$Equity_group, levels = Equity_group_order)   # Change the order for regression
LSAY_2006_data$Equity_group <- factor(LSAY_2006_data$Equity_group, levels = Equity_group_order) 
LSAY_2009_data$Equity_group <- factor(LSAY_2009_data$Equity_group, levels = Equity_group_order) 
LSAY_2015_data$Equity_group <- factor(LSAY_2015_data$Equity_group, levels = Equity_group_order) 

rm(Equity_group_order)

##########----------------------------------------------------------------------------------------------------##########
#         Additional variables for decomp
##########----------------------------------------------------------------------------------------------------##########

LSAY_1995_data$Low_SES <- ifelse(LSAY_1995_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_1995_data$Low_PISA <- ifelse(LSAY_1995_data$Achievement_quartile %in% "lower","Low_PISA","Not_Low_PISA")
LSAY_1995_data$Low_sch_SES <- ifelse(LSAY_1995_data$School_SES_quartile %in% "SQ1","Low_Sch_SES","Not_Low_Sch_SES")
LSAY_1995_data$Government <- ifelse(LSAY_1995_data$Sector %in% "Government","Government","Not_Government")
LSAY_1995_data$Metropolitan <- ifelse(LSAY_1995_data$Location %in% c("Regional","Remote"),"Regional_or_remote","Metropolitan")
LSAY_1995_data$Closest_uni_distance_over_40km <- ifelse(LSAY_1995_data$Closest_uni_distance_g2 == "Under 40km",
                                                        "Under 40km","Over_40km")
LSAY_1995_data$Number_of_books <- ifelse(LSAY_1995_data$Books %in% c("101-500","<500"), ">100", 
                                         unlist(lapply(LSAY_1995_data$Books, as.character)))

LSAY_1995_data$Low_SES <- factor(LSAY_1995_data$Low_SES, levels = c("Low_SES","Not_Low_SES"))   # re-order factor levels
LSAY_1995_data$Low_PISA <- factor(LSAY_1995_data$Low_PISA, levels = c("Low_PISA","Not_Low_PISA"))   # re-order factor levels
LSAY_1995_data$Low_sch_SES <- factor(LSAY_1995_data$Low_sch_SES, levels = c("Low_Sch_SES","Not_Low_Sch_SES"))   # re-order factor levels
LSAY_1995_data$Government <- factor(LSAY_1995_data$Government, levels = c("Government","Not_Government"))   # re-order factor levels
LSAY_1995_data$Metropolitan <- factor(LSAY_1995_data$Metropolitan, levels = c("Regional_or_remote","Metropolitan"))   # re-order factor levels
LSAY_1995_data$Closest_uni_distance_over_40km <- factor(LSAY_1995_data$Closest_uni_distance_over_40km, 
                                                        levels = c("Over_40km","Under 40km"))   # re-order factor levels
LSAY_1995_data$Number_of_books <- factor(LSAY_1995_data$Number_of_books, 
                                         levels = c(">500","0-100"))   # re-order factor levels
LSAY_1995_data <- LSAY_1995_data %>% mutate(Low_v_high_SES = ifelse(Parent_SES_quartile %in% "PQ1","Low_SES",
                                                                    ifelse(Parent_SES_quartile %in% "PQ4","High_SES",NA))) %>% 
  mutate(Low_v_high_SES = factor(Low_v_high_SES, levels = c("Low_SES","High_SES"))) 

LSAY_1998_data$Low_SES <- ifelse(LSAY_1998_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_1998_data$Low_PISA <- ifelse(LSAY_1998_data$Achievement_quartile %in% "lower","Low_PISA","Not_Low_PISA")
LSAY_1998_data$Low_sch_SES <- ifelse(LSAY_1998_data$School_SES_quartile %in% "SQ1","Low_Sch_SES","Not_Low_Sch_SES")
LSAY_1998_data$Government <- ifelse(LSAY_1998_data$Sector %in% "Government","Government","Not_Government")
LSAY_1998_data$Metropolitan <- ifelse(LSAY_1998_data$Location %in% c("Regional","Remote"),"Regional_or_remote","Metropolitan")
LSAY_1998_data$Closest_uni_distance_over_40km <- ifelse(LSAY_1998_data$Closest_uni_distance_g2 == "Under 40km",
                                                        "Under 40km","Over_40km")
LSAY_1998_data$Number_of_books <- ifelse(LSAY_1998_data$Books %in% c("101-500","<500"), ">100", 
                                         unlist(lapply(LSAY_1998_data$Books, as.character)))

LSAY_1998_data$Low_SES <- factor(LSAY_1998_data$Low_SES, levels = c("Low_SES","Not_Low_SES"))   # re-order factor levels
LSAY_1998_data$Low_PISA <- factor(LSAY_1998_data$Low_PISA, levels = c("Low_PISA","Not_Low_PISA"))   # re-order factor levels
LSAY_1998_data$Low_sch_SES <- factor(LSAY_1998_data$Low_sch_SES, levels = c("Low_Sch_SES","Not_Low_Sch_SES"))   # re-order factor levels
LSAY_1998_data$Government <- factor(LSAY_1998_data$Government, levels = c("Government","Not_Government"))   # re-order factor levels
LSAY_1998_data$Metropolitan <- factor(LSAY_1998_data$Metropolitan, levels = c("Regional_or_remote","Metropolitan"))   # re-order factor levels
LSAY_1998_data$Closest_uni_distance_over_40km <- factor(LSAY_1998_data$Closest_uni_distance_over_40km, 
                                                        levels = c("Over_40km","Under 40km"))   # re-order factor levels
LSAY_1998_data$Number_of_books <- factor(LSAY_1998_data$Number_of_books, 
                                         levels = c(">500","0-100"))   # re-order factor levels
LSAY_1998_data <- LSAY_1998_data %>% mutate(Low_v_high_SES = ifelse(Parent_SES_quartile %in% "PQ1","Low_SES",
                                                                    ifelse(Parent_SES_quartile %in% "PQ4","High_SES",NA))) %>% 
  mutate(Low_v_high_SES = factor(Low_v_high_SES, levels = c("Low_SES","High_SES"))) 


LSAY_2003_data$Low_SES <- ifelse(LSAY_2003_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_2003_data$Low_PISA <- ifelse(LSAY_2003_data$PISA_quartile %in% "lower","Low_PISA","Not_Low_PISA")
LSAY_2003_data$Low_sch_SES <- ifelse(LSAY_2003_data$School_SES_quartile %in% "SQ1","Low_Sch_SES","Not_Low_Sch_SES")
LSAY_2003_data$Government <- ifelse(LSAY_2003_data$Sector %in% "Government","Government","Not_Government")
LSAY_2003_data$Metropolitan <- ifelse(LSAY_2003_data$Location %in% c("Regional","Remote"),"Regional_or_remote","Metropolitan")
LSAY_2003_data$Closest_uni_distance_over_40km <- ifelse(LSAY_2003_data$Closest_uni_distance_g2 == "Under 40km",
                                                        "Under 40km","Over_40km")
LSAY_2003_data$Number_of_books <- ifelse(LSAY_2003_data$Books %in% c("101-500","<500"), ">100", 
                                         unlist(lapply(LSAY_2003_data$Books, as.character)))

LSAY_2003_data$Low_SES <- factor(LSAY_2003_data$Low_SES, levels = c("Low_SES","Not_Low_SES"))   # re-order factor levels
LSAY_2003_data$Low_PISA <- factor(LSAY_2003_data$Low_PISA, levels = c("Low_PISA","Not_Low_PISA"))   # re-order factor levels
LSAY_2003_data$Low_sch_SES <- factor(LSAY_2003_data$Low_sch_SES, levels = c("Low_Sch_SES","Not_Low_Sch_SES"))   # re-order factor levels
LSAY_2003_data$Government <- factor(LSAY_2003_data$Government, levels = c("Government","Not_Government"))   # re-order factor levels
LSAY_2003_data$Metropolitan <- factor(LSAY_2003_data$Metropolitan, levels = c("Regional_or_remote","Metropolitan"))   # re-order factor levels
LSAY_2003_data$Closest_uni_distance_over_40km <- factor(LSAY_2003_data$Closest_uni_distance_over_40km, 
                                                        levels = c("Over_40km","Under 40km"))   # re-order factor levels
LSAY_2003_data$Number_of_books <- factor(LSAY_2003_data$Number_of_books, 
                                         levels = c(">500","0-100"))   # re-order factor levels
LSAY_2003_data <- LSAY_2003_data %>% mutate(Low_v_high_SES = ifelse(Parent_SES_quartile %in% "PQ1","Low_SES",
                                                                    ifelse(Parent_SES_quartile %in% "PQ4","High_SES",NA))) %>% 
  mutate(Low_v_high_SES = factor(Low_v_high_SES, levels = c("Low_SES","High_SES")))  


LSAY_2006_data$Low_SES <- ifelse(LSAY_2006_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_2006_data$Low_PISA <- ifelse(LSAY_2006_data$PISA_quartile %in% "lower","Low_PISA","Not_Low_PISA")
LSAY_2006_data$Low_sch_SES <- ifelse(LSAY_2006_data$School_SES_quartile %in% "SQ1","Low_Sch_SES","Not_Low_Sch_SES")
LSAY_2006_data$Government <- ifelse(LSAY_2006_data$Sector %in% "Government","Government","Not_Government")
LSAY_2006_data$Metropolitan <- ifelse(LSAY_2006_data$Location %in% c("Regional","Remote"),"Regional_or_remote","Metropolitan")
LSAY_2006_data$Closest_uni_distance_over_40km <- ifelse(LSAY_2006_data$Closest_uni_distance_g2 == "Under 40km","Under 40km","Over_40km")
LSAY_2006_data$Number_of_books <- ifelse(LSAY_2006_data$Books %in% c("101-500","<500"), ">100", 
                                         unlist(lapply(LSAY_2006_data$Books, as.character)))

LSAY_2006_data$Low_SES <- factor(LSAY_2006_data$Low_SES, levels = c("Low_SES","Not_Low_SES"))   # re-order factor levels
LSAY_2006_data$Low_PISA <- factor(LSAY_2006_data$Low_PISA, levels = c("Low_PISA","Not_Low_PISA"))   # re-order factor levels
LSAY_2006_data$Low_sch_SES <- factor(LSAY_2006_data$Low_sch_SES, levels = c("Low_Sch_SES","Not_Low_Sch_SES"))   # re-order factor levels
LSAY_2006_data$Government <- factor(LSAY_2006_data$Government, levels = c("Government","Not_Government"))   # re-order factor levels
LSAY_2006_data$Metropolitan <- factor(LSAY_2006_data$Metropolitan, levels = c("Regional_or_remote","Metropolitan"))   # re-order factor levels
LSAY_2006_data$Closest_uni_distance_over_40km <- factor(LSAY_2006_data$Closest_uni_distance_over_40km, 
                                                        levels = c("Over_40km","Under 40km"))   # re-order factor levels
LSAY_2006_data$Number_of_books <- factor(LSAY_2006_data$Number_of_books, 
                                         levels = c(">500","0-100"))   # re-order factor levels
LSAY_2006_data <- LSAY_2006_data %>% mutate(Low_v_high_SES = ifelse(Parent_SES_quartile %in% "PQ1","Low_SES",
                                                                    ifelse(Parent_SES_quartile %in% "PQ4","High_SES",NA))) %>% 
  mutate(Low_v_high_SES = factor(Low_v_high_SES, levels = c("Low_SES","High_SES")))  

LSAY_2009_data$Low_SES <- ifelse(LSAY_2009_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_2009_data$Low_PISA <- ifelse(LSAY_2009_data$PISA_quartile %in% "lower","Low_PISA","Not_Low_PISA")
LSAY_2009_data$Low_sch_SES <- ifelse(LSAY_2009_data$School_SES_quartile %in% "SQ1","Low_Sch_SES","Not_Low_Sch_SES")
LSAY_2009_data$Government <- ifelse(LSAY_2009_data$Sector %in% "Government","Government","Not_Government")
LSAY_2009_data$Metropolitan <- ifelse(LSAY_2009_data$Location %in% c("Regional","Remote"),"Regional_or_remote","Metropolitan")
LSAY_2009_data$Closest_uni_distance_over_40km <- ifelse(LSAY_2009_data$Closest_uni_distance_g2 == "Under 40km","Under 40km","Over_40km")
LSAY_2009_data$Number_of_books <- ifelse(LSAY_2009_data$Books %in% c("101-500","<500"), ">100", 
                                         unlist(lapply(LSAY_2009_data$Books, as.character)))

LSAY_2009_data$Low_SES <- factor(LSAY_2009_data$Low_SES, levels = c("Low_SES","Not_Low_SES"))   # re-order factor levels
LSAY_2009_data$Low_PISA <- factor(LSAY_2009_data$Low_PISA, levels = c("Low_PISA","Not_Low_PISA"))   # re-order factor levels
LSAY_2009_data$Low_sch_SES <- factor(LSAY_2009_data$Low_sch_SES, levels = c("Low_Sch_SES","Not_Low_Sch_SES"))   # re-order factor levels
LSAY_2009_data$Government <- factor(LSAY_2009_data$Government, levels = c("Government","Not_Government"))   # re-order factor levels
LSAY_2009_data$Metropolitan <- factor(LSAY_2009_data$Metropolitan, levels = c("Regional_or_remote","Metropolitan"))   # re-order factor levels
LSAY_2009_data$Closest_uni_distance_over_40km <- factor(LSAY_2009_data$Closest_uni_distance_over_40km, 
                                                        levels = c("Over_40km","Under 40km"))   # re-order factor levels
LSAY_2009_data$Number_of_books <- factor(LSAY_2009_data$Number_of_books, 
                                         levels = c(">500","0-100"))   # re-order factor levels

LSAY_2003_data$Low_SES <- factor(LSAY_2003_data$Low_SES , levels = c("Not_Low_SES","Low_SES"))
LSAY_2006_data$Low_SES <- factor(LSAY_2006_data$Low_SES , levels = c("Not_Low_SES","Low_SES"))
LSAY_2009_data$Low_SES <- factor(LSAY_2009_data$Low_SES , levels = c("Not_Low_SES","Low_SES"))

LSAY_2009_data <- LSAY_2009_data %>% mutate(Low_v_high_SES = ifelse(Parent_SES_quartile %in% "PQ1","Low_SES",
                                                                    ifelse(Parent_SES_quartile %in% "PQ4","High_SES",NA))) %>% 
  mutate(Low_v_high_SES = factor(Low_v_high_SES, levels = c("Low_SES","High_SES")))   

LSAY_2009_data <- LSAY_2009_data %>% mutate(Low_v_high_PISA = ifelse(PISA_quartile %in% "lower","Low_PISA",
                                                                     ifelse(PISA_quartile %in% "upper","High_PISA",NA))) %>% 
  mutate(Low_v_high_PISA = factor(Low_v_high_PISA, levels = c("Low_PISA","High_PISA")))   

LSAY_2009_data <- LSAY_2009_data %>% mutate(Low_v_high_PC_SES = ifelse(Educ_occupation_quartile %in% "EQ1","Low_postcode_SES",
                                                                       ifelse(Educ_occupation_quartile %in% "EQ4","High_postcode_SES",NA))) %>% 
  mutate(Low_v_high_PC_SES = factor(Low_v_high_PC_SES, levels = c("Low_postcode_SES","High_postcode_SES")))   

LSAY_2015_data$Low_SES <- ifelse(LSAY_2015_data$Parent_SES_quartile %in% "PQ1","Low_SES","Not_Low_SES")
LSAY_2015_data$Low_SES <- factor(LSAY_2015_data$Low_SES, levels = c("Low_SES","Not_Low_SES")) 




LSAY_1995_data <- LSAY_1995_data %>% 
  mutate(Ability = recode(Achievement_quartile, `mid-lower` = "lower", `mid-upper` = "Middle"))

LSAY_1998_data <- LSAY_1998_data %>% 
  mutate(Ability = recode(Achievement_quartile, `mid-lower` = "lower", `mid-upper` = "Middle"))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Ability = recode(PISA_quartile, `mid-lower` = "lower", `mid-upper` = "Middle"))

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Ability = recode(PISA_quartile, `mid-lower` = "lower", `mid-upper` = "Middle"))

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Ability = recode(PISA_quartile, `mid-lower` = "lower", `mid-upper` = "Middle"))




##########----------------------------------------------------------------------------------------------------##########
#         ATAR variables
##########----------------------------------------------------------------------------------------------------##########

OP_to_ATAR_table <- tibble(OP_score = c(1:25),
                       ATAR_score = c(99.5,98,96.5,94.5,92.5,90,88,85,82.5,80,77.5,75,72.5,70,65,62.5,60,55,52.5,50,45,40,35,30,25))


LSAY_1995_data$Enter_Score_received_1999  <- ifelse(LSAY_1995$EA014 <7, "Yes",
                                                    ifelse(LSAY_1995$EA014 == 7, "No",
                                                           ifelse(LSAY_1995$EA014 == 8, "Dont Know", NA)))

LSAY_1995_data$Enter_Score_received_2000  <- ifelse(LSAY_1995$FA012 ==1, "Yes",
                                                    ifelse(LSAY_1995$FA012 == 0, "No",
                                                           ifelse(LSAY_1995$FA012 == 3, "Dont Know", NA)))

LSAY_1995_data$Enter_Score_received_2001  <- ifelse(LSAY_1995$GA015 ==1, "Yes",
                                                    ifelse(LSAY_1995$GA015 == 2, "No",NA))

LSAY_1995_data$Entrance_score_1999  <- as.numeric(LSAY_1995$EA015) /100

LSAY_1995_data$Entrance_score_2000  <- LSAY_1995$FA013B
LSAY_1995_data <- LSAY_1995_data %>% mutate(Entrance_score_2000 = replace (Entrance_score_2000,Entrance_score_2000 >900,NA))

LSAY_1995_data$ATAR <- ifelse(!is.na(LSAY_1995_data$Entrance_score_1999),LSAY_1995_data$Entrance_score_1999,
                              ifelse(!is.na(LSAY_1995_data$Entrance_score_2000),LSAY_1995_data$Entrance_score_2000, NA))

LSAY_1995_data$ATAR <- ifelse(LSAY_1995$EA014 == "4", vlookup(LSAY_1995_data$ATAR, OP_to_ATAR_table, "OP_score", "ATAR_score"), LSAY_1995_data$ATAR)

LSAY_1995_data$Enter_Score_received <- LSAY_1995_data %>% 
  select(.,Enter_Score_received_1999:Enter_Score_received_2001) %>% 
  apply(1, function(x) any(x %in% c("Yes")))

LSAY_1995_data$Enter_Score_not_received <- LSAY_1995_data %>% 
  select(.,Enter_Score_received_1999:Enter_Score_received_2001) %>% 
  apply(1, function(x) any(x %in% c("No")))

LSAY_1995_data$Enter_Score_received <- ifelse(LSAY_1995_data$Enter_Score_received == TRUE,"Yes",
                                              ifelse(LSAY_1995_data$Enter_Score_not_received == TRUE, "No",NA))

LSAY_1995_data$Enter_Score_not_received <- NULL

LSAY_1995_data$ATAR_group <- cut(LSAY_1995_data$ATAR, 
                                 breaks=c(-Inf, 50, 60, 70, 80, 90, Inf), 
                                 labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()

LSAY_1995_data$ATAR_group <- ifelse(!is.na(LSAY_1995_data$ATAR_group),LSAY_1995_data$ATAR_group,
                                    ifelse(LSAY_1995_data$Enter_Score_received %in% "Yes","Not_reported",
                                           ifelse(LSAY_1995_data$Enter_Score_received %in% "No","No_ATAR","Not_available")))

# 1998
LSAY_1998_data$Enter_Score_received_2002  <- ifelse(LSAY_1998$EA012 =="1", "Yes",
                                                    ifelse(LSAY_1998$EA012 =="3", "Dont Know",
                                                           ifelse(LSAY_1998$EA012 =="0", "No", NA)))

LSAY_1998_data$Enter_Score_received_2003  <- ifelse(LSAY_1998$FA012 =="1", "Yes",
                                                    ifelse(LSAY_1998$FA012 =="3", "Dont Know",
                                                           ifelse(LSAY_1998$FA012 =="0", "No", NA)))

LSAY_1998_data$Enter_Score_received_2004  <- ifelse(LSAY_1998$GA012 =="1", "Yes",
                                                    ifelse(LSAY_1998$GA012 =="3", "Dont Know",
                                                           ifelse(LSAY_1998$GA012 =="0", "No", NA)))

LSAY_1998_data$Entrance_score_2002  <- LSAY_1998$EA013B
LSAY_1998_data$Entrance_score_2003  <- LSAY_1998$FA013B

LSAY_1998_data$ATAR <- ifelse(!is.na(LSAY_1998_data$Entrance_score_2002),LSAY_1998_data$Entrance_score_2002,
                              ifelse(!is.na(LSAY_1998_data$Entrance_score_2003),LSAY_1998_data$Entrance_score_2003, NA))

LSAY_1998_data$ATAR <- ifelse(LSAY_1998$EA009A %in% "3" & (LSAY_1998_data$Entrance_score_2002 <= 25) | 
                                LSAY_1998$FA009A %in% "3" & (LSAY_1998_data$Entrance_score_2003 <= 25) , 
                              vlookup(LSAY_1998_data$ATAR, OP_to_ATAR_table, "OP_score", "ATAR_score"), LSAY_1998_data$ATAR)

LSAY_1998_data$Enter_Score_received <- LSAY_1998_data %>% 
  select(.,Enter_Score_received_2002:Enter_Score_received_2004) %>% 
  apply(1, function(x) any(x %in% c("Yes")))

LSAY_1998_data$Enter_Score_not_received <- LSAY_1998_data %>% 
  select(.,Enter_Score_received_2002:Enter_Score_received_2004) %>% 
  apply(1, function(x) any(x %in% c("No")))

LSAY_1998_data$Enter_Score_received <- ifelse(LSAY_1998_data$Enter_Score_received == TRUE,"Yes",
                                              ifelse(LSAY_1998_data$Enter_Score_not_received == TRUE, "No",NA))

LSAY_1998_data$Enter_Score_not_received <- NULL

LSAY_1998_data$ATAR_group <- cut(LSAY_1998_data$ATAR, 
                                 breaks=c(-Inf, 50, 60, 70, 80, 90, Inf), 
                                 labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()

LSAY_1998_data$ATAR_group <- ifelse(!is.na(LSAY_1998_data$ATAR_group),LSAY_1998_data$ATAR_group,
                                    ifelse(LSAY_1998_data$Enter_Score_received %in% "Yes","Not_reported",
                                           ifelse(LSAY_1998_data$Enter_Score_received %in% "No","No_ATAR","Not_available"))) 

# 2003


LSAY_2003_data$Enter_Score_received_2005  <- ifelse(LSAY_2003$LCB017 =="1", "Yes",
                                                    ifelse(LSAY_2003$LCB017 =="3", "Dont Know",
                                                           ifelse(LSAY_2003$LCB017 =="2", "No", NA)))

LSAY_2003_data$Enter_Score_received_2006  <- ifelse(LSAY_2003$LDB017 =="1", "Yes",
                                                    ifelse(LSAY_2003$LDB017 =="3", "Dont Know",
                                                           ifelse(LSAY_2003$LDB017 =="2", "No", NA)))

LSAY_2003_data$Enter_Score_received_2007  <- ifelse(LSAY_2003$LEB017 =="1", "Yes",
                                                    ifelse(LSAY_2003$LEB017 =="3", "Dont Know",
                                                           ifelse(LSAY_2003$LEB017 =="2", "No", NA)))

LSAY_2003_data$Enter_Score_received_2008  <- ifelse(LSAY_2003$LFB017 =="1", "Yes",
                                                    ifelse(LSAY_2003$LFB017 =="3", "Dont Know",
                                                           ifelse(LSAY_2003$LFB017 =="2", "No", NA)))


LSAY_2003_data$Entrance_score_2005  <- LSAY_2003$LCB018
LSAY_2003_data$Entrance_score_2006  <- LSAY_2003$LDB018
LSAY_2003_data$Entrance_score_2007  <- LSAY_2003$LEB018
LSAY_2003_data$Entrance_score_2008  <- LSAY_2003$LFB018S

LSAY_2003_data$QLD <- ifelse((  LSAY_2003$LCB014A %in% "3" & !is.na(LSAY_2003_data$Entrance_score_2005)) |
                                (LSAY_2003$LDA012 %in% "3" & !is.na(LSAY_2003_data$Entrance_score_2006)) |
                                (LSAY_2003$LEB014A %in% "3" & !is.na(LSAY_2003_data$Entrance_score_2007)),"QLD",NA)


LSAY_2003_data$ATAR <- ifelse(!is.na(LSAY_2003_data$Entrance_score_2005),LSAY_2003_data$Entrance_score_2005,
                              ifelse(!is.na(LSAY_2003_data$Entrance_score_2006),LSAY_2003_data$Entrance_score_2006,
                                     ifelse(!is.na(LSAY_2003_data$Entrance_score_2007),LSAY_2003_data$Entrance_score_2007,
                                            ifelse(!is.na(LSAY_2003_data$Entrance_score_2008),LSAY_2003_data$Entrance_score_2008,NA))))


# LSAY_2003_data$ATAR <- ifelse(LSAY_2003$LCB014A %in% "3" & (LSAY_2003_data$Entrance_score_2005 <= 25) | 
#                                 LSAY_2003$LDA012 %in% "3" & (LSAY_2003_data$Entrance_score_2006 <= 25) | 
#                                 LSAY_2003$LEB014A %in% "3" & (LSAY_2003_data$Entrance_score_2007 <= 25), 
#                               vlookup(LSAY_2003_data$ATAR, OP_to_ATAR_table, "OP_score", "ATAR_score"), 
#                               ifelse(LSAY_2003_data$ATAR >25,LSAY_2003_data$ATAR,NA))

LSAY_2003_data$ATAR <- ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 1,99.5,
                              ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 2,98,
                                     ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 3,96.5,
                                            ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 4,94.5,
                                                   ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 5,92.5,
                                                          ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 6,90,
                                                                 ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 7,88,
                                                                        ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 8,85,
                                                                               ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 9,82.5,
                                                                                      ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 10,80,
                                                                                             ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 11,77.5,
                                                                                                    ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 12,75,
                                                                                                           ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 13,72.5,
                                                                                                                  ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 14,70,
                                                                                                                         ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 15,65,
                                                                                                                                ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 16,62.5,
                                                                                                                                       ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 17,60,
                                                                                                                                              ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 18,55,
                                                                                                                                                     ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 19,52.5,
                                                                                                                                                            ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 20,50,
                                                                                                                                                                   ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 21,45,
                                                                                                                                                                          ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 22,40,
                                                                                                                                                                                 ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 23,35,
                                                                                                                                                                                        ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 24,30,
                                                                                                                                                                                               ifelse(LSAY_2003_data$QLD =="QLD" & LSAY_2003_data$ATAR == 25,25, LSAY_2003_data$ATAR )))))))))))))))))))))))))

















 

LSAY_2003_data$Enter_Score_received <- LSAY_2003_data %>% 
  select(.,Enter_Score_received_2005:Enter_Score_received_2008) %>% 
  apply(1, function(x) any(x %in% c("Yes")))

LSAY_2003_data$Enter_Score_not_received <- LSAY_2003_data %>% 
  select(.,Enter_Score_received_2005:Enter_Score_received_2008) %>% 
  apply(1, function(x) any(x %in% c("No")))

LSAY_2003_data$Enter_Score_received <- ifelse(LSAY_2003_data$Enter_Score_received == TRUE,"Yes",
                                              ifelse(LSAY_2003_data$Enter_Score_not_received == TRUE, "No",NA))

LSAY_2003_data$Enter_Score_not_received <- NULL

LSAY_2003_data$ATAR_group <- cut(LSAY_2003_data$ATAR, 
                                 breaks=c(0, 50, 60, 70, 80, 90, 100), 
                                 labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()

LSAY_2003_data$ATAR_group <- ifelse(is.na(LSAY_2003_data$ATAR_group) & is.na(LSAY_2003_data$Enter_Score_received) & !is.na(LSAY_2003_data$Uni_attend_age_19),"Not_available",
                                    ifelse(is.na(LSAY_2003_data$ATAR_group) & is.na(LSAY_2003_data$Enter_Score_received),NA,
                                           ifelse(!is.na(LSAY_2003_data$ATAR_group),LSAY_2003_data$ATAR_group,
                                                  ifelse(LSAY_2003_data$Enter_Score_received %in% "Yes","Not_reported",
                                                         ifelse(LSAY_2003_data$Enter_Score_received %in% "No","No_ATAR","Not_available")))))

LSAY_2003_data$ATAR_group <- ifelse(LSAY_2003_data$Completed_year_12 %in% "Not_completed" & LSAY_2003_data$ATAR_group %in% "Not_available","No_ATAR",LSAY_2003_data$ATAR_group) # If they didn't report whether they received an ATAR and they didn't attend year 12, then code them as "No_ATAR"

# 2006


LSAY_2006_data$Enter_Score_received_2008  <- ifelse(LSAY_2006$LCB018 =="1", "Yes",
                                                    ifelse(LSAY_2006$LCB018 =="3", "Dont Know",
                                                           ifelse(LSAY_2006$LCB018 =="2", "No", NA))) # 3 is "refused" - might be different than "no" as per 2003 cohort

LSAY_2006_data$Enter_Score_received_2009  <- ifelse(LSAY_2006$LDB018 =="1", "Yes",
                                                    ifelse(LSAY_2006$LDB018 =="3", "Dont Know",
                                                           ifelse(LSAY_2006$LDB018 =="2", "No", NA)))

LSAY_2006_data$Enter_Score_received_2010  <- ifelse(LSAY_2006$LEB020=="1", "Yes",
                                                    ifelse(LSAY_2006$LEB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2006$LEB020 =="2", "No", NA)))

LSAY_2006_data$Enter_Score_received_2011  <- ifelse(LSAY_2006$LFB020 =="1", "Yes",
                                                    ifelse(LSAY_2006$LFB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2006$LFB020 =="2", "No", NA)))

LSAY_2006_data$Entrance_score_2008  <- LSAY_2006$LCB019S
LSAY_2006_data$Entrance_score_2009  <- LSAY_2006$LDB019S
LSAY_2006_data$Entrance_score_2010  <- LSAY_2006$LEB021S
LSAY_2006_data$Entrance_score_2011  <- LSAY_2006$LFB021S

LSAY_2006_data$QLD <- ifelse((  LSAY_2006$LCB015 %in% "3" & !is.na(LSAY_2006_data$Entrance_score_2008)) |
                               (LSAY_2006$LDB015 %in% "3" & !is.na(LSAY_2006_data$Entrance_score_2009)) |
                               (LSAY_2006$LEB017 %in% "3" & !is.na(LSAY_2006_data$Entrance_score_2010)) |
                               (LSAY_2006$LFB017 %in% "3" & !is.na(LSAY_2006_data$Entrance_score_2011)),"QLD",NA)

LSAY_2006_data$ATAR <- ifelse(!is.na(LSAY_2006_data$Entrance_score_2008),LSAY_2006_data$Entrance_score_2008,
                              ifelse(!is.na(LSAY_2006_data$Entrance_score_2009),LSAY_2006_data$Entrance_score_2009,
                                     ifelse(!is.na(LSAY_2006_data$Entrance_score_2010),LSAY_2006_data$Entrance_score_2010,
                                            ifelse(!is.na(LSAY_2006_data$Entrance_score_2011),LSAY_2006_data$Entrance_score_2011,NA))))

LSAY_2006_data$ATAR <- ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 1,99.5,
                              ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 2,98,
                                     ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 3,96.5,
                                            ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 4,94.5,
                                                   ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 5,92.5,
                                                          ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 6,90,
                                                                 ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 7,88,
                                                                        ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 8,85,
                                                                               ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 9,82.5,
                                                                                      ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 10,80,
                                                                                             ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 11,77.5,
                                                                                                    ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 12,75,
                                                                                                           ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 13,72.5,
                                                                                                                  ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 14,70,
                                                                                                                         ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 15,65,
                                                                                                                                ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 16,62.5,
                                                                                                                                       ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 17,60,
                                                                                                                                              ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 18,55,
                                                                                                                                                     ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 19,52.5,
                                                                                                                                                            ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 20,50,
                                                                                                                                                                   ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 21,45,
                                                                                                                                                                          ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 22,40,
                                                                                                                                                                                 ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 23,35,
                                                                                                                                                                                        ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 24,30,
                                                                                                                                                                                               ifelse(LSAY_2006_data$QLD =="QLD" & LSAY_2006_data$ATAR == 25,25, LSAY_2006_data$ATAR )))))))))))))))))))))))))











# LSAY_2006_data$ATAR <- ifelse(LSAY_2006$LCB015 %in% "3" & (LSAY_2006_data$Entrance_score_2008 <= 25) | 
#                                 LSAY_2006$LDB015 %in% "3" & (LSAY_2006_data$Entrance_score_2009 <= 25) | 
#                                 LSAY_2006$LEB017 %in% "3" & (LSAY_2006_data$Entrance_score_2010 <= 25) | 
#                                 LSAY_2006$LFB017 %in% "3" & (LSAY_2006_data$Entrance_score_2011 <= 25), 
#                               vlookup(LSAY_2006_data$ATAR, OP_to_ATAR_table, "OP_score", "ATAR_score"), LSAY_2006_data$ATAR)


LSAY_2006_data$Enter_Score_received <- LSAY_2006_data %>% 
  select(.,Enter_Score_received_2008:Enter_Score_received_2011) %>% 
  apply(1, function(x) any(x %in% c("Yes")))

LSAY_2006_data$Enter_Score_not_received <- LSAY_2006_data %>% 
  select(.,Enter_Score_received_2008:Enter_Score_received_2011) %>% 
  apply(1, function(x) any(x %in% c("No")))

LSAY_2006_data$Enter_Score_received <- ifelse(LSAY_2006_data$Enter_Score_received == TRUE,"Yes",
                                              ifelse(LSAY_2006_data$Enter_Score_not_received == TRUE, "No",NA))

LSAY_2006_data$Enter_Score_not_received <- NULL

LSAY_2006_data$ATAR_group <- cut(LSAY_2006_data$ATAR, 
                                 breaks=c(0, 50, 60, 70, 80, 90, 100), 
                                 labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()

LSAY_2006_data$ATAR_group <- ifelse(is.na(LSAY_2006_data$ATAR_group) & is.na(LSAY_2006_data$Enter_Score_received) & !is.na(LSAY_2006_data$Uni_attend_age_19),"Not_available",
                                    ifelse(is.na(LSAY_2006_data$ATAR) & is.na(LSAY_2006_data$Enter_Score_received),NA,
                                           ifelse(!is.na(LSAY_2006_data$ATAR_group),LSAY_2006_data$ATAR_group,
                                                  ifelse(LSAY_2006_data$Enter_Score_received %in% "Yes","Not_reported",
                                                         ifelse(LSAY_2006_data$Enter_Score_received %in% "No","No_ATAR","Not_available")))))

LSAY_2006_data$ATAR_group <- ifelse(LSAY_2006_data$Completed_year_12 %in% "Not_completed" & LSAY_2006_data$ATAR_group %in% "Not_available","No_ATAR",LSAY_2006_data$ATAR_group) 

# 2009

LSAY_2009_data$Enter_Score_received_2011  <- ifelse(LSAY_2009$LCB020 =="1", "Yes",
                                                    ifelse(LSAY_2009$LCB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2009$LCB020 =="2", "No", NA)))

LSAY_2009_data$Enter_Score_received_2012  <- ifelse(LSAY_2009$LDB020 =="1", "Yes",
                                                    ifelse(LSAY_2009$LDB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2009$LDB020 =="2", "No", NA)))

LSAY_2009_data$Enter_Score_received_2013  <- ifelse(LSAY_2009$LEB020 =="1", "Yes",
                                                    ifelse(LSAY_2009$LEB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2009$LEB020 =="2", "No", NA)))

LSAY_2009_data$Enter_Score_received_2014  <- ifelse(LSAY_2009$LFB020 =="1", "Yes",
                                                    ifelse(LSAY_2009$LFB020 =="3", "Dont Know",
                                                           ifelse(LSAY_2009$LFB020 =="2", "No", NA)))

LSAY_2009_data$Entrance_score_2011  <- LSAY_2009$LCB021
LSAY_2009_data$Entrance_score_2012  <- LSAY_2009$LDB021S
LSAY_2009_data$Entrance_score_2013  <- LSAY_2009$LEB021S
LSAY_2009_data$Entrance_score_2014  <- LSAY_2009$LFB021S

LSAY_2009_data$ATAR <- ifelse(!is.na(LSAY_2009_data$Entrance_score_2011),LSAY_2009_data$Entrance_score_2011,
                              ifelse(!is.na(LSAY_2009_data$Entrance_score_2012),LSAY_2009_data$Entrance_score_2012,
                                     ifelse(!is.na(LSAY_2009_data$Entrance_score_2013),LSAY_2009_data$Entrance_score_2013,
                                            ifelse(!is.na(LSAY_2009_data$Entrance_score_2014),LSAY_2009_data$Entrance_score_2014,NA))))

LSAY_2009_data$QLD <- ifelse((  LSAY_2009$LCB017 %in% "3" & !is.na(LSAY_2009_data$Entrance_score_2011)) |
                                (LSAY_2009$LDB017 %in% "3" & !is.na(LSAY_2009_data$Entrance_score_2012)) |
                                (LSAY_2009$LEB017 %in% "3" & !is.na(LSAY_2009_data$Entrance_score_2013)) |
                                (LSAY_2009$LFB017 %in% "3" & !is.na(LSAY_2009_data$Entrance_score_2014)),"QLD",NA)

LSAY_2009_data$ATAR <- ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 1,99.5,
                              ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 2,98,
                                     ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 3,96.5,
                                            ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 4,94.5,
                                                   ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 5,92.5,
                                                          ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 6,90,
                                                                 ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 7,88,
                                                                        ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 8,85,
                                                                               ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 9,82.5,
                                                                                      ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 10,80,
                                                                                             ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 11,77.5,
                                                                                                    ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 12,75,
                                                                                                           ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 13,72.5,
                                                                                                                  ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 14,70,
                                                                                                                         ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 15,65,
                                                                                                                                ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 16,62.5,
                                                                                                                                       ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 17,60,
                                                                                                                                              ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 18,55,
                                                                                                                                                     ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 19,52.5,
                                                                                                                                                            ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 20,50,
                                                                                                                                                                   ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 21,45,
                                                                                                                                                                          ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 22,40,
                                                                                                                                                                                 ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 23,35,
                                                                                                                                                                                        ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 24,30,
                                                                                                                                                                                               ifelse(LSAY_2009_data$QLD =="QLD" & LSAY_2009_data$ATAR == 25,25, LSAY_2009_data$ATAR )))))))))))))))))))))))))

# LSAY_2009_data$ATAR <- ifelse((  LSAY_2009$LCB017 %in% "3" & (LSAY_2009_data$Entrance_score_2011 <= 25)) | 
#                                 (LSAY_2009$LDB017 %in% "3" & (LSAY_2009_data$Entrance_score_2012 <= 25)) |
#                                 (LSAY_2009$LEB017 %in% "3" & (LSAY_2009_data$Entrance_score_2013 <= 25)) |
#                                 (LSAY_2009$LFB017 %in% "3" & (LSAY_2009_data$Entrance_score_2014 <= 25)), 
#                               vlookup(LSAY_2009_data$ATAR, OP_to_ATAR_table, "OP_score", "ATAR_score"), LSAY_2009_data$ATAR)


#
LSAY_2009_data$Enter_Score_received <- LSAY_2009_data %>% 
  select(.,Enter_Score_received_2011:Enter_Score_received_2014) %>% 
  apply(1, function(x) any(x %in% c("Yes")))

LSAY_2009_data$Enter_Score_not_received <- LSAY_2009_data %>% 
  select(.,Enter_Score_received_2011:Enter_Score_received_2014) %>% 
  apply(1, function(x) any(x %in% c("No")))

LSAY_2009_data$Enter_Score_received <- ifelse(LSAY_2009_data$Enter_Score_received == TRUE,"Yes",
                                              ifelse(LSAY_2009_data$Enter_Score_not_received == TRUE, "No",NA))

LSAY_2009_data$Enter_Score_not_received <- NULL

LSAY_2009_data$ATAR_group <- cut(LSAY_2009_data$ATAR, 
                                 breaks=c(0, 50, 60, 70, 80, 90, 100), 
                                 labels=c("0-50","50-60","60-70","70-80","80-90","90-100")) %>% as.character()

LSAY_2009_data$ATAR_group <- ifelse(is.na(LSAY_2009_data$ATAR_group) & is.na(LSAY_2009_data$Enter_Score_received) & !is.na(LSAY_2009_data$Uni_attend_age_19),"Not_available",
                                    ifelse(is.na(LSAY_2009_data$ATAR_group) & is.na(LSAY_2009_data$Enter_Score_received),NA,
                                           ifelse(!is.na(LSAY_2009_data$ATAR_group),LSAY_2009_data$ATAR_group,
                                                  ifelse(LSAY_2009_data$Enter_Score_received %in% "Yes","Not_reported",
                                                         ifelse(LSAY_2009_data$Enter_Score_received %in% "No","No_ATAR","Not_available")))))

LSAY_2009_data$ATAR_group <- ifelse(LSAY_2009_data$Completed_year_12 %in% "Not_completed" & LSAY_2009_data$ATAR_group %in% "Not_available","No_ATAR",LSAY_2009_data$ATAR_group) 


# Missing ATAR dummy

LSAY_2003_data$ATAR_missing_dummy <- ifelse(LSAY_2003_data$ATAR_group == "Not_reported","Not_reported",
                                            ifelse(LSAY_2003_data$ATAR_group == "Not_available", "Not_available","ATAR_responded"))

LSAY_2006_data$ATAR_missing_dummy <- ifelse(LSAY_2006_data$ATAR_group == "Not_reported","Not_reported",
                                            ifelse(LSAY_2006_data$ATAR_group == "Not_available", "Not_available","ATAR_responded"))

LSAY_2009_data$ATAR_missing_dummy <- ifelse(LSAY_2009_data$ATAR_group == "Not_reported","Not_reported",
                                            ifelse(LSAY_2009_data$ATAR_group == "Not_available", "Not_available","ATAR_responded"))

ATAR_group_order <- c("90-100","80-90","70-80","60-70","50-60","0-50","No_ATAR","Not_reported","Not_available")
LSAY_2003_data$ATAR_group <- factor(LSAY_2003_data$ATAR_group, levels = ATAR_group_order)   # Change the order for regression
LSAY_2006_data$ATAR_group <- factor(LSAY_2006_data$ATAR_group, levels = ATAR_group_order)   # Change the order for regression
LSAY_2009_data$ATAR_group <- factor(LSAY_2009_data$ATAR_group, levels = ATAR_group_order)   # Change the order for regression

ATAR_missing_order <- c("ATAR_responded","Not_reported","Not_available")
LSAY_2003_data$ATAR_missing_dummy <- factor(LSAY_2003_data$ATAR_missing_dummy, levels = ATAR_missing_order)
LSAY_2006_data$ATAR_missing_dummy <- factor(LSAY_2006_data$ATAR_missing_dummy, levels = ATAR_missing_order)
LSAY_2009_data$ATAR_missing_dummy <- factor(LSAY_2009_data$ATAR_missing_dummy, levels = ATAR_missing_order)

rm(ATAR_missing_order)

LSAY_2003_data$ATAR_drop_miss <- ifelse(LSAY_2003_data$ATAR_group %ni% c("Not_reported","Not_available"),
                                        as.character(LSAY_2003_data$ATAR_group),NA)

LSAY_2006_data$ATAR_drop_miss <- ifelse(LSAY_2006_data$ATAR_group %ni% c("Not_reported","Not_available"),
                                        as.character(LSAY_2006_data$ATAR_group),NA)

LSAY_2009_data$ATAR_drop_miss <- ifelse(LSAY_2009_data$ATAR_group %ni% c("Not_reported","Not_available"),
                                        as.character(LSAY_2009_data$ATAR_group),NA)


LSAY_2003_data$ATAR_drop_miss <- droplevels(factor(LSAY_2003_data$ATAR_drop_miss, levels = ATAR_group_order))
LSAY_2006_data$ATAR_drop_miss <- droplevels(factor(LSAY_2006_data$ATAR_drop_miss, levels = ATAR_group_order))
LSAY_2009_data$ATAR_drop_miss <- droplevels(factor(LSAY_2009_data$ATAR_drop_miss, levels = ATAR_group_order))

rm(ATAR_group_order)

source("ATAR_Imputation.R") 

Cohorts <- c(1995,1998,2003,2006,2009) # takes about 10 mins to run
for (i in Cohorts) {
  if (file.exists(paste0("ATAR_imputed_PMM_",i,".RData"))) { 
    cat("ATAR scores were imputed in a previous run. Loading results from ATAR_imputed_PMM_cohort.RData file ... \n")
    env <- new.env() 
    load(paste0("ATAR_imputed_PMM_",i,".RData"), envir=env) 
    loadedObjects <- objects(env, all=TRUE) 
    stopifnot(length(loadedObjects)==1) 
    assign(paste0("ATAR_imputed_PMM_",i),env[[loadedObjects]],.GlobalEnv)
    rm(env)} else {
      cat(paste0("Imputing missing ATAR scores using the ATAR_imputation_function() from 'ATAR_Imputation.R'. This will take about 5 minutes. \n
          Results will be saved to ATAR_imputed_PMM_",i,".RData ... \n"))
      assign(paste0("ATAR_imputed_PMM_",i),ATAR_imputation_function(i))
      save(list=paste0("ATAR_imputed_PMM_",i),file=paste0("ATAR_imputed_PMM_",i,".RData")) }
}


LSAY_1995_data$ATAR_group_imputed <- with(ATAR_imputed_PMM_1995,ATAR_group_imputed[match(LSAY_1995_data$stuidno,stuidno)])
LSAY_1998_data$ATAR_group_imputed <- with(ATAR_imputed_PMM_1998,ATAR_group_imputed[match(LSAY_1998_data$stuidno,stuidno)])
LSAY_2003_data$ATAR_group_imputed <- with(ATAR_imputed_PMM_2003,ATAR_group_imputed[match(LSAY_2003_data$STIDSTD,STIDSTD)])  
LSAY_2006_data$ATAR_group_imputed <- with(ATAR_imputed_PMM_2006,ATAR_group_imputed[match(LSAY_2006_data$STIDSTD,STIDSTD)])  
LSAY_2009_data$ATAR_group_imputed <- with(ATAR_imputed_PMM_2009,ATAR_group_imputed[match(LSAY_2009_data$STIDSTD,STIDSTD)])  

ATAR_imputed_PMM_1995$ATAR_score_imputed <- ifelse(ATAR_imputed_PMM_1995$ATAR_group_imputed %in% "No_ATAR", NA,ATAR_imputed_PMM_1995$ATAR_mice)
ATAR_imputed_PMM_1998$ATAR_score_imputed <- ifelse(ATAR_imputed_PMM_1998$ATAR_group_imputed %in% "No_ATAR", NA,ATAR_imputed_PMM_1998$ATAR_mice)

ATAR_imputed_PMM_2003$ATAR_score_imputed <- ifelse(ATAR_imputed_PMM_2003$ATAR_group_imputed %in% "No_ATAR", NA,ATAR_imputed_PMM_2003$ATAR_mice)
ATAR_imputed_PMM_2006$ATAR_score_imputed <- ifelse(ATAR_imputed_PMM_2006$ATAR_group_imputed %in% "No_ATAR", NA,ATAR_imputed_PMM_2006$ATAR_mice)
ATAR_imputed_PMM_2009$ATAR_score_imputed <- ifelse(ATAR_imputed_PMM_2009$ATAR_group_imputed %in% "No_ATAR", NA,ATAR_imputed_PMM_2009$ATAR_mice)

LSAY_1995_data$ATAR_score_imputed <- with(ATAR_imputed_PMM_1995,ATAR_score_imputed[match(LSAY_1995_data$stuidno,stuidno)])  #NA means no ATAR
LSAY_1998_data$ATAR_score_imputed <- with(ATAR_imputed_PMM_1998,ATAR_score_imputed[match(LSAY_1998_data$stuidno,stuidno)])  #NA means no ATAR
LSAY_2003_data$ATAR_score_imputed <- with(ATAR_imputed_PMM_2003,ATAR_score_imputed[match(LSAY_2003_data$STIDSTD,STIDSTD)])  #NA means no ATAR
LSAY_2006_data$ATAR_score_imputed <- with(ATAR_imputed_PMM_2006,ATAR_score_imputed[match(LSAY_2006_data$STIDSTD,STIDSTD)])  #NA means no ATAR
LSAY_2009_data$ATAR_score_imputed <- with(ATAR_imputed_PMM_2009,ATAR_score_imputed[match(LSAY_2009_data$STIDSTD,STIDSTD)])  #NA means no ATAR


rm(ATAR_imputed_PMM_1995,ATAR_imputed_PMM_1998,ATAR_imputed_PMM_2003,ATAR_imputed_PMM_2006,ATAR_imputed_PMM_2009)

Cohorts <- c(2003,2006,2009)

for (Cohort in Cohorts) { 
  assign(paste0("LSAY_",Cohort,"_data"), 
         get(paste0("LSAY_",Cohort,"_data")) %>% 
           mutate(ATAR_missing_dummy=recode(ATAR_missing_dummy, Not_available = "Missing",Not_reported = "Missing")))

}

Cohorts <- c(1995,1998,2003,2006,2009)

for (Cohort in Cohorts) { 
  assign(paste0("LSAY_",Cohort,"_data"), 
         get(paste0("LSAY_",Cohort,"_data")) %>% 
           mutate(ATAR_group_imputed_3 = recode(ATAR_group_imputed, No_ATAR = "0-60",`0-50` = "0-60", `50-60` = "0-60", `60-70` = "60-80", 
                                                                      `70-80` = "60-80", `80-90` = "80-100", `90-100` = "80-100")))
  
}

LSAY_1995_data <- LSAY_1995_data %>% 
  mutate(Outcome_age_23 = ifelse(Manager_prof2004 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2004 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2004 %in% 0 & Employment_status_2004 %in% c("Full_time","Part_time"), "Other occupation",NA))))

LSAY_1998_data <- LSAY_1998_data %>% 
  mutate(Outcome_age_23 = ifelse(Manager_prof2007 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2007 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2007 %in% 0 & Employment_status_2007 %in% c("Full_time","Part_time"), "Other occupation",NA))))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Outcome_age_23 = ifelse(Manager_prof2011 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2011 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2011 %in% 0 & Employment_status_2011 %in% c("Full_time","Part_time"), "Other occupation",NA))))

LSAY_2003_data <- LSAY_2003_data %>% 
  mutate(Outcome_age_25 = ifelse(Manager_prof2013 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2013 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2013 %in% 0 & Employment_status_2013 %in% c("Full_time","Part_time"), "Other occupation",NA))))


LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Outcome_age_23 = ifelse(Manager_prof2014 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2014 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2014 %in% 0 & Employment_status_2014 %in% c("Full_time","Part_time"), "Other occupation",NA))))

LSAY_2006_data <- LSAY_2006_data %>% 
  mutate(Outcome_age_25 = ifelse(Manager_prof2016 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2016 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2016 %in% 0 & Employment_status_2016 %in% c("Full_time","Part_time"), "Other occupation",NA))))

LSAY_2009_data <- LSAY_2009_data %>% 
  mutate(Outcome_age_23 = ifelse(Manager_prof2017 %in% 1,"Manager or professional",
                                 ifelse(Employment_status_2017 %in% c("NILF","Unemployed"),"Not working",
                                        ifelse(Manager_prof2017 %in% 0 & Employment_status_2017 %in% c("Full_time","Part_time"), "Other occupation",NA))))



##########----------------------------------------------------------------------------------------------------##########
#         More variables ...
##########----------------------------------------------------------------------------------------------------##########

LSAY_1995_data$Log_closest_uni <- log(LSAY_1995_data$Closest_uni)
LSAY_1998_data$Log_closest_uni <- log(LSAY_1998_data$Closest_uni)
LSAY_2003_data$Log_closest_uni <- log(LSAY_2003_data$Closest_uni)
LSAY_2006_data$Log_closest_uni <- log(LSAY_2006_data$Closest_uni)
LSAY_2009_data$Log_closest_uni <- log(LSAY_2009_data$Closest_uni)

LSAY_1995_data$Total <- "Total"
LSAY_1998_data$Total <- "Total"
LSAY_2003_data$Total <- "Total"
LSAY_2006_data$Total <- "Total"
LSAY_2009_data$Total <- "Total"

############# 

LSAY_2006_data$Worked_first_year_university <- ifelse(LSAY_2006_data$Hours_worked_commencement > 0, 1,
                                                      ifelse(LSAY_2006_data$Hours_worked_commencement <=0, 0, NA))

LSAY_2006_data$Part_time_first_year <- ifelse(LSAY_2006_data$Study_status_commencement %in% "Part-time", 1,
                                              ifelse(LSAY_2006_data$Study_status_commencement %in% "Full-time",0, NA))

LSAY_2006_data$No_ATAR <- ifelse(LSAY_2006_data$ATAR_group_imputed == "No_ATAR",1,0)


LSAY_2009_data$Worked_first_year_university <- ifelse(LSAY_2009_data$Hours_worked_commencement > 0, 1,
                                                      ifelse(LSAY_2009_data$Hours_worked_commencement <=0, 0, NA))



LSAY_2009_data$Part_time_first_year <- ifelse(LSAY_2009_data$Study_status_commencement %in% "Part-time", 1,
                                              ifelse(LSAY_2009_data$Study_status_commencement %in% "Full-time",0, NA))

LSAY_2009_data$No_ATAR <- ifelse(LSAY_2009_data$ATAR_group_imputed == "No_ATAR",1,0)


