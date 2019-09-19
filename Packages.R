##########----------------------------------------------------------------------------------------------------##########
#         Install and load packages for education analysis
##########----------------------------------------------------------------------------------------------------##########

# Note, this analysis was conducted with the following packages that were last updated in June 2019. 
# Any problems with running this code may be attributed to differences in package versions


packages <- c("ggpubr","openxlsx","data.table","lubridate","stringi","tidyverse","lme4","devtools","sandwich",
              "lmtest","ggplot2","survey","readstata13","ggmap","geosphere","stargazer","devEMF",
              "splitstackshape","readxl","mice", "parallel")

for (i in packages) { 
  if(!(i %in% rownames(installed.packages()))) {
    install.packages(i) }
  library(i, character.only = TRUE) 
}   

# if(!("PCcharts" %in% rownames(installed.packages()))) install.packages("http://inet.pc.gov.au/Rcommunity/rcode/PCcharts_3.0.0.tar.gz", repo = NULL, type = "source")
# 
# library(PCcharts)

rm(packages)

##########----------------------------------------------------------------------------------------------------##########
#         FYI: Session info from the most recent run of this R code in June 2019 
##########----------------------------------------------------------------------------------------------------##########


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
# 
