# The Demand Driven University System: A Mixed Report Card

The report can be found here: https://www.pc.gov.au/research/completed/university-report-card


## How to run this program
The code to generate the results is "LSAY analysis master.R". All necessary packages, data and functions are read into this file.
Most of the custom functions are sourced in from the other R files included with this. E.g. the function for calculating the survey attrition weights in "Weights.R".


## Requirements
1. Longitudinal Surveys of Austalian Youth (see https://www.lsay.edu.au/data), including confidentialised postcode data
    (individuals and schools). This is available by formal request to the Australian Data Archive at the Australian National
    University.

To replicate the findings of this research, it is recommended that the same versions of the LSAY data are used.
The versions of each of the LSAY data files used are the following:

a. LSAY 1995 Cohort Version 3 (Stata)

b. LSAY 1998 Cohort Version 3.1 (Stata)

c. LSAY 2003 Cohort Version 7 (Stata)

d. LSAY 2006 Cohort Version 10 (Stata)

e. LSAY 2009 Cohort Version 7 (Stata)

f. LSAY 2015 Cohort Version 2 (Stata)



2. The R packages specified in "Packages.R" need to be installed. Please check package versions for compatibility (packages are current as of June 2019)



3. Other data files sourced mainly from the Australian Bureau of Statistics. As these data are publically available, they are
    included in the data files provided with this document.