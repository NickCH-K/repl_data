# DATA DOWNLOAD:
# On the usa.ipums.org website, downloaded the 1-year ACS files
# For 2008-2011 and 2013-216
# For all the below variables:
# [1] "YEAR"      "SAMPLE"    "SERIAL"    "CBSERIAL"  "HHWT"      "HHTYPE"    "CLUSTER"   "STATEFIP"  "DENSITY"  
# [10] "METRO"     "METPOP10"  "STRATA"    "GQ"        "FARM"      "OWNERSHP"  "OWNERSHPD" "MORTAMT1"  "RENT"     
# [19] "HHINCOME"  "FOODSTMP"  "NFAMS"     "NSUBFAM"   "NCOUPLES"  "MULTGEN"   "MULTGEND"  "PERNUM"    "PERWT"    
# [28] "SLWT"      "FAMUNIT"   "FAMSIZE"   "SUBFAM"    "SFTYPE"    "NCHILD"    "NCHLT5"    "NSIBS"     "ELDCH"    
# [37] "YNGCH"     "RELATE"    "RELATED"   "SEX"       "AGE"       "BIRTHQTR"  "MARST"     "BIRTHYR"   "MARRNO"   
# [46] "MARRINYR"  "YRMARR"    "DIVINYR"   "WIDINYR"   "FERTYR"    "RACE"      "RACED"     "HISPAN"    "HISPAND"  
# [55] "BPL"       "BPLD"      "CITIZEN"   "YRNATUR"   "YRIMMIG"   "YRSUSA1"   "LANGUAGE"  "LANGUAGED" "SPEAKENG" 
# [64] "HCOVANY"   "SCHOOL"    "EDUC"      "EDUCD"     "GRADEATT"  "GRADEATTD" "EMPSTAT"   "EMPSTATD"  "LABFORCE" 
# [73] "CLASSWKR"  "CLASSWKRD" "OCC"       "IND"       "UHRSWORK"  "INCTOT"    "FTOTINC"   "INCWELFR"  "INCEARN"  
# [82] "POVERTY"   "DIFFREM"   "DIFFPHYS"  "DIFFMOB"   "DIFFCARE"  "DIFFSENS"  "DIFFEYE"   "DIFFHEAR"  "VETSTAT"  
# [91] "VETSTATD" 
# With the following case restrictions:
# HISPAN = 1 (Mexican ethnicity)
# BPL = 200 (Born in Mexico)
# AGE from 20 to 39
# CITIZEN = 3 or 5 ("Not a citizen" or "Foreign born, citizenship status not reported")

# Load packages
# install.packages(c('ipumsr','tidyverse','vtable','stringr','sjlabelled'))
library(ipumsr) # Importing IPUMS data
library(tidyverse) # For preparing data
library(stringr) # For converting numeric state FIPS codes to character
library(vtable) # For preparing data documentation
library(sjlabelled) # For simplifying labels for data documentation

# Import data
# Import the ACS pull
ddi = read_ipums_ddi("usa_00026.xml")
dat = read_ipums_micro(ddi)

# Import the state-level policy and labor market data
labor = read_csv('policy_labor_market_data.csv')

# Additional case selection
dat = dat %>%
  filter(GQ %in% 1:2)

# These variables are redundant, or have no variation left after case selection
dat = dat %>%
  select(-SAMPLE, -GQ, -HISPAN, -HISPAND, -BPL, -BPLD, -CITIZEN,
         -YRNATUR)

# Set to explicit missing codes
# For example, MORTAMT1, FARM, etc., are all set to NA if they have values of 0
dat = dat %>%
  mutate(across(c(MORTAMT1, FARM, HHTYPE,
                  OWNERSHP, OWNERSHPD, RENT,
                  FOODSTMP, MULTGEN, MULTGEND,
                  BIRTHQTR, MARRINYR, YRMARR,
                  DIVINYR, WIDINYR, FERTYR,
                  LANGUAGE, LANGUAGED, SPEAKENG, SCHOOL,
                  EDUC, EDUCD, GRADEATT, GRADEATTD, EMPSTAT,
                  EMPSTATD, LABFORCE, CLASSWKR, CLASSWKRD, OCC, IND,
                  INCEARN, POVERTY, DIFFREM, DIFFPHYS, DIFFMOB,
                  DIFFCARE, DIFFSENS, DIFFEYE, DIFFHEAR, VETSTAT,
                  VETSTATD), function(x) na_if(x, 0))) %>%
  mutate(across(c(HHINCOME, FTOTINC), function(x) na_if(x, 9999999))) %>%
  mutate(across(c(ELDCH, YNGCH), function(x) na_if(x, 99))) %>%
  mutate(EDUC = na_if(EDUC, 999)) %>%
  mutate(VETSTAT = na_if(VETSTAT, 9)) %>%
  mutate(MARRNO = na_if(MARRNO, 7) %>% na_if(8) %>% na_if(9))

# These should be purely numeric
dat = dat %>%
  mutate(across(c(MORTAMT1, RENT, OCC, IND, HHINCOME, FTOTINC,
                  INCWELFR, INCEARN, POVERTY, AGE, BIRTHYR,
                  YRIMMIG, UHRSWORK, INCTOT,
                  NFAMS, NSUBFAM, NCOUPLES, FAMSIZE,
                  NCHILD, NCHLT5, ELDCH, YNGCH, YRMARR,
                  YRSUSA1), as.numeric))

# Simplified Versions
dat = dat %>%
  mutate(EDUC_RECODE = factor(case_when(
    EDUC < 6 ~ 'Less than High School', 
    EDUC == 6 ~ 'High School Degree', 
    EDUC == 7 ~ 'Some College', 
    EDUC == 8 ~ 'Two-Year Degree',
    TRUE ~ 'BA+'
  ), levels = c('Less than High School','High School Degree','Some College', 'Two-Year Degree', 'BA+')),
  RACE_RECODE = factor(case_when(
    RACE == 1 ~ 'White',
    RACE == 2 ~ 'Black/African-American',
    RACE == 3 ~ 'American Indian / Alaskan Native',
    RACE %in% 4:6 ~ 'Asian',
    RACE == 7 ~ 'Other Race',
    RACE %in% 8:9 ~ 'Multiracial'
  )))

# Bring in the state policy data
dat = dat %>%
  mutate(state_fips = str_pad(STATEFIP, 2,'left','0')) %>%
  inner_join(labor %>% rename(YEAR = year)) %>%
  select(-state_fips) %>%
  mutate(statename = factor(statename),
         CensusRegion = factor(CensusRegion)) %>%
  var_labels(DRIVERSLICENSES = 'Can undocumented immigrants get state drivers\' licenses?',
             INSTATETUITION = 'Can undocumented immigrants receive in-state tuition at the state’s public colleges?',
             STATEFINANCIALAID = 'Can undocumented immigrants receive financial aid for college from the state?',
             HIGHEREDBAN = 'Are undocumented immigrants banned from attending the state’s public colleges?',
             EVERIFY = 'This refers to state-level E-Verify laws, which help or require that employers verify the work authorization of new hires. This policy constitutes a barrier to working for undocumented immigrants.',
             LIMITEVERIFY = 'In addition to some states not having an E-Verify mandate, some states block local governments like cities or counties from implementing their own E-Verify mandates. This policy prevents local governments from imposing an E-Verify barrier to working for undocumented immigrants.',
             OMNIBUS = 'These policies vary across states but in general increase enforcement of immigration law, by forcing immigrants to carry immigration registration documents, requiring law enforcement to verify immigration status during traffic stops, and creating penalties for harboring immigrants who were in violation of immigration law.',
             TASK287G = '287(g) policies allow local law enforcement to perform certain functions of federal immigration law, increasing the number of law enforcement agencies that can enforce immigration laws.',
             JAIL287G = '287(g) policies allow local law enforcement to perform certain functions of federal immigration law, increasing the number of law enforcement agencies that can enforce immigration laws.',
             SECURECOMMUNITIES = 'Through Secure Communities, fingerprints submitted by local law enforcement agencies to the FBI are shared with immigration enforcement agencies for checks against immigration databases. Depending on the result, immigration officials decide whether to take enforcement action, such as issuing a detainer request.',
             LFPR = 'This is the labor force participation rate, on a scale from 0 to 100.',
             UNEMP = 'This is the state unemployment rate, on a scale from 0 to 100.')

# Create eligibility variables
dat = dat %>%
  mutate(FT = 1*(UHRSWORK >= 35),
         AFTER = 1*(YEAR >= 2013),
         AGE_IN_JUNE_2012 = 2012 - BIRTHYR + .5 - .25*BIRTHQTR,
         AGE_AT_IMMIGRATION = YRIMMIG - BIRTHYR,
         HS_DEGREE = EDUC >= 6) %>%
  # Only keep observations in specified age range, and otherwise eligible
  filter(AGE_IN_JUNE_2012 >= 26 & AGE_IN_JUNE_2012 <= 35,
         AGE_AT_IMMIGRATION < 16,
         YRIMMIG <= 2007,
         HS_DEGREE | (VETSTAT == 2)) %>%
  mutate(ELIGIBLE = 1*(AGE_IN_JUNE_2012 <= 30)) %>%
  var_labels(FT = 'Works full-time.',
             AFTER = 'After the year 2012 when DACA is implemented.',
             AGE_IN_JUNE_2012 = 'Age in the quarter when DACA was implemented.',
             AGE_AT_IMMIGRATION = 'Age in the year of immigration to the United States.',
             ELIGIBLE = 'Someone who eligible for DACA (whether or not DACA has been implemented by this time.')

# Remove unused labels
dat = dat %>%
  mutate(across(where(is.labelled), drop_labels))

# Create variable documentation
vtable(dat, 
       file = 'Prepared Data Documentation.html', 
       data.title = 'Pre-Cleaned Data for DACA Analysis',
       factor.limit = 0)

# Create unlabelled numeric version
dat %>%
  remove_all_labels() %>%
  write_csv('prepared_data_numeric_version.csv')
# And create labeled mostly-string version
dat %>%
  mutate(CLUSTER = as.character(CLUSTER)) %>%
  mutate(across(where(is.labelled), as_label)) %>%
  write_csv('prepared_data_labelled_version.csv')
