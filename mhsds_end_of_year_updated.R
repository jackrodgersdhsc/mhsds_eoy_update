
library(utils)
#install.packages('readr')
library(readr)
#install.packages('tidyverse')
library(tidyverse)


# set wd
setwd("C:/Users/Jack.Rodgers/OneDrive - Department of Health and Social Care/Documents/R")


# get paths for End of year files
zips<-c(
  'https://files.digital.nhs.uk/BE/BB5663/MHSDS%20Data_MarFinal_2024.zip',#Mar24
  'https://files.digital.nhs.uk/4E/79EB40/MHSDS%20Data_FebFinal_2024.zip', #Feb24
  'https://files.digital.nhs.uk/FD/00BD2B/MHSDS%20Data_JanFinal_2024.zip',#Jan24
  'https://files.digital.nhs.uk/D4/A9F766/MHSDS%20Data_DecFinal_2023.zip', #Dec23
  'https://files.digital.nhs.uk/93/78DE50/MHSDS%20Data_NovFinal_2023.zip', #Nov23
  'https://files.digital.nhs.uk/D1/35E12C/MHSDS%20Data_OctFinal_2023.zip', #Oct23
  'https://files.digital.nhs.uk/98/D11928/MHSDS%20Data_SepFinal_2023.zip', #Sep23
  'https://files.digital.nhs.uk/0A/6B8A9D/MHSDS%20Data_AugFinal_2023.zip', #Aug23
  'https://files.digital.nhs.uk/2C/CD777F/MHSDS%20Data_JulFinal_2023.zip', #Jul23
  'https://files.digital.nhs.uk/F4/F03EC1/MHSDS%20Data_JunFinal_2023.zip', #jun23
  'https://files.digital.nhs.uk/54/BB6840/MHSDS%20Data_MayFinal_2023.zip', #may23
  'https://files.digital.nhs.uk/48/3FB7CB/MHSDS%20Data_AprFinal_2023.zip' #apr23
)



### UPDATE FOR MAIN SPREADSHEET
## NEED VARIABLES AS ROWS AND MONTHS AS COLUMNS

# filter to england

for (file in zips){
  
  # load in zip
  download.file(file, "MHSDS.zip")
  unzip("MHSDS.zip")
  MHSDS_monthly_data <- read_csv("MHSDS.zip")
  
  # filter data 
  mhsds_eoy<-MHSDS_monthly_data %>% filter(BREAKDOWN == 'England') 
  
  
  # append it on - if first one then set that as the data
  if(file == zips[1]){
    df_2<-mhsds_eoy
  }else{
    df_2<-df_2 %>% 
      bind_rows(mhsds_eoy)}
}


##### some have weird date and secondary level
errors<- df_2 %>% group_by(MEASURE_NAME) %>% summarise(n=n()) %>% arrange(desc(n)) %>% filter(n>12)

#are they in the data if they're removed as secondary level?
df_w_secondary<-df_2 %>% 
  filter(SECONDARY_LEVEL != 'NONE')

df_no_secondary<-df_2 %>% 
  filter(SECONDARY_LEVEL == 'NONE' &
          MEASURE_NAME %in% df_w_secondary$MEASURE_NAME)
#############


# tidy df - drop awkward ones for now
df_tidy<-df_2 %>% 
  filter(SECONDARY_LEVEL == 'NONE' ) %>% 
  mutate(
    
    # months in two different formats, so apply tidying two ways and then merge columns
    start_date = as.Date(REPORTING_PERIOD_START, format = '%d/%m/%Y'),
         start_date_2 = as.Date(REPORTING_PERIOD_START, format = '%Y-%m-%d'),
         start_date_3 = as.Date(ifelse(is.na(start_date),start_date_2,start_date),origin="1970-01-01"),
         end_date = as.Date(REPORTING_PERIOD_END, format = '%d/%m/%Y'),
         end_date_2 = as.Date(REPORTING_PERIOD_END, format = '%Y-%m-%d'),
         end_date_3 = as.Date(ifelse(is.na(end_date),end_date_2,end_date),origin="1970-01-01"),
    
    # get month/year from end date     
    month = format(end_date_3,format="%b-%y"),
         measure_id = MEASURE_ID,
         measure = MEASURE_NAME,
         value = MEASURE_VALUE)

# only need select columns
df_tidy_2<-df_tidy %>% 
  select(month,measure_id,measure,value)


# pivot wider
df_tidy_wide<-df_tidy_2 %>% 
  pivot_wider(values_from = value, names_from = month)

# save out data
write.csv(df_tidy_wide, file = 'MHSDS_EOY_update_2324.csv')

####### ICB CYP metrics

# CYP metrics
cyp_codes<-c('MHS130','MHS131','MHS132','MHS133','MHS134','MHS135')


# filter to england

for (file in zips){
  
  # load in zip
  download.file(file, "MHSDS.zip")
  unzip("MHSDS.zip")
  MHSDS_monthly_data <- read_csv("MHSDS.zip")
  
  # filter data 
  mhsds_cyp<-MHSDS_monthly_data %>% filter(MEASURE_ID %in% cyp_codes & 
                                             BREAKDOWN == 'Sub ICB of Residence') 
  
  
  # append it on - if first one then set that as the data
  if(file == zips[1]){
    df_2<-mhsds_cyp
  }else{
    df_2<-df_2 %>% 
      bind_rows(mhsds_cyp)}
}

# tidy df
# tidy df - drop awkward ones for now
df_cyp_tidy<-df_2 %>% 
  mutate(
    
    # months in two different formats, so apply tidying two ways and then merge columns
    start_date = as.Date(REPORTING_PERIOD_START, format = '%d/%m/%Y'),
    start_date_2 = as.Date(REPORTING_PERIOD_START, format = '%Y-%m-%d'),
    start_date_3 = as.Date(ifelse(is.na(start_date),start_date_2,start_date),origin="1970-01-01"),
    end_date = as.Date(REPORTING_PERIOD_END, format = '%d/%m/%Y'),
    end_date_2 = as.Date(REPORTING_PERIOD_END, format = '%Y-%m-%d'),
    end_date_3 = as.Date(ifelse(is.na(end_date),end_date_2,end_date),origin="1970-01-01"),
    
    # get month/year from end date     
    month = format(paste0(year(end_date_3),"-",month(end_date_3),"-01"),format="%Y-%m-%d"),
    #month_middle = ymd(month) %m+% months(1),
    measure_id = MEASURE_ID,
    measure = MEASURE_NAME,
    value = MEASURE_VALUE,
    sub_icb = PRIMARY_LEVEL_DESCRIPTION)

# select columns
df_cyp_tidy_2<-df_cyp_tidy %>% 
  select(month, sub_icb, measure, value) %>% 
  pivot_wider(names_from = measure, values_from = value)

# save out
write.csv(df_cyp_tidy_2, file = 'MHSDS_CYP_EOY_update_2324.csv')

