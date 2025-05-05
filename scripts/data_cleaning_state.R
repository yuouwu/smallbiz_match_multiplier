installation_needed  <- FALSE
loading_needed       <- TRUE

package_list <- c("foreign", "sandwich","ggplot2", "reshape", "boot", "zoo", 
                  "quantreg", "dummies", "stargazer", "lmtest", "plm", , "stringr",
                  "moments",'readstata13','gmm', 'dplyr', 'tidyr','xtable', 'sem', 'bdynsys', 'ivpack')

if(installation_needed){install.packages(package_list)}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

rm(list=ls())

dir <- "data"
setwd(paste0(dir,"/raw",sep=""))

# id
id <- read.xlsx("state_fips.xlsx")

## income
 # earnings
earning_df <-  read.xlsx("average_weekly_earning.xlsx", 1)
names(earning_df) <- earning_df[2,]
names(earning_df)[names(earning_df) == 'Series ID'] <- 'ID'
earning_df$state_fips <- as.numeric(substr(earning_df$ID, 4,5))
earnings<- earning_df %>% 
  select(-1) %>%
  slice(3:n()) %>%
  inner_join(select(id,c(state_fips,state_name)), by ='state_fips') %>%
  rename_with(.cols = -c(state_name,state_fips), function(x){paste0("earnings_w", x)}) %>%
  gather(my,earnings_w,-c(state_name,state_fips)) %>%
  mutate(my = gsub("earnings_w", "", my)) %>%
  mutate(m = substr(my, 1,3)) %>%
  mutate(year = substr(my, 5,9)) %>%
  mutate_at(c("year","earnings_w"), as.numeric)
earnings$month <- match(earnings$m,month.abb)
earnings <- earnings %>%
  select (-c(my,m))

  # get Jan employment level
emp_df <-  read.xlsx("unemployment_rate.xlsx", 1)
names(emp_df)[1] <- 'ID'
emp_df$state_fips <- as.numeric(substr(emp_df$ID, 6,7))
emp<- emp_df %>% 
  slice(3:n()) %>%
  inner_join(select(id,c(state_fips,state_name)), by ='state_fips') %>%
  mutate_at(c("X3"),as.numeric) %>%
  mutate(emp_rate_m=100-X3) %>%
  select (-c(1:3))

  # labor force
lf_df <-  read.xlsx("labor_force.xlsx", 1)
names(lf_df) <- lf_df[2,]
names(lf_df)[names(lf_df) == 'Series ID'] <- 'ID'
lf_df$state_fips <- as.numeric(substr(lf_df$ID, 6,7))
lf<- lf_df %>% 
  select(-1) %>%
  slice(3:n()) %>%
  inner_join(select(id,c(state_fips,state_name)), by ='state_fips') %>%
  rename_with(.cols = -c(state_name,state_fips), function(x){paste0("lf", x)}) %>%
  gather(my,labor_force,-c(state_name,state_fips)) %>%
  mutate(my = gsub("lf", "", my)) %>%
  mutate(m = substr(my, 1,3)) %>%
  mutate(year = substr(my, 5,9)) %>%
  mutate_at(c("year","labor_force"), as.numeric)
lf$month <- match(lf$m,month.abb)
lf <- lf %>%
  select (-c(my,m))
  
  # get percentage change in employment level
change_df <- read.csv(paste0(dir,"/raw/EconomicTracker/data/Employment - State - Daily.csv",sep=""))
names(change_df)[names(change_df) == 'statefips'] <- 'state_fips'
change <- change_df %>% 
  select(year,month,day,state_fips,emp_combined) %>%
  mutate_at(c("year","month","day"), as.numeric)
change$date <- paste(change$year,change$month,change$day,sep="-")
change$weekday <- weekdays(as.Date(change$date))
change <- change %>% filter(str_detect(weekday,"Tuesday"))
is.na(change) <- change == "."

emp_merge <- change %>% 
  inner_join(emp,by ='state_fips') %>% 
  inner_join(select(lf,c(-state_name)),by=c('state_fips','month','year')) %>%
  inner_join(select(earnings,c(-state_name)),by=c('state_fips','month','year')) %>%
  mutate(emp_rate= emp_rate_m*(1+as.numeric(as.character(emp_combined)))) %>%
  mutate(emp_level=emp_rate*labor_force) %>%
  mutate(income=emp_level*earnings_w)

# Consumption
df <- read.csv(paste0(dir,"/raw/intrvw19_CE/mtbi192.csv",sep=""))
jan <- subset(df, REF_MO == "1")
keep <- c("newid","REF_MO")
yrframe <- subset(df, REF_MO == "1")

df <- read.csv(file = 'public_up_to_150k_4.csv')
keep <- c("DateApproved", "BorrowerState", "BorrowerZip","CurrentApprovalAmount","ProjectState","ProjectZip","BusinessType")
ppp <- df[keep]






