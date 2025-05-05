installation_needed  <- FALSE
loading_needed       <- TRUE

package_list <- c("foreign", "sandwich","ggplot2", "reshape", "boot", "zoo", 
                  "quantreg", "dummies", "stargazer", "lmtest", "plm","openxlsx", "stringr",
                  "moments",'readstata13','gmm', 'dplyr', 'tidyr','xtable', 'sem', 'bdynsys', 'ivpack')

if(installation_needed){install.packages(package_list)}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

rm(list=ls())

dir <- "data"

# employment
setwd(paste0(dir,"/raw",sep=""))
income_df <-  read.xlsx("lau.xlsx", 1)
income_df$county_fips <- paste0(income_df$X2,income_df$X3)
names(income_df) <- income_df[3,]
names(income_df) <- tolower(names(income_df))
oldnames = c("force","county name/state abbreviation","(%)","codecode")
newnames = c("labor_force","county_names","unemployment_rate","countyfips")
employ<- income_df %>% 
  select(-c(1:3)) %>%
  slice(4:n()) %>%
  rename_at(vars(oldnames), ~ newnames) %>% 
  separate(period, c("month", "year"), "-") %>%
  filter(str_detect(month, "Jan") & year==20) %>%
  mutate_at(c("countyfips","employed","unemployed","unemployment_rate","labor_force"), as.numeric) %>%
  mutate(employment_rate=employed/labor_force*100) %>%
  mutate(state_fips= as.numeric(substr(countyfips,1,nchar(countyfips)-3)))

# emp percentage change
change_df <- read.csv(paste0(dir,"/raw/EconomicTracker/data/Employment - County - Daily.csv",sep=""))
change <- change_df
change <- change %>% 
  select(year,month,day,countyfips,emp) %>%
  mutate_at(c("year","month","day"), as.numeric) %>%
  mutate(date=paste(change$year,change$month,change$day,sep="/")) %>%
  # mutate(wag=as.Date(date, format ="%Y/%m/%d"))
  mutate(date=as.Date(date)) %>%
  mutate(weekday=weekdays(date)) %>%
  filter(str_detect(weekday,"Tuesday")) %>%
  mutate(year=format(date,"%Y")) %>%
  mutate(week=format(date, "%W")) %>%
  select(-c(day,date,weekday))
# count<- change %>% group_by(weekday) %>% summarise(count = sum(emp!='.',na.rm = TRUE))

# state id
id <- read.xlsx("state_fips.xlsx")

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

# merge
emp_merge <- change %>% 
  inner_join(employ %>% select(-c(year,month)),by ='countyfips') %>%
  mutate(year=as.numeric(year))%>%
  inner_join(select(earnings,c(-state_name)),by=c('state_fips','month','year')) %>%
  mutate(emp_rate= employment_rate/100*(1+as.numeric(as.character(emp)))) %>%
  mutate(emp_level=emp_rate*labor_force) %>%
  mutate(annual_income=emp_rate*earnings_w*52) %>%
  mutate(income_region=emp_level*earnings_w)

# weekly emp_rate backed out from employment variation and montly employment rate

new_loc=paste0(dir,"/temp/income_emp.csv",sep="")
write.csv(emp_merge,new_loc, row.names = FALSE)

# covid infection
infection_df=read.csv(paste0(dir,"/raw/EconomicTracker/data/COVID - County - Daily.csv",sep=""))
infection <- infection_df %>%
  unite('date',year:day,sep="/") %>%
  mutate(date=as.Date(date)) %>%
  mutate(weekday=weekdays(date)) %>%
  filter(str_detect(weekday,"Tuesday")) %>%
  mutate(week=format(date, "%W")) %>%
  mutate(year=format(date, "%Y")) 
infection$fips <- str_pad(infection$countyfips, 5, "left", pad = "0")
new_loc=paste0(dir,"/temp/infection.xlsx",sep="")
write.xlsx(infection,new_loc, row.names = FALSE,overwrite=TRUE)

# estimate elasticity of income to small business revenue
small_biz=read.csv(paste0(dir,'/raw/EconomicTracker/data/Womply - County - Weekly.csv',sep=""))
small_biz_df <- small_biz %>%
  unite('date',year:day_endofweek,sep="/") %>%
  mutate(date=as.Date(date)) %>%
  mutate(week=format(date, "%W")) %>%
  mutate(year=format(date, "%Y")) 
small_biz_df$fips=str_pad(small_biz$countyfips, 5, "left", pad = "0")
small_biz_df$date_yw=paste0(small_biz$year,str_pad(small_biz_df$week,2,"left",pad = "0"))
new_loc=paste0(dir,"/temp/small_biz.xlsx",sep="")
write.xlsx(small_biz_df,new_loc, row.names = FALSE, overwrite=TRUE)
