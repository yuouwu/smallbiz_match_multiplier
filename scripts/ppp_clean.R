rm(list=ls())

installation_needed  <- FALSE
loading_needed       <- TRUE

package_list <- c('dplyr', 'tidyr')

#if(installation_needed){install.packages(package_list)}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

# other keeps: "SBAGuarantyPercentage","ProcessingMethod","RuralUrbanIndicator","BusinessType","NAICSCode",
id <- read.csv("COUNTY_ZIP_122020.csv")
convert <- c("ZIP","COUNTY")
id[,convert]<- lapply(id[,convert],as.numeric)

# df <- read.csv(file = "subset.csv")
# keeps <- c("DateApproved","InitialApprovalAmount","ProjectState","ProjectZip","JobsReported","PAYROLL_PROCEED")
# loan <- df[keeps]
# loan[loan=="N/A"] = NA
# filter <- loan %>%
#   mutate_all(na_if,"") %>%
#   filter(! is.na(ProjectState) | ! is.na(ProjectZip)) %>%
#   slice_sample(n=100) %>%
#   mutate(ZIP=as.numeric(substr(ProjectZip,1,5))) %>%
#   inner_join(select(id,c(ZIP,COUNTY)),by="ZIP") %>%
#   mutate(payroll_per_emp=PAYROLL_PROCEED/JobsReported) %>%
#   mutate(date=as.Date(DateApproved, format ="%m/%d/%Y")) %>%
#   mutate(year=format(date,"%Y")) %>%
#   mutate(week=format(date, "%W")) %>%
#   group_by(COUNTY,week,ProjectState,year) %>%
#   summarise(sum_county = sum(payroll_per_emp))

# alternatively use mutate and ungroup() %>%

#files <- list.files(path="ppp/b", pattern="*.csv", full.names=TRUE, recursive=FALSE)
files <- list.files(path="input", pattern="*.csv", full.names=TRUE, recursive=FALSE)
list <- lapply(files, function(x) {
  df <- read.csv(file = x)
  keeps <- c("DateApproved","ProcessingMethod","SBAGuarantyPercentage","InitialApprovalAmount","RuralUrbanIndicator","ProjectState","ProjectZip","JobsReported","NAICSCode","PAYROLL_PROCEED","BusinessType")
  loan <- df[keeps]
  loan[loan=="N/A"] = NA
  filter <- loan %>%
  mutate_all(na_if,"") %>%
  filter(! is.na(ProjectState) | ! is.na(ProjectZip)) %>%
  slice_sample(n=100) %>%
  mutate(ZIP=as.numeric(substr(ProjectZip,1,5))) %>%
  inner_join(select(id,c(ZIP,COUNTY)),by="ZIP") %>%
  mutate(payroll_per_emp=PAYROLL_PROCEED/JobsReported) %>%
  mutate(date=as.Date(DateApproved, format ="%m/%d/%Y")) %>%
  mutate(year=format(date,"%Y")) %>%
  mutate(week=format(date, "%W")) %>%
  group_by(week,ProjectState,year) %>%
  summarise(sum_county = sum(payroll_per_emp)) 
})

merged <- bind_rows(list)
merged <- merged %>%
  group_by (week,ProjectState,year) %>%
  summarise(ppp = sum(sum_county))
write.csv(merged,"output_m.csv")


