setwd("/Users/raines/Desktop/PixelsParcels/src")
# Set directories -----------------------
home_dir <- dirname(getwd())
src_dir <- paste0(home_dir, "/src")
in_dir <- paste0(home_dir, "/input")
out_dir <- paste0(home_dir, "/output")
#----------------------------------------

library(tidyr)

census <- read.csv(paste0(in_dir, "/CENSUS_2017_WI.csv"))

##########################################################
#                 OWNERSHIP
##########################################################

ownership <- census %>% 
  filter(Commodity == "AG LAND",
         Data.Item == "AG LAND, CROPLAND, HARVESTED - ACRES",
         Domain == "TENURE" | Domain == "TOTAL") %>%
  select(County, Domain.Category, Value) %>%
  pivot_wider(names_from = Domain.Category,
              values_from = Value) %>%
  rename("full_owner" = "TENURE: (FULL OWNER)",
         "part_owner" = "TENURE: (PART OWNER)",
         "tenant" = "TENURE: (TENANT)",
         "total" = "NOT SPECIFIED") %>%
  filter(full_owner != " (D)",
         part_owner != " (D)",
         tenant != " (D)") 

ownership$full_owner = as.numeric(gsub(",","",ownership$full_owner))
ownership$part_owner = as.numeric(gsub(",","",ownership$part_owner))
ownership$tenant = as.numeric(gsub(",","",ownership$tenant))
ownership$total = as.numeric(gsub(",","",ownership$total))

ownership <- ownership %>%
  mutate(pct_full = 100*(full_owner / total),
         pct_part = 100*(part_owner / total),
         pct_tenant = 100*(tenant / total))

##########################################################
#                   LABOR
##########################################################

totalfarms <- census %>%
  filter(Data.Item == "FARM OPERATIONS - NUMBER OF OPERATIONS",
         Domain == "TOTAL") %>%
  select(County, Value) %>%
  rename("totalfarms" = "Value")
totalfarms$totalfarms = as.numeric(gsub(",","",totalfarms$totalfarms))

labor <- census %>% 
  filter(Commodity == "LABOR",
         Data.Item == "LABOR, HIRED - NUMBER OF WORKERS",
         Domain.Category == "NOT SPECIFIED") %>%
  select(County, Data.Item, Value) %>%
  pivot_wider(names_from = Data.Item,
              values_from = Value) %>%
  left_join(totalfarms)
  
labor$`LABOR, HIRED - NUMBER OF WORKERS` = as.numeric(gsub(",","",labor$`LABOR, HIRED - NUMBER OF WORKERS`))

labor <- labor %>%
  mutate(avg_num_workers = `LABOR, HIRED - NUMBER OF WORKERS`/ totalfarms)
#Taking number of workers over number of total farms

# Here is the code for average number of workers on farms which have workers:
#labor <- census %>% 
#  filter(Commodity == "LABOR",
#         Data.Item == "LABOR, HIRED - NUMBER OF WORKERS" |
#           Data.Item == "LABOR, HIRED - OPERATIONS WITH WORKERS",
#         Domain.Category == "NOT SPECIFIED") %>%
#  select(County, Data.Item, Value) %>%
#  pivot_wider(names_from = Data.Item,
#              values_from = Value) 

#labor$`LABOR, HIRED - NUMBER OF WORKERS` = as.numeric(gsub(",","",labor$`LABOR, HIRED - NUMBER OF WORKERS`))
#labor$`LABOR, HIRED - OPERATIONS WITH WORKERS` = as.numeric(labor$`LABOR, HIRED - OPERATIONS WITH WORKERS`)

#labor <- labor %>%
#  mutate(avg_num_workers = `LABOR, HIRED - NUMBER OF WORKERS`/`LABOR, HIRED - OPERATIONS WITH WORKERS`)


##########################################################
#                   INCOME
##########################################################

income <- census %>%
  filter(Data.Item == "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION") %>%
  select(County, Value) %>%
  filter(Value != " (D)") %>%
  rename("avg_income_perfarm" = "Value")
  
income$avg_income_perfarm = as.numeric(gsub(",","",income$avg_income_perfarm))


##########################################################
#           % land in farmland
##########################################################

getacres <- read.csv(paste0(in_dir, "/WI_Counties_stable_exp.csv")) %>%
  select(NAME, SQMI) %>%
  mutate(totalacres = SQMI*640) %>%
  select(NAME, totalacres) %>%
  rename("County" = "NAME")
getacres$County <- toupper(gsub(" County", "", getacres$County))

getfarmop <- census %>%
  filter(Data.Item == "FARM OPERATIONS - ACRES OPERATED",
         Domain == "TOTAL") %>%
  select(County, Value) %>%
  rename("totalfarmacres" = "Value")
getfarmop$totalfarmacres = as.numeric(gsub(",","",getfarmop$totalfarmacres))

pct_landinfarms <- left_join(getacres, getfarmop) %>%
  mutate(pct_landinfarms = 100*(totalfarmacres / totalacres))


##########################################################
#           Population
##########################################################

population <- read.csv(paste0(in_dir, "/WI_Counties_stable_exp.csv")) %>%
  select(NAME, POPULATION) %>%
  rename("County" = "NAME")
population$County <- toupper(gsub(" County", "", population$County))

##########################################################
#           Farm practices
##########################################################

practices <- census %>%
  filter(Data.Item == "PRACTICES, CONSERVATION METHODS USED - NUMBER OF OPERATIONS" |
           Data.Item == "PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS") %>%
  select(County, Data.Item, Value) %>%
  pivot_wider(names_from = Data.Item,
              values_from = Value) %>%
  left_join(totalfarms)
  
practices$`PRACTICES, CONSERVATION METHODS USED - NUMBER OF OPERATIONS` = as.numeric(gsub(",","",practices$`PRACTICES, CONSERVATION METHODS USED - NUMBER OF OPERATIONS`))
practices$`PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS` <- as.numeric(practices$`PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS`)

practices <- practices %>%
  mutate(pctfarms_conserv_prac = 100*(`PRACTICES, CONSERVATION METHODS USED - NUMBER OF OPERATIONS` / totalfarms),
         pctfarms_grazing_prac = 100*(`PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS` / totalfarms))

##########################################################
#           County Acres in CRP in 2008
##########################################################
# Christina I believe has a way to dtmn the amount of CRP land per property

CRPhistory <- read.csv(paste0(in_dir, "/CRPHistoryCounty.csv")) %>%
  filter(STATE == "WISCONSIN") %>%
  select(COUNTY, X2007) %>%
  rename("CRPacres_2007" = "X2007", "County" = "COUNTY")
CRPhistory$CRPacres_2007 = as.numeric(gsub(",","",CRPhistory$CRPacres_2007))
CRPhistory <- left_join(CRPhistory, getfarmop)

CRPhistory$pct_07_farmlandinCRP <- 100*(CRPhistory$CRPacres_2007 / CRPhistory$totalfarmacres)


#######################################################
#######################################################

# Lastly, let's create forjoin dataframes which isolate
# County and the columns which we will join with crop_prop

ownership_forjoin <- ownership %>% select(County, pct_full, pct_part, pct_tenant)

labor_forjoin <- labor %>% select(County, avg_num_workers)

income_forjoin <- income

pct_landinfarms_forjoin <- pct_landinfarms %>% select(County, pct_landinfarms)

population_forjoin <- population

practices_forjoin <- practices %>% select(County, pctfarms_conserv_prac, pctfarms_grazing_prac)

CRP2007_forjoin <- CRPhistory %>% select(County, pct_07_farmlandinCRP)

countydataforjoin_list <- list(ownership_forjoin, labor_forjoin, income_forjoin, CRP2007_forjoin,
                               pct_landinfarms_forjoin, population_forjoin, practices_forjoin)

