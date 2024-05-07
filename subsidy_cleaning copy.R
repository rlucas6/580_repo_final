
setwd("/Users/raines/Desktop/PixelsParcels/input/subsidy_data/raw")

library("dplyr")
library("tidyr")
library("openxlsx")

# Set directories -----------------------
home_dir <- dirname(getwd())
raw_dir <- paste0(home_dir, "/raw")
WI_dir <- paste0(home_dir, "/WI")


# OPTIONAL: Recreate state specific CSVs
#for (year in 2008:2016) {
#  read.xlsx(paste0(raw_dir, "/subsidies_WI_", year, "_raw.xlsx")) %>%
#    filter(State.FSA.Name == "Wisconsin") %>%
#    write.csv(paste0(WI_dir, "/subsidies_WI_", year, ".csv"), row.names = FALSE)
#}

##########################
# Read in all CSVs

sub_WI_list <- list()

for (year in 2008:2016) {
  file_path <- paste0(WI_dir, "/subsidies_WI_", year, ".csv")
  sub_WI_list[[paste0("sub_WI_", year)]] <- read.csv(file_path)
}

###############################
# Clean dataframes by grouping by person and payment type and summing payment

sub_WI_clean_list <- list()

# Clean and group 2008 and 2009, where we have Formatted.Payee.Name and Disbursement.Amount
for (year in 2008:2009) {
  sub_WI_clean_list[[paste0("sub_WI_", year, "_clean")]] <- sub_WI_list[[paste0("sub_WI_", year)]] %>%
    mutate(paymentyr = year) %>%
    select(State.FSA.Name, County.FSA.Name, Formatted.Payee.Name, Disbursement.Amount, Accounting.Program.Description, paymentyr) %>%
    group_by(Formatted.Payee.Name, State.FSA.Name, County.FSA.Name, Accounting.Program.Description, paymentyr) %>%
    summarise(total_paid = sum(Disbursement.Amount))
}

# For 2010-2016, we replace those two names with Customer.Name and Payment.Request.Amount
for (year in 2010:2016) {
  sub_WI_clean_list[[paste0("sub_WI_", year, "_clean")]] <- sub_WI_list[[paste0("sub_WI_", year)]] %>%
    mutate(paymentyr = year) %>%
    select(State.FSA.Name, County.FSA.Name, Customer.Name, Payment.Request.Amount, Accounting.Program.Description, paymentyr) %>%
    group_by(Customer.Name, State.FSA.Name, County.FSA.Name, Accounting.Program.Description, paymentyr) %>%
    summarise(total_paid = sum(Payment.Request.Amount))
}

# Bind all dataframes and use coalesce() to take care of the different names
sub_WI_08_16 <- bind_rows(sub_WI_clean_list) %>%
  mutate(name = coalesce(Formatted.Payee.Name, Customer.Name)) %>%
  ungroup() %>%
  select(-Formatted.Payee.Name, -Customer.Name)


############################
# Time to attach group ids to names.
# This can be done with the full WI holdings groupid
# Provided by Christina in danegroups_properties/2016State/2016_csvs_forjoin_holdings/wi_2016_holds.csv

namestoIDs_df_hold <- read.csv(paste0(dirname(home_dir), "/holds_groupid_wnames.csv")) %>%
  select(ownernme1, ownernme2, groupid) %>%
  pivot_longer(-groupid) %>% 
  filter(value != "") %>%
  select(-name) %>%
  distinct()
# 2713335

sub_WI_08_16_hold <- left_join(sub_WI_08_16, namestoIDs_df_hold, by = c("name" = "value"))
# notice joining with groupIDs adds 20,000 rows. I believe these are
# people who have more than one group ID asscociated to their names.
# ex. William C Oertel and Sorin Strugariu from the namestoIDs_df
# both have multiple holdings associated to their name.
# Is this a matching issue on Christina's part?

#####
# Repeat for properties

namestoIDs_df_prop <- read.csv(paste0(dirname(home_dir), "/props_groupid_wnames.csv")) %>%
  select(ownernme1, ownernme2, groupid) %>%
  pivot_longer(-groupid) %>% 
  filter(value != "") %>%
  select(-name) %>%
  distinct()

sub_WI_08_16_prop <- left_join(sub_WI_08_16, namestoIDs_df_prop, by = c("name" = "value"))

subsidized_prop_IDs <- sub_WI_08_16_prop %>%
  select(groupid) %>%
  distinct() %>%
  filter(!is.na(groupid))



1 - sum(is.na(sub_WI_08_16_hold$groupid)) / nrow(sub_WI_08_16_hold)
# Only 42% of subsidy entries in the subsidy data match up to a groupid


sub_WI_08_16_hold[is.na(sub_WI_08_16_hold$groupid),] %>% count(paymentyr) %>%
  mutate(pct = n / sum(n)) %>%
  mutate(cumpct = cumsum(pct))
# the last 3/9 years are only responsible for 15% of mismatches,
# suggesting that a good part of mismatches has to do with
# the fact one datasets has names across 2008-16, while the other just 2016


