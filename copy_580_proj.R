setwd("/Users/raines/Desktop/PixelsParcels/src")
# Set directories -----------------------
home_dir <- dirname(getwd())
src_dir <- paste0(home_dir, "/src")
in_dir <- paste0(home_dir, "/input")
out_dir <- paste0(home_dir, "/output")
#----------------------------------------

source(paste0(src_dir, "/Create_Crop_DFs.R"))
source(paste0(src_dir, "/Countylevel_Data.R"))
source(paste0(src_dir, "/subsidy_cleaning.R"))

library(lmtest)
library(AER)
library(xtable)


######################################################################
# Incorporate crop analysis code (old code broke when columns in crop_prop changed)

tabulated_crops_prop <- read.csv(paste0(in_dir, "/crops_tabulated_prop.csv"))
crop_prop_tabd <- left_join(crop_prop,tabulated_crops_prop, by = "GROUPID")

# Columns 13:51
sum_cols1 <- rowSums(crop_prop_tabd[, 13:51], na.rm = TRUE)
# Columns 66:87
sum_cols2 <- rowSums(crop_prop_tabd[, 66:87], na.rm = TRUE)
# Sum across both sets of columns
crop_prop_tabd$total_CDL_cropland_m2 <- sum_cols1 + sum_cols2

crop_prop_tabd$perc_corn <- 100*((crop_prop_tabd$Corn) / crop_prop_tabd$total_CDL_cropland_m2)
crop_prop_tabd$perc_soy  <- 100*((crop_prop_tabd$Soybeans) / crop_prop_tabd$total_CDL_cropland_m2)
crop_prop_tabd$perc_corn[is.nan(crop_prop_tabd$perc_corn)] <- 0
crop_prop_tabd$perc_soy[is.nan(crop_prop_tabd$perc_soy)] <- 0

cornsoycolumns <- crop_prop_tabd %>% select(GROUPID, perc_corn, perc_soy)

crop_prop <- crop_prop %>% left_join(cornsoycolumns)

####################################################################
# Let's load in data that will give us the County for each property

property_counties <- read.csv(paste0(in_dir, "/crop_prop_county.csv")) %>%
  select(seq(6,12))
holding_counties <- read.csv(paste0(in_dir, "/crop_hold_county.csv")) %>%
  select(seq(6,12))

countycount <- property_counties %>% count(GROUPID)
# looking good
# but for lots of properties, they lie within multiple counties

# for those properties, let's randomly choose one of
# the counties that they lie in and use that

set.seed(22)

prop_wcounty_unique <- property_counties %>%
  group_by(GROUPID) %>%
  summarise(County = sample(NAME, 1))

prop_wcounty_unique$County <- toupper(gsub(" County", "", prop_wcounty_unique$County))


crop_prop <- crop_prop %>% left_join(prop_wcounty_unique)


####################################################################
# Now let's load in all the actual county data

for (df in countydataforjoin_list) {
  # Perform a left join on the main dataframe
  crop_prop <- left_join(crop_prop, df, by = "County")
}


####################################################################
# Now let's add in presence of subsidy

crop_prop$subsidized <- "no"
# Check if each groupid in crop_prop appears in subsidized_prop_IDs
crop_prop$subsidized <- ifelse(crop_prop$GROUPID %in% subsidized_prop_IDs$groupid, "yes", "no")
# can make this more detailed later, adding in subsidized amount
# and such, but will wait until I have better matching rules

crop_prop %>% count(subsidized)
# A little unsettling. There are 54237 subsidies which
# match to property groupIDs across all of Wisconsin.
# However, when matching those to crop_prop IDs, over half
# of them get dropped. Subsidies must be going to people whose
# properties did not have mtr land on them and thus did not get 
# registered in crop_prop ?

####################################################################
# Now let's add in property-specific geocharacteristics

WIpropslope <- read.csv(paste0(in_dir, "/property_specific/WIprop_mean_slope.csv")) %>%
  select(GROUPID, MEAN) %>%
  rename("avg_slope" = MEAN)
WIpropaspect <- read.csv(paste0(in_dir, "/property_specific/WIprop_mean_aspect.csv")) %>%
  select(GROUPID, MEAN) %>%
  rename("avg_aspect" = MEAN)
WIpropelev <- read.csv(paste0(in_dir, "/property_specific/WIprop_mean_elev.csv")) %>%
  select(GROUPID, MEAN) %>%
  rename("avg_elev" = MEAN)

crop_prop <- left_join(crop_prop, WIpropslope) %>%
  left_join(WIpropaspect) %>%
  left_join(WIpropelev)



crop_prop_10_filter <- crop_prop %>% filter(Stable_count + Exp_count > 10)




#############################
#
# FINAL REGRESSIONS
#

crop_prop_10_filter$availablearea_ha <- crop_prop_10_filter$percentareanoncrop/100 * crop_prop_10_filter$Shape_Area_ha
crop_prop_10_filter$log_available_ha <- log(crop_prop_10_filter$availablearea_ha +1)
crop_prop_10_filter$Stable_Area_ha <- .09*crop_prop_10_filter$Stable_count
crop_prop_10_filter$log_exp_ha <- log(crop_prop_10_filter$Exp_Area_ha +1)
crop_prop_10_filter$log_stable_ha <- log(crop_prop_10_filter$Stable_Area_ha +1)
crop_prop_10_filter$log_area_ha <- log(crop_prop_10_filter$Shape_Area_ha)

crop_prop_10_filter$has_exp <- ifelse(crop_prop_10_filter$Exp_Area_ha > 0, 1, 0)

crop_prop_10_filter <- left_join(crop_prop_10_filter, crop_prop_over10 %>% select(GROUPID, cropland_ha_group), by = "GROUPID")


crop_prop_10_filter$small <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("<2", "2-5", "5-10"), 1, 0)
crop_prop_10_filter$medium <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c( "10-25", "25-50", "50-100"), 1, 0)
crop_prop_10_filter$large <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("100-200", "200-500", "500-1000", ">1000"), 1, 0)


# Table 1
summary(lm(Exp_Area_ha ~ Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy,
           data = crop_prop_10_filter))
summary(lm(Exp_Area_ha ~ Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             avg_slope + avg_elev,
           data = crop_prop_10_filter))
summary(lm(Exp_Area_ha ~ Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             avg_slope + avg_elev +
             factor(County),
           data = crop_prop_10_filter))
summary(lm(Exp_Area_ha ~ avg_slope + avg_elev +
             factor(County) +
             Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             Stable_Area_ha:medium +
             Stable_Area_ha:large,
           data = crop_prop_10_filter))
summary(lm(Exp_Area_ha ~ avg_slope + avg_elev +
             factor(County) +
             Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             availablearea_ha:medium +
             availablearea_ha:large,
           data = crop_prop_10_filter))
summary(lm(Exp_Area_ha ~ avg_slope + avg_elev +
             factor(County) +
             Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             availablearea_ha:medium +
             availablearea_ha:large +
             Stable_Area_ha:medium +
             Stable_Area_ha:large,
           data = crop_prop_10_filter))

# Table 2
summary(lm(log_exp_ha ~ log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy,
           data = crop_prop_10_filter))
summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy,
           data = crop_prop_10_filter))
summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy,
           data = crop_prop_10_filter))
summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy +
             log_stable_ha:medium +
             log_stable_ha:large,
           data = crop_prop_10_filter))
summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy +
             log_available_ha:medium +
             log_available_ha:large,
           data = crop_prop_10_filter))
summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy +
             log_stable_ha:medium +
             log_stable_ha:large +
             log_available_ha:medium +
             log_available_ha:large,
           data = crop_prop_10_filter))


##########
# Furhter delineations

crop_prop_10_filter$one <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("<2"), 1, 0)
crop_prop_10_filter$two <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("2-5"), 1, 0)
crop_prop_10_filter$three <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("5-10"), 1, 0)
crop_prop_10_filter$four <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("10-25"), 1, 0)
crop_prop_10_filter$five <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("25-50"), 1, 0)
crop_prop_10_filter$six <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("50-100"), 1, 0)
crop_prop_10_filter$seven <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("100-200"), 1, 0)
crop_prop_10_filter$eight <- ifelse(crop_prop_10_filter$cropland_ha_group %in% c("200-500", "500-1000", ">1000"), 1, 0)

summary(lm(Exp_Area_ha ~ avg_slope + avg_elev +
             factor(County) +
             Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             Stable_Area_ha:two +
             Stable_Area_ha:three +
             Stable_Area_ha:four +
             Stable_Area_ha:five +
             Stable_Area_ha:six +
             Stable_Area_ha:seven +
             Stable_Area_ha:eight,
           data = crop_prop_10_filter))

summary(lm(Exp_Area_ha ~ avg_slope + avg_elev +
             factor(County) +
             Stable_Area_ha + availablearea_ha + subsidized + perc_corn + perc_soy + 
             availablearea_ha:two +
             availablearea_ha:three +
             availablearea_ha:four +
             availablearea_ha:five +
             availablearea_ha:six +
             availablearea_ha:seven +
             availablearea_ha:eight,
           data = crop_prop_10_filter))

summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy +
             log_stable_ha:two +
             log_stable_ha:three +
             log_stable_ha:four +
             log_stable_ha:five +
             log_stable_ha:six +
             log_stable_ha:seven +
             log_stable_ha:eight,
           data = crop_prop_10_filter))

summary(lm(log_exp_ha ~ avg_slope + avg_elev +
             factor(County) +
             log_stable_ha + log_available_ha + subsidized + perc_corn + perc_soy +
             log_available_ha:two +
             log_available_ha:three +
             log_available_ha:four +
             log_available_ha:five +
             log_available_ha:six +
             log_available_ha:seven +
             log_available_ha:eight,
           data = crop_prop_10_filter))


# LPM
summary(lm(has_exp ~ 
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             availablearea_ha:medium +
             availablearea_ha:large,
           data = crop_prop_10_filter))
summary(lm(has_exp ~
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             Stable_Area_ha:medium +
             Stable_Area_ha:large,
           data = crop_prop_10_filter))
summary(lm(has_exp ~ avg_slope + avg_elev +
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             availablearea_ha:medium +
             availablearea_ha:large,
           data = crop_prop_10_filter))
summary(lm(has_exp ~ avg_slope + avg_elev +
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             Stable_Area_ha:medium +
             Stable_Area_ha:large,
           data = crop_prop_10_filter))
summary(lm(has_exp ~ avg_slope + avg_elev +
             factor(County) +
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             availablearea_ha:medium +
             availablearea_ha:large,
           data = crop_prop_10_filter))
summary(lm(has_exp ~ avg_slope + avg_elev +
             factor(County) +
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             Stable_Area_ha:medium +
             Stable_Area_ha:large,
           data = crop_prop_10_filter))


summary(lm(has_exp ~ avg_slope + avg_elev +
             factor(County) +
             availablearea_ha + Stable_Area_ha + subsidized + perc_corn + perc_soy +
             subsidized:two +
             subsidized:three +
             subsidized:four +
             subsidized:five +
             subsidized:six +
             subsidized:seven +
             subsidized:eight,
           data = crop_prop_10_filter))

