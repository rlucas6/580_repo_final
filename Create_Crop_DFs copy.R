#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################################
##################################################
#     PREPARATION OF crop_prop and crop_hold
##################################################
##################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tidyr)


# Set directories -----------------------
home_dir <- dirname(getwd())
src_dir <- paste0(home_dir, "/src")
in_dir <- paste0(home_dir, "/input")
out_dir <- paste0(home_dir, "/output")
#----------------------------------------

#Load in our two core dataframes: properties and holdings

crop_prop <- read.csv(paste0(in_dir, "/crop_properties.csv")) %>%
  select(-OID_, -Shape_Length)
crop_hold <- read.csv(paste0(in_dir, "/crop_holdings.csv")) %>%
  select(-OID_, -Shape_Length)

# Add shape area in hectares
crop_prop$Shape_Area_ha <- crop_prop$Shape_Area / 10000
crop_hold$Shape_Area_ha <- crop_hold$Shape_Area / 10000

# First, get total point count of crops

crop_prop$totalpoints <- crop_prop$Exp_count + crop_prop$Stable_count + crop_prop$Aban_count
crop_hold$totalpoints <- crop_hold$Exp_count + crop_hold$Stable_count + crop_hold$Aban_count

crop_prop$Crop_Area_ha <- crop_prop$totalpoints * .09
crop_hold$Crop_Area_ha <- crop_hold$totalpoints * .09

# Exp_area
crop_prop$Exp_Area_ha <- 0.09*crop_prop$Exp_count
crop_hold$Exp_Area_ha <- 0.09*crop_hold$Exp_count



##########################################
# CREATE NONCROP DATA
 
# 4/1/24 modified to be percent of area which was not cropland
# in 2008. That would be the Stable and Abandoned area minus the Expansion area
crop_prop$percentareanoncrop <- (1-((crop_prop$Crop_Area_ha - crop_prop$Exp_Area_ha)/crop_prop$Shape_Area_ha))*100  #little quicker to define it this way
crop_prop$percentareacropped <- ((crop_prop$Crop_Area_ha - crop_prop$Exp_Area_ha) / crop_prop$Shape_Area_ha)*100
crop_prop$percentareanoncrop[crop_prop$percentareanoncrop<0] <- 0
crop_prop$percentareacropped[crop_prop$percentareacropped>100] <- 100

crop_hold$percentareanoncrop <- (1-((crop_hold$Crop_Area_ha - crop_hold$Exp_Area_ha)/crop_hold$Shape_Area_ha))*100  #little quicker to define it this way
crop_hold$percentareacropped <- ((crop_hold$Crop_Area_ha - crop_hold$Exp_Area_ha) / crop_hold$Shape_Area_ha)*100
crop_hold$percentareanoncrop[crop_hold$percentareanoncrop<0] <- 0
crop_hold$percentareacropped[crop_hold$percentareacropped>100] <- 100
##########################################




#################################################
# CREATE FARM SIZE GROUPING

#Properties
crop_prop_over10 <- crop_prop %>% filter(totalpoints > 10)
summary(crop_prop_over10$Crop_Area_ha)

crop_prop_over10 <- crop_prop_over10 %>%
  mutate( cropland_ha_group = case_when(
    Crop_Area_ha > 1000 ~ '>1000',          
    Crop_Area_ha > 500 ~ '500-1000',
    Crop_Area_ha > 200 ~ '200-500',
    Crop_Area_ha > 100 ~ '100-200',
    Crop_Area_ha > 50 ~ '50-100',
    Crop_Area_ha > 25 ~ '25-50',
    Crop_Area_ha > 10 ~ '10-25',
    Crop_Area_ha > 5 ~ '5-10',
    Crop_Area_ha > 2 ~ '2-5',
    TRUE             ~ '<2')
  )

mylevels <- crop_prop_over10 %>% 
  group_by(cropland_ha_group) %>% 
  summarise(count = n()) %>%
  mutate(arranger = c(10,100,2,200,25,5,50,500,1,1000)) %>%
  arrange(arranger) %>%
  pull(cropland_ha_group)

#Holdings
crop_hold_over10 <- crop_hold %>% filter(totalpoints > 10)
summary(crop_hold_over10$Crop_Area_ha)

crop_hold_over10 <- crop_hold_over10 %>%
  mutate( cropland_ha_group = case_when(
    Crop_Area_ha > 1000 ~ '>1000',          
    Crop_Area_ha > 500 ~ '500-1000',
    Crop_Area_ha > 200 ~ '200-500',
    Crop_Area_ha > 100 ~ '100-200',
    Crop_Area_ha > 50 ~ '50-100',
    Crop_Area_ha > 25 ~ '25-50',
    Crop_Area_ha > 10 ~ '10-25',
    Crop_Area_ha > 5 ~ '5-10',
    Crop_Area_ha > 2 ~ '2-5',
    TRUE             ~ '<2')
  )
##########################################


##########################################
# CREATE FARM SIZE QUINTILES
# 
# each group will have roughly the same amount of farms

quantile(crop_hold_over10$Crop_Area_ha, probs = seq(0, 1, length.out = 21))

crop_hold_over10 <- crop_hold_over10 %>%
  mutate( actual_quantiles = case_when(
    Crop_Area_ha > 83.70 ~ '100',          # yellow is the upper quantile
    Crop_Area_ha > 54.72 ~ '95',
    Crop_Area_ha > 40.59 ~ '90',
    Crop_Area_ha > 31.32 ~ '85',
    Crop_Area_ha > 25.56 ~ '80',
    Crop_Area_ha > 20.70 ~ '75',
    Crop_Area_ha > 16.65 ~ '70',
    Crop_Area_ha > 14.04 ~ '65',
    Crop_Area_ha > 11.79 ~ '60',
    Crop_Area_ha > 9.81 ~ '55',
    Crop_Area_ha > 8.10 ~ '50',
    Crop_Area_ha > 6.66 ~ '45',
    Crop_Area_ha > 5.40 ~ '40',
    Crop_Area_ha > 4.41 ~ '35',
    Crop_Area_ha > 3.60 ~ '30',
    Crop_Area_ha > 2.88 ~ '25',
    Crop_Area_ha > 2.25 ~ '20',
    Crop_Area_ha > 1.71 ~ '15',
    Crop_Area_ha > 1.26 ~ '10',
    TRUE            ~ '5')
  )

crop_hold_over10 %>% count(actual_quantiles) #decent



################################################
# CREATE props_forplot, holds_forplot
#
# This dataframe creates repeated observations
# It puts the dataframes into a form where we can use them to create
# the density plots
# It classifies each farm, but a farm can have multiple classifications (ex. Has Stable, Has Expansion, Stable and Expansion)
# hence why we have repeat observations
# These will be used for density plots

# We elect to use a 10-pixel filter here

# CLASSIFY

#PROPERTIES
prop_hascrop <- crop_prop %>% filter(totalpoints > 10)
prop_hasexp <- crop_prop %>% filter(Exp_count > 10)
prop_hasstable <- crop_prop %>% filter(Stable_count > 10)
prop_hasaban <- crop_prop %>% filter(Aban_count > 10)
prop_stable_exp <- crop_prop %>% filter(Stable_count>5, Exp_count>5)
prop_stable_only <- crop_prop %>% filter(Stable_count > 10, Exp_count==0, Aban_count==0)
prop_exp_only <- crop_prop %>% filter(Stable_count == 0, Exp_count>10, Aban_count==0)
prop_hascrop$Category <- "Has/Had Cropland"  #no filter was applied
prop_hasexp$Category <- "Has Expansion"
prop_hasstable$Category <- "Has Stable"
prop_hasaban$Category <- "Has Abandonment"
prop_stable_exp$Category <- "Stable and Expansion"
prop_stable_only$Category <- "Stable ONLY"
prop_exp_only$Category <- "Expansion ONLY"

props_forplot <- rbind(prop_hascrop,prop_hasexp,prop_hasstable,prop_hasaban,
                       prop_stable_exp,prop_stable_only,prop_exp_only)

#HOLDINGS

hold_hascrop <- crop_hold %>% filter(totalpoints > 10)
hold_hasexp <- crop_hold %>% filter(Exp_count > 10)
hold_hasstable <- crop_hold %>% filter(Stable_count > 10)
hold_hasaban <- crop_hold %>% filter(Aban_count > 10)
hold_stable_exp <- crop_hold %>% filter(Stable_count>5, Exp_count>5)
hold_stable_only <- crop_hold %>% filter(Stable_count > 10, Exp_count==0, Aban_count==0)
hold_exp_only <- crop_hold %>% filter(Stable_count == 0, Exp_count>10, Aban_count==0)
hold_hascrop$Category <- "Has/Had Cropland"
hold_hasexp$Category <- "Has Expansion"
hold_hasstable$Category <- "Has Stable"
hold_hasaban$Category <- "Has Abandonment"
hold_stable_exp$Category <- "Stable and Expansion"
hold_stable_only$Category <- "Stable ONLY"
hold_exp_only$Category <- "Expansion ONLY"

holds_forplot <- rbind(hold_hascrop,hold_hasexp,hold_hasstable,hold_hasaban,
                       hold_stable_exp,hold_stable_only,hold_exp_only)
############################
# Add in farm size groupings
#Properties
props_forplot_over10 <- props_forplot %>% filter(totalpoints > 10)

props_forplot_over10 <- props_forplot_over10 %>%
  mutate( cropland_ha_group = case_when(
    Crop_Area_ha > 1000 ~ '>1000',          
    Crop_Area_ha > 500 ~ '500-1000',
    Crop_Area_ha > 200 ~ '200-500',
    Crop_Area_ha > 100 ~ '100-200',
    Crop_Area_ha > 50 ~ '50-100',
    Crop_Area_ha > 25 ~ '25-50',
    Crop_Area_ha > 10 ~ '10-25',
    Crop_Area_ha > 5 ~ '5-10',
    Crop_Area_ha > 2 ~ '2-5',
    TRUE             ~ '<2')
  )


#Holdings
holds_forplot_over10 <- holds_forplot %>% filter(totalpoints > 10)

holds_forplot_over10 <- holds_forplot_over10 %>%
  mutate( cropland_ha_group = case_when(
    Crop_Area_ha > 1000 ~ '>1000',          
    Crop_Area_ha > 500 ~ '500-1000',
    Crop_Area_ha > 200 ~ '200-500',
    Crop_Area_ha > 100 ~ '100-200',
    Crop_Area_ha > 50 ~ '50-100',
    Crop_Area_ha > 25 ~ '25-50',
    Crop_Area_ha > 10 ~ '10-25',
    Crop_Area_ha > 5 ~ '5-10',
    Crop_Area_ha > 2 ~ '2-5',
    TRUE             ~ '<2')
  )


#########################
# Add extra non-crop data

props_forplot$noncroparea_ha <- props_forplot$Shape_Area_ha - props_forplot$Crop_Area_ha
holds_forplot$noncroparea_ha <- holds_forplot$Shape_Area_ha - holds_forplot$Crop_Area_ha

#negative values are unwatned and just due to measurement area. We'll assign
#0 to negative non crop areas to reflect the fact that these are parcels
#that are fully cropland

props_forplot$noncroparea_ha[props_forplot$noncroparea_ha<0] <- 0
holds_forplot$noncroparea_ha[holds_forplot$noncroparea_ha<0] <- 0
props_forplot$percentarea_noncrop <- (props_forplot$noncroparea_ha / props_forplot$Shape_Area_ha)*100
holds_forplot$percentarea_noncrop <- (holds_forplot$noncroparea_ha / holds_forplot$Shape_Area_ha)*100





