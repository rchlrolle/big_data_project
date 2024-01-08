# Need to create our list of violent crimes per DC
viol_crime <- c("HOMICIDE", "SEX ABUSE", "ROBBERY", "ASSAULT W/DANGEROUS WEAPON")

# create our violent_crime column and reorder the columns so its first followed by offense
Crime_Incidents_in_2023 = Crime_Incidents_in_2023 %>% 
  mutate(VIOLENT_CRIME = ifelse(OFFENSE %in% viol_crime, 1, 0)) %>% 
  select(VIOLENT_CRIME, OFFENSE, everything())