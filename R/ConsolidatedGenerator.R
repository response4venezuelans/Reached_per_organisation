##### Generate consolidated figures model per organisation per country per sector #####
##### Author: Francis Fayolle, R4V IM Team ######

library(tidyverse)
library(writexl)

#### Model used ####
#### Sector level: SUM All beneficiaries ####

#### Intersector level: MAX Sector value at Admin1 level ####

#### Montly volume of attention ###
#### General ####

## Check if need to insert filters here

monthlysectors <- df5Wconsolidated %>%
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise('Monthly Total Beneficiaries' = sum(Total_monthly))

monthlytotal <- monthlysectors%>%
  group_by(Country, Appealing_org, Month)%>%
  summarise(Subsector = "Intersector", 
            'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`))

monthly<- rbind(monthlysectors, monthlytotal)

### Monthly CVA Beneficiaries #####

CVAmonthlysectors <- df5Wconsolidated%>%
  filter(CVA == 'Yes')%>%
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise('Monthly CVA Beneficiaries' = sum(Total_monthly))

CVAmonthlytotal <- CVAmonthlysectors%>%
  group_by(Country, Appealing_org, Month)%>%
  summarise(Subsector = "Intersector", 
            'Monthly CVA Beneficiaries' = sum(`Monthly CVA Beneficiaries`))

CVAmonthly<- rbind(CVAmonthlysectors, CVAmonthlytotal)

finalmonthly <- monthly%>%
  full_join(CVAmonthly, by = c("Country", "Appealing_org", "Month", "Subsector"))%>%
  mutate(across(where(is.numeric),
                replace_na, 0),
         Admin1 = "Country level")%>%
  arrange(Country, Appealing_org, Month, Subsector)

rm(monthlysectors, monthlytotal, monthly, CVAmonthlysectors, CVAmonthlytotal, CVAmonthly)


############ People Reached: Consolidated Beneficiaries ###################
############ SUM All beneficiaries at sector level ########################

conssectors <-  df5Wconsolidated%>%
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise(Consolidated_Total = sum(New_beneficiaries),
            Consolidated_RMindestination = sum(IN_DESTINATION), 
            Consolidated_RM_in_transit = sum(IN_TRANSIT),
            Consolidated_Host_Community = sum(Host_Communities),
            Consolidated_RM_Pendulars = sum(PENDULARS),
            Consolidated_Colombian_Returnees = sum(Returnees),
            Consolidated_Girls = sum(Girls), 
            Consolidated_Boys = sum(Boys),
            Consolidated_Women = sum(Women),
            Consolidated_Men = sum(Men),
            Consolidated_Other_under_18 = sum(Other_under),
            Consolidated_Other_above_18 = sum(Other_above)
  )

consCVAsectors <- df5Wconsolidated%>%
  filter(CVA == "Yes")%>%
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise(Consolidated_CVA_Beneficiaries = sum(New_beneficiaries))

consallsectors <- conssectors%>%
  full_join(consCVAsectors, by = c("Country", "Appealing_org", "Month", "Subsector"))%>%
  mutate(across(where(is.numeric),
                replace_na, 0),
         Admin1 = "Country level")%>%
  arrange(Country, Appealing_org, Month, Subsector)

rm(conssectors, consCVAsectors)

######### Generate Intersector data: MAX per sector at Admin1 Level ###########

## Generate Admin1 level per org data

conssectorsadm1 <-  df5Wconsolidated%>%
  group_by(Country, Admin1, Appealing_org, Month, Subsector)%>%
  summarise(Monthly_Consolidated = sum(New_beneficiaries),
            Consolidated_RMindestination = sum(IN_DESTINATION), 
            Consolidated_RM_in_transit = sum(IN_TRANSIT),
            Consolidated_Host_Community = sum(Host_Communities),
            Consolidated_RM_Pendulars = sum(PENDULARS),
            Consolidated_Colombian_Returnees = sum(Returnees),
            Consolidated_Girls = sum(Girls), 
            Consolidated_Boys = sum(Boys),
            Consolidated_Women = sum(Women),
            Consolidated_Men = sum(Men),
            Consolidated_Other_under_18 = sum(Other_under),
            Consolidated_Other_above_18 = sum(Other_above)
  )

consCVAsectorsadm1 <- df5Wconsolidated%>%
  filter(CVA == "Yes")%>%
  group_by(Country,Admin1, Appealing_org, Month, Subsector)%>%
  summarise(Consolidated_CVA_Beneficiaries = sum(New_beneficiaries))

consallsectorsadm1 <- conssectorsadm1%>%
  full_join(consCVAsectorsadm1, by = c("Country", "Admin1", "Appealing_org", "Month", "Subsector"))%>%
  mutate(across(where(is.numeric),
                replace_na, 0))%>%
  arrange(Country, Admin1,  Appealing_org, Month, Subsector)

rm(conssectorsadm1, consCVAsectorsadm1)

## Calculate intersector per org per admin1 with MAX Sector values ##

consmaxsect <- consallsectorsadm1%>%
  # Get MAX Values for each  population type per admin1
  group_by(Country, Admin1, Appealing_org, Month)%>%
  summarise(Subsector = "Intersector",
            Consolidated_RMindestination = max(Consolidated_RMindestination),
            Consolidated_RM_in_transit = max(Consolidated_RM_in_transit),
            Consolidated_Host_Community = max(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = max(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = max(Consolidated_Colombian_Returnees),
            Consolidated_CVA_Beneficiaries = max(Consolidated_CVA_Beneficiaries))%>%
  # Gather data for national level per org by summing all admin1
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise(Admin1 = "Country level",
            Consolidated_RMindestination = sum(Consolidated_RMindestination),
            Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
            Consolidated_Host_Community = sum(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
            Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries))%>%
  rowwise()%>%
  # Get Consolidated Total by summing all the categories
  mutate(Consolidated_Total =  sum(Consolidated_RMindestination, Consolidated_RM_in_transit,
                                   Consolidated_Host_Community,Consolidated_RM_Pendulars,
                                   Consolidated_Colombian_Returnees, na.rm = TRUE))%>%
  ungroup()

rm(consallsectorsadm1)

# Join with sector detail table

ConsolidatedReportPerOrg <- rbind(consallsectors, consmaxsect)
  
rm(consallsectors, consmaxsect)

# Do Cumulative sum for Consolidated


FinalConsolidated <- ConsolidatedReportPerOrg%>%
  group_by(Country, Admin1, Appealing_org, Subsector)%>%
  arrange(Country, Admin1, Appealing_org, Month)%>%
  mutate('Consolidated Total' = cumsum(Consolidated_Total),
         'Consolidated In Destination' = cumsum(Consolidated_RMindestination),
         'Consolidated In Transit' = cumsum(Consolidated_RM_in_transit),
         'Consolidated Host Communities' = cumsum(Consolidated_Host_Community),
         'Consolidated Pendular' = cumsum(Consolidated_RM_Pendulars),
         'Consolidated Returnees' = cumsum(Consolidated_Colombian_Returnees),
         'Consolidated Girls' = cumsum(Consolidated_Girls),
         'Consolidated Boys' = cumsum(Consolidated_Boys),
         'Consolidated Women' = cumsum(Consolidated_Women),
         'Consolidated Men' = cumsum(Consolidated_Men),
         'Consolidated Other under 18' = cumsum(Consolidated_Other_under_18),
         'Consolidated Other above 18' = cumsum(Consolidated_Other_above_18),
         'Consolidated CVA Beneficiaries' = cumsum(Consolidated_CVA_Beneficiaries))%>%
  select(Country,
         Admin1,
         Appealing_org,
         Month,
         Subsector,
         `Consolidated Total`,
         `Consolidated In Destination`,
         `Consolidated In Transit`,
         `Consolidated Host Communities`,
         `Consolidated Pendular`,
         `Consolidated Returnees`,
         `Consolidated Girls`,
         `Consolidated Boys`,
         `Consolidated Women`,
         `Consolidated Men`,
         `Consolidated Other under 18`,
         `Consolidated Other above 18`,
         `Consolidated CVA Beneficiaries`)%>%
  full_join( finalmonthly, by = c("Country", "Admin1", "Appealing_org", "Month", "Subsector"))%>%
  select(Country,
         Admin1,
         Appealing_org,
         Month,
         Subsector,
         `Monthly Total Beneficiaries`,
         `Monthly CVA Beneficiaries`,
         `Consolidated Total`,
         `Consolidated In Destination`,
         `Consolidated In Transit`,
         `Consolidated Host Communities`,
         `Consolidated Pendular`,
         `Consolidated Returnees`,
         `Consolidated Girls`,
         `Consolidated Boys`,
         `Consolidated Women`,
         `Consolidated Men`,
         `Consolidated Other under 18`,
         `Consolidated Other above 18`,
         `Consolidated CVA Beneficiaries`)

# Create regional figures 

RegionalFigures <- FinalConsolidated%>%
  group_by(Appealing_org, 
           Month, 
           Subsector)%>%
  summarise(Country = "Regional",
            Admin1 = "Regional level",
            `Monthly Total Beneficiaries` = sum( `Monthly Total Beneficiaries`),
            `Monthly CVA Beneficiaries`= sum( `Monthly CVA Beneficiaries`),
            `Consolidated Total`= sum(`Consolidated Total` ),
            `Consolidated In Destination`= sum( `Consolidated In Destination`),
            `Consolidated In Transit`= sum( `Consolidated In Transit`),
            `Consolidated Host Communities`= sum(`Consolidated Host Communities` ),
            `Consolidated Pendular`= sum(`Consolidated Pendular` ),
            `Consolidated Returnees`= sum( `Consolidated Returnees` ),
            `Consolidated Girls`= sum( `Consolidated Girls` ),
            `Consolidated Boys`= sum(`Consolidated Boys` ),
            `Consolidated Women`= sum(`Consolidated Women` ),
            `Consolidated Men`= sum(`Consolidated Men` ),
            `Consolidated Other under 18`= sum( `Consolidated Other under 18`),
            `Consolidated Other above 18`= sum(`Consolidated Other above 18` ),
            `Consolidated CVA Beneficiaries`= sum(`Consolidated CVA Beneficiaries` ))

FinalConsolidatedReg <- rbind(FinalConsolidated, RegionalFigures)%>%
  # modify to arrange rows 
  rowwise()%>%
  mutate(Subsector= ifelse(Subsector == "Intersector", "ZIntersector", Subsector))%>%
  ungroup()%>%
  arrange(Country, Admin1, Appealing_org, Month, Subsector)%>%
  rowwise()%>%
  mutate(Subsector= ifelse(Subsector == "ZIntersector", "Intersector", Subsector))%>%
  ungroup()

# Print file as single table # 

#write_xlsx(FinalConsolidatedReg, "Consolidated_per_OrgSingleTable.xlsx")

# Print file split by countries

ConsolidatedPerCountryperOrg <- FinalConsolidatedReg %>%
  group_by(Country) %>% 
  group_split()

  ConsolidatedPerCountryperOrg  %>%
  purrr::map(~pull(., Country)) %>% # Pull out Species variable
  purrr::map(~as.character(.)) %>% # Convert factor to character
  purrr::map(~unique(.)) -> names(ConsolidatedPerCountryperOrg) # Set this as names for list members

write_xlsx(ConsolidatedPerCountryperOrg, "Consolidated_per_Org_and_Per_Country.xlsx")
  

  