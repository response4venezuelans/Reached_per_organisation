### Get Data to generate consolidated report per organisation ###
##### Author: Francis Fayolle, R4V IM Team ######

## Packages ##

library(activityinfo)
library(tidyverse)
library(writexl)

### Get 5W ###

activityinfo::activityInfoLogin("fayolle@unhcr.org", "126c199a4206a96f62a3d4f88e996c33")


df5W <- queryTable("cw1o8nbkx69lc9l3",
                   "Country" = "cezj1rqkxeqrsy57.c8u26b8kxeqpy0k4",
                   "Country Admin1" = "cezj1rqkxeqrsy57.c3ns3zikxeqq4h95",
                   "Admin2" = "c89klrbkx6hp4j58.cs2esadkx6hkt7j6",
                   "Appealing organisation Name" = "c5648gjkx69ra2v9.ckj5zamkumvyysv9",
                   "Implementation Set up" = "ckjtet4kx69smeog",
                   "Implementing partner Name" = "cflyi17kx69vdxei.ckj5zamkumvyysv9",
                   "Month" = "clqgqrqkyueahma8",
                   "Subsector" = "cq7t4s8kx6a7fyx3.cgdeh97ktn4sdek3s.cfvkmslkpy3tg94n",
                   "Indicator" = "cq7t4s8kx6a7fyx3.cwkj9p4kteeh4ls5",
                   "Activity Name" = "c3p669wkx6a7oyo4",
                   "Activity Description" = "c8hxf50kx6a7vp65",
                   "RMRP Activity" = "cuf3og8kx6amylmf",
                   "COVID 19 Situation" = "c3sg2p7kx6am3uk8",
                   "CVA" = "cbvqg4jkx6b1kii7",
                   "Value (in USD)" = "clwkfmckx6b2msu9",
                   "Delivery mechanism" = "cg3rikqkx6b3z1kf",
                   "Quantity of output" = "cm6no26kx6b8fqoh",
                   "Total monthly beneficiaries" = "cto1biukx6kwvnj4k",
                   "New beneficiaries of the month" = "c43j49ikx6kxyyc4l",
                   "Refugees and Migrants IN DESTINATION" = "cz3yof2kx6l024p4m",
                   "Refugees and Migrants IN TRANSIT" = "c8kl5o2kx6l0jip4n",
                   "Host Communities Beneficiaries" = "c5z8bvakx6l10d84o",
                   "Refugees and Migrants PENDULARS" = "c72dmskkx6l1hl04p",
                   "Colombian Returnees" = "cmoqhuckx6l4q9z4q",
                   "Women under 18" = "cwrxeaekx6l63na4s",
                   "Men under 18" = "ccx7xhekx6l6jnk4t",
                   "Women above 18" = "c3l36n2kx6l70kp4u",
                   "Men above 18" = "ctd27ackx6l7g814v",
                   "Other under 18" = "ckjcuiokx6l9a504w",
                   "Other above 18" = "cq4hs3skx6lggpj4x", truncate.strings = FALSE)

# format column names for easier data processing

colnames(df5W) <- c("Country",
                    "Admin1",
                    "Admin2",
                    "Appealing_org",
                    "Implementation",
                    "Implementing_partner",
                    "Month",
                    "Subsector",
                    "Indicator",
                    "Activity_Name",
                    "Activity_Description",
                    "RMRPActivity",
                    "COVID19",
                    "CVA",
                    "Value",
                    "Delivery_mechanism",
                    "Quantity_output",
                    "Total_monthly",
                    "New_beneficiaries",
                    "IN_DESTINATION",
                    "IN_TRANSIT",
                    "Host_Communities",
                    "PENDULARS",
                    "Returnees",
                    "Girls",
                    "Boys",
                    "Women",
                    "Men",
                    "Other_under",
                    "Other_above")

# Short data wrangling for integer values

df5W <- df5W %>%
  mutate_at(c("Value",
              "Quantity_output",
              "Total_monthly",
              "New_beneficiaries",
              "IN_DESTINATION",
              "IN_TRANSIT",
              "Host_Communities",
              "PENDULARS",
              "Returnees",
              "Girls",
              "Boys",
              "Women",
              "Men",
              "Other_under",
              "Other_above"), as.numeric)%>%
  arrange(Country, Month)

### Get indicator table #####

dfindicator  <<- queryTable("c49gyhmktedz4uj2",
                            "Code" = "cob8rivktedzp0f3",
                            "Subsector" = "cgdeh97ktn4sdek3s.cfvkmslkpy3tg94n",
                            "Indicator" = "cwkj9p4kteeh4ls5",
                            "Indicatortype" = "cprepl2ktk2l76a3", truncate.strings = FALSE)

### Merge and filter table to only keep the activities related to consolidated report ####

df5Wconsolidated <<- df5W %>%
  left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
  select(-Code)%>%
  filter(Indicatortype == "PiN" & RMRPActivity == "Yes")%>%
  mutate_if(is.numeric, replace_na, replace = 0)

countrylist <- sort(as.vector(unique(df5Wconsolidated$Country)))

orglist <- sort(as.vector(unique(df5Wconsolidated$Appealing_org)))


############### COnsolidated figures ########################################

df5Wconsolidated2 <- df5Wconsolidated%>%
 filter(Country %in% countrylist)%>%
  filter(Appealing_org %in% orglist)

monthlysectors <- df5Wconsolidated2 %>%
  group_by(Country, Appealing_org, Month, Subsector)%>%
  summarise('Monthly Total Beneficiaries' = sum(Total_monthly))

monthlytotal <- monthlysectors%>%
  group_by(Country, Appealing_org, Month)%>%
  summarise(Subsector = "Intersector", 
            'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`))

monthly<- rbind(monthlysectors, monthlytotal)

### Monthly CVA Beneficiaries #####

CVAmonthlysectors <- df5Wconsolidated2%>%
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

conssectors <-  df5Wconsolidated2%>%
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

consCVAsectors <- df5Wconsolidated2%>%
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

conssectorsadm1 <-  df5Wconsolidated2%>%
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

consCVAsectorsadm1 <- df5Wconsolidated2%>%
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



