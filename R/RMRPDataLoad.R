### Get Data to generate consolidated report per organisation ###
##### Author: Francis Fayolle, R4V IM Team ######

## Packages ##

library(activityinfo)
library(tidyverse)

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

#rm(df5W, dfindicator)
