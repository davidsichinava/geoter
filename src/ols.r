### Run and combine fixed effects models

fmodel0 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC + Urban
				+ votInv + dist_intern_road+dist_sec_road+altitude_median, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel1 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC + Urban
				+ votInv + dist_intern_road+dist_sec_road, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel2 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC + Urban
				+ votInv + dist_intern_road, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel3 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC + Urban
				+ votInv, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel4 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC + Urban, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel5 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + WHC, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel6 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel7 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel8 <- plm(UNMProp~TurnoutProp + Orthodox, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel9 <- plm(UNMProp~TurnoutProp, 
               index=c("Elections", "District"),
               data=geoelect, model="within")


#### Save regression tables
			   
stargazer(fmodel0, fmodel1, fmodel2, fmodel3, fmodel4, fmodel5, fmodel6,
          fmodel7, fmodel8, fmodel9, type="text",  
          dep.var.labels=c("Vote share for the United National Movement or its candidate"),
          covariate.labels=c("Total turnout", "Proportion of orthodox population", "Proportion of native Georgian speakers", "Proportion of population with higher education", "Proportion of white collar workers in the working-age population", "Proportion of urban population","Inverse of the district size", "Distance from international roads (meters)", "Distance from national roads (meters)", "Median altitude of the district above the sea level (meters)"),
          out="print/models.htm")