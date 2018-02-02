### Run and combine fixed effects models
### geoelect$altitude_median <- geoelect$altitude_median/1000
### geoelect$dist_sec_road <- geoelect$dist_sec_road/1000
### geoelect$dist_intern_road <- geoelect$dist_intern_road/1000


fmodel1 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + Urban
				+ votInv, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel2 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd + Urban, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel3 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian + HiEd, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel4 <- plm(UNMProp~TurnoutProp + Orthodox + Georgian, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel5 <- plm(UNMProp~TurnoutProp + Orthodox, 
               index=c("Elections", "District"),
               data=geoelect, model="within")
fmodel6 <- plm(UNMProp~TurnoutProp, 
               index=c("Elections", "District"),
               data=geoelect, model="within")

			   

#### Save regression tables
			   
stargazer(fmodel1, fmodel2, fmodel3, fmodel4, fmodel5, fmodel6, type="text",  
          dep.var.labels=c("Vote share for the United National Movement or its candidate"),
          covariate.labels=c("Total turnout", "Proportion of orthodox population", "Proportion of native Georgian speakers", "Proportion of population with higher education", "Proportion of urban population","Inverse of the district size"),
          out="print/models.htm")
