####### Read and process spatial data
####### Shapefiles for electoral precincts were created for the National Democratic Institute - Georgia
####### Shapefiles for the district and country boundaries were obtained from Georgian National Statistics Agency (Geostat) through freedom of information (FOI) request
####### Voting data was obtained from the Central Elections Commission (CEC) of Georgia through freedom of information (FOI) request
####### Demographic data was obtained from Georgian National Statistics Agency through freedom of information (FOI) request

### Define ggplot theme for maps and diagrams

theme_mplot <- theme(
  axis.text.y = element_text(colour="black", size = 18, family = "Times New Roman"),
  axis.text.x = element_text(colour="black", size = 18, family="Times New Roman"),
  axis.title.x = element_text(size=20, family = "Times New Roman"),
  axis.title.y = element_text(size=20, family = "Times New Roman"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_line(colour = "grey"),
  plot.title = element_text(colour = "Black", size=22, family = "Times New Roman")
  # legend.position = "none"
)



### Read shapefiles
shp2008<-readShapePoly("data/parl2008/parl_precinct_2008.shp")
shp2008@data<-subset(shp2008@data, select=c(OBJECTID, PrecID, District_c, Precinct_c, Precinct_i))

shp2008p<-readShapePoly("data/pres2008/pres2008.shp")

shp2012<-readShapePoly("data/parl2012/parliamentary_precinct_2012.shp")
shp2013<-readShapePoly("data/pres2013/pres_2013_precincts.shp")
shp2016<-readShapePoly("data/parl2016/parl2016.shp")
shpgeo<-readShapePoly("data/georgia_outline/outline.shp")

shp2008<-as(shp2008, 'SpatialPolygonsDataFrame')
shp2008p<-as(shp2008p, 'SpatialPolygonsDataFrame')

shp2012<-as(shp2012, 'SpatialPolygonsDataFrame')
shp2013<-as(shp2013, 'SpatialPolygonsDataFrame')
shp2016<-as(shp2016, 'SpatialPolygonsDataFrame')

### Read and fortify country outline boundaries

georgia<-as(shpgeo, 'SpatialPolygonsDataFrame')
fgeorgia <- fortify(georgia, region="OBJECTID_1")


### Read raw vote counts
res2008<-read.csv("data/parl2008/2_parliamentary_2008.csv")
res2008p<- read.csv("data/voting/pres08.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)

res2012<-read.csv("data/parl2012/4_parliamentary_2012.csv")
res2013 <- read.csv("data/voting/pres13.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
res2016<-read.csv("data/parl2016/7_parliamentary_2016.csv")

### Merge vote counts with boundary data, as we will be working on the merged data

shp2016@data$PrecID<-shp2016@data$MID*100000+shp2016@data$District*1000+shp2016@data$Precinct

m2008 <- merge(shp2008, res2008, by.x='PrecID', by.y="Prec_ID")
m2008p <- merge(shp2008p, res2008p, by.x='PrecID', by.y="Precinct_ID")
m2012 <- merge(shp2012, res2012, by.x='PrecID', by.y="Prec_ID")
m2013 <- merge(shp2013, res2013, by.x='PrecID', by.y="Prec_ID")
m2016 <- merge(shp2016, res2016, by.x='PrecID', by.y="precinct_id")

m2008@data$year<-2008
m2008p@data$year<-2008

m2012@data$year<-2012
m2013@data$year<-2013
m2016@data$year<-2016

#### Read and process demographic data from raw datasets provided by the CEC and Geostat

### Load district codes

areacode <- read.csv("data/voting/areacode.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)


### Load and calculate the proportion of higher education holders in the district

edu <- read.csv("data/voting/edu.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
edu <- merge(edu, areacode, by="Area")

edu <- edu %>% group_by(ID) %>%
      summarise(PhD=sum(PhD, na.rm = TRUE), MA=sum(MA, na.rm = TRUE), BA=sum(BA, na.rm = TRUE), Voc1=sum(Voc1, na.rm = TRUE),
                Voc2=sum(Voc2, na.rm = TRUE), Voc3=sum(Voc3, na.rm = TRUE), HiS1=sum(HiS1, na.rm = TRUE), HiS2=sum(HiS2, na.rm = TRUE),
                Middle=sum(Middle, na.rm = TRUE), Liter=sum(Liter, na.rm = TRUE), Illit=sum(Illit, na.rm = TRUE), NI=sum(NI, na.rm = TRUE))

edu$HiEd <- (edu$PhD+edu$MA+edu$BA)/(edu$PhD+edu$MA+edu$BA+edu$NI+edu$Illit+edu$Liter+
                                                           edu$Middle+edu$HiS2+edu$HiS1+
                                                           edu$Voc3+edu$Voc2+edu$Voc1)

### Employment type

empl <- read.csv("data/voting/empl.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = FALSE)
empl <- merge(empl, areacode, by="Area")
empl <- empl %>% group_by(ID) %>%
  summarise(WHC=sum(WHC1+WHC2+WHC3, na.rm = TRUE), BLC=sum(BLC1+BLC2+BLC3+BLC4+BLC5+BLC6, na.rm = TRUE), PopTot=sum(PopTot, na.rm = TRUE))
empl$WHC <- empl$WHC/empl$PopTot

### Mother tongue
mot <- read.csv("data/voting/mother.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = FALSE)
mot <- merge(mot, areacode, by="Area")
mot <- mot %>% group_by(ID) %>%
  summarise(Georgian=sum(Georgian, na.rm = TRUE), PopTot=sum(PopTot, na.rm = TRUE))
mot$Georgian <- mot$Georgian/mot$PopTot

### Religion
relig <- read.csv("data/voting/relig.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = FALSE)
relig <- merge(relig, areacode, by="Area")
relig <- relig %>% group_by(ID) %>%
  summarise(Orthodox=sum(X.U.10DB..U.10D0..U.10E0..U.10D7..U.10DA..U.10DB..U.10D0..U.10D3..U.10D8..U.10D3..U.10D4..U.10D1..U.10D4..U.10DA..U.10D8., na.rm = TRUE), PopTot=sum(Total, na.rm = TRUE))
relig$Orthodox <- relig$Orthodox/relig$PopTot

### Urbanization
urban <- read.csv("data/voting/urban.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = FALSE)
urban <- urban %>% group_by(ID) %>%
  summarise(Urban=sum(urban_all, na.rm = TRUE), PopTot=sum(total, na.rm = TRUE))
urban$Urban <- urban$Urban/urban$PopTot

### geography

dem <- read.csv("data/voting/demogeo.csv")
dem <- subset(dem, select=c("District", "dist_intern_road", "dist_sec_road"))
dem1 <- read.csv("data/voting/alt.csv")
dem1 <- subset(dem1, select=c("District", "altitude_median"))
dem <- merge(dem, dem1, by="District")

### Voting

pres08 <- read.csv("data/voting/pres08.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
parl08p <- read.csv("data/voting/parl08.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
parl12 <- read.csv("data/voting/parl12.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
pres13 <- read.csv("data/voting/pres13.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)
parl16 <- read.csv("data/voting/parl16.csv", encoding = "UTF-8", sep="\t", stringsAsFactors = TRUE, na.strings = FALSE)


pres08 <- subset(pres08, select=c("District", "Total_Voters", "Turnout", "Mikheil_Saakashvili"))
pres08<-pres08 %>% group_by(District) %>% 
  summarise(Total_Voters=sum(Total_Voters), Turnout=sum(Turnout), UNM = sum(Mikheil_Saakashvili))

parl08p <- subset(parl08p, select=c("District", "Total_voters", "Turnout", "UNM"))
parl08p<-parl08p %>% group_by(District) %>% 
  summarise(Total_Voters=sum(Total_voters), Turnout=sum(Turnout), UNM = sum(UNM))

parl12 <- subset(parl12, select=c("District", "Total_Voters", "Turnout", "UNM"))
parl12<-parl12 %>% group_by(District) %>% 
  summarise(Total_Voters=sum(Total_Voters), Turnout=sum(Turnout), UNM = sum(UNM))

pres13 <- subset(pres13, select=c("District", "Total_voters", "Turnout", "Bakradze"))
pres13<-pres13 %>% group_by(District) %>% 
  summarise(Total_Voters=sum(Total_voters), Turnout=sum(Turnout), UNM = sum(Bakradze))

parl16 <- subset(parl16, select=c("distr", "tot_vot", "num_votes", "X5...United.National.Movement"))
parl16<-parl16 %>% group_by(distr) %>% 
summarise(Total_Voters=sum(tot_vot), Turnout=sum(num_votes), UNM = sum(X5...United.National.Movement))
names(parl16) <- c("District", "Total_Voters", "Turnout", "UNM")

pres08$Elections <- 1
parl08p$Elections <- 2
parl12$Elections <- 3
pres13$Elections <- 4
parl16$Elections <- 5

geoelect <- rbind(pres08, parl08p, parl12, pres13, parl16)
geoelect$Elections <- factor(geoelect$Elections, levels=c(1, 2, 3, 4, 5), labels = c("Presidential 08",
                                                                                     "Parliamentary 08",
                                                                                     "Parliamentary 12",
                                                                                     "Presidential 13",
                                                                                     "Parliamentary 16"))
geoelect <- merge(geoelect, edu, by.x="District", by.y="ID")
geoelect <- merge(geoelect, empl, by.x="District", by.y="ID")
geoelect <- merge(geoelect, relig, by.x="District", by.y="ID")
geoelect <- merge(geoelect, mot, by.x="District", by.y="ID")
geoelect <- merge(geoelect, urban, by.x="District", by.y="ID")
geoelect <- merge(geoelect, dem, by="District")


geoelect$votInv<-1/geoelect$Total_Voters
geoelect$TurnoutProp<-geoelect$Turnout/geoelect$Total_Voters
geoelect$UNMProp<-geoelect$UNM/geoelect$Turnout