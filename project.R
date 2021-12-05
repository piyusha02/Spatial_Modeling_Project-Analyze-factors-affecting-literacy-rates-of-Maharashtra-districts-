
library(rgdal)
library(rgeos)
library(tmap)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sp)
library(raster)
library(RColorBrewer)
display.brewer.all()
library(mapdata)
library(leaflet)

#reading maharashtra shapefile into variable MHdistrict.file
MHdistrict.file <-readOGR("C:\\Users\\HP\\Desktop\\Maharashtra State\\20070243015\\Dataset\\District.shp")
View(MHdistrict.file)
names(MHdistrict.file)
      #[1] "ID"         "DISTRICT"   "DIST_HQ"    "STATE"      "Z_POP_2001" "MALE_01"   
      #[7] "FEMALE_01" 

#checking CRS & extent of shapefile
crs(MHdistrict.file)
extent(MHdistrict.file)

#reading education dataset into EducationCsvFile
EducationCsvFile1 <- read.csv("C:\\Users\\HP\\Desktop\\Maharashtra State\\20070243015\\Dataset\\stateEducation.csv")
View(EducationCsvFile)
is.na(EducationCsvFile1)
EducationCsvFile<-na.omit(EducationCsvFile1)

#to view entire datset
EducationCsvFile

#merging csv datset and maharashtra shapefile by 'district' column
MH.Census1 <- merge(MHdistrict.file, EducationCsvFile, by.x="DISTRICT", by.y="DISTRICT")
MH.Census <- na.omit(MH.Census1)
MH.Census
names(MH.Census)
View(MH.Census)

#extract required columns from dataset
EducationCsvFile <- EducationCsvFile[,c(5,3,10,17)]

#to check whether output comes
plot(EducationCsvFile$DISTCD , EducationCsvFile$OVERALL_LI)

#map using (10th,17th column)
MH.points <- SpatialPointsDataFrame(EducationCsvFile[,3:4] ,
                                    EducationCsvFile, proj4string = CRS("+init=EPSG:7767"))
plot(MH.points)

#plotting maharashtra shapefile
tm_shape(MH.Census) + tm_borders(alpha=.4)

#district wise literacy rates
tm_shape(MH.Census) + tm_fill("OVERALL_LI" , palette = "Reds", title = "Literacy") + 
  tm_borders(lwd = 2) + tm_layout(title= "District wise literacy", title.position = c(.35,.10))

# Low literacy rate distrcits
selLow <- MH.Census$OVERALL_LI < 78.00
newdLowliteracy<- na.omit(selLow)
newdLowliteracy
View(newdLowliteracy)
plot(MH.Census, col="lightgrey", main="Top 10 low literate districts")
plot(MH.Census[ newdLowliteracy, ], col ="yellow", add =TRUE)
box(lty = '1373',col = 'black')
# High literacy rate districts
selHigh <-MH.Census$OVERALL_LI > 85.00
newHighLiteracy <- na.omit(selHigh)
newHighLiteracy
plot(MH.Census, col="lightgrey", main="Top 10 high literate districts")
plot(MH.Census[ newHighLiteracy, ], col = "red", add = TRUE)
box(lty = '1373',col = 'black')

#tm_shape(MHdistrict.file) + tm_borders(alpha = .7) + tm_shape(MH.points) +
  #tm_dots(col = "OVERALL_LI", palette = "Reds", style = "quantile", size = 4)

#MHcities <- readOGR("C:\\Users\\HP\\Desktop\\Maharashtra State\\cities.shp")
View(MHcities)

#after knowing literacies of top 10 low and high districts next i will show 3 factors affecting 
#low or high literacies :----
#1. i will show population of each district to understand comparitively
#2. i will show number of schools in each district
#3. i will show number of families which are below poverty line

#1. 
tm_shape(MH.Census) + tm_fill("TOTPOPULAT" , palette = "Greens", style="quantile" ,
                              title = "Population") + tm_borders(lwd = 3) +
  tm_layout(title= "District wise population", legend.text.size = 0.8,
            legend.title.size = 2, legend.position = c(.70,.05),frame = TRUE) +
  tm_style("cobalt")

#extracted districts and number of schools & families below poverty line in data frame
    #For Low 
populationBarPlot <- data.frame(District = c("Nandurbar","Gadchiroli","Bid","Jalna","Dhule",
                                             "Parbhani","Hingoli","Osmanabad","Nanded","Solapur"),
                                No_of_schools = c(32877, 32115, 55323, 34949, 32124, 28342, 19416,
                                                  26377, 54412,75410),
                                Rural_families_below_poverty_line = c(73.0,55.0,26.0,38.3,53.6,
                                                                      33.2,34.5,32.8,30.6,29.3))
    #For High
populationhighBarplot <- data.frame(District = c("Mumbai(Suburb)","Nagpur","G_Mumbai",
                                                 "Amravati","Akola","Wardha","Pune","Sindhudurg",
                                                 "Thane","Gondia"),
                                No_of_schools = c(32664, 58632, 16768, 42498, 25326, 23766, 105862,
                                                      29964, 55685, 25367),
                                Rural_families_below_poverty_line = c(55.0,47.4,20.0,49.6,48.1,41.1,19.5,
                                                                   39.1,45.0,57.9))
  #to view dataframes
populationBarPlot
populationhighBarplot

#2.
  #ploting barplots for low with respect to schools
ggplot(populationBarPlot, aes(x=District, y=No_of_schools)) +
  geom_bar(stat = "identity", fill ="steelblue") + theme_minimal()

  #ploting barplots for high with respect to schools
ggplot(populationhighBarplot, aes(x=District, y=No_of_schools)) +
  geom_bar(stat = "identity", fill="orange") + theme_minimal()

#3.
  #ploting barplots for low with respect to poverty line low
ggplot(populationBarPlot, aes(x=District, y=Rural_families_below_poverty_line)) +
  geom_bar(stat = "identity") + theme_minimal()

  #ploting barplots for low with respect to poverty line high
ggplot(populationhighBarplot,title="Poverty", aes(x=District, y=Rural_families_below_poverty_line)) +
  geom_bar(stat = "identity") + theme_minimal()

#leaflet interactive map to check in-depth cities and roads
leaflet() %>%
  addTiles() %>%
  addPolygons(data = MH.Census)
#Figure 14: The MH.census object loaded in rstudio via the leaflet package

