## This code returns a set of basic analysis around the beach litter issue
## with the objective to subsidy management plans and efforts
# Code developed by Jessica Leiria Schattschneider in linux-gnu OS using R-3.5.3 

## This is a reproducible example, you just need to press run and see it working! 
## BUT,if you are running this code with your own data you MUST especify your own directory and transform
## the QField result (a shapefile) to a data.frame FIRST! To do this, read, modify to your case run 
## the above steps and jump the Section 01 ("OPEN THE DATA"):

## Good practice: Clean the workspace before anything else:
#rm(list=ls())

##### Section 00: STEPS FOR NEW LITTER DATA#####

## STEP ZERO - Load required packages for this section:
# library(rgdal)  ##packageversion: 1.4.3

## FIRST STEP: 
# setwd("/home/YOUR-OWN-DIRECTORY") ## Setting the path of the paste where your data is
#                                       ## eg.: setwd("/home/jessica/Documentos/litter")
#                                         ## P.S: if you don't change this part until the end of the code the plot will be saved in this path 
## SECOND STEP:
# install.packages("rgdal",dependencies = TRUE)
# library("rgdal") ##Install and load "rgdal" a spatial library

## THIRD STEP:
# litter_shp <- readOGR(".", "THE-NAME-OF-YOUR-QFIELD-LITTER-LAYER") ## Inform the name of your QField litter layer
                                                                        ## eg.: litter_shp02 <- readOGR(".", "litter")

## FOURTH STEP:
# litter_new <- data.frame(litter_shp) ##Transform the shapefile to a data.frame:
# litter_new$dd_mm_yyyy<- as.Date.character(litter_new$dd_mm_yyyy, format = "%Y/%m/%d") ##set the column containing the date ($dd_mm_yyyy) in date format:

##FIFTH STEP:

# litter_all<-read.csv("PREVIOUS-DATA-PATH.csv", header = TRUE) ##Open the previous .csv file cointaining all data
                                                                ## eg.:read.csv("/home/jessica/Documentos/litter/litter_all.csv", header = TRUE)
# litter_all<-litter_all[,-1] #remove the column with the row-ID
# litter_all$dd_mm_yyyy<- as.Date.character(litter_all$dd_mm_yyyy, format = "%Y-%m-%d") ## Set the column containing the date ($dd_mm_yyyy) in date format:

#### Section 01: Open the data####
## Set a work directory:
#setwd("/home/jessica/Documentos/litter")

##Open the previous .csv file cointaining all data
#### 
litter_all<-read.csv("https://raw.githubusercontent.com/JesSchattschneider/litter/master/litter_all.csv", header = TRUE)
litter_all<-litter_all[,-1] #remove the column with the row-ID
## Set the column containing the date ($dd_mm_yyyy) in date format:
litter_all$dd_mm_yyyy<- as.Date.character(litter_all$dd_mm_yyyy, format = "%Y-%m-%d")

##Open the last shapefile from QField project:
litter_new<-read.csv("https://raw.githubusercontent.com/JesSchattschneider/litter/master/survey03.csv", header = TRUE)
litter_new<-litter_new[,-1] #remove the column with the row-ID
## Set the column containing the date ($dd_mm_yyyy) in date format:
litter_new$dd_mm_yyyy<- as.Date.character(litter_new$dd_mm_yyyy, format = "%Y-%m-%d")

  #### Section 02: Creating auxiliar tables####

## Naming the beaches
  ### In this case the beach "01" refers to Shipwreck, "02" to Wind Park, ... 
beach<-as.data.frame(cbind(c(1,2,3) ,c("Shipwreck","Wind Park","Cassino")))
colnames(beach) <- c("id", "beach_name")
## Set the column containing the beach ID ($id) as numeric format:
beach$id<-as.numeric(as.character(beach$id))

## Naming the transects
  ### In this case the transect "1" and "3" refers to samples associated to the Upper Zone and "2" 
  ### to the Lower Zone
transect<-as.data.frame(cbind(c("1","2","3"), c("Upper Zone","Lower Zone","Upper Zone")))
colnames(transect) <- c("id", "transects")
## Set the column containing the transect ID ($id) as numeric format:
transect$id<-as.numeric(as.character(transect$id))

## Although in QField project the user has access to the litter name list this attribute is saved 
## using the respective ID from the Ocean Conservancy list. 
## The above lines create a 47 X 3 matrix relating the litter name list with its 
## associated Ocean Conservancy ID as with the activity source:

litter_class<-as.data.frame(cbind(c(as.factor(01:47)),c("paper-bag","plastic-bag","baloons","beverage-PET","beverage-glass","beverage-cans-metal",
                      "caps-lids","clothing-shoes","cups-plates-forks-knives-spoons","food-packing","pull-tabs",
                      "six-pack-holders","shotgun-shells","straw-stirrers","toys","bait-countainers","bleach-cleaner-bottles",
                      "buoys-floats","crab-lobster-fish-traps","crates","fishing-line","fishing-lures-lightstick",
                      "fishing-net","oil-bottles","pallets","plastic-sheeting-tarps","ropes","strapping-bands","cigarettes-cigarettes-filters",
                      "cigarette-lighters","cigar-tips-plastic","tobacco-packing",'appliances',"batteries","building-materials",
                      "cars-car-parts","galoon-drums","tires","condoms","diapers","syringes-needles","tampons-tampon-applicators",
                      "shoreline-and-recreational-activities","ocean-waterway-activities","smoking-related-activities",
                      "dumping-activities","medical-personal-hygiene"),
                    c("shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","shoreline and recreational",
                      "shoreline and recreational","ocean waterway",
                      "ocean waterway","ocean waterway","ocean waterway",
                      "ocean waterway","ocean waterway","ocean waterway",
                      "ocean waterway","ocean waterway","ocean waterway",
                      "ocean waterway","ocean waterway","ocean waterway",
                      "smoking related","smoking related","smoking related",
                      "smoking related","dumping","dumping","dumping",
                      "dumping","dumping","dumping","medical personal hygiene",
                      "medical personal hygiene","medical personal hygiene","medical personal hygiene",
                      "shoreline and recreational","ocean waterway","smoking related",
                      "dumping","medical personal hygiene")))

colnames(litter_class) <- c("id","item","activity")

## Set the column containing the litter ID ($id) as numeric format:
litter_class$id<-as.numeric(as.character(litter_class$id))

    #### Section 03: Merging tables and new samples to previous data####

## Adjusting data format to some columns to merge correctly:
## Set the column containing the number of items ($n_items) as numeric format:
litter_new$n_items<-as.numeric(as.character(litter_new$n_items))

## Set the column containing the beach ID ($beach) as numeric format:
litter_new$beach<-as.numeric(as.character(litter_new$beach))

## Set the column containing the transect ID ($transect) as numeric format:
litter_new$transect<-as.numeric(as.character(litter_new$transect))

## Set the column containing the Ocean Conservancy ID ($oceancons) as numeric format: 
litter_new$oceancons<-as.numeric(as.character(litter_new$oceancons))

## Merge to the new field work data.frame the litter polluting activity and the item name:
litter_new<-merge(litter_new, litter_class, by.x = "oceancons", by.y = "id", all=FALSE)

## Cleaning undesirable columns imported with "merge steps":
litter_new <- litter_new[,-(01)]  

### Adding the new data to the previous:
ldf<-merge(litter_all,litter_new, all = TRUE)

#########Remove this line, just correcting a particular input error
ldf$n_items[ldf$n_items==26]<-2

## Adding the beach name (defined into the "beach" matrix):
## Set the column containing the beach number ($beach) as numeric format:
ldf$beach<-as.numeric(ldf$beach)
#write.csv(ldf,"./litter_all.csv") ## uncomment to update the litter list with all surveys
                                    ## by this way, the next time you need to import a new
                                    ## monitoring dataset all the previous data will be already
                                    ## joinned and saved in this .csv

ldf<-merge(ldf, beach, by.x = "beach", by.y = "id", all=TRUE)
## Adding the transect name (defined into the "transect" matrix):
ldf<-merge(ldf, transect, by.x = "transect", by.y = "id", all=TRUE)
##Cleaning undesirable columns imported with "merge steps":  
ldf <- ldf[,-c(02,14)]
##Rename the column transect:
names(ldf)[16]<-"Zones"

## Cleaning the R Environment:
rm(list = "beach", "litter_all", "litter_class", "transect","litter_new")

      #### Section 04: Graphs maps and tables####
# ## STEP ZERO - Load required packages for this section:
library(plyr)             ##packageversion: 1.8.4
library(ggplot2)          ##packageversion: 3.1.0
library(gridExtra)        ##packageversion: 2.3
library(treemapify)       ##packageversion: 2.5.3
library(sf)               ##packageversion: 0.7.2
## install.packages("mapview", dependencies = TRUE) ## run this row if mapview is not installed
library(mapview)          ##packageversion: 2.6.3
library(sp)               ##packageversion: 1.3.3

## Section 4.1: GRAPH 01 - ### Analysing the representativeness of different polluting activities along the sampled beaches ####  
####
## Summarize the percentage of items corresponding to each polluting activity in each sampled beach: 
hist1 <- ddply(ldf, c("activity", "beach_name"), summarise, tot=sum(n_items))
hist1<-ddply(hist1, "beach_name", transform, percent_weight = tot / sum(tot) * 100)

##Define the beach name order to be displayed in the histogram:
hist1$beach_name <- factor(hist1$beach_name,levels = c("Shipwreck", "Wind Park", "Cassino"))

## Plot "hist1":
a = 
  ggplot(hist1, aes(x=beach_name, y=percent_weight, fill=activity)) + geom_bar(stat="identity", colour="black") + 
  guides(fill=guide_legend(reverse=TRUE)) + guides(fill=guide_legend(title = "")) +
  scale_fill_brewer(palette="Pastel1")+
  theme(legend.position="bottom", legend.title=element_text(size=23), 
        legend.text=element_text(size=20))+
  xlab('')+ylab("Percentage of Items (%)")+
  theme(axis.text.x = element_text(size = 23),
          axis.title.y = element_text(size = 23),
          axis.text.y = element_text(size = 23))
        
a
  
#To save the result as image uncomment the next row and define 
#a directory inside your PC where you would like to save your plots:
##ggsave(a, filename = "/home/jessica/Documentos/litter/graph01_histogram.png",width = 45, height = 30, units = "cm")
  
## Section 4.2: GRAPH 02 ## Analysing beaches temporal trend in terms of quantity and percentage of items grouped by corresponding polluting activity class####
##
## Summarize the percentage of items corresponding to each polluting activity in each sampled beach per field trip: 
  line2<- ddply(ldf, c("beach_name", "activity","dd_mm_yyyy"), summarise, tot=sum(n_items))
  line2<-ddply(line2, c("dd_mm_yyyy", "beach_name"), transform, percent_weight = tot / sum(tot) * 100)
  
  ##Define the beach name order to be displayed in the histogram:
  line2$beach_name <- factor(line2$beach_name,levels = c("Shipwreck", "Wind Park", "Cassino"))
  
  ### Plot the percentage 
  b<-ggplot(line2, aes(x=dd_mm_yyyy, y=percent_weight, colour=activity)) + geom_line(linetype="dotted", size = 0.8) +
    scale_color_brewer(palette="Set1")+
    geom_point(size=2)+
    facet_wrap(line2$beach)+
    theme(strip.text = element_text(size=16))+
    labs(x="",
        y="Percentage (%)") + guides(colour="none") + theme()+
    theme(axis.title.y = element_text(size = 23),
          axis.text.y = element_text(size = 23))
  #### Plot the total items 
  c<-ggplot(line2, aes(x=dd_mm_yyyy, y=tot, colour=activity)) + geom_line(linetype="dotted", size = 0.8) +
    geom_point(size=2)+
    facet_wrap(line2$beach)+
    theme(strip.text = element_text(size=16))+
    labs(x="",
         y="N Items")+
    theme(axis.title.y = element_text(size = 23),
          axis.text.y = element_text(size = 23))+
    scale_color_brewer(palette="Set1", guide = guide_legend(),name  ="") +
    theme(legend.position="bottom", legend.title=element_text(size=20), 
          legend.text=element_text(size=18))+
    theme(legend.position="bottom")

  ## Arrange both graphs (percentage and total items) in the same plot
  d = grid.arrange(b, c, nrow = 2)
  d
  
  #Save:
 # ggsave(d, filename = "/home/jessica/Documentos/litter/graph02_perc_n_lines.png", 
  #       width = 35, height = 30, units = "cm",dpi=300)
  
## Section 4.3: GRAPH 03 and 04 - ## Analysing difference in litter patterns along different beach sectors (in this case, Lower Zone and Upper Zone)####
  ####
  
  ##Summarizing the data for Graph 03 - Analyzing the difference of litter amount between sectors (Upper Zone and Lower Zone):
  pt3 <- ddply(ldf, c("transect","Zones","dd_mm_yyyy","beach_name"), summarise, tot=sum(n_items))
  ##Calculate the mean and standard deviation of items collected in all field trips by beach and sector ($transect) 
  pt3 <- ddply(pt3, c("Zones","beach_name"), summarise, mean=mean(tot),sd=sd(tot))
  
  ##Define the beach name order to be displayed in the histogram:
  pt3$beach_name <- factor(pt3$beach_name,levels = c("Shipwreck", "Wind Park", "Cassino"))
  
  ##Plot vertical lines
  j<-ggplot(pt3,aes(Zones,mean,ymin=mean-sd,ymax=mean+sd))+
    facet_wrap(pt3$beach_name)+
    theme(strip.text = element_text(size=16))+
    labs(x="",
         y="Number of Items (n)")+
    theme(axis.title.y = element_text(size = 23),
          axis.text.y = element_text(size = 23),
          axis.text.x = element_text(size = 23))
  j<-j + geom_pointrange()
  j
  #Save:
  #ggsave(j, filename = "/home/jessica/Documentos/litter/graph03_sector_vlines.png",width = 30, height = 20, units = "cm")
  
  ##Summarizing the data for Graph 04 - Analyzing the difference between the amount of litter items collected 
  ##(associated to their  polluting activities) around the Lower Zone and the Upper Zone
  
  library(treemapify)
  map4 <- ddply(ldf, c("transect","activity"), summarise, tot=sum(n_items))
  e<-ggplot(map4, aes(area = tot, fill = activity, label=activity, subgroup=transect)) +
    geom_treemap()+
    scale_fill_brewer(palette="Pastel1")+
    guides(fill=guide_legend(title = ""))+
    theme(legend.position="bottom",legend.text=element_text(size=18))+
    geom_treemap_subgroup_border(colour = "white") +
    geom_treemap_subgroup_text(colour = "#333333", place = "bottomleft",
                               grow = FALSE) 
  e
  #Save:
  #ggsave(e, filename = "/home/jessica/Documentos/litter/graph04_maptree.png",width = 35, height = 20, units = "cm")
  
  rm(list = "a","b","c","d")
## Section 4.4: MAP ## Litter spatial representation####
  
  ##Extracting from the litter dataframe only the columns where long, lat are recorded:
  xy <- ldf[,c(10,11)]
  ##Create a new spatial frame informing: 
  ##"coords" = the dataframe with the coordinates extraction (created in the last step)
  ##"data" = litter dataframe
  ##"proj4string" = the Coordinate Reference System code (in this case, the QGIS project was designed in WGS84, so the
  ##corresponding code is 4326)
  spdf <- SpatialPointsDataFrame(coords = xy, data = ldf,
                                 proj4string = CRS("+init=epsg:4326"))
  
  ##plot points in a map and classify by poluting activity
  m <-  mapview(spdf, popup = popupTable(spdf,
                                         zcol = c("beach_name",
                                                  "item",
                                                  "activity",
                                                  "n_items")), zcol = "activity",cex = "n_items")
  m
  #save as Widget
  #mapshot(m, url = "m.html",file = paste0(getwd(), "/map.png")) ## saving as html and .png
  

  