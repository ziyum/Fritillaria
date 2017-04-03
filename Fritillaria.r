library(sp)
library(raster)
library(maptools)
# Make a map of China with Taiwan
getData('ISO3') 
China = getData('GADM', country='CHN', level=1)
Taiwan = getData('GADM', country='TWN', level=1) # Taiwan is regarded as a country on GADM.org
Taiwan = unionSpatialPolygons(Taiwan, IDs = Taiwan$NAME_0) # Dissolve to one spatial polygon
TaiwanData = as.data.frame(China[1,]) # Use data of one Chinese province as model
TaiwanData$OBJECTID = nrow(China) + 1 # Change the data to create "Taiwan Province"
TaiwanData$NAME_1 = "Taiwan"
TaiwanData$VARNAME_1 = "Taiwan"
Taiwan = SpatialPolygonsDataFrame(Sr = Taiwan, match.ID = F, data = TaiwanData) # Attach data to spatial info
China = rbind(China, Taiwan) # China reunification!
ChinaPr = CRS("+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")
China = spTransform(China, ChinaPr) # Use a better projection, so that people are used to it.
plot(China)
#  Get names of Porvinces/ARs/Municipalities
Provinces = China$NAME_1
# Load the page on Fritillaria genus from Flora of China
# http://efloras.org/browse.aspx?flora_id=2&name_str=Firtiilaria&btnSearch=Search
FOC = readLines("http://efloras.org/florataxon.aspx?flora_id=2&taxon_id=113029")
Links = FOC[grep("<A HREF='florataxon.aspx",FOC)] 
Fritillaria = Links[grep("Fritillaria", Links)] # Lines containing links to Fritillaria species
# Use a costomized function to extract info in HTML page
From1stTo1st = function(Text = "AACGTCUCAT", Start = "C", End = "C")
{ # A function to extract string between 1st occassion of Start and first occassion of End
  nohead = paste(strsplit(Text, split = Start)[[1]][-1],collapse = Start)
  output = strsplit(nohead, split= End)[[1]][1]
  return(output)
}
# Prepare data for species, links and their webpages
Links = rep(NA, length(Fritillaria))
Species = rep(NA, length(Fritillaria))
Pages = list()
for(i in 1:length(Fritillaria))
{
  Links[i] = From1stTo1st(Fritillaria[i],  "<A HREF='", "'  ")
  Links[i] = paste("http://efloras.org/", Links[i], sep="")
  Pages[[i]] = readLines(Links[i])
  Species[i] = From1stTo1st(Fritillaria[i], "TARGET='_top' >", "</A>")
}
# Occurrence matrix of species by provinces:
Occur = matrix(NA, nrow = length(Provinces), ncol = length(Species))
rownames(Occur) = sapply(Provinces, function(x) strsplit(x, split = " ")[[1]][1])
rownames(Occur)[which(Provinces == "Nei Mongol")] = "Mong" # Search the page with "Mong" for Innor Mongolia
colnames(Occur) = Species
for(i in 1:length(Provinces))
  for(j in 1:length(Species))
    Occur[i,j] = any(grepl(rownames(Occur)[i], Pages[[j]]))
# Sum occurrence true cases to get number of Fritillaria spp. per province:
China$Num_Fritillaria = apply(Occur, 1, sum)
library(grDevices) # start plotting:
plot(China, col = colorRampPalette(c("white", "red"))(12)[China$Num_Fritillaria+1])
Centroids = getSpPPolygonsLabptSlots(China) # Get centroids for each province for labelling:
Centroids[grep("Gansu", Provinces),][1] = -181000.7 #-331143.7 # Move centroid of Gansu left a little
Centroids[grep("Hebei", Provinces),][2] = 4197450 # 4297450 # Move centroid of Hebei down, etc...
Centroids[grep("Shanghai", Provinces),][1] = 1626703 # 1536703
Centroids[grep("Shanghai", Provinces),][2] = 3500745 #  3420745
text(Centroids[,1],Centroids[,2],labels = China$Num_Fritillaria) # Labels with numbers in provinces
