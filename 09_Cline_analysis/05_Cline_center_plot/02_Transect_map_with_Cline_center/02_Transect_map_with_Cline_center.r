################################
######## c. Transect map with Cline center ##########
######## Cline center information comes from the last step ########
######## To convert Cline center information into coordinates information based on transect coordinates and Cline center distance. 
######## Distance, initial bearing between transects and destination points of low and high cline centers 
######## were calculated in https://www.movable-type.co.uk/scripts/latlong.html, or to apply package of this website mentioned about.

library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)

cap <- function(x, max) ifelse(x <= max, x, max)

file_prefix <- "03_Research_citizen_combine_17094.4DegreeBlocks_rmEnvValue.frequency_processed_rmNA39_groundcolour"

records_4deg <- read.csv("03_Research_citizen_combine_17094.4DegreeBlocks_rmEnvValue.frequency_processed_rmNA39_groundcolour.csv", header = T, stringsAsFactors = F)



# regions to include on map
regions = c("Afghanistan"  , "Angola"  , "Albania"  , "Finland"  , "Andorra"  , "United Arab Emirates"  , "Armenia"  , "American Samoa"  , "Australia"  , "French Southern and Antarctic Lands" , "Austria"  , "Azerbaijan"  , "Burundi"  , "Belgium"  , "Benin"  , "Burkina Faso"  , "Bangladesh"  , "Bulgaria"  , "Bahrain"  , "Bosnia and Herzegovina"  , "Belarus"  , "Brunei"  , "Bhutan"  , "Botswana"  , "Central African Republic"  , "Switzerland"  , "China"  , "Ivory Coast"  , "Cameroon"  , "Democratic Republic of the Congo"  , "Republic of Congo"  , "Cook Islands"  , "Comoros"  , "Cape Verde"  , "Cyprus"  , "Czech Republic"  , "Germany"  , "Djibouti"  , "Denmark"  , "Algeria"  , "Egypt"  , "Eritrea"  , "Canary Islands"  , "Spain"  , "Estonia"  , "Ethiopia"  , "Fiji"  , "Reunion"  , "Mayotte"  , "France"  , "Faroe Islands"  , "Micronesia"  , "Gabon"  , "UK"  , "Georgia"  , "Guernsey"  , "Ghana"  , "Guinea"  , "Gambia"  , "Guinea-Bissau"  , "Equatorial Guinea"  , "Greece"  , "Heard Island"  , "Croatia"  , "Hungary"  , "Indonesia"  , "Isle of Man"  , "India"  , "Cocos Islands"  , "Christmas Island"  , "Chagos Archipelago"  , "Ireland"  , "Iran"  , "Iraq"  , "Iceland"  , "Israel"  , "Italy"  , "San Marino"  , "Jersey"  , "Jordan"  , "Japan"  , "Siachen Glacier"  , "Kazakhstan"  , "Kenya"  , "Kyrgyzstan"  , "Cambodia"  , "Kiribati"  , "South Korea"  , "Kosovo"  , "Kuwait"  , "Laos"  , "Lebanon"  , "Liberia"  , "Libya"  , "Liechtenstein"  , "Sri Lanka"  , "Lesotho"  , "Lithuania"  , "Luxembourg"  , "Latvia"  , "Morocco"  , "Monaco"  , "Moldova"  , "Madagascar"  , "Maldives"  , "Marshall Islands"  , "Macedonia"  , "Mali"  , "Malta"  , "Myanmar"  , "Montenegro"  , "Mongolia"  , "Northern Mariana Islands"  , "Mozambique"  , "Mauritania"  , "Mauritius"  , "Malawi"  , "Malaysia"  , "Namibia"  , "New Caledonia"  , "Niger"  , "Norfolk Island"  , "Nigeria"  , "Niue"  , "Bonaire"  , "Netherlands"  , "Norway"  , "Nepal"  , "Nauru"  , "New Zealand"  , "Oman"  , "Pakistan"  , "Panama"  , "Pitcairn Islands"  , "Philippines"  , "Palau"  , "Papua New Guinea"  , "Poland"  , "North Korea"  , "Madeira Islands"  , "Azores"  , "Portugal"  , "Palestine"  , "French Polynesia"  , "Qatar"  , "Romania"  , "Russia"  , "Rwanda"  , "Western Sahara"  , "Saudi Arabia"  , "Sudan"  , "South Sudan"  , "Senegal"  , "Singapore"  , "South Sandwich Islands"  , "South Georgia"  , "Saint Helena"  , "Ascension Island"  , "Solomon Islands"  , "Sierra Leone"  , "El Salvador"  , "Somalia"  , "Serbia"  , "Slovakia"  , "Slovenia"  , "Sweden"  , "Swaziland"  , "Seychelles"  , "Syria"  , "Turks and Caicos Islands"  , "Chad"  , "Togo"  , "Thailand"  , "Tajikistan"  , "Turkmenistan"  , "Timor-Leste"  , "Tonga"  , "Trinidad"  , "Tobago"  , "Tunisia"  , "Turkey"  , "Taiwan"  , "Tanzania"  , "Uganda"  , "Ukraine"  , "Uzbekistan"  , "Vatican"  , "Vietnam"  , "Vanuatu"  , "Wallis and Futuna"  , "Samoa"  , "Yemen"  , "South Africa"  , "Zambia"  , "Zimbabwe")

map <- map_data("world", regions=regions)

#ggplot map function
plot_map <- ggplot(map, aes(x = long, y = lat, group=group)) +
    geom_polygon(fill="gray95", colour = "gray60") +
    labs(x = "", y = "") +
    theme_bw()

#define butterfly range or subsets of the range
whole_range <- coord_map("mercator", xlim = c(-30,160), ylim = c(-50,50))

trait <- records_4deg$hindwingWhite_a
n <- cap(records_4deg$hindwingWhite_n, 12)
trait_name <- "Hindwing White freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)


trait <- records_4deg$forewingBand_c
n <- cap(records_4deg$forewingBand_n, 12)
trait_name <- "Forewing Tip freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)


trait <- records_4deg$groundColour_b
n <- cap(records_4deg$groundColour_n, 12)
trait_name <- "Orange Ground Colour freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)


###############################  transects  ############################

library(geosphere)

#define transects
transects <- as.data.frame(rbind(c(lon_start=-14,   lat_start=16,  lon_end=38,  lat_end=-8), #Central Africa
                                 c(lon_start=26,  lat_start=-32, lon_end=42,  lat_end=8),  #South East Africa
                                 c(lon_start=10,  lat_start=12,   lon_end=18,  lat_end=-28),#South West Africa
                                 c(lon_start=-18, lat_start=12,  lon_end=2, lat_end=44), #North West Africa
                                 c(lon_start=102  , lat_start=0, lon_end=122, lat_end=-12)))#South East Asia


# For each transect, we select which blocks are close enough to the transect.
# To do this, we calculate the perpendicular distance of each block to the transect.
# This is called the cross-track distance or distance to grand circle

max_dist = 450


records_4deg_transects <- list()


for (i in 1:nrow(transects)){
    dist_from_transect <- dist2gc(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                  p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                  p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000

    # and also calculate the position along the transect that the perpendicular line intersects
    # This is the along-track distance
    
    transect_pos <- alongTrackDistance(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                       p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                       p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000
    
    # Identify blocks close to the transect and not beyond the start or end
    transect_blocks <- which(dist_from_transect <= max_dist &
                             records_4deg$block_midpoint_long >= min(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_long <= max(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_lat >= min(c(transects$lat_start[i], c(transects$lat_end[i]))) &
                             records_4deg$block_midpoint_lat <= max(c(transects$lat_start[i], c(transects$lat_end[i]))))
    
    records_4deg_transects[[i]] <- records_4deg[transect_blocks,]
    
    #add position along transect
    
    records_4deg_transects[[i]]$transect_pos <- transect_pos[transect_blocks]
    
    }


#export cline data for analysis in another script.
# I was unable to successfully install hzar in the same conda environment as the R mapping libraries
# It appears that this is because it needed Rcpp 1.0.3 and only 1.0.1 was available on conda

for (i in 1:nrow(transects)){
    write.csv(records_4deg_transects[[i]], file=paste0(file_prefix, ".processed.4DegreeBlocks.transect", i, ".csv"),
              row.names=FALSE, quote=FALSE)
    }


########################  plotting transects  ############################


# First define map regions for the different transects

Central_Africa <- coord_map("mercator", xlim = c(-20,60), ylim = c(-20,30))

Southern_Africa <- coord_map("mercator", xlim = c(-5,42), ylim = c(-35,20))

Northwest_Africa <- coord_map("mercator", xlim = c(-20,10), ylim = c(5,45))

Southeast_Asia <- coord_map("mercator", xlim = c(90,130), ylim = c(-15,25))


transect_col = "blue"

# Transect 1 is central Africa and involves hindwing and forewing

i=1

trait <- records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + Central_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=19.404444444444444, xend=21.806944444444444,
             y=1.0225, yend=-0.164166666666666, colour = "Blue", size=10, alpha=0.8) ## add coordinate of both ends of cline center interval



trait <- records_4deg_transects[[i]]$forewingBand_c
n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
trait_name <- "Forewing tip freq."

plot_map + Central_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=31.56972222222222, xend=33.11527777777778,
             y=-4.95, yend=-5.693333333333333, colour = "Blue", size=10, alpha=0.8)





# Transect 2 is Southeast Africa and involves forewing and ground colour

i=2

trait <- records_4deg_transects[[i]]$forewingBand_c
n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
trait_name <- "Forewing tip freq."

plot_map + Southern_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=36.05972222222222, xend=36.611111111111114,
             y=-8.28111111111111, yend=-6.786944444444444, colour = "Blue", size=10, alpha=0.8)

trait <- records_4deg_transects[[i]]$groundColour_b
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

plot_map + Southern_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=34.88333333333333, xend=35.825,
             y=-11.426944444444445, yend=-8.914166666666667, colour = "Blue", size=10, alpha=0.8)


# Transect 3 is Southeast Africa and involves hindwing and ground colour

i=3

trait <- records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + Southern_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=11.7025, xend=12.203055555555556,
             y=3.0919444444444446, yend=0.4244444444444444, colour = "Blue", size=10, alpha=0.8)

trait <- records_4deg_transects[[i]]$groundColour_b
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

plot_map + Southern_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=12.546388888888888, xend=12.775833333333333,
             y=-1.4088888888888889, yend=-2.6308333333333334, colour = "Blue", size=10, alpha=0.8)


# Transect 4 is Northwest Africa and involves hindwing and ground colour

i=4

trait <- records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + Northwest_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=-12.34, xend=-10.974444444444444,
             y=23.383055555555554, yend=25.86777777777778, colour = "Blue", size=10, alpha=0.8)

trait <- records_4deg_transects[[i]]$groundColour_b
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

plot_map + Northwest_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=-12.239166666666666, xend=-10.472777777777777,
             y=23.570555555555554, yend=26.752222222222223, colour = "Blue", size=10, alpha=0.8)


# Transect 5 is Southeast Asia and involves hindwing and ground colour

i=5

trait <- records_4deg_transects[[i]]$hindwingWhite_a
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + Southeast_Asia +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=104.86777777777777, xend=107.05944444444444,
             y=-1.781111111111111, yend=-3.1372222222222224, colour = "Blue", size=10, alpha=0.8)

trait <- records_4deg_transects[[i]]$groundColour_b
n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

plot_map + Southeast_Asia +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2) +
    
    annotate(geom="segment", x=105.25083333333333, xend=115.05944444444444,
             y=-2.0183333333333335, yend=-7.993888888888889, colour = "Blue", size=10, alpha=0.8)