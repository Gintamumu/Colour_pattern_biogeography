######### Viridis colour map #################
######### viridis package - Colorblind-Friendly Color Maps for R ##########
######### https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html #########

library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)

cap <- function(x, max) ifelse(x <= max, x, max)

file_prefix <- "your_file"

# records_all <- read.csv(paste0(file_prefix,".processed.csv"), header = T, stringsAsFactors = F)

records_4deg <- read.csv("your_file", header = T, stringsAsFactors = F)

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



records_4deg_Hind <- subset(records_4deg, is.na(hindwingWhite_aa)==FALSE)
trait <- records_4deg_Hind$hindwingWhite_aa
n <- cap(records_4deg_Hind$hindwingWhite_n, 12)
trait_name <- "Hindwing White freq."

plot_map + whole_range +
    geom_tile(data = records_4deg_Hind, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) 



records_4deg_Fore <- subset(records_4deg, is.na(forewingBand_cc)==FALSE)
trait <- records_4deg_Fore$forewingBand_cc
n <- cap(records_4deg_Fore$forewingBand_n, 12)
trait_name <- "Forewing Tip freq."

plot_map + whole_range +
    geom_tile(data = records_4deg_Fore, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)


records_4deg_Ground <- subset(records_4deg, is.na(groundColour_bb)==FALSE)
trait <- records_4deg_Ground$groundColour_bb
n <- cap(records_4deg_Ground$groundColour_n, 12)
trait_name <- "Orange Ground Colour freq."

plot_map + whole_range +
    geom_tile(data = records_4deg_Ground, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) 



### Sex distribution map ###

records_4deg_Female_ZW <- subset(records_4deg, is.na(Female_ZW)==FALSE)
trait <- records_4deg_Female_ZW$Female_ZW
# n <- cap(records_4deg_Female_ZW$sex_n, 12)
trait_name <- "Female ZW freq."

plot_map + whole_range +
    geom_tile(data = records_4deg_Female_ZW, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name) #+ labs(alpha = "No. records")