###### Residual_Map of Trait frequency between research and citizen datasets ###########
######Research and citizen data files comes from Step 5.Trait/phenotype/genotype frequency and allele frequency calculation#####
###### a. To make a file combine research and citizen trait frequency data together ##### 
###### and extract blocks that both have research and citizen trait frquency ############
###### b. Extract three file for each trait that remove all individual of each trait frequency#####


setwd("/data/martin/genomics/analyses/Danaus_SDM/06_map/residual/")

perfal_G <- read.csv("01_Research_02_Citizen_2_rmNAG.csv")
perfal_G$groundColour_bb_citizen.x <- scale(perfal$groundColour_bb_citizen)
perfal_G$groundColour_bb_research.x <- scale(perfal$groundColour_bb_research)


perfal_H <- read.csv("01_Research_02_Citizen_2_rmNAH.csv")
perfal_H$hindwingWhite_aa_citizen.x <- scale(perfal$hindwingWhite_aa_citizen)
perfal_H$hindwingWhite_aa_research.x <- scale(perfal$hindwingWhite_aa_research)


perfal_F <- read.csv("01_Research_02_Citizen_2_rmNAF.csv")
perfal_F$forewingBand_cc_citizen.x <- scale(perfal$forewingBand_cc_citizen)
perfal_F$forewingBand_cc_research.x <- scale(perfal$forewingBand_cc_research)



#groundColour

groundColour.global <- lm(groundColour_bb_citizen ~ groundColour_bb_research.x, data=perfal_G)

summary(groundColour.global)

groundColour.global$res_fit1 <- residuals(groundColour.global)

groundColour.global$res_fit1

write.csv(groundColour.global$res_fit1, file = "groundColour.global_ncovr_sf$res_fit1.csv")

groundColour.global$fitted_fit1 <- fitted(groundColour.global)

groundColour.global$fitted_fit1

write.csv(groundColour.global$fitted_fit1, file = "groundColour.global_ncovr_sf$fitted_fit1.csv")

groundColour.global$sd_breaks <- scale(groundColour.global$res_fit1)[,1] # because scale is made for matrices, we just need to get the first column using [,1]
# this is equal to (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1)) / sd(ncovr_sf$res_fit1)

groundColour.global$sd_breaks

write.csv(groundColour.global$sd_breaks, file = "groundColour.global_ncovr_sf$sd_breaks.csv")

summary(groundColour.global$sd_breaks)



#hindwingWhite

hindwingWhite.global <- lm(hindwingWhite_aa_citizen ~ hindwingWhite_aa_research.x, data=perfal_H)

summary(hindwingWhite.global)

hindwingWhite.global$res_fit1 <- residuals(hindwingWhite.global)

hindwingWhite.global$res_fit1

write.csv(hindwingWhite.global$res_fit1, file = "hindwingWhite.global_ncovr_sf$res_fit1.csv")

hindwingWhite.global$fitted_fit1 <- fitted(hindwingWhite.global)

hindwingWhite.global$fitted_fit1

write.csv(hindwingWhite.global$fitted_fit1, file = "hindwingWhite.global_ncovr_sf$fitted_fit1.csv")

hindwingWhite.global$sd_breaks <- scale(hindwingWhite.global$res_fit1)[,1] # because scale is made for matrices, we just need to get the first column using [,1]
# this is equal to (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1)) / sd(ncovr_sf$res_fit1)

hindwingWhite.global$sd_breaks

write.csv(hindwingWhite.global$sd_breaks, file = "hindwingWhite.global_ncovr_sf$sd_breaks.csv")

summary(hindwingWhite.global$sd_breaks)



#forewingBand

forewingBand.global <- lm(forewingBand_cc_citizen ~ forewingBand_cc_research.x, data=perfal_F)

summary(forewingBand.global)

forewingBand.global$res_fit1 <- residuals(forewingBand.global)

forewingBand.global$res_fit1

write.csv(forewingBand.global$res_fit1, file = "forewingBand.global_ncovr_sf$res_fit1.csv")

forewingBand.global$fitted_fit1 <- fitted(forewingBand.global)

forewingBand.global$fitted_fit1

write.csv(forewingBand.global$fitted_fit1, file = "forewingBand.global_ncovr_sf$fitted_fit1.csv")

forewingBand.global$sd_breaks <- scale(forewingBand.global$res_fit1)[,1] # because scale is made for matrices, we just need to get the first column using [,1]
# this is equal to (ncovr_sf$res_fit1 - mean(ncovr_sf$res_fit1)) / sd(ncovr_sf$res_fit1)

forewingBand.global$sd_breaks

write.csv(forewingBand.global$sd_breaks, file = "forewingBand.global_ncovr_sf$sd_breaks.csv")

summary(forewingBand.global$sd_breaks)


##### Viridis residual map #####


library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)

cap <- function(x, max) ifelse(x <= max, x, max)


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

### entire distributions blocks

cols = c("#5a9bff", "#ffc44e")

# hindwingWhite
# cols = c("SteelBlue", "LightSteelBlue", "LightSalmon","HotPink","BlueViolet")

records_4deg <- read.csv("01_Research_02_Citizen_2_addResidual01_rmNA_H.csv", header = T, stringsAsFactors = F)

trait <- records_4deg$hindwingWhite_aa_research_citizen_sd_breaks
trait_name <- "Hindwing white Residuals sd breaks"
plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)


# forewingBand
# cols = c("SteelBlue", "LightSteelBlue", "LightSalmon","HotPink","BlueViolet")

records_4deg <- read.csv("01_Research_02_Citizen_2_addResidual01_rmNA_F.csv", header = T, stringsAsFactors = F)

trait <- records_4deg$forewingBand_cc_research_and_citizen_sd_breaks
trait_name <- "ForewingBand Residuals sd breaks"
plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)



# groundColour
# cols = c("SteelBlue", "LightSteelBlue", "LightSalmon","HotPink","BlueViolet")

records_4deg <- read.csv("01_Research_02_Citizen_2_addResidual01_rmNA_G.csv", header = T, stringsAsFactors = F)

trait <- records_4deg$groundColour_bb_research_citizen_sd_breaks
trait_name <- "GroundColour Residuals sd breaks"
plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_viridis_c() + labs(fill = trait_name)

