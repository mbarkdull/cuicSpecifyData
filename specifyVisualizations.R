library(tidyverse)
library(plotly)

#### Data prep ####
# Read in data:
formicidae <- read_csv("./FormicidaeThroughJuly19.csv") %>%
  distinct() %>%
  filter(!is.na(`Unique Identifier`))

# Clean up collector names a bit:
sort(unique(formicidaeCleaned$collector))
formicidaeCleaned <- formicidae %>% 
  mutate(collector = case_when(`Collectors - Last Name` == "C. Saux" ~ "C. S. Moreau",
                               `Collectors - Last Name` == "Corrie S. Moreau" ~ "C. S. Moreau",
                               `Collectors - Last Name` == "Corrie Saux" ~ "C. S. Moreau",
                               `Collectors - Last Name` == "Corrie Saux Moreau" ~ "C. S. Moreau",
                               `Collectors - Last Name` == "M.  A. Deyrup" ~ "M. Deyrup",
                               `Collectors - Last Name` == "R.C. Shannon" ~ "R. C. Shannon",
                               `Collectors - Last Name` == "R C. Shannon" ~ "R. C. Shannon",
                               `Collectors - Last Name` == "MR Smith" ~ "M. R. Smith",
                               `Collectors - Last Name` == "M.R. Smith" ~ "M. R. Smith",
                               `Collectors - Last Name` == "L.R. Davis Jr." ~ "L. R. Davis Jr.",
                               `Collectors - Last Name` == "Lloyd R. Davis Jr." ~ "L. R. Davis Jr.",
                               `Collectors - Last Name` == "Lloyd R. Davis, Jr." ~ "L. R. Davis Jr.",
                               `Collectors - Last Name` == "J.K. Liebherr" ~ "J. K. Liebherr",
                               `Collectors - Last Name` == "J. Liebherr" ~ "J. K. Liebherr",
                               `Collectors - Last Name` == "J.K Liebherr" ~ "J. K. Liebherr",
                               `Collectors - Last Name` == "J. Ch. Bradley" ~ "J. C. Bradley",
                               `Collectors - Last Name` == "J. C. B." ~ "J. C. Bradley",
                               `Collectors - Last Name` == "A & H. Dietrich" ~ "A. & H. Dietrich",
                               `Collectors - Last Name` == "A & H Dietrich" ~ "A. & H. Dietrich",
                               `Collectors - Last Name` == "André Francoeur" ~ "A. Francoeur",
                               `Collectors - Last Name` == "A Francoeur" ~ "A. Francoeur",
                               `Collectors - Last Name` == "Wm. M. Wheeler" ~ "W. M. Wheeler",
                               `Collectors - Last Name` == "William Morton Wheeler" ~ "W. M. Wheeler",
                               `Collectors - Last Name` == "Wheeler" ~ "W. M. Wheeler",
                               `Collectors - Last Name` == "W.M. Wheeler" ~ "W. M. Wheeler",
                               `Collectors - Last Name` == "W. M. Wheeler, leg." ~ "W. M. Wheeler",
                               
                               TRUE ~ `Collectors - Last Name`))

# Format the date as a date:
formicidaeCleaned$`date` <- as.Date(formicidaeCleaned$`Start Date`,
                                          format = "%d/%m/%Y")
formicidaeCleaned$`year` <- format(as.Date(formicidaeCleaned$`date`, 
                                           format="%d/%m/%y"),
                                   "%Y") %>%
  as.numeric()

floor_decade <- function(value) {
  return(value - value %% 10) 
}

formicidaeCleaned <- formicidaeCleaned %>%
  mutate(decade = floor_decade(year))

#### Plots ####
# Plot collectors with more than 5 specimens:
ggplot(data = (formicidaeCleaned %>%
                 na.omit() %>%
                 group_by(collector) %>%
                 filter(n() > 5) %>%
                 arrange(collector, 
                         .by_group = TRUE)),
       mapping = aes(x = fct_infreq(collector),
                     fill = decade,
                     group = decade)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) + 
  coord_flip() +
  scale_fill_gradientn(colors = c("#df6322", 
                                  "#e9e0a6",
                                  "#529a92")) +
  ggtitle("CUIC ant collectors with more than five databased specimens")

# Plot collection by decade and color by zoogeography:
ggplot(data = formicidaeCleaned) +
  geom_bar(mapping = aes(x = decade,
                         fill = `Zoogeography - Full Name`)) + 
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1)) 

# Plot by country:
ggplot(data = formicidaeCleaned) +
  geom_bar(mapping = aes(x = fct_infreq(`Country - Full Name`),
                         fill = `Zoogeography - Full Name`)) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# Try to plot a map:
library(sf)
my_sf <- st_as_sf(filter(formicidaeCleaned,
                         !is.na(Longitude1) |
                           !is.na(Latitude1)), 
                  coords = c('Longitude1', 
                             'Latitude1'))
my_sf <- st_set_crs(my_sf, value = 4326)

#Plot it:
countries <- map_data("world")
collectingLocations <- ggplot() +
  geom_polygon(data = countries, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group),
               col = NA, 
               lwd = 3) +
  geom_polygon(data = countries, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group), 
               fill = 'white', 
               color = 'grey', 
               linewidth = 0.2) + 
  geom_sf(data = my_sf,
          mapping = aes(color = as.numeric(decade),
                        text = `Unique Identifier`,
                        text2 = collector,
                        text3 = Locality)) +
  scale_color_gradientn(colors = c("#df6322", 
                                   "#e9e0a6",
                                   "#529a92"))

collectingLocations

ggplotly(collectingLocations)


