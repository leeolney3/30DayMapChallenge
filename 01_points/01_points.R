#30DayMapChallenge 01 Points
#Supermarket locations in Singapore
#Data source: https://data.gov.sg/dataset/list-of-supermarket-licences?view_id=97a106cf-d9af-4808-9476-1d8bcf8dd78b&resource_id=3561a136-4ee4-4029-a5cd-ddf591cce643 (last updated March 7, 2017)

# load libraries 
library(tidyverse)
library(rjson)
library(sf) 

# import supermarket .csv file
df = read_csv("data/list-of-supermarket-licences.csv")

# import .json file (SG postal codes with coordinates)
# data source: https://github.com/isen-ng/singapore-postal-codes-1
rj = fromJSON('data/singpostcode.json')

# create missing coordinates tribble
coords = tribble(
  ~postal, ~latt1, ~long1,
  "180001",1.3025329,103.8373585,
  "218120",1.3123449,103.8382245,
  "731006",1.4430429,103.7526025,
  "757027",1.4543114,103.7725838,
  "538848",1.3450459,103.8722555,
  "437150",1.2996279,103.8796215,
  "427540",1.3097979,103.8846055,
  "628087",1.3306339,103.6822915,
  "588175",1.3410249,103.7583665,
  "649698",1.3488639,103.7174235,
)

# join data 
map_df = df1 %>% left_join(rj, by="POSTAL") %>%
  select(id, postal=POSTAL, lat = LATITUDE, long= LONGTITUDE) %>%
  group_by(id, postal) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(coords, by="postal") %>%
  mutate(lat=parse_number(lat),
         long=parse_number(long)) %>%
  mutate(lat = case_when(is.na(lat)~ latt1, TRUE~lat),
         long = case_when(is.na(long)~ long1, TRUE~long)) %>%
  select(-latt1, -long1) %>%
  drop_na() %>%
  filter(postal!="757027")

# convert to sf object 
map_df2 = map_df %>% st_as_sf(coords=c("long","lat"), crs = 4326)

# read SG shape file
# shp source: https://data.gov.sg/dataset/region-census-2010
sg2 <- read_sf('data/region-census2010-shp/Region_Census2010.shp')

# plot map
ggplot() + geom_sf(data= sg2, fill=NA, size=.2, color="#618985") +
  geom_sf(data=map_df2, size=.3, color="#ffff3f", alpha=.9) +
  coord_sf() +
  theme_void() +
  theme(plot.background=element_rect(fill="#223843", color=NA)) +
annotate(geom="richtext",x=39000, y=19000, fill = NA, label.color = NA, hjust=0,
         label="<span style = 'color:#BFCFCE';'>Supermarkets in Singapore</span><br><span style = 'color:#BFCFCE;font-size:7.8pt';'>Locations of 476 supermarket licences</span><br><span style = 'color:#BFCFCE;font-size:6pt';'>Data source: data.gov.sg (updated: March 7, 2017)</span>")
         
