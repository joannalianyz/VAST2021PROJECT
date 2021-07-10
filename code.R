packages = c('tmap','clock','wesanderson','DT', 'ggiraph', 'plotly','shiny', 'tidyverse','dplyr','lubridate','rgdal','readr','sf','raster')

for(p in packages){
  if(!require(p, character.only = T)){    
    install.packages(p)  
  }  
  library(p, character.only = T)
}


cc_data <- read_csv("cc_data.csv")
loyalty_data <- read_csv("loyalty_data.csv")
car_assignments <- read_csv("car-assignments.csv")
gps <- read_csv("gps.csv") 


cc_data <- separate(cc_data, timestamp, into = c("Date", "Time"), sep = " ")
cc_data <- separate(cc_data, Time, 
                into = c("Hour" ,"Minute"), 
                sep = ":", remove = FALSE, fill = "left", convert = TRUE)


colnames(cc_data)
glimpse(cc_data)
names(loyalty_data)[names(loyalty_data) == "timestamp"] <- "Date"
colnames(loyalty_data)
names(gps)[names(gps) == "id"] <- "CarID"
colnames(gps)
cc_data$Date <- date_time_parse(cc_data$Date,
                                zone = "",
                                format = "%m/%d/%Y")
glimpse(cc_data)

loyalty_data$Date <- date_time_parse(loyalty_data$Date,
                                     zone = "",
                                     format = "%m/%d/%Y")
glimpse(loyalty_data)

gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "",
                                 format = "%m/%d/%Y %H:%M:%S")
glimpse(gps)
gps$CarID <- as_factor(gps$CarID)
car_assignments$CarID <- as_factor(car_assignments$CarID)


df_cc_data = data.frame(cc_data)
glimpse(df_cc_data)
df_loyalty_data = data.frame(loyalty_data)
df_car_assignments = data.frame(car_assignments)
df_gps = data.frame(gps)


cc_loyalty <- merge(df_cc_data,df_loyalty_data,by=c("Date","location", "price"))
glimpse(cc_loyalty)
location_count <- df_cc_data %>% count(location)
location_count

df_cc_data = data.frame(lapply(df_cc_data, gsub, pattern = "é", replacement = "e"))

df_cc_data

df1<-cc_loyalty %>%
  group_by(location) %>%
  arrange(location, desc(Hour))

p <- ggplot(data = df1, aes(x = location, fill=Hour)) +
  geom_bar()
p

df1 <- cc_loyalty %>% group_by(location,Hour) %>% summarise(count=n())

gps_car_assignments <- merge(df_gps,df_car_assignments,by=c("CarID"))
glimpse(gps_car_assignments)
glimpse(df_gps)
location_count <- df_cc_data %>% count(location)
location_count
glimpse(car_assignments)

location_count <- df_cc_data %>% count(location)
location_count

top_5_locations <- head(arrange(location_count,desc(n)),n=5)
ggplot(top_5_locations, aes(x=reorder(location, -n),y=n))+
  geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label = n), vjust = -0.2)+
  ggtitle("No. of CC transactions at the top 5 most popular locations")+
  xlab("Location") + ylab("No. of CC transactions")+
  ylim(0, max(top_5_locations$n) * 1.1)

boxplot <- ggplot(cc_data,aes(y = price, x= location)) +
  geom_boxplot_interactive(aes(tooltip = price)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ylim(0, max(cc_data$price) * 1.1)

girafe(
  ggobj = boxplot,
  width_svg = 6,
  height_svg = 6*0.618
)


CC_owner <- read_csv("CC_owner.csv")
View(CC_owner)

df_CC_owner = data.frame(CC_owner)

gps_cc<- merge(gps_car_assignments,df_CC_owner,by=c("CarID"))
car_cc_location<- merge(gps_cc,cc_loyalty,by=c("last4ccnum"))
glimpse(gps_cc)
glimpse(car_cc_location)


location_count <- cc_data1 %>% count(location)
top_10_locations <- head(arrange(location_count,desc(n)),n=10)
p1 <- ggplot(top_10_locations, aes(x=reorder(location, -n),y=n))+
  geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label = n), vjust = -0.2)+
  ggtitle("No. of CC transactions at the top 10 most popular locations")+
  xlab("Location") + ylab("No. of CC transactions")+
  ylim(0, max(top_10_locations$n) * 1.1)
  
location_count1 <- loyalty_data %>% count(location)
top_10_locations1 <- head(arrange(location_count1,desc(n)),n=10)
ggplot(top_10_locations1, aes(x=reorder(location, -n),y=n))+
  geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label = n), vjust = -0.2)+
  ggtitle("No. of loyalty transactions at the top 10 most popular locations")+
  xlab("Location") + ylab("No. of loyalty transactions")+
  ylim(0, max(top_10_locations$n) * 1.1)

Katcafe <- df_cc_data %>% filter(location == "Katerina Cafe") %>% count(Hour)
ggplot(Katcafe, aes(x=reorder(Hour, -n),y=n))+
  geom_col()+
  ggtitle("No. of transactions per hour at Katerinas Cafe")+
  xlab("Location") + ylab("No. of CC transactions")  

view(cc_loyalty)

nrow(df_cc_data)
nrow(df_loyalty_data)
library(plotly)  

bxplt <- ggplot(cc_data,aes(y = price, x= location)) +
  geom_boxplot_interactive(aes(tooltip = price)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ylim(0, max(cc_data$price) * 1.1)
  
girafe(
  ggobj = boxplot,
  width_svg = 6,
  height_svg = 6*0.618
)

Hippo <- df_cc_data %>% filter(location == "Hippokampos") %>% count(Hour)
ggplot(Hippo, aes(x=reorder(Hour, -n),y=n))+
  geom_col()+
  ggtitle("No. of transactions per hour at Hippokampos")+
  xlab("Location") + ylab("No. of CC transactions")

packages = c('raster', 'sf', 
             'tmap', 'clock', 
             'tidyverse')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
  
bgmap <- raster("MC2-tourist_modified.tif")
bgmap

Abila_st <- st_read(dsn = "Geospatial",
                    layer = "Abila")
gps_sf <- st_as_sf(df_gps, 
                   coords = c("long", "lat"),
                       crs= 4326)
gps_sf                       


df_car_cc_location = data.frame(car_cc_location)

car_cc_location_sf <- st_as_sf(df_car_cc_location, 
                           coords = c("long", "lat"),
                           crs= 4326)
glimpse(car_cc_location_sf)
view(car_cc_location)

gps_path <- gps_sf %>%
  group_by(CarID) %>%
  summarize(m = mean(Timestamp), 
            do_union=FALSE) %>%
  st_cast("LINESTRING")
gps_path

gps_path1 <- gps_sf %>%
  group_by(Timestamp) %>%
  st_cast("LINESTRING")
gps_path1

car_cc_location_path <- car_cc_location_sf %>%
  group_by(CarID) %>%
  summarize(m = mean(Timestamp), 
            do_union=FALSE) %>%
  st_cast("LINESTRING")
car_cc_location_path


gps_path_selected <- gps_path %>%
  filter(CarID==28)
tmap_mode("view")
tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1,g = 2,b = 3,
       alpha = NA,
       saturation = 1,
       interpolate = TRUE,
       max.value = 255) +
  tm_shape(gps_path_selected) +
  tm_lines()
  
gps_path_selected1 <- gps_path1 %>%
  filter(CarID==16)
tmap_mode("view")
tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1,g = 2,b = 3,
       alpha = NA,
       saturation = 1,
       interpolate = TRUE,
       max.value = 255) +
  tm_shape(gps_path_selected1) +
  tm_lines()

car_path_selected <- car_cc_location_path  %>%
  filter(CarID==16)
tmap_mode("view")
tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1,g = 2,b = 3,
         alpha = NA,
         saturation = 1,
         interpolate = TRUE,
         max.value = 255) +
  tm_shape(gps_path_selected1) +
  tm_lines()

df_car_cc_location$price <- factor(df_car_cc_location$price,
                            levels = df_car_cc_location$price[nrow(df_car_cc_location):1])

ChotusHotel <- df_car_cc_location %>%
  filter(df_car_cc_location$location == "Chotus Hotel")
plot_gantt<-  qplot(ymin = start,
                    ymax = end,
                    x = price,
                    colour = CarID,
                    geom = "linerange",
                    data = ChotusHotel,
                    size = I(5)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("No. of CC transactions at each point of time")



location_count1 <- df_loyalty_data %>% count(location)
top_5_locations1 <- head(arrange(location_count1,desc(n)),n=5)
ggplot(top_5_locations1, aes(x=reorder(location, -n),y=n))+
  geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label = n), vjust = -0.2)+
  ggtitle("No. of loyalty transactions at the top 5 most popular locations")+
  xlab("Location") + ylab("No. of loyalty transactions")+
  ylim(0, max(top_5_locations$n) * 1.1)

packages = c('igraph', 'tidygraph', 
             'ggraph', 'visNetwork', 
             'lubridate', 'clock',
             'tidyverse')
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

cc_data <- read_csv("cc_data.csv")
cc_data$timestamp <-  date_time_parse(cc_data$timestamp,
                                      zone = "",
                                      format = "%m/%d/%Y %H:%M")
cc_data$last4ccnum <- as.character(cc_data$last4ccnum)
cc_data$Day  = get_day(cc_data$timestamp)
cc_data$Hour = get_hour(cc_data$timestamp)

sources <- cc_data %>%
  distinct(last4ccnum) %>%
  rename(label = last4ccnum)
destinations <- cc_data %>%
  distinct(location) %>%
  rename(label = location)
cc_nodes <- full_join(sources, 
                      destinations, 
                      by = "label")
cc_nodes <- cc_nodes %>% 
  rowid_to_column("id")
edges <- cc_data %>%  
  group_by(last4ccnum, location, Day, Hour) %>%
  summarise(weight = n()) %>% 
  ungroup()
edges
cc_edges <- edges %>% 
  left_join(cc_nodes, 
            by = c("last4ccnum" = "label")) %>% 
  rename(from = id)
cc_edges <- cc_edges %>% 
  left_join(cc_nodes, 
            by = c("location" = "label")) %>% 
  rename(to = id)
cc_edges <- select(cc_edges, from, to, 
                   Day, Hour, weight)
cc_edges

library(tidygraph)
cc_graph <- tbl_graph(nodes = cc_nodes, 
                      edges = cc_edges, 
                      directed = FALSE)
cc_graph

library(ggraph)
ggraph(cc_graph, 
       layout = "lgl") +
  geom_edge_link(aes(width=weight)) +
  geom_node_point(aes(colour=id),
                  size=2) +
  facet_edges(cc_data$Day)  


theme_graph()

