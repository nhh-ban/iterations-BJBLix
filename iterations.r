library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


#### 4: Task 4 code is found in their respective files, as asked for in the task


### 5: Final volume query: 

source("gql-queries/vol_qry.r")


GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)


# I am saving a random station extracted as a variable
# so it is easier to extract the name for the plot legend
sampled_station <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1)

# Revised plot call
sampled_station %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>%
  GQL(.url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line(color = "black", size = 0.7) + 
  geom_area(aes(x=from, y=volume), fill="grey", alpha=0.2) + # adding a filler under line
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day") + # format x-axis
  theme_minimal() +  # use a different theme
  theme(
    plot.title = element_text(face="bold", size=14, hjust=0.5),
    axis.title.x = element_text(face="bold", size=12),
    axis.title.y = element_text(face="bold", size=12),
    axis.text.x = element_text(angle=45, hjust=1)
  ) +  # adjusting text elements
  labs(
    title = glue::glue("Trafikkdata for de siste 7 dagene - Trafikkstasjon: {sampled_station$name}"),
    y = "Trafikkvolum",
    x = "Dato"
  ) 

