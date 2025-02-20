# geo_epidemiology

## A example of geo-epidemiology map - Vietnam CPI 2018 

``` r
# Reference: http://pci2018.pcivietnam.vn/uploads/2019/ho-so-63-tinh-vie.pdf
# Data Source: http://pci2018.pcivietnam.vn/


"#00008F", "#00009F", "#0000AF", "#0000BF", 
"#0000CF", "#0000DF", "#0000EF", "#0000FF", "#0010FF", 
"#0020FF", "#0030FF", "#0040FF", "#0050FF", "#0060FF", 
"#0070FF", "#0080FF", "#008FFF", "#009FFF", "#00AFFF", 
"#00BFFF", "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF", 
"#10FFEF", "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", 
"#60FF9F", "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", 
"#AFFF50", "#BFFF40", "#CFFF30", "#DFFF20", "#EFFF10", 
"#FFFF00", "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", 
"#FFAF00", "#FF9F00", "#FF8F00", "#FF8000", "#FF7000", 
"#FF6000", "#FF5000", "#FF4000", "#FF3000", "#FF2000", 
"#FF1000", "#FF0000", "#EF0000", "#DF0000", "#CF0000", 
"#BF0000", "#AF0000", "#9F0000", "#8F0000", "#800000"


cpi_colors <- c("#00008F", "#0070FF", "#AFFF50", "#FFAF00", "#BF0000")

rm(list = ls())
df_cpi <- read_excel("C:/Users/HP/Downloads/du-lieu-pci-2018.xlsx", sheet = 1) %>% 

  select(1:3) %>% 
  slice(1:63)

library(viridis)
library(tidyverse)
library(readxl)
library(extrafont)
library(mapproj)
library(maptools)
names(df_cpi) <- c("Province", "Rank", "Score")



df_cpi %>% 
  mutate(Province = case_when(str_detect(Province, "BRVT") ~ "Bà Rịa - Vũng Tàu", TRUE ~ Province)) %>% 
  mutate(fake_rank = case_when(Rank < 10 ~ paste0("0", Rank), TRUE ~ as.character(Rank))) %>% 
  mutate(Province = paste(Province, fake_rank)) %>% 
  mutate(my_colors = case_when(Rank <= 2 ~ "Excellent", 
                               Rank >= 3 & Rank <= 9 ~ "Good", 
                               Rank >= 10 & Rank <= 41 ~ "Fair", 
                               Rank >= 42 & Rank <= 61 ~ "Mediocre", 
                               TRUE ~ "Poor")) %>% 
  arrange(-Rank) %>% 
  mutate(Province = factor(Province, levels = Province)) %>% 
  mutate(my_colors = factor(my_colors, levels = my_colors %>% unique() %>% .[5:1])) %>% 
  mutate(label = round(Score, 2) %>% as.character()) %>% 
  mutate(label = case_when(str_count(label) == 2 ~ paste0(label, ".00"), 
                           str_count(label) == 4 ~ paste0(label, "0"), 
                           TRUE ~ label)) -> df_ploting2


#-------------
#  Bar Plot 
#-------------


my_font <- "Roboto Condensed"

df_ploting2 %>% 
  ggplot(aes(Province, Score, fill = my_colors)) + 
  geom_col(width = 0.85) + 
  coord_flip() + 
  scale_fill_viridis(discrete = TRUE, name = "", option = "D") + 
  geom_text(aes(label = label), size = 3, hjust = -0.1) + 
  scale_y_continuous(limits = c(0, 80), expand = c(0.001, 0)) +
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_text(size = 8, family = my_font, color = "black")) +
  theme(plot.title = element_text(family = my_font, color = "grey20", size = 22, face = "bold")) + 
  theme(plot.subtitle = element_text(family = my_font, size = 13, color = "gray40")) + 
  theme(plot.caption = element_text(family = my_font, size = 11, colour = "grey40", face = "italic")) + 
  theme(legend.text = element_text(family = my_font, size = 10)) + 
  theme(plot.margin = unit(c(1, 1, 1, 2), "cm")) +
  labs(x = NULL, y = NULL, 
       title = "Vietnam CPI Index 2018", 
      # subtitle = "R Used for Data Visualization", 
       caption = "Data Source: http://pci2018.pcivietnam.vn") -> p1

#--------------
#  Mapping
#--------------


# Get geospatial data for Viet Nam: 

library(raster)
vietnam_province <- getData("GADM", country = "Vietnam", level = 1)

detach(package:raster)
vietnam_df <- vietnam_province %>% fortify(region = "NAME_1")


library(stringi)

vietnam_df %>% 
  mutate(id_prov = stri_trans_general(id, "Latin-ASCII")) %>% 
  mutate(id_prov = case_when(str_detect(id_prov, "Ba Ria") ~ "BRVT", 
                             str_detect(id_prov, "Ho Chi Minh") ~ "TP.HCM", 
                             str_detect(id_prov, "Thua Thien Hue") ~ "TT-Hue", 
                             TRUE ~ id_prov)) -> vietnam_df

df_cpi %>% 
  mutate(id_prov = stri_trans_general(Province, "Latin-ASCII")) -> df_cpi


# Joint data sets: 

df_cpi_mapping <- right_join(vietnam_df, df_cpi, by = "id_prov")

# Function creates new theme: 



ggplot() + 
  geom_polygon(data = df_cpi_mapping, aes(long, lat, group = group, fill = Score), color = "white") +
  coord_map("albers", lat0 = 30, lat1 = 40) + 
  labs(title = "Vietnam CPI Index 2018",
       subtitle = "Vietnam's Paracel and Spratly Islands\nare not shown in this map.",
       caption = "Data Source: http://pci2018.pcivietnam.vn") + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  theme(plot.title = element_text(family = my_font, color = "grey20", size = 22, face = "bold")) + 
  theme(plot.subtitle = element_text(family = my_font, size = 13, color = "gray40")) + 
  theme(plot.caption = element_text(family = my_font, size = 11, colour = "grey40", face = "italic")) + 
  theme(legend.text = element_text(family = my_font, color = "grey40", size = 12)) + 
  theme(legend.title = element_text(family = my_font, color = "grey20", size = 12)) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  # theme(legend.position = "top") + 
  theme(legend.position = c(0.1, 0.3)) + 
  scale_fill_viridis(direction = -1, 
                     option = "D", 
                     name = "CPI Index", 
                     guide = guide_colourbar(direction = "horizontal",
                                             barheight = unit(3, units = "mm"),
                                             barwidth = unit(40, units = "mm"),
                                             title.hjust = 0.5,
                                             label.hjust = 0.5, 
                                             title.position = "top")) -> p2



gridExtra::grid.arrange(p1, p2, ncol = 2)





- Daily Graph Series Nguyen Chi Dung

``` 

![alt text](https://github.com/vanhungtran/geo_epidemiology/blob/master/Rplot02.png)














## Cancer prevelence

``` r



#install.packages(c("grid","rworldmap"))

library(ggplot2)
library(grid)
library(rworldmap)


# download from https://ourworldindata.org/cancer

mydf_cancer <- read.csv("C:/Users/vht/OneDrive/book/share-of-population-with-cancer.csv")
mydf_cancer <- mydf_cancer[,c(1,3,4)]
colnames(mydf_cancer) <- c("Country","Year","Prevalence")


mydf <- mydf_cancer  %>% filter(Year==2017)
data(countryExData)

mydf1 <- merge(countryExData, mydf, by = "Country")


worldMap <- getMap()

# Member States of the European Union

ASEANp <- mydf1$Country
                 
# Select the index of states 
inASEANp <- which(worldMap$NAME%in%ASEANp)

#ASEANp <- which()

# Extract longitude and latitude border's coordinates of members states of E.U. 
ASEANpCoords <- lapply(inASEANp, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

ASEANpCoords <- do.call("rbind", ASEANpCoords)






# Prevalence
#value <- sample(x = seq(0,3,by = 0.1), size = length(ASEANp), replace = TRUE)

value <- mydf1$Prevalence
ASEANpTable <- data.frame(country = ASEANp, value = value)
ASEANpCoords$value <- ASEANpTable$value[match(ASEANpCoords$region,ASEANpTable$country)]





# Plot the map
P <- ggplot() + geom_polygon(data = ASEANpCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1)  +
  coord_map(xlim = c(-180, 180),  ylim = c(-60, 90))

P <- P + scale_fill_gradient(name = "Prevalence of reported cancer", low = "green", high = "red", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))



P
``` 

![alt text](https://github.com/vanhungtran/geo_epidemiology/blob/master/map_cancer_inci.png)



## Other maps
``` r
data(countryExData) 
mapDevice() #create world map shaped window 
mapByRegion(countryExData ,nameDataColumn="CLIMATE" ,joinCode="ISO3" ,nameJoinColumn="ISO3V10" ,regionType="Stern" ,FUN="mean") 

``` 


![alt text](https://github.com/vanhungtran/geo_epidemiology/blob/master/climate.emf)














