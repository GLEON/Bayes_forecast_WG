#Title: 8C_Fig1_locator_map
#History: created 25APR20 by MEL

#load packages
pacman::p_load(ggmap)

#set local directory
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs_v2/"

#load map
ne <- c(left = -76.9, bottom = 40, right = -63.7, top = 49)

#make dataframe with Sunapee location
points <- data.frame(name = c("Lake Sunapee"),
                     x = c(-72.053953),
                     y = c(43.387814))

#write map
mymap <- get_stamenmap(ne, zoom = 5, maptype = "toner-lite") %>%
  ggmap()+
  geom_point(data = points, aes(x = x, y = y),shape = 23, color = "black",fill = "red", size = 3)+
  geom_text(data = points, aes(x = x, y = y, label = name),hjust = 0.5, vjust = 1.5, fontface = "italic",col = "red")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))
mymap

#save map
ggsave(mymap, filename = file.path(my_directory,paste0("locator_map.tif")),device = "tiff",
       height = 3, width = 3, units = "in", scale = 1, dpi = 300)
