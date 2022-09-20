# Figure Test
library(tidyverse)
library(patchwork)
library(rphylopic)

# import data
canada_raw <- read.csv("raw_data/canada_detect.csv")

usa_raw <- read.csv("raw_data/usa_detect.csv")

# organize data
colnames(canada_raw)

str(canada_raw)

canada_data <- canada_raw %>% 
  group_by(Group) %>% 
  summarise(total_species = n_distinct(Common.Name),
            species_estimates = sum(num_papers > 0)) %>% 
  mutate(proportion = (species_estimates/total_species)*100) %>% 
  add_column(Country = "Canada")

canada_data

write.csv(canada_data,"data/canada_data.csv", row.names = FALSE)

colnames(usa_raw)


usa_data <- usa_raw %>% 
  group_by(Group) %>% 
  summarise(total_species = n_distinct(Common.Name),
            species_estimates = sum(num_papers > 0)) %>% 
  mutate(proportion = (species_estimates/total_species)*100) %>% 
  add_column(Country = "USA")

usa_data

write.csv(usa_data,"data/usa_data.csv", row.names = FALSE)


# calculate proportion of 'with detect'
data_combined <- rbind(canada_data, usa_data)



write.csv(data_combined,"data/data_combined.csv", row.names = FALSE)


 # ggplot figure
canada <- read.csv("data/canada_data.csv")
usa <- read.csv("data/usa_data.csv")

# Plot
colnames(canada)

can_pic <- canada %>% 
  mutate(Group = fct_reorder(Group, proportion)) %>% 
ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = 0, yend = proportion),
                colour = "#0A9396", lwd = 1.5) +
  geom_point(aes(x = Group, y = proportion), colour = "#00778F",
             fill = "white", size=5, shape = 19, stroke = 1.5) +
  #geom_point( aes(x = Group, y = total_papers), colour = "#0A9396", size=5 ) +
  xlab("") +
  ylab(" ") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  ggtitle("CANADA") +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(-0.1, 60)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        plot.title = element_text(hjust = 0.5)) 

usa_pic <- usa %>% 
  mutate(Group = fct_reorder(Group, proportion)) %>% 
  ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = 0, yend = proportion),
                colour = "#0A9396", lwd = 1.5) +
  geom_point(aes(x = Group, y = proportion), colour = "#00778F",
             fill = "white", size=5, shape = 19, stroke = 1.5) +
  xlab("") +
  ylab(" ") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("USA") +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(-0.1, 60)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1)) +
  annotate("text", x = 17, y = 0.29, label = "", size = 5) 

# Put it together

combine <- usa_pic + plot_spacer() + can_pic +
  plot_layout(widths = c(5, -1.05, 5),
              guides = 'collect')

combine


ggsave("figures/combined_figure.TIFF", combine,
       width = 15.8,
       height = 7.6)


####### paired plot
library(ggalt)

data_combined <- read.csv("data/data_combined.csv")

combined_wide <- data_combined %>% 
  pivot_wider(names_from = Country,
              values_from = num_papers)

combined_wide[is.na(combined_wide)] <- 0
colnames(combined_wide)

combined_wide %<>% 
  rowwise() %>% 
  mutate(mean_value = mean(c(Canada, USA))) %>% 
  arrange(mean_value)



test <- combined_wide %>% 
  mutate(Group = fct_reorder(Group, mean_value)) %>% 
  ggplot() +
  geom_segment(aes(x = Group, xend = Group, y = USA, yend = Canada),
                colour = "black", lwd = 1.5) +
  geom_point(aes(x = Group, y = USA), colour = "#00778F",
              fill = "white", size=5, shape = 15, stroke = 1.5) +
  geom_point(aes(x = Group, y = Canada), colour = "#00778F",
             fill = "white", size=5, shape = 19, stroke = 1.5) +
  xlab("") +
  ylab(" ") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(0, 140, 10), limits = c(-1, 140))
  #coord_flip() 



# Phylopic
# http://phylopic.org/
colnames(canada_raw)

unique(combined_wide$Group)

# [1] "Amphibians"x          "Arachnids"           "Birds" x          
# [4] "Clams" x              "Conifers and Cycads" "Ferns and Allies"   
# [7] "Fishes" x             "Flowering Plants" x    "Insects" x           
# [10] "Lichens" x            "Mammals"x             "Mosses"             
# [13] "Reptiles"x            "Snails"              "Crustaceans"   

colnames(canada_raw)

canada_raw %>% 
  filter(Group == "Birds") %>% 
  unique()

# mammal - grizzly or black bear, or humpback whale
# amphibian - jefferson salamander, fowler toad (many toads)
# bird - goshawk, golden eagle, northern bobwhite, marbled murrelet
# clams - northern riffleshell
# fish - atlantic salmon, bull trout, white shark, rainbow trout, striped bass
# angiosperm - Spalding's campion
# insect - Frosted elfin, cobblestone tiger beetle, monarch, american burying beetle
# lichen - vole ears lichen
# reptile - loggerhead sea turtle, eastern box turtle, snapping turtle

#### MAMMAL ####
bear <- name_search(text = "Grizzly bear", options = "namebankID")[[1]] 
bear

bear_id_all <- name_images(uuid = bear$uid[1])  # list images
bear_id_all


bear_id <- name_images(uuid = bear$uid[1])$same[[1]]$uid  # get individual image id
bear_id

bear_pic <- image_data(bear_id, size = 256)[[1]]

#### AMPHIBIAN ####

toad <- name_search(text = "Bufo", options = "namebankID")[[1]]
toad

toad_id_all <- name_images(uuid = toad$uid[1])  # list images
toad_id_all

toad_pic <- image_data("e1a404c9-4edc-45c3-a694-0817157832fc", size = 256)[[1]]

##### Fish ####

fish <- name_search(text = "Salmoninae", options = "namebankID")[[1]]
fish

fish_id <- name_images(uuid = fish$uid[1])

fish_pic <- image_data("68304786-f291-4115-9ccd-ead068ba6f19", size = 256)[[1]]


#### Birds #####

bird <- name_search(text = "Charadriidae", options = "namebankID")[[1]]
bird

bird_id <- name_images(uuid = bird$uid[1])

bird_pic <- image_data("5ccc0a37-77ae-4e69-b824-ad3e85f802c6", size = 256)[[1]]

###### Reptiles ####

reptile <- name_search(text = "snapping turtle", options = "namebankID")[[1]]
reptile

reptile_id <- name_images(uuid = reptile$uid[3])

reptile_pic <- image_data("fec4ae89-b3ea-4693-b63e-85dad8bc9ed3", size = 256)[[1]]

##### Flower ####

flower <- name_search(text = "Helianthus annuus ", options = "namebankID")[[1]]
flower

flower_id <- name_images(uuid = flower$uid[1])

flower_pic <- image_data("40b91c6c-4ef8-4a5a-9b90-85fb50d74c5b", size = 256)[[1]]


##### Insects ####

insect <- name_search(text = "monarch", options = "namebankID")[[1]]
insect

insect_id <- name_images(uuid = insect$uid[1])

insect_pic <- image_data("e6705593-91ba-4b6e-9dfd-515005e35fc6", size = 256)[[1]]



###### Clams ####
clam <- name_search(text = "clam", options = "namebankID")[[1]]
clam

clam_id <- name_images(uuid = clam$uid[23])

clam_pic <- image_data("70f895b9-ac43-4d4f-bd8c-604834bce948", size = 256)[[1]]

####### LICHEN ####

lichen <- name_search(text = "Lecanoromycetes", options = "namebankID")[[1]]
lichen

lichen_id <- name_images(uuid = lichen$uid[1])

lichen_pic <- image_data( "c918dfe0-a7da-48c1-bc82-78d7041e4a13", size = 256)[[1]]

##### SNAILS #########
snail<- name_search(text = "gastropoda", options = "namebankID")[[1]]
snail

snail_id <- name_images(uuid = snail$uid[1])

snail_pic <- image_data("c5835123-e2d3-4c20-9e7a-f7b6528bbf8e", size = 256)[[1]]

########## Ferns and Allies ##########

fern<- name_search(text = "Monilophyta", options = "namebankID")[[1]]
fern

fern_id <- name_images(uuid = fern$uid[1])

fern_pic <- image_data("f2d82e69-89b2-4164-9dfe-e9e623cbd848", size = 256)[[1]]


###### Mosses ######
 
moss<- name_search(text = "Bryozoa", options = "namebankID")[[1]]
moss

moss_id <- name_images(uuid = moss$uid[1])

moss_pic <- image_data("191ad6ce-78f2-444d-b4ad-9c4162b1803f", size = 256)[[1]]

##### Crustacean #####
 
crust<- name_search(text = "Decapoda", options = "namebankID")[[1]]
crust

crust_id <- name_images(uuid = crust$uid[1])

crust_pic <- image_data("786bbbd0-529d-4e64-a8ac-f54f88194033", size = 256)[[1]]


####### "Arachnids"  ######
 
arach<- name_search(text = "Araneidae", options = "namebankID")[[1]]
arach

arach_id <- name_images(uuid = arach$uid[1])

arach_pic <- image_data("593cd880-1440-4562-b589-264cc6f9e5f2", size = 256)[[1]]

####### "Conifers and Cycads" #####

conifer<- name_search(text = "Pinaceae", options = "namebankID")[[1]]
conifer

conifer_id <- name_images(uuid = conifer$uid[1])

conifer_pic <- image_data("38c636a7-f4b3-4a6c-89b1-fb2dcbdafa06", size = 256)[[1]]


#####  IMAGE #####

USA_phylo <- usa_pic + 
  coord_flip() +
  add_phylopic(bear_pic, alpha = 1, x = 8, y = 140, ysize = 20) +
  add_phylopic(toad_pic, alpha = 1, x = 4, y = 140, ysize = 15) +
  add_phylopic(fish_pic, alpha = 1, x = 5, y = 140, ysize = 25) +
  add_phylopic(bird_pic, alpha = 1, x = 6.1, y = 140, ysize = 20) +
  add_phylopic(reptile_pic, alpha = 1, x = 7, y = 140, ysize = 15) +
  add_phylopic(flower_pic, alpha = 1, x = 3, y = 140, ysize = 20) +
  add_phylopic(insect_pic, alpha = 1, x = 2, y = 140, ysize = 20) +
  add_phylopic(clam_pic, alpha = 1, x = 1, y = 140, ysize = 20)


can_phylo <- can_pic + 
  coord_flip() +
  add_phylopic(bear_pic, alpha = 1, x = 9, y = 170, ysize = 20) +
  add_phylopic(toad_pic, alpha = 1, x = 5, y = 140, ysize = 20) +
  add_phylopic(fish_pic, alpha = 1, x = 7, y = 140, ysize = 25) +
  add_phylopic(bird_pic, alpha = 1, x = 8, y = 140, ysize = 20) +
  add_phylopic(reptile_pic, alpha = 1, x = 6, y = 140, ysize = 20) +
  add_phylopic(flower_pic, alpha = 1, x = 1, y = 140, ysize = 25) +
  add_phylopic(insect_pic, alpha = 1, x = 4, y = 140, ysize = 25) +
  add_phylopic(clam_pic, alpha = 1, x = 2, y = 140, ysize = 20) +
  add_phylopic(lichen_pic, alpha = 1, x = 3, y = 140, ysize = 20)

combine_phylopic <- USA_phylo + can_phylo

ggsave("figures/combined_figure_phylopic.jpeg", combine_phylopic)





test + 
  add_phylopic(bear_pic, alpha = 1, x = 14, y = 105, ysize = 10) +
  add_phylopic(toad_pic, alpha = 1, x = 11, y = 30, ysize = 10) +
  add_phylopic(fish_pic, alpha = 1, x = 12, y = 55, ysize = 9) +
  add_phylopic(bird_pic, alpha = 1, x = 15, y = 142, ysize = 10) +
  add_phylopic(reptile_pic, alpha = 1, x = 10, y = 25, ysize = 15) +
  add_phylopic(flower_pic, alpha = 1, x = 13, y = 100, ysize = 15) +
  add_phylopic(insect_pic, alpha = 1, x = 8, y = 20, ysize = 12) +
  add_phylopic(clam_pic, alpha = 1, x = 9, y = 25, ysize = 10)+
  add_phylopic(snail_pic, alpha = 1, x = 6, y = 20, ysize = 15) +
  add_phylopic(fern_pic, alpha = 1, x = 5, y = 15, ysize = 10) +
  add_phylopic(moss_pic, alpha = 1, x = 3, y = 10, ysize = 12) +
  add_phylopic(crust_pic, alpha = 1, x = 7, y = 20, ysize = 10) +
  add_phylopic(arach_pic, alpha = 1, x = 1, y = 12, ysize = 14)+
  add_phylopic(conifer_pic, alpha = 1, x = 2, y = 12, ysize = 17) +
  add_phylopic(lichen_pic, alpha = 1, x = 4, y = 10, ysize = 10) 


##### double figure

usa_phylo <- usa_pic + 
  add_phylopic(arach_pic, alpha = 1, x = 1, y = 7, ysize = 10) +
  add_phylopic(conifer_pic, alpha = 1, x = 2, y = 7, ysize = 10) +
  add_phylopic(lichen_pic, alpha = 1, x = 3, y = 7, ysize = 6) +
  add_phylopic(crust_pic, alpha = 1, x = 4, y = 12, ysize = 7) +
  add_phylopic(flower_pic, alpha = 1, x = 5, y = 10, ysize = 5) +
  add_phylopic(clam_pic, alpha = 1, x = 6, y = 10, ysize = 5) +
  add_phylopic(insect_pic, alpha = 1, x = 7, y = 12, ysize = 6) +
  add_phylopic(snail_pic, alpha = 1, x = 8, y = 13, ysize = 10) +
  add_phylopic(fish_pic, alpha = 1, x = 9, y = 15, ysize = 9) +
  add_phylopic(fern_pic, alpha = 1, x = 10, y = 16, ysize = 6) +
  add_phylopic(reptile_pic, alpha = 1, x = 11, y = 20, ysize = 9) +
  add_phylopic(bear_pic, alpha = 1, x = 12, y = 20.5, ysize = 5) +
  add_phylopic(bird_pic, alpha = 1, x = 13, y = 23, ysize = 5) +
  add_phylopic(toad_pic, alpha = 1, x = 14, y = 40, ysize = 7) 
  
 
  
can_phylo <- can_pic + 
  add_phylopic(bird_pic, alpha = 1, x = 14, y = 58, ysize = 5) +
  add_phylopic(bear_pic, alpha = 1, x = 13, y = 43, ysize = 5) +
  add_phylopic(toad_pic, alpha = 1, x = 12, y = 35, ysize = 6) +
  add_phylopic(reptile_pic, alpha = 1, x = 11, y = 23, ysize = 10) +
  add_phylopic(fish_pic, alpha = 1, x = 10, y = 20, ysize = 8) +
  add_phylopic(lichen_pic, alpha = 1, x = 9, y = 15, ysize = 7) +
  add_phylopic(insect_pic, alpha = 1, x = 8, y = 11, ysize = 6) +
  add_phylopic(clam_pic, alpha = 1, x = 7, y = 11, ysize = 5) +
  add_phylopic(flower_pic, alpha = 1, x = 6, y = 7, ysize = 7) +
  add_phylopic(arach_pic, alpha = 1, x = 5, y = 5, ysize = 7) +
  add_phylopic(conifer_pic, alpha = 1, x = 4, y = 5, ysize = 8) +
  add_phylopic(fern_pic, alpha = 1, x = 3, y = 5, ysize = 6) +
  add_phylopic(moss_pic, alpha = 1, x = 2, y = 5, ysize = 7) + 
  add_phylopic(snail_pic, alpha = 1, x = 1, y = 5, ysize = 7)
  
  
combine_phylo <- usa_phylo + plot_spacer() + can_phylo +
  plot_layout(widths = c(5.5, -1.05, 5),
              guides = 'collect')

combine_phylo


ggsave("figures/combined_figure_phylo.TIFF",
       combine_phylo,
       height = 8.62,
       width = 17.9)
   

  
