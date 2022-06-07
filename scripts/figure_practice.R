# Figure Test
library(tidyverse)
library(patchwork)
library(rphylopic)

# import data
canada_raw <- read.csv("raw_data/canada_detect.csv")

usa_raw <- read.csv("raw_data/usa_detect.csv")

# organize data

canada_data <- canada_raw %>% group_by(Group) %>% 
  summarize_at(vars(num_hits, num_papers), sum) %>% 
  rename(total_papers = num_hits,
         with_detect = num_papers) %>% 
  add_column(Country = "Canada")%>% 
  mutate(proportion = (with_detect/total_papers)) %>% 
  mutate_all(~replace(., is.nan(.), 0)) %>% 
  filter(proportion != 0) %>% 
  mutate(Group = fct_recode(Group,
                            "Angiosperms" = "Flowering Plants"))
  
canada_data

write.csv(canada_data,"data/canada_data.csv", row.names = FALSE)


usa_data <- usa_raw %>% 
  select(Scientific.Name, Common.Name, num_hits,
         num_papers, Group) %>% 
  group_by(Group) %>% 
  summarize_at(vars(num_hits, num_papers), sum) %>% 
  rename(total_papers = num_hits,
         with_detect = num_papers) %>% 
  add_column(Country = "USA") %>% 
  mutate(proportion = (with_detect/total_papers)) %>% 
  mutate_all(~replace(., is.nan(.), 0)) %>% 
  filter(proportion != 0) %>% 
  mutate(Group = fct_recode(Group,
                            "Angiosperms" = "Flowering Plants"))

usa_data

write.csv(usa_data,"data/usa_data.csv", row.names = FALSE)


# calculate proportion of 'with detect'
data_combined <- rbind(canada_data, usa_data)

write.csv(data_combine,"data/data_combined.csv", row.names = FALSE)


 # ggplot figure
canada <- read.csv("data/canada_data.csv")
usa <- read.csv("data/usa_data.csv")

# Plot

can_pic <- canada %>% 
  mutate(Group = fct_reorder(Group, with_detect)) %>% 
ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = with_detect, yend = total_papers),
                colour = "#0A9396", lwd = 1.5) +
  geom_point( aes(x = Group, y = with_detect), colour = "#00778F", fill = "white", size=5, shape = 21, stroke = 1.5) +
  geom_point( aes(x = Group, y = total_papers), colour = "#0A9396", size=5 ) +
  xlab("") +
  ylab(" ") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  ggtitle("CANADA") +
  ylim(0, 175)

usa_pic <- usa %>% 
  mutate(Group = fct_reorder(Group, with_detect)) %>% 
  ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = with_detect, yend = total_papers),
                colour = "#0A9396", lwd = 1.5) +
  geom_point( aes(x = Group, y = with_detect), colour = "#00778F", fill = "white", size=5, shape = 21, stroke = 1.5) +
  geom_point( aes(x = Group, y = total_papers), colour = "#0A9396", size=5 ) +
  xlab("") +
  ylab(" ") +
  theme_minimal() +
  theme(text = element_text(size = 16)) +
  ylim(0, 175) +
  ggtitle("USA")

# Put it together

combine <- can_pic + usa_pic

ggsave("figures/combined_figure.TIFF", combine,
       height = 8.31,
       width = 20)

# Phylopic
# http://phylopic.org/
colnames(canada_raw)

unique(canada_data$Group)
# Amphibians  Birds       Clams       Fishes      Angiosperms Insects     Lichens     Mammals     Reptiles

canada_raw %>% 
  filter(Group == "Reptiles") %>% 
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

fish <- name_search(text = "Atlantic Salmon", options = "namebankID")[[1]]
fish

fish_id <- name_images(uuid = fish$uid[1])

fish_pic <- image_data("9150be88-6910-4374-aa54-a7f8f3d79fb6", size = 256)[[1]]


#### Birds #####

bird <- name_search(text = "northern bobwhite", options = "namebankID")[[1]]
bird

bird_id <- name_images(uuid = bird$uid[1])

bird_pic <- image_data("cb1f00e7-0f74-43f0-89b0-7c1b8ed8e365", size = 256)[[1]]

###### Reptiles ####

reptile <- name_search(text = "snapping turtle", options = "namebankID")[[1]]
reptile

reptile_id <- name_images(uuid = reptile$uid[3])

reptile_pic <- image_data("fec4ae89-b3ea-4693-b63e-85dad8bc9ed3", size = 256)[[1]]

##### Flower ####

flower <- name_search(text = "orchid", options = "namebankID")[[1]]
flower

flower_id <- name_images(uuid = flower$uid[1])

flower_pic <- image_data("bb459b30-2370-4b7f-a1f9-f0d836aa35d1", size = 256)[[1]]


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

lichen <- name_search(text = "lichen", options = "namebankID")[[1]]
lichen

lichen_id <- name_images(uuid = lichen$uid[1])

lichen_pic <- image_data("2ab612df-c754-4db8-88e3-f8ae1b1f2a44", size = 256)[[1]]




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
