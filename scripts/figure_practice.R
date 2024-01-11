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
  mutate(proportion = (species_estimates/total_species)) %>% 
  add_column(Country = "Canada")

canada_data

write.csv(canada_data,"data/canada_data.csv", row.names = FALSE)

colnames(usa_raw)


usa_data <- usa_raw %>% 
  group_by(Group) %>% 
  summarise(total_species = n_distinct(Common.Name),
            species_estimates = sum(num_papers > 0)) %>% 
  mutate(proportion = (species_estimates/total_species)) %>% 
  add_column(Country = "USA")

usa_data

write.csv(usa_data,"data/usa_data.csv", row.names = FALSE)


# calculate proportion of 'with detect'
data_combined <- rbind(canada_data, usa_data)



write.csv(data_combined,"data/data_combined.csv", row.names = FALSE)


# ggplot figure -----------------------------------------------------------
library(extrafont)
library(showtext)

font_import(pattern = "Helvetica")
loadfonts(device = "win")
windowsFonts()

font_add(family = "helvetica",
         regular = 'C:\\Users\\court\\AppData\\Local\\Microsoft\\Windows\\Fonts\\Helvetica Roman.ttf')
showtext_auto()

canada <- read.csv("data/canada_data.csv")
usa <- read.csv("data/usa_data.csv")


# Plot
colnames(canada)

can_pic <- canada %>% 
  mutate(Group = fct_reorder(Group, proportion, .desc = TRUE)) %>% 
ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = 0, yend = proportion),
                colour = "#0A9396", lwd = 1.5) +
  geom_point(aes(x = Group, y = proportion), colour = "#00778F",
             fill = "white", size=5, shape = 19, stroke = 1.5) +
  #geom_point( aes(x = Group, y = total_papers), colour = "#0A9396", size=5 ) +
  xlab("") +
  ylab(" ") +
  theme_classic() +
  theme(text = element_text(size = 26)) +
  ggtitle("CANADA") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme(text = element_text(family = "helvetica"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        plot.title = element_text(hjust = 0.5,size = 20))

usa_pic <- usa %>% 
  mutate(Group = fct_reorder(Group, proportion, .desc = TRUE)) %>% 
  ggplot() +
  geom_segment( aes(x = Group, xend = Group, y = 0, yend = proportion),
                colour = "#0A9396", lwd = 1.5) +
  geom_point(aes(x = Group, y = proportion), colour = "#00778F",
             fill = "white", size=5, shape = 19, stroke = 1.5) +
  xlab("") +
  ylab(" ") +
  theme_classic() +
  theme(text = element_text(size = 26, family = "helvetica"),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle("USA") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1))+
  annotate("text", x = 17, y = 0.29, label = "", size = 5) 

# Put it together

usa_pic + can_pic


ggsave("figures/combined_figure.TIFF", combine,
       width = 15.8,
       height = 7.6)


####### paired plot #######
library(ggalt)

data_combined <- read.csv("data/data_combined.csv")
colnames(data_combined)

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



###### Phylopic ############
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

bear_uuid <- get_uuid(name = "Grizzly bear", n = 1)
bear_img <- get_phylopic(uuid = bear_uuid)

#### AMPHIBIAN ####

toad <- get_uuid(name = "Bufo", n = 1)
toad_img <- get_phylopic(uuid = toad)

##### Fish ####

fish <- get_uuid(name = "Salvelinus", n = 1)
fish_img <- get_phylopic(uuid = fish)

#### Birds #####

bird <- get_uuid(name = "Charadriidae", n = 1)
bird_img <- get_phylopic(uuid = bird)

###### Reptiles ####

reptile <- get_uuid(name = "Crotalus viridis", n = 1)
reptile_img <- get_phylopic(uuid = reptile)


##### Flower ####

flower <- get_uuid(name = "Helianthus annuus", n = 1)
flower_img <- get_phylopic(uuid = flower)


##### Insects ####

insect <- get_uuid(name = "monarch",n = 1)
insect_img <- get_phylopic(uuid = insect)


###### Clams ####
clam <- get_uuid(name = "Neonucula pratasensis", n = 1)
clam_img <- get_phylopic(uuid = clam)


####### LICHEN ####

lichen <- get_uuid(name = "Lecanoromycetes", n = 1)
lichen_img <- get_phylopic(uuid = lichen)


##### SNAILS #########
snail<- get_uuid(name = "Myosotella myosotis", n = 1)
snail_img <- get_phylopic(uuid = snail)

########## Ferns and Allies ##########

fern <- get_uuid(name = "Monilophyta", n = 1)
fern_img <- get_phylopic(uuid = fern)

###### Mosses ######
 
moss <- get_uuid(name = "Takakia lepidozioides", n = 1)
moss_img <- get_phylopic(uuid = moss)

##### Crustacean #####
 
crust <- get_uuid(name = "shrimp", n = 1)
crust_img <- get_phylopic(uuid = crust)

####### "Arachnids"  ######
 
arach <- get_uuid(name = "Argiope bruennichi", n = 1)
arach_img <- get_phylopic(uuid = arach)

####### "Conifers and Cycads" #####

conifer <- get_uuid(name = "Pinaceae", n = 1)
conifer_img <- get_phylopic(uuid = conifer)

#####  IMAGE #####

USA_phylo <- usa_pic + 
  coord_flip() +
  add_phylopic(bear_img, alpha = 1, x = 8, y = 1, ysize = 2) +
  add_phylopic(toad_img, alpha = 1, x = 4, y = 1, ysize = 2) +
  add_phylopic(fish_img, alpha = 1, x = 5, y = 1, ysize = 2) +
  add_phylopic(bird_img, alpha = 1, x = 6.1, y = 1, ysize = 2) +
  add_phylopic(reptile_img, alpha = 1, x = 7, y = 1, ysize = 2) +
  add_phylopic(flower_img, alpha = 1, x = 3, y = 1, ysize = 2) +
  add_phylopic(insect_img, alpha = 1, x = 2, y = 1, ysize = 2) +
  add_phylopic(clam_img, alpha = 1, x = 1, y = 1, ysize = 2)


can_phylo <- can_pic + 
  coord_flip() +
  add_phylopic(bear_img, alpha = 1, x = 9, y = 1, ysize = 2) +
  add_phylopic(toad_img, alpha = 1, x = 5, y = 1, ysize = 2) +
  add_phylopic(fish_img, alpha = 1, x = 7, y = 1, ysize = 2) +
  add_phylopic(bird_img, alpha = 1, x = 8, y = 1, ysize = 2) +
  add_phylopic(reptile_img, alpha = 1, x = 6, y = 1, ysize = 2) +
  add_phylopic(flower_img, alpha = 1, x = 1, y = 1, ysize = 2) +
  add_phylopic(insect_img, alpha = 1, x = 4, y = 1, ysize = 2) +
  add_phylopic(clam_img, alpha = 1, x = 2, y = 1, ysize = 2) +
  add_phylopic(lichen_img, alpha = 1, x = 3, y = 1, ysize = 2)

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


##### double figure #######

usa_phylo <- usa_pic + 
  add_phylopic(arach_img, alpha = 1, x = 12, y = 0.1, ysize = 0.15) +
  add_phylopic(conifer_img, alpha = 1, x = 13, y = 0.13, ysize = 0.2) +
  add_phylopic(lichen_img, alpha = 1, x = 14, y = 0.1, ysize = 0.1) +
  add_phylopic(crust_img, alpha = 1, x = 11, y = 0.1, ysize = 0.09) +
  add_phylopic(flower_img, alpha = 1, x = 10, y = 0.15, ysize = 0.1) +
  add_phylopic(clam_img, alpha = 1, x = 9, y = 0.12, ysize = 0.09) +
  add_phylopic(insect_img, alpha = 1, x = 7.9, y = 0.15, ysize = 0.12) +
  add_phylopic(snail_img, alpha = 1, x = 7, y = 0.15, ysize = 0.11) +
  add_phylopic(fish_img, alpha = 1, x = 6, y = 0.17, ysize = 0.13) +
  add_phylopic(fern_img, alpha = 1, x = 5, y = 0.23, ysize = 0.2) +
  add_phylopic(reptile_img, alpha = 1, x = 4.2, y = 0.2, ysize = 0.1) +
  add_phylopic(bear_img, alpha = 1, x = 3, y = 0.22, ysize = 0.08) +
  add_phylopic(bird_img, alpha = 1, x = 2, y = 0.28, ysize = 0.1) +
  add_phylopic(toad_img, alpha = 1, x = 1, y = 0.4, ysize = 0.1) 
  
 
  
can_phylo <- can_pic + 
  add_phylopic(bird_img, alpha = 1, x = 1.15, y = 0.6, ysize = 0.1) +
  add_phylopic(bear_img, alpha = 1, x = 2, y = 0.44, ysize = 0.09) +
  add_phylopic(toad_img, alpha = 1, x = 3.1, y = 0.36, ysize = 0.09) +
  add_phylopic(reptile_img, alpha = 1, x = 4, y = 0.25, ysize = 0.1) +
  add_phylopic(fish_img, alpha = 1, x = 5, y = 0.22, ysize = 0.13) +
  add_phylopic(lichen_img, alpha = 1, x = 6, y = 0.17, ysize = 0.1) +
  add_phylopic(insect_img, alpha = 1, x = 7, y = 0.15, ysize = 0.1) +
  add_phylopic(clam_img, alpha = 1, x = 8, y = 0.14, ysize = 0.08) +
  add_phylopic(flower_img, alpha = 1, x = 9, y = 0.1, ysize = 0.1) +
  add_phylopic(arach_img, alpha = 1, x = 10, y = 0.1, ysize = 0.15) +
  add_phylopic(conifer_img, alpha = 1, x = 11, y = 0.14, ysize = 0.22) +
  add_phylopic(fern_img, alpha = 1, x = 12, y = 0.14, ysize = 0.22) +
  add_phylopic(moss_img, alpha = 1, x = 13, y = 0.15, ysize = 0.22) + 
  add_phylopic(snail_img, alpha = 1, x = 14, y = 0.1, ysize = 0.15)
  
  
combine_phylo <- usa_phylo + plot_spacer() + can_phylo +
  plot_layout(widths = c(5.5, -1.05, 5),
              guides = 'collect')

combine_phylo

ggsave("figures/combined_figure_phylo.TIFF",
       combine_phylo,
       width = 18.7,
       height = 10,
       dpi = 300)


ggsave("figures/combined_figure_phylo.pdf",
       combine_phylo,
       width = 18.7,
       height = 10,
       dpi = 300)
   

ggsave("figures/combined_figure_phylo.jpg",
       combine_phylo,
       width = 18.7,
       height = 10,
       dpi = 300)

ggsave("figures/combined_figure_phylo.png",
       combine_phylo,
       width = 18.7,
       height = 10,
       dpi = 300)
