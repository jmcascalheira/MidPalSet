library(tidyverse)
library(tidyr)
library(MASS)
library(ggpubr)
library(ggsci)
library(factoextra)

# Load files
all_sites <- read.csv("analysis/data/raw_data/all_sites.csv")
short_sample <- read.csv("analysis/data/raw_data/sites_short_list.csv")

# All sites
all_site_type <- all_sites %>%
  filter(!is.na(Type)) %>%
  mutate(top3 = Type %>% fct_lump(3) %>% fct_infreq() %>% fct_rev()) %>%
  count(top3) %>%
  mutate(prop = n / sum(n))

jpeg("all_sites_type.jpeg", units="in", width=8, height=5, res=300)

all_site_type %>%
  ggplot(aes(top3, prop)) +
  geom_col(fill="#999999") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Percent of sites"
  ) +
  theme_light() +
  coord_flip()+
  theme(text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("All sites")

dev.off()


# Short sample
short_site_type <- short_sample %>%
  filter(!is.na(Type)) %>%
  mutate(Type = Type %>% fct_infreq() %>% fct_rev()) %>%
  count(Type) %>%
  mutate(prop = n / sum(n))


jpeg("analysis/figures/short_sites_type.jpeg", units="in", width=8, height=5, res=300)

short_site_type %>%
  ggplot(aes(Type, prop)) +
  geom_col(aes(fill=Type)) +
  scale_fill_manual(values = c("Cave" = "red", "Open air" = "blue")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Percent of sites"
  ) +
  theme_light() +
  coord_flip()+
  theme(text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Site type")

dev.off()


# Short sample chronology

short_site_chronology <- short_sample %>%
  filter(!is.na(Chronology)) %>%
  mutate(Chronology = Chronology %>% fct_relevel(c('MIS 3', 'MIS 3/MIS 4', 'MIS 4', 'MIS 5', '> MIS 5'))) %>%
  group_by(Type) %>%
  count(Chronology)


jpeg("analysis/figures/chronology.jpeg", units="in", width=8, height=5, res=300)

short_site_chronology %>%
  ggplot(aes(Chronology, n, fill = Type)) +
  geom_col(position = "dodge", width = .9) +
  scale_fill_manual(values = c("Cave" = "red", "Open air" = "blue")) +
  labs(
    x = NULL,
    y = "Number of sites"
  ) +
  theme_light() +
  coord_flip()+
  theme(text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Chronology")


dev.off()



# Stone tool data

short_site_levallois <- short_sample %>%
  filter(Lithics.data != "No") %>%
  filter(!is.na(Bifaces)) %>%
  group_by(Type) %>%
  count(Levallois) %>%
  mutate(prop =  n / sum(n))

short_site_bifaces <- short_sample %>%
  filter(Lithics.data != "No") %>%
  filter(!is.na(Bifaces)) %>%
  group_by(Type) %>%
  count(Bifaces) %>%
  mutate(prop =  n / sum(n))

short_site_stone <- short_sample %>%
  filter(Lithics.data != "No") %>%
  filter(!is.na(Bifaces)) %>%
  group_by(Type, Levallois, Bifaces) %>%
  count()

# Plot levallois

jpeg("levallois.jpeg", units="in", width=8, height=5, res=300)

short_site_levallois %>%
  ggplot(aes(Type, prop, fill = Levallois)) +
  geom_col() +
  labs(
    x = NULL,
    y = "Percent of sites"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Levallois")+
  scale_fill_manual(values=c("#999999", "#E69F00"))

dev.off()


jpeg("bifaces.jpeg", units="in", width=8, height=5, res=300)

short_site_bifaces %>%
  ggplot(aes(Type, prop, fill = Bifaces)) +
  geom_col() +
  labs(
    x = NULL,
    y = "Percent of sites"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Bifaces") +
  scale_fill_manual(values=c("#999999", "#E69F00"))

dev.off()



#####################################
# ELEVATION

site_altitude <- read.csv("analysis/data/raw_data/spatial_data/site_altitude.csv", sep = ";")
random_360_cut_altitude <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_altitude.csv")
caves_altitude <- read.csv("analysis/data/raw_data/spatial_data/caves_altitude.csv")

elevation <- left_join(site_altitude, short_sample, by = "SITE")
elevation <- elevation %>% dplyr::select(SITE, MEAN, Type, Chronology, Dominant.Raw.Material)

random_360_cut_altitude$Type <- "Random points"
caves_altitude$Type <- "UP caves"


open_elevation <- bind_rows(elevation, random_360_cut_altitude)
open_elevation <- open_elevation %>% filter(!is.na(Type)) %>% filter(Type %in% c("Open air", "Random points"))

cave_elevation <- bind_rows(elevation, caves_altitude)
cave_elevation <- cave_elevation %>% filter(!is.na(Type)) %>% filter(Type %in% c("Cave", "UP caves"))


jpeg("analysis/figures/open_elevation.jpeg", units="in", width=8, height=6, res=300)

open_elevation %>%
  ggplot(aes(x = Type, y = MEAN, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .7, label.y = 1300) +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position =  "none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Elevation") +
  ylab("m.a.s.l") + xlab("")

dev.off()


jpeg("analysis/figures/cave_elevation.jpeg", units="in", width=8, height=5, res=300)

cave_elevation %>%
  ggplot(aes(x = Type, y = MEAN, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .7, label.y = 650) +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Elevation")+
  ylab("m.a.s.l") + xlab("")

  dev.off()


#####################################
# ASPECT

site_aspect <- read.csv("analysis/data/raw_data/spatial_data/site_aspect.csv", sep = ";")
random_360_aspect <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_aspect.csv")
caves_aspect <- read.csv("analysis/data/raw_data/spatial_data/caves_aspect.csv")


site_aspect$MEAN_cat <- cut(site_aspect$MEAN,
                              breaks = c(-Inf, 0, 22.4, 67.4, 112.4, 157.4,
                                         202.4, 247.4, 292.4, 337.4, Inf),
                              labels = c("Flat", "North", "Northeast", "East", "Southeast", "South",
                                         "Southwest", "West", "Northwest", "North"),
                              right = FALSE)

random_360_aspect$MEAN_cat <- cut(random_360_aspect$MEAN,
                            breaks = c(-Inf, 0, 22.4, 67.4, 112.4, 157.4,
                                       202.4, 247.4, 292.4, 337.4, Inf),
                            labels = c("Flat", "North", "Northeast", "East", "Southeast", "South",
                                       "Southwest", "West", "Northwest", "North"),
                            right = FALSE)

caves_aspect$MEAN_cat <- cut(caves_aspect$MEAN,
                            breaks = c(-Inf, 0, 22.4, 67.4, 112.4, 157.4,
                                       202.4, 247.4, 292.4, 337.4, Inf),
                            labels = c("Flat", "North", "Northeast", "East", "Southeast", "South",
                                       "Southwest", "West", "Northwest", "North"),
                            right = FALSE)

aspect <- left_join(site_aspect, short_sample, by = "SITE")
aspect <- aspect %>% dplyr::select(SITE, MEAN_cat, Type)

random_360_aspect$Type <- "Random points"
caves_aspect$Type <- "UP caves"


open_aspect <- bind_rows(aspect, random_360_aspect)
open_aspect <- open_aspect %>% filter(!is.na(Type)) %>% filter(Type %in% c("Open air", "Random points"))

cave_aspect <- bind_rows(aspect, caves_aspect)
cave_aspect <- cave_aspect %>% filter(!is.na(Type)) %>% filter(Type %in% c("Cave", "UP caves"))


# Test
tbl <- table(open_aspect$Type, open_aspect$MEAN_cat)
aspect_f_test <- fisher.test(tbl, simulate.p.value = TRUE)
pval <- aspect_f_test$p.value


# Plot
plot_open_aspect <- open_aspect %>%
  group_by(Type, MEAN_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))


jpeg("analysis/figures/open_aspect.jpeg", units="in", width=10, height=5, res=300)

plot_open_aspect %>%
  ggplot(aes(x = MEAN_cat, y = prop, fill = Type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "", labels = scales::percent) +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(strip.background = element_blank()) +
  theme_light() +
  theme(legend.title = element_blank(), axis.title.y=element_blank(), text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Aspect") +
  annotate("text", x=2.3, y=.17,
           label=paste("Fisher, p = ", round(pval,3)), size = 6) +
  coord_flip()

dev.off()


# Test
tbl <- table(cave_aspect$Type, cave_aspect$MEAN_cat)
aspect_f_test <- fisher.test(tbl, simulate.p.value = TRUE)
pval <- aspect_f_test$p.value


# Plot
plot_cave_aspect <- cave_aspect %>%
  group_by(Type, MEAN_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))


jpeg("analysis/figures/cave_aspect.jpeg", units="in", width=10, height=5, res=300)

plot_cave_aspect %>%
  ggplot(aes(x = MEAN_cat, y = prop, fill = Type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    name = "",
    labels = scales::percent) +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(strip.background = element_blank()) +
  theme_light() +
  theme(legend.title = element_blank(), axis.title.y=element_blank(), text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Aspect") +
  annotate("text", x=2.4, y=.21,
           label=paste("Fisher, p = ", round(pval,3)), size = 6) +
  coord_flip()

dev.off()



################################################################################
# SLOPE

site_slope <- read.csv("analysis/data/raw_data/spatial_data/site_slope.csv", sep = ";")
random_360_slope <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_slope.csv")
caves_slope <- read.csv("analysis/data/raw_data/spatial_data/caves_slope.csv")

slope <- left_join(site_slope, short_sample, by = "SITE")
slope <- slope %>% dplyr::select(SITE, MEAN, Type)

random_360_slope$Type <- "Random points"
caves_slope$Type <- "UP caves"

open_slope <- bind_rows(slope, random_360_slope)
open_slope <- open_slope %>% filter(!is.na(Type)) %>% filter(Type %in% c("Open air", "Random points"))

cave_slope <- bind_rows(slope, caves_slope)
cave_slope <- cave_slope %>% filter(!is.na(Type)) %>% filter(Type %in% c("Cave", "UP caves"))


jpeg("analysis/figures/open_slope.jpeg", units="in", width=10, height=5, res=300)

open_slope %>%
  ggplot(aes(x = Type, y = MEAN, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .7, label.y = 35) +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Slope") +
  ylab("") + xlab("")

dev.off()


jpeg("analysis/figures/cave_slope.jpeg", units="in", width=10, height=5, res=300)

cave_slope %>%
  ggplot(aes(x = Type, y = MEAN, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .7, label.y = 49) +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Slope") +
  ylab("") + xlab("")

dev.off()



####################################################################
# RIVERS

site_river <- read.csv("analysis/data/raw_data/spatial_data/site_all_river.csv")
random_360_river <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_river.csv")
caves_river <- read.csv("analysis/data/raw_data/spatial_data/caves_river.csv")

site_river <- mutate(site_river, Distance = DISTANCE..M.)

river <- left_join(site_river, short_sample, by = "SITE")
river <- river %>% dplyr::select(SITE, Distance, Type, Chronology, `Raw Material` = Dominant.Raw.Material, Latitude, Longitude)

random_360_river$Type <- "Random points"
caves_river$Type <- "UP caves"

open_river <- bind_rows(river, random_360_river)
open_river <- open_river %>% filter(!is.na(Type)) %>% filter(Type %in% c("Open air", "Random points"))

cave_river <- bind_rows(river, caves_river)
cave_river <- cave_river %>% filter(!is.na(Type)) %>% filter(Type %in% c("Cave", "UP caves"))


jpeg("analysis/figures/open_river.jpeg", units="in", width=10, height=6, res=300)

open_river %>%
  ggplot(aes(x = Type, y = Distance, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .6, label.y = 3500) +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position = "none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Distance to river") +
  ylab("meters") + xlab("")

dev.off()


jpeg("analysis/figures/cave_river.jpeg", units="in", width=10, height=6, res=300)

cave_river %>%
  ggplot(aes(x = Type, y = Distance, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_jitter(aes(shape = `Raw Material`, size =3)) +
  theme_light() +
  stat_compare_means(method = "wilcox.test", size = 6, label.x = .6, label.y = 4800) +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position = "none", text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Distance to river") +
  labs(shape = "Dominant Raw Material") +
  scale_shape_manual(values=c(0, 17, 3, 4)) +
  ylab("meters") + xlab("") +
  guides(shape = guide_legend(override.aes = list(size=5)))

dev.off()


#######################################################################
# Lithology vs Raw Material

site_lithology <- read.csv("analysis/data/raw_data/spatial_data/site_lithology.csv")

lithology_data <- site_lithology %>%
  dplyr::select(Background, Type = TYPE_, Dominant.Raw.Material) %>%
  drop_na(Dominant.Raw.Material) %>%
  filter(Type == "Open air")

lithology_data <- lithology_data %>%
  group_by(Background, Type) %>%
  count(Dominant.Raw.Material) %>%
  mutate(prop = n/sum(n))


jpeg("analysis/figures/lithology.jpeg", units="in", width=10, height=6, res=300)

lithology_data %>%
  ggplot(aes(Background, prop, fill = Dominant.Raw.Material)) +
  geom_bar(stat = "identity", position = "dodge", width =.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Background lithology",   y = "", fill = "Raw Material") +
  theme_light() +
  facet_wrap("Type") +
  theme(strip.text = element_text(size=15, color = "black"),text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Raw Material vs Lithology") +
  scale_fill_jco()

dev.off()

tb <- table(lithology_data$Background, lithology_data$Dominant.Raw.Material)



###########################################################
## PCA

pca_data <- left_join(short_sample, site_altitude, by = "SITE") %>%
  left_join(., site_aspect, by='SITE') %>%
  left_join(., site_slope, by='SITE') %>%
  left_join(., site_river, by='SITE')

pca_data <- pca_data %>%
  dplyr::select(SITE, Latitude, Longitude, Type, Chronology, Dominant.Raw.Material,
                Altitude = MEAN.x, Aspect = MEAN.y, Slope = MEAN,
                Distance = DISTANCE..M.)

pca_data_active <- dplyr::select(pca_data, Altitude, Latitude,
                                  Aspect, Slope, Distance)


res.pca <- prcomp(pca_data_active, scale = TRUE)

groups <- as.factor(pca_data$Dominant.Raw.Material)

jpeg("analysis/figures/pca.jpeg", units="in", width=10, height=6, res=300)

fviz_pca_biplot(res.pca,
             col.ind = groups, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Raw Material",
             repel = TRUE,
             label = "var",
             pointsize = 3
) +
  theme(text = element_text(size=15), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm"))

dev.off()

  # Results for variables
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}

loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev))
var.coord[, 1:3]
