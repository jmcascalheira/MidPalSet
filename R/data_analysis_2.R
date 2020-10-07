library(tidyverse)
library(tidyr)
library(MASS)
library(ggpubr)
library(ggsci)
library(factoextra)
library(plyr)
library(knitr)
library(tab)
library(arsenal)
library(gridExtra)
library(psych)
library(rela)



# Load files
short_sample <- read.csv("analysis/data/raw_data/sites_short_list.csv")

site_altitude <- read.csv("analysis/data/raw_data/spatial_data/site_altitude.csv", sep = ";")
random_360_cut_altitude <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_altitude.csv")
caves_altitude <- read.csv("analysis/data/raw_data/spatial_data/all_caves_elevation.csv")

site_aspect <- read.csv("analysis/data/raw_data/spatial_data/site_aspect.csv", sep = ";")
random_360_aspect <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_aspect.csv")
caves_aspect <- read.csv("analysis/data/raw_data/spatial_data/all_caves_aspect.csv")

site_slope <- read.csv("analysis/data/raw_data/spatial_data/site_slope.csv", sep = ";")
random_360_slope <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_slope.csv")
caves_slope <- read.csv("analysis/data/raw_data/spatial_data/all_caves_slope.csv")

site_river <- read.csv("analysis/data/raw_data/spatial_data/site_all_river.csv")
random_360_river <- read.csv("analysis/data/raw_data/spatial_data/random_360_cut_river.csv")
caves_river <- read.csv("analysis/data/raw_data/spatial_data/all_caves_river.csv")

site_lithology <- read.csv("analysis/data/raw_data/spatial_data/site_lithology.csv")



# General table

elevation <- site_altitude %>%
  dplyr::select(SITE, Elevation = MEAN)

aspect <- site_aspect %>%
  dplyr::select(SITE, Aspect = MEAN)

slope <- site_slope %>%
  dplyr::select(SITE, Slope = MEAN)

river <- site_river %>%
  dplyr::select(SITE, Distance = DISTANCE..M.)

general_tab <- join_all(list(elevation, aspect, slope, river, short_sample), by='SITE', type='left')


general_table <- as.data.frame(tabmulti(data = general_tab, xvarname = "Type",
              yvarnames = c("Elevation", "Aspect", "Slope", "Distance"),
              ymeasures = c("median", "median", "median", "median")))




# Wilcox tests


# Caves

caves <- general_tab %>%
  filter(Type == "Cave") %>%
  dplyr::select(Type, SITE, Elevation, Aspect, Slope, Distance)


Allcaves_elevation <- caves_altitude %>%
  dplyr::select(SITE, Elevation = MEAN)

Allcaves_aspect <- caves_aspect %>%
  dplyr::select(SITE, Aspect = MEAN)

Allcaves_slope <- caves_slope %>%
  dplyr::select(SITE, Slope = MEAN)

Allcaves_river <- caves_river %>%
  dplyr::select(SITE = `ï..SITE`, Distance)


Allcaves <- join_all(list(Allcaves_elevation, Allcaves_aspect, Allcaves_slope, Allcaves_river), by = "SITE", type = "left")

Allcaves <- tibble::add_column(Allcaves, Type = "All Caves")


caves_test <- rbind(caves, Allcaves)

caves_test <- caves_test %>%
  dplyr::select(Type, Elevation, Aspect, Slope, Distance)


caves_kwt <- tableby(Type ~ ., data = caves_test, numeric.test="kwt")


caves_kwt <- as.data.frame(summary(caves_kwt, text = T))

caves_kwt[caves_kwt==""]<-NA

caves_kwt <- caves_kwt %>%
  drop_na("p value") %>%
  dplyr::rename(Variable = "") %>%
  dplyr::select(Variable, `Caves vs. All Caves` = "p value")



# Open air

openair <- general_tab %>%
  filter(Type == "Open air")

random_elevation <- random_360_cut_altitude %>%
  dplyr::select(ID = OBJECTID.., Elevation = MEAN)

random_aspect <- random_360_aspect %>%
  dplyr::select(ID = OBJECTID.., Aspect = MEAN)

random_slope <- random_360_slope %>%
  dplyr::select(ID = OBJECTID.., Slope = MEAN)

random_river <- random_360_river %>%
  dplyr::select(ID = OBJECTID_1.., Distance)


random <- join_all(list(random_elevation, random_aspect, random_slope, random_river),
                   by = "ID", type = "left")

random <- random %>%
  tibble::add_column(Type = "Random") %>%
  dplyr::select(-ID)

openair_test <- openair %>%
  dplyr::select(Type, Elevation, Aspect, Slope, Distance)

openair_test <- rbind(openair_test, random)



openair_kwt <- tableby(Type ~ ., data = openair_test, numeric.test="kwt")


openair_kwt <- as.data.frame(summary(openair_kwt, text = T))

openair_kwt[openair_kwt==""]<-NA

openair_kwt <- openair_kwt %>%
  drop_na("p value") %>%
  dplyr::rename(Variable = "") %>%
  dplyr::select(Variable, `Open air vs. Random points` = "p value")



# All

all_kwt <- left_join(caves_kwt, openair_kwt, by = "Variable")



# Boxplots Caves

caves_elevation <- caves_test %>%
  ggplot(aes(x = Type, y = Elevation, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Elevation")+
  ylab("m.a.s.l") + xlab("")

caves_aspect <- caves_test %>%
  ggplot(aes(x = Type, y = Aspect, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Aspect")+
  ylab("degrees") + xlab("")

caves_slope <- caves_test %>%
  ggplot(aes(x = Type, y = Slope, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Slope")+
  ylab("degrees") + xlab("")

caves_distance <- caves_test %>%
  ggplot(aes(x = Type, y = Distance, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("red", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Distance to river")+
  ylab("meters") + xlab("")

grid.arrange(caves_elevation, caves_aspect, caves_slope, caves_distance)


# Boxplot open air

openair_elevation <- openair_test %>%
  ggplot(aes(x = Type, y = Elevation, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Elevation")+
  ylab("m.a.s.l") + xlab("")

openair_aspect <- openair_test %>%
  ggplot(aes(x = Type, y = Aspect, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Aspect")+
  ylab("degrees") + xlab("")


openair_slope <- openair_test %>%
  ggplot(aes(x = Type, y = Slope, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Slope")+
  ylab("degrees") + xlab("")


openair_distance <- openair_test %>%
  ggplot(aes(x = Type, y = Distance, fill = Type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_light() +
  scale_fill_manual(values = c("blue", "grey50")) +
  theme(legend.position="none", text = element_text(size=20), plot.title = element_text(lineheight=.8, face="bold"), plot.margin=unit(c(1,1,1,1),"cm")) +
  ggtitle("Distance to river")+
  ylab("meters") + xlab("")

grid.arrange(openair_elevation, openair_aspect, openair_slope, openair_distance)



## PCA

pca_data <- left_join(short_sample, site_altitude, by = "SITE") %>%
  left_join(., site_aspect, by='SITE') %>%
  left_join(., site_slope, by='SITE') %>%
  left_join(., site_river, by='SITE')

pca_data <- pca_data %>%
  dplyr::select(SITE, Latitude, Longitude, Type, Chronology, Dominant.Raw.Material,
                Altitude = MEAN.x, Aspect = MEAN.y, Slope = MEAN,
                Distance = DISTANCE..M.)

pca_data_active <- dplyr::select(pca_data, Altitude,
                                 Aspect, Slope, Distance)


# Test assumptions
pca_Cor <- cor(pca_data_active)

# Sampling adequacy or an appropriate number of observations relative to the number of variables being examined
assumptions <- paf(as.matrix(pca_data_active), eigcrit = 1, convcrit = .001)
round(print(assumptions$KMO), 3)


# Positive determinant of the correlation or variance-covariance matrices
round(det(pca_Cor), 3)


# Sphericity or the existence of the identity matrix
bartlettTest <- cortest.bartlett(pca_Cor, n = 54)
round(bartlettTest$p.value, 3)




# Plot Results

res.pca <- prcomp(pca_data_active, scale = TRUE)

groups <- as.factor(pca_data$Dominant.Raw.Material)

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



loadings_table <- round(as.data.frame(res.pca$rotation[,1:2]), 3)





