### Vertebrate species richness at littoral beaver lodges

library(tidyverse)

# setwd
setwd(dirname(file.choose()))

# Get table with richness, detection rates, NDVI, weather, etc. 
all <- readxl::read_excel("Data S2.xlsx")

# Camera trap data -----------
# assign data into vectors to make it easier for model testing
L1DR <- all$`Lodge 1 Beaver DR (Days)`
L2DR <- all$`Lodge 2 Beaver DR (Days)`
L1richness <- all$`Lodge 1 Species Richness`
L2richness <- all$`Lodge 2 Species Richness`
hours <- all$`Beaver DR (Hours)`
NDVI <- as.numeric(all$`NDVI Avg.`)
air <- all$`Temperature (Celsius)`
Humidity <- all$Humidity
wt <- all$`Water Temperature (Celsius)`

# fix missing values
L1DR[L1DR == "NA"] <- NA
L1richness[L1richness == "NA"] <- NA
NDVI[NDVI == "NA"] <- NA

# check distributions
hist(air)
hist(Humidity)
hist(wt)

hist(as.numeric(L1richness))
hist(as.numeric(L1DR))
hist(as.numeric(L1richness))
hist(as.numeric(L2DR))

# it looks like detection rates could be log-transformed. *maybe* wt

library(bootStepAIC)

# loading this package will overwrite the select function in dplyr***

# Richness~Weather -------
# regression for lodge 1 species richness with weather variables -- p = 0.4614
lmFitL1richness <- lm(L1richness~air+Humidity+wt)
summary(lmFitL1richness)

boot.stepAIC(lmFitL1richness, all, B=1000, direction="both")

# regression for lodge 2 species richness with weather variables -- p = 0.9173
lmFitL2richness <- lm(L2richness~air+Humidity+wt)
summary(lmFitL2richness)

boot.stepAIC(lmFitL2richness, all, B=1000, direction="both")

# regression for lodge 1 species richness with log-transformed variables -- p = 0.4707
lmFitL1richnesslog <- lm(L1richness~air+Humidity+log(wt))
summary(lmFitL1richnesslog)

boot.stepAIC(lmFitL1richnesslog, all, B=1000, direction="both")

# regression for lodge 2 species richness with log-transformed variables -- p = 0.7084
lmFitL2richnesslog <- lm(L2richness~air+Humidity+log(wt))
summary(lmFitL2richnesslog)

boot.stepAIC(lmFitL2richnesslog, all, B=1000, direction="both")

# Beaver detections~Weather -----------
# regression for lodge 1 detection rate with all variables -- p = 0.3738
lmFitL1dr <- lm(L1DR~air+Humidity+wt)
summary(lmFitL1dr)

boot.stepAIC(lmFitL1dr, all, B=1000, direction="both")

# regression for lodge 2 detection rate with all variables -- p = 0.9097
lmFitL2dr <- lm(L2DR~air+Humidity+wt)
summary(lmFitL2dr)

boot.stepAIC(lmFitL2dr, all, B=1000, direction="both")

# regression for lodge 1 detection rate with log-transformed variables
# first log-transform lodge 1 detection rate and ignore Inf value
logL1DR <- log(as.numeric(L1DR))
logL1DR[is.infinite(logL1DR)] <- NA

lmFitL1drlog <- lm(logL1DR~air+Humidity+wt) # p = 0.464
summary(lmFitL1drlog)

boot.stepAIC(lmFitL1drlog, all, B=1000, direction="both")

# regression for lodge 1 detection rate with log-transformed variables
# first log-transform lodge 2 detection rate and ignore Inf value
logL2DR <- log(L2DR)
logL2DR[is.infinite(logL2DR)] <- NA

lmFitL2drlog <- lm(logL2DR~air+Humidity+wt) # p = 0.1174
summary(lmFitL2drlog)

boot.stepAIC(lmFitL2drlog, all, B=1000, direction="both")

# Beaver detections~NDVI ---------
# regression for lodge 1 detection rate with NDVI -- p = 0.2089
lmFitL1drNDVI <- lm(L1DR~NDVI)
summary(lmFitL1drNDVI)

# regression for lodge 2 detection rate with NDVI -- p = 0.1789
lmFitL2drNDVI <- lm(L2DR~NDVI)
summary(lmFitL2drNDVI)

# regression for lodge 1 detection rate (log-transformed) with NDVI -- p = 0.06408
lmFitL1drNDVIlog <- lm(logL1DR~NDVI)
summary(lmFitL1drNDVIlog)

# regression for lodge 2 detection rate (log-transformed) with NDVI -- p = 0.07857
lmFitL2drNDVIlog <- lm(logL2DR~NDVI)
summary(lmFitL2drNDVIlog)

# Richness~NDVI ------------
# regression for lodge 1 richness with NDVI -- p = 0.1237
lmFitL1richnessNDVI <- lm(L1richness~NDVI)
summary(lmFitL1richnessNDVI)

# regression for lodge 2 richness with NDVI -- p = 0.9112
lmFitL2richnessNDVI <- lm(L2richness~NDVI)
summary(lmFitL2richnessNDVI)

# Richness~Beaver Detections ------------
# regression of lodge 1 richness on lodge 1 beaver detection rate -- p = 0.6684
lmFitL1richnessL1DR <- lm(L1richness~L1DR)
summary(lmFitL1richnessL1DR)

# regression of lodge 2 richness on lodge 2 beaver detection rate -- p = 0.2564
lmFitL2richnessL2DR <- lm(L2richness~L2DR)
summary(lmFitL2richnessL2DR)

# regression of lodge 1 richness on log lodge 1 beaver detection rate -- p = 0.307
lmFitL1richnesslogL1DR <- lm(L1richness~logL1DR)
summary(lmFitL1richnesslogL1DR)

# regression of lodge 2 richness on lodge 2 beaver detection rate -- p = 0.712
lmFitL2richnesslogL2DR <- lm(L2richness~logL2DR)
summary(lmFitL2richnesslogL2DR)

# Plots -------------
# Insert column for season (will use to make plot of NDVI and beaver detection rate)
all <- all %>% 
  mutate(season = c("Autumn 2016", "Autumn 2016", "Winter 2016", "Winter 2017", "Winter 2017", "Spring 2017", "Spring 2017", "Spring 2017", "Summer 2017", "Summer 2017", "Summer 2017", "Autumn 2017", "Autumn 2017", "Autumn 2017", "Winter 2017", "Winter 2018", "Winter 2018"))  

library(ggrepel)

# FIG 3 log(L2DR) vs. NDVI -----
ggplot(all) +
  geom_point(mapping = aes(x = NDVI, y = logL2DR)) + 
  geom_smooth(method = "lm", mapping = aes(x = NDVI, y = logL2DR)) +
  geom_text_repel(aes(x = NDVI, y = logL2DR, label = season, force = 8), hjust = 0.2,  vjust = -0.75) +
  xlab("NDVI") +
  ylab("Lodge 2 Beaver Detection Count") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# FIG S1 plot log(L1DR) vs. NDVI -----
ggplot(all) +
  geom_point(mapping = aes(x = NDVI, y = logL1DR)) + 
  geom_smooth(method = "lm", mapping = aes(x = NDVI, y = logL1DR)) +
  geom_text_repel(aes(x = NDVI, y = logL1DR, label= season), hjust = 0.8,  vjust = 1.75) +
  xlab("NDVI") +
  ylab("Lodge 1 Beaver Detection Count") +
  scale_x_continuous(limits = c(0.0,0.8), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# import species detection data for lodge 1
library (naniar)

lodge1data <- readxl::read_excel("Data S1.xlsx", sheet = "Lodge One")

# fix all NAs and convert all species detection counts to numeric
lodge1data <- lodge1data %>% 
  replace_with_na_if(.predicate = is.character, condition = ~.x == "NA") %>% 
  type.convert(lodge1data, na.strings = "NA")

# get total sums for all species, save values in the same order in a vector
L1detections <- colSums(lodge1data[,c(2:28)], na.rm = TRUE)
L1detections <- as.vector(L1detections)

# get vector of species names
species <- colnames(lodge1data[,c(2:28)])

# make dataframe of detection counts and species
L1sum <- data.frame(L1detections, species)

# make barplot for any species with >0 detections at Lodge 1, rank species from most to least detected
L1barplot <- ggplot(L1sum[which(L1sum$L1detections>0),], aes(x=reorder(species, -L1detections), L1detections)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("") +
  scale_y_continuous(limits = c(0,60), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# import and plot species detection data for lodge 2
lodge2data <- readxl::read_excel("Data S1.xlsx", sheet = "Lodge Two")

# get total sums for all species, save values in the same order in a vector
L2detections <- colSums(lodge2data[,c(2:28)])
L2detections <- as.vector(L2detections)

# make dataframe of detection counts and species
L2sum <- data.frame(L2detections, species)

# make barplot for any species with >0 detections at Lodge 2, rank species from most to least detected
L2barplot <- ggplot(L2sum[which(L2sum$L2detections>0),], aes(x=reorder(species, -L2detections), L2detections)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("") +
  scale_y_continuous(limits = c(0,70), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Put the two barplots in same figure
library(ggplot2)
library(cowplot)
library(gridExtra)

F1 <- plot_grid(L1barplot, L2barplot, align = "h", labels = "AUTO")

# create x and y labels for new figure
y.grob <- textGrob("Total Detections (days)", 
                   gp=gpar(fontsize=12), rot=90, hjust = -0.1, vjust = 1)

x.grob <- textGrob("Species", 
                   gp=gpar(fontsize=12), vjust = -2)

# FIG 1 combine barplots for each lodge into a single figure with axes labels -----
grid.arrange(arrangeGrob(F1, left = y.grob, bottom = x.grob))

# get vector of months with correct date format
months <- as.Date(lodge1data$Month, format = "%Y-%m-%d")
months <- format(months, format="%Y-%m")

# insert reformatted vector in lodge data, remove original column for Month
lodge1data <- lodge1data %>% 
  mutate(month = months) %>% 
  select(-Month)

lodge2data <- lodge2data %>% 
  mutate(month = months) %>% 
  select(-Month)

# beaver detections per month for lodge 1
ggplot(data=lodge1data) +
  geom_line(mapping = aes(x = lodge1data$month, y = lodge1data$`Castor canadensis`), group = 1, size = 2) +
  geom_point(mapping = aes(x = lodge1data$month, y = lodge1data$`Castor canadensis`), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Month") +
  ylab("Detections (days)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# beaver detections per month for lodge 2
ggplot(data=lodge2data) +
  geom_line(mapping = aes(x = lodge2data$month, y = lodge2data$`Castor canadensis`), group = 1, size = 2) +
  geom_point(mapping = aes(x = lodge2data$month, y = lodge2data$`Castor canadensis`), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Month") +
  ylab("Detections (days)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# FIG 2 combine beaver detections from both lodges into a single figure ------
lodgesumdata <- data.frame(months, lodge1data$`Castor canadensis`, lodge2data$`Castor canadensis`)

ggplot(data=lodgesumdata) +
  geom_line(mapping = aes(x = lodgesumdata$months, y = lodgesumdata$lodge1data..Castor.canadensis.), group = 1, size = 2, linetype = "solid") +
  geom_line(mapping = aes(x = lodgesumdata$months, y = lodgesumdata$lodge2data..Castor.canadensis.), group = 1, size = 2, linetype = "twodash") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Month") +
  ylab("Detections (days)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




