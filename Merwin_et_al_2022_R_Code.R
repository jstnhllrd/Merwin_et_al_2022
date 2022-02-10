#R Code for Merwin et al. Oh, the places you will grow: intraspecific latitudinal clines in butterfly size suggest a phylogenetic signal

###Load required packages

require(data.table)
require(maps)
require(mapdata)
require(ape)
require(ggplot2)
require(ggthemes)
require(mgcv)
require(phylobase)
require(phylosignal)
require(caper)
require(party)
require(dplyr)
require(moreparty)

#loading the full dataset of bioclim variables and bfly wings
bfs <- read.csv("YOUR FILE PATH/Merwin_et_al_2022_butterfly.csv", header = TRUE)

#Make data a data.table 
setDT(bfs)

#Load nitrogen data and make a data table
meanN <- read.csv("YOUR FILE PATH/Merwin_et_al_2022_nitrogenEstimates.csv", header = TRUE)
meanN<-setDT(meanN)

#Load phylogenetic tree
tree <- read.tree("YOUR FILE PATH/Merwin_et_al_2022_butterfly.nwk")
#Renaming the Labels of the tree
tree$tip.label<-c("A_mormo",
                   "F_tarquinius",
                   "S_edwardsii",
                   "C_irus",      
                   "C_gryneus",
                   "C_niphon",
                   "G_piasus",
                   "G_lygdamus",
                   "A_glandon")

###Figure 2. Maps of specimens for each species

#Reorder species to reflect phylogeny and rename
bfs$spCode <- factor(bfs$spCode, levels = c(
  "A_mormo",
  "F_tarquinius",
  "A_glandon",
  "G_lygdamus",
  "G_piasus",
  "S_edwardsii",
  "C_irus",
  "C_niphon",
  "C_gryneus"))

spNames<-c("A_glandon"="Agriades glandon",
           "A_mormo"="Apodemia mormo",
           "C_gryneus"="Callophrys gryneus",
           "C_irus"="Callophrys irus",
           "C_niphon"="Callophrys niphon",
           "F_tarquinius"="Feniseca tarquinius",
           "G_lygdamus"="Glaucopsyche lygdamus",
           "S_edwardsii"="Satyrium edwardsii",
           "G_piasus"="Glaucopsyche piasus")

#Load map data
usa <- map_data("worldHires","USA")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

#Produce maps
NAmap <- ggplot(data = bfs, aes(x = long, y = lat, group = spCode)) +
  facet_wrap(~spCode, labeller = as_labeller(spNames))+
  geom_polygon(data = usa,
               aes(x=long, y = lat, group = group),
               fill = "grey") +
  geom_polygon(data = canada,
               aes(x=long, y = lat, group = group),
               fill = "grey") +
  geom_polygon(data = mexico,
               aes(x=long, y = lat, group = group),
               fill = "grey") +
  coord_fixed(xlim = c(-150, -55),  ylim = c(25, 65), ratio = 1.5)+
  scale_color_gradient(low = "yellow", high = "red", na.value = NA)+
  geom_point(data = bfs, aes(x = jitter(lon, amount = 2), y = lat, color = FW_avg), size = 0.5) +
  theme_minimal() +
  labs(color = "average\nforewing\nlength (mm)") +
  theme(strip.text = element_text(face = "italic"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 8),
        legend.title.align = 0.5,
        legend.key.size = unit(0.5,'cm'),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank())


### Random Forest Models, Figure 3, and ALE plots (Appendix figures)

# Example Random Forest Model generation code for A. glandon
glandon.fw.cforest <- cforest(FW_avg~lat+lon+Temp+Prec+elevation_geonames, data = bfly.fw[bfly.fw$spCode=="A_glandon",], set.seed(37), controls = cforest_unbiased(ntree=3000,mtry=3))
glandon.fw.cforest.72 <- cforest(FW_avg~lat+lon+Temp+Prec+elevation_geonames, data = bfly.fw[bfly.fw$spCode=="A_glandon",], set.seed(72), controls = cforest_unbiased(ntree=3000,mtry=3))

# Figure 3
importance.data <- read.csv("YOUR FILE PATH/importance.csv", na.strings = "NULL", stringsAsFactors = T)

importance.data$varimp[importance.data$varimp<0] <- 0 #Negative importance values indicate no importance, so they were forced to be zero. See link for explanation: https://stats.stackexchange.com/questions/52482/variable-importance-randomforest-negative-values 

importance.data$species <- factor(importance.data$species, levels = rev(c(
  "mormo",
  "tarquinius",
  "glandon",
  "lygdamus",
  "piasus",
  "edwardsii",
  "irus",
  "niphon",
  "gryneus")))

sp.Names<-c("glandon"="A. glandon",
            "mormo"="A. mormo",
            "gryneus"="C. gryneus",
            "irus"="C. irus",
            "niphon"="C. niphon",
            "tarquinius"="F. tarquinius",
            "lygdamus"="G. lygdamus",
            "edwardsii"="S. edwardsii",
            "piasus"="G. piasus")
covarNames<-c("elevation"="Elevation",
              "lat"="Latitude",
              "lon"="Longitude",
              "Prec"="Precipitation",
              "Temp"="Temperature")

ggplot(data = importance.data, aes(x=predictor, y=species, size = varimp)) +
  geom_point()+ scale_size_area(max_size = 25, name = str_wrap("Variable Importance (Permuted Mean Squared Error)",20)) + 
  theme_bw() +
  labs(y="Species", x="Predictor") +
  scale_y_discrete(labels=sp.Names) +
  scale_x_discrete(labels=covarNames) +
  theme(axis.text.y = element_text(face = "italic", size = 11),
        axis.text.x = element_text(size = 11),
        legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 11),
        legend.title.align = 0.5,
        axis.title = element_text(size = 12,face = "bold"))


## ALE plots

# Example of ALE data generation
glandon.ale.data <- GetAleData(glandon.fw.cforest)

# All ALE data were compiled into a single file
ale.data <- read.csv("YOUR FILE PATH/ale.csv", na.strings = "NULL", stringsAsFactors = T)

# Order taxa to reflect phylogenetic tree arrangement
ale.data$sp <- factor(ale.data$sp, levels = c(
  "mormo",
  "tarquinius",
  "glandon",
  "lygdamus",
  "piasus",
  "edwardsii",
  "irus",
  "niphon",
  "gryneus"))


# Example plot for Latitude. All other plots were generated with the same code, only modified for each covariate.
lat<-ale.data %>%
  filter(var == "lat") %>%
  ggplot()+
  geom_point(aes(x=x.value,y=ale, colour = sp), size = 2)+
  geom_line(aes(x=x.value,y=ale, colour = sp), size = 1)+
  labs(x="Latitude", y="ALE Value")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_colour_manual('Species',values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#e0bf00','#a65628','#f781bf','#999999'),labels = spNames)+
  theme(axis.text = element_text(size = 11),
        legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 11, face = "italic"),
        axis.title = element_text(size = 12,face = "bold"))+
  ylim(-1,1.5)



### Gamms for each species, results used in Table 2.
Am_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "A_mormo"])
Ft_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "F_tarquinius"])
Ag_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "A_glandon"])
Gl_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "G_lygdamus"])
Gp_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "G_piasus"])
Se_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "S_edwardsii"])
Ci_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "C_irus"])
Cn_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "C_niphon"])
Cg_mod<-gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = bfs[spCode == "C_gryneus"])

### Figure 4 Plots of wing length v latitude
#Aggregate latitudinal slopes and intercepts
latPlot<-bfs[,.(latEst = gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = .SD)$lme$coefficients$fixed[[2]], intEst = gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = .SD)$lme$coefficients$fixed[[1]]), 
             by = .(spCode)]

#To make dashed and solid lines, 1 = significant, 2 = insignificant/ marginally significant
latPlot$P<-c(1,2,1,1,1,2,2,2,2)

#Reorder species to reflect phylogeny
bfs$spCode <- factor(Measured$spCode, levels = c(
  "A_mormo",
  "F_tarquinius",
  "A_glandon",
  "G_lygdamus",
  "G_piasus",
  "S_edwardsii",
  "C_irus",
  "C_niphon",
  "C_gryneus"))

#plot figure
latPlots <- ggplot(data = bfs, aes(x = lat, y = FW_avg, group = spCode)) +
  geom_abline(data = latPlot, aes(slope = latEst, intercept = intEst, linetype = as.factor(P)))+
  geom_point(size = 0.5)+
  labs(y = "average forewing length (mm)", x = "latitude")+
  scale_x_continuous(limits=c(26,65)) + 
  scale_y_continuous(limits=c(9,20)) +
  scale_linetype_discrete(guide = FALSE) +
  facet_wrap(~spCode, labeller = as_labeller(spNames), scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(face = "italic"),
        panel.border = element_blank(),
        strip.background = element_blank())


### Phylogenetic Signal and Figure 5
#Get the estimates for latitudinal clines in forewing size
Lat.N<-bfs[,.(estimate = gamm(FW_avg~lat+s(lon), random = list(latlon=~1), data = .SD)$lme$coefficients$fixed[[2]]/mean(FW_avg, na.rm = TRUE)), 
           by = .(spCode)]

#Make sure the labels are lining up
Lat.N <- Lat.N[match(tree$tip.label, Lat.N$spCode),]

#Make a phylo4d object
p4d<-phylo4d(tree, Lat.N$estimate)

# Phylogenetic signal 
phyloSignal(p4d, methods = "all")

# Figure 5 Barplot phylogeny figure
barplot.phylo4d(p4d, tree.type = "phylo", scale = FALSE, center = FALSE,tree.ladderize = TRUE,
                trait.labels = "change of forewing length proportion with latitude",
                trait.bg.col = "white",
                trait.cex = 0.75,
                tip.labels = c("Apodemia mormo",
                               "Feniseca tarquinius",
                               "Satyrium edwardsii",
                               "Callophrys irus",      
                               "Callophrys gryneus",
                               "Callophrys niphon",
                               "Glaucopsyche piasus",
                               "Glaucopsyche lygdamus",
                               "Agriades glandon"))

### PGLS and Figure 6

#Make sure Nitrogen data lines up.
meanN <- meanN[match(tree$tip.label, meanN$spCode),]

#Combine latitude and Nitrogen Data
Lat.N<-as.data.frame(Lat.N)
Lat.N$nitrogen <- meanN$N_avg

#Create comparative data set
comp.data<-comparative.data(tree, Lat.N, names.col = "spCode",vcv.dim=2, warn.dropped=TRUE)

#Run pgls
mod<-pgls(estimate~Lat.N$nitrogen, data=comp.data)

# Figure 6 Latitudinal slope of wing length with nitrogen

ggplot(data = Lat.N, aes(y = estimate, x = nitrogen))+
  geom_point()+
  labs(y = "change of forewing length proportion with latitude", x = "% nitrogen of host")+
  geom_text(aes(label=spCode),hjust=c(-0.15,1.1,-0.15,-0.15,-0.15,-0.15,-0.15,-0.15,-0.15),
            vjust=0.4, size = 2.5)+
  theme_classic()

