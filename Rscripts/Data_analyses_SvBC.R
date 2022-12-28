library("mgcv")
library("dplyr")
library("DHARMa")
library("ggplot2")
library("sjPlot")
library("corrplot")
library("effects")


# ----------------------------------------------------------------------------------------
#### Data preparation ####
# ----------------------------------------------------------------------------------------


#### Data preparation -- Model 10-km #### 
# red_EU<- read.csv("Data\\Loc_env_final.csv", header = T, sep = ",")
red_EU<-read.csv("Data\\Data_SvBC_RedDeer.csv", header = T, sep = ",")
red_EU<- red_EU[,-1]

# Change all character to factor
red_EU[sapply(red_EU, is.character)]<- lapply(red_EU[sapply(red_EU, is.character)], as.factor) 

# Changes names dataset for easy use
names(red_EU)[names(red_EU) == "IUCN_Catergory"]<- "IUCN_Category"
names(red_EU)[names(red_EU) == "Year_publ.x"]<- "Year_publ"
names(red_EU)[names(red_EU) == "Country.x"]<- "Country"
names(red_EU)[names(red_EU) == "Latitude.x"]<- "Latitude"

#### Remove areas that are enclosures or other irrevant to our study - based on co-author #
red_EU<- red_EU[!(red_EU$Study_area == "Altopiano di Budduso" | red_EU$Study_area == "Ulassai" | red_EU$Study_area == "Montarbu" | red_EU$Study_area == "Sarcidano" | red_EU$Study_area == "Monte Arci" | red_EU$Study_area == "Tramonti" | red_EU$Study_area == "Monticolo"),]


#### Drop forest integrity and biogeographic regions - contain NA's and not important for further analyses
red_EU<- red_EU[!names(red_EU) %in% c("Forest_integrity", "Biogeographic")]


# Few NA's hunting, human_influence_index and Forest integrity.
red_EU<- na.omit(red_EU)

# add IUCN variable
red_EU$Protected<- ifelse(red_EU$IUCN_Category == "I", "Strict", 
                          ifelse(red_EU$IUCN_Category == "II", "Strict", 
                                 ifelse(red_EU$IUCN_Category == "III", "Strict", 
                                        ifelse(red_EU$IUCN_Category == "IV", "Less strict", 
                                               ifelse(red_EU$IUCN_Category == "V", "Less strict", "Not Protected")))))
red_EU$Protected<- as.factor(red_EU$Protected)

# Change Predation column as only very few observation with Bear leading to statistical difficulties - all taken together for now
red_EU$Predation_adj<- ifelse(red_EU$Predation == "None", "None", 
                              ifelse(red_EU$Predation == "All", "All", 
                                     ifelse(red_EU$Predation == "Wolf_only", "Wolf", 
                                            ifelse(red_EU$Predation == "Wolf_lynx", "Wolf/Lynx", 
                                                   ifelse(red_EU$Predation == "Lynx_only", "Lynx", "Bear")))))

red_EU$Predation_adj <- as.factor(red_EU$Predation_adj)
red_EU$Predation_adj<- relevel(red_EU$Predation_adj, ref = "None")

# Decided to only include densities obtained from papers >2000, otherwise predator presence variables too unreliable.
red_EU<- red_EU[which(red_EU$Year_publ > 2000),]

# Change hunting variable for interpretation
red_EU$hunting<- ifelse(red_EU$hunting == "0", "Non-hunted", "Hunted")
red_EU$hunting<- as.factor(red_EU$hunting)

# Set reference group predators
red_EU$Predation_adj<- relevel(red_EU$Predation_adj, ref = "None")
red_EU$Predation<- relevel(red_EU$Predation, ref = "None")



# -------------------------------------------------------------------------------------
#### Final Model  #### 
# ------------------------------------------------------------------------------------


# Bear_only, Bear-Wolf and Bear-Lynx summarized within one level "Bear", Additionally bear is represented in level where all large carnivores are present
# Differences between countries and density estimation methods not accounted for in random variable due to dispersion and variance problems

# 10km - buffer
gam10km_rev<- mgcv::gam(log(Deer_density) ~ hunting + Predation_adj + s(Human_influence_index, by = Predation_adj)+ s(NPP, by = Predation_adj)+ s(Tree_canopy_cover)+ s(NDSI_Snow_Cover) + s(Palmer_drought_summer) + Protected, data = red_EU)

sim_gam_rev<-simulateResiduals(gam10km_rev) # Tested and looks good
x11()
plot(sim_gam_rev)

anova(gam10km_rev)
summary(gam10km_rev)


# ------------------------------------------------------------------------------
##### Figures - final model 10km #####
#-------------------------------------------------------------------------------

# Hunting
Hunt_boxplot<- ggplot(data = red_EU, mapping = aes(x = hunting, y = log(Deer_density))) +
  geom_point(shape = 20, size = 1, position = "jitter", colour = "gray50") + 
  geom_boxplot(data = red_EU, alpha = 0.6, fill = "gray75", outlier.shape = NA) +
  theme_bw() + theme(legend.position = "None") + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "grey"))+ theme(axis.title.x = element_blank()) + labs(y = "Log(Red deer density)") + theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(colour = "black")) + theme(text = element_text(size = 20, colour = "black"))

# Predation
Predation_boxplot<- ggplot(data = red_EU, mapping = aes(x = Predation_adj, y = log(Deer_density))) +
  geom_point(shape = 20, size = 1, position = "jitter", colour = "gray50") + 
  geom_boxplot(data = red_EU, alpha = 0.6, fill = "gray75", outlier.shape = NA) +
  theme_bw() + theme(legend.position = "None") + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "grey"))+ theme(axis.title.x = element_blank()) + labs(y = "Log(Red deer density)")+ theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(colour = "black")) + theme(text = element_text(size = 20)) 

tiff("Results\\Hunt_Pred_10km.tiff", width = 7200, height = 3600, units = "px", res = 600, compression = "lzw")
par(mfrow=c(1,2),mar=c(5,5,1,1))

gridExtra::grid.arrange(Hunt_boxplot, Predation_boxplot, ncol = 2)

dev.off()

##### Smooth functions ####

plotsmooth <- function(fit,se.fit,S,k1,k2,lty=lty){
  P <- fit
  ps <- se.fit
  ucl <- P+1.96*ps
  lcl <- P-1.96*ps
  i.for <- c(1:(length(S)))
  i.back <- rev(i.for)
  x.polygon <- c(S[i.for],rev(S[i.for]))
  y.polygon <- c( ucl[i.for] , lcl[i.back] )
  polygon( x.polygon , y.polygon , col =k2, border = NA)
  lines( S, P[i.for], col = k1 , lwd = 3 ,lty=lty)
  # Smooth terms
}

X<- plot(gam10km_rev, residuals = T, by.resids = T)

#### HII * Predation ####
tiff("Results\\HII_Pred.tiff", width = 3200, height = 3200, units = "px", res = 600, compression = "lzw")
par(mfrow=c(1,1),mar=c(6,5,1,1),cex.axis=1.4, cex.lab = 1.8)

plot(1,1,type="n",ylim=range(c(-6,6)), xlim = range(c(red_EU$Human_influence_index))
     ,ylab="Centred red deer density (100 ha-1)", 
     xlab="", cex.lab = 1.4, las = 1)
plotsmooth(X[[2]]$fit, X[[2]]$se, X[[2]]$x,"black", adjustcolor("gray90", alpha.f = 0.6), 1) # All carnivores;sign
points(red_EU[which(red_EU$Predation=="All"),]$Human_influence_index, X[[2]]$p.resid[which(red_EU$Predation=="All")], col = "gray20", pch = 20, lwd = 1)
plotsmooth(X[[1]]$fit,X[[1]]$se,X[[1]]$x,"black",adjustcolor("gray70", alpha.f = 0.5), 2) # No carnivores;trend
points(red_EU[which(red_EU$Predation=="None"),]$Human_influence_index,X[[1]]$p.resid[which(red_EU$Predation=="None")], col = alpha("gray50", 0.4), pch = 20, lwd = 1, cex = 1.0)
abline(h=0,lty=3,lwd=1.5)
mtext("Human influence index", side = 1, outer = FALSE, cex = 1.4, line = 3, adj = 0.5)
rug(red_EU$Human_influence_index)
legend(6,-9.3, inset = c(-0.5, 0),xpd = T, legend = c("All large carnivores", "No large carnivores"), col = c("black", "black"), lty = 1:2, lwd = 3, cex = 1.02, box.lty = 0, horiz = T)

dev.off()



## NPP * Predation

tiff("Results\\NPP_Pred.tiff", width = 3200, height = 3200, units = "px", res = 600, compression = "lzw")
par(mfrow=c(1,1),mar=c(6,5,1,1),cex.axis=1.4, cex.lab = 1.8)

plot(1,1,type="n",ylim=range(c(-10,10)), xlim = range(c(red_EU$NPP))
     ,ylab="Centred red deer density (100 ha-1)", 
     xlab="", cex.lab = 1.4, las = 1)
plotsmooth(X[[8]]$fit, X[[8]]$se, X[[8]]$x,"black", adjustcolor("gray90", alpha.f = 0.6), 1) # All carnivores; not sign
points(red_EU[which(red_EU$Predation_adj=="All"),]$NPP, X[[8]]$p.resid[which(red_EU$Predation_adj=="All")], col = "gray20", pch = 20, lwd = 1)
plotsmooth(X[[7]]$fit,X[[7]]$se,X[[7]]$x,"black",adjustcolor("gray70", alpha.f = 0.5), 2) # No large carnivores: not sign
points(red_EU[which(red_EU$Predation_adj=="None"),]$NPP, X[[7]]$p.resid[which(red_EU$Predation_adj=="None")], col = alpha("gray50", 0.4), pch = 20, lwd = 1)
abline(h=0,lty=3,lwd=1.5)
mtext("Net primary productivity", side = 1, outer = FALSE, cex = 1.4, line = 3, adj = 0.5)
rug(red_EU$NPP)
legend(0.05,-15.5, inset = c(-0.5, 0),xpd = T, legend = c("All large carnivores", "No large carnivores"), col = c("black", "black"), lty = 1:2, lwd = 3, cex = 1.02, box.lty = 0, horiz = T)

dev.off()


#### Environmental plots ####

tiff("Results\\Env_all_10km_rev.tiff", width = 9600, height = 3200, units = "px", res = 600, compression = "lzw")
par(mfrow=c(1,3),mar=c(5,5,1,1),cex.axis=1.8)

plot(1,1,type="n",ylim=range(c(-3,3)), xlim = range(c(red_EU$Tree_canopy_cover))
     ,ylab="Centred red deer density (100 ha-1)", 
     xlab="", cex.lab = 2.0, las = 1)
plotsmooth(X[[13]]$fit, X[[13]]$se, X[[13]]$x,"black", adjustcolor("gray75", alpha.f = 0.6), 1) 
points(red_EU$Tree_canopy_cover, X[[13]]$p.resid, col = alpha("gray50", 0.4), pch = 20, lwd = 1)
abline(h=0,lty=3,lwd=1.5)
abline(h=0,lty=3,lwd=1.5)
mtext("Tree canopy cover", side = 1, outer = FALSE, cex = 1.4, line = 3, adj = 0.5)
rug(red_EU$Tree_canopy_cover)

plot(1,1,type="n",ylim=range(c(-3,3)), xlim = range(c(red_EU$NDSI_Snow_Cover))
     ,ylab="Centred red deer density (/100 ha)", 
     xlab="", cex.lab = 2.0, las = 1)
plotsmooth(X[[14]]$fit, X[[14]]$se, X[[14]]$x, "black", adjustcolor("gray75", alpha.f = 0.6), 2)
points(red_EU$NDSI_Snow_Cover, X[[14]]$p.resid, col = alpha("gray50", 0.4), pch = 20, lwd = 1)
abline(h=0,lty=3,lwd=1.5)
mtext("NDSI", side = 1, outer = FALSE, cex = 1.4, line = 3, adj = 0.5)
rug(red_EU$NDSI_Snow_Cover)

plot(1,1,type="n",ylim=range(c(-3,3)), xlim = range(c(red_EU$Palmer_drought_summer))
     ,ylab="Centred red deer density (/100 ha)", 
     xlab="", cex.lab = 2.0, las = 1)
plotsmooth(X[[15]]$fit, X[[15]]$se, X[[15]]$x, "black", adjustcolor("gray75", alpha.f = 0.6), 2)
points(red_EU$Palmer_drought_summer, X[[15]]$p.resid, col = alpha("gray50", 0.4), pch = 20, lwd = 1)
abline(h=0,lty=3,lwd=1.5)
mtext("Palmer drought index", side = 1, outer = FALSE, cex = 1.4, line = 3, adj = 0.5)
rug(red_EU$Palmer_drought_summer)

dev.off()



# --------------------------------------------------------------------------------------
#### Model - 5km  - Supporting material
# -------------------------------------------------------------------------------------

red_EU_5km_rev<- read.csv("Data\\Supp_SvBC_RedDeer_5km.csv", header = T, sep = ",")
red_EU_5km_rev<- red_EU_5km_rev[,-1]


# Change all character to factor
red_EU_5km_rev[sapply(red_EU_5km_rev, is.character)]<- lapply(red_EU_5km_rev[sapply(red_EU_5km_rev, is.character)], as.factor) 

names(red_EU_5km_rev)[names(red_EU_5km_rev) == "IUCN_Catergory"]<- "IUCN_Category"

#### Remove areas stated by Marco Apollonio #
red_EU_5km_rev<- red_EU_5km_rev[!(red_EU_5km_rev$Study_area == "Altopiano di Budduso" | red_EU_5km_rev$Study_area == "Ulassai" | red_EU_5km_rev$Study_area == "Montarbu" | red_EU_5km_rev$Study_area == "Sarcidano" | red_EU_5km_rev$Study_area == "Monte Arci" | red_EU_5km_rev$Study_area == "Tramonti" | red_EU_5km_rev$Study_area == "Monticolo"),]


red_EU_5km_rev$Year_publ[is.na(red_EU_5km_rev$Year_publ)]<- "3000"
red_EU_5km_rev$Year_publ<- as.numeric(red_EU_5km_rev$Year_publ)

#### Drop forest integrity and biogeographic regions - contain NA's and not important for further analyses
red_EU_5km_rev<- red_EU_5km_rev[!names(red_EU_5km_rev) %in% c("Forest_integrity", "Biogeographic")]

# Few NA's hunting, human_influence_index and Forest integrity.
red_EU_5km_rev<- na.omit(red_EU_5km_rev)

#### Further data preparation after exploration models

red_EU_5km_rev$hunting<- as.factor(red_EU_5km_rev$hunting)

# add IUCN variable
red_EU_5km_rev$Protected<- ifelse(red_EU_5km_rev$IUCN_Category == "I", "Strict", 
                              ifelse(red_EU_5km_rev$IUCN_Category == "II", "Strict", 
                                     ifelse(red_EU_5km_rev$IUCN_Category == "III", "Strict", 
                                            ifelse(red_EU_5km_rev$IUCN_Category == "IV", "Less strict", 
                                                   ifelse(red_EU_5km_rev$IUCN_Category == "V", "Less strict", "Not Protected")))))
red_EU_5km_rev$Protected<- as.factor(red_EU_5km_rev$Protected)

# Change Predation column as only very few observation with Bear, so all taken together for now
red_EU_5km_rev$Predation_adj<- ifelse(red_EU_5km_rev$Predation == "None", "None", 
                                  ifelse(red_EU_5km_rev$Predation == "All", "All", 
                                         ifelse(red_EU_5km_rev$Predation == "Wolf_only", "Wolf", 
                                                ifelse(red_EU_5km_rev$Predation == "Wolf_lynx", "Wolf/lynx", 
                                                       ifelse(red_EU_5km_rev$Predation == "Lynx_only", "Lynx", "Bear")))))

red_EU_5km_rev$Predation_adj <- as.factor(red_EU_5km_rev$Predation_adj)



# Decided to only include densities obtained from papers >2000, otherwise predator presence variables too unreliable.
red_EU_5km_rev<- red_EU_5km_rev[which(red_EU_5km_rev$Year_publ > 2000),]

# Change hunting variable for interpretation
red_EU_5km_rev$hunting<- ifelse(red_EU_5km_rev$hunting == "0", "Non-hunted", "Hunted")
red_EU_5km_rev$hunting<- as.factor(red_EU_5km_rev$hunting)

# Set reference group predators
red_EU_5km_rev$Predation_adj<- relevel(red_EU_5km_rev$Predation_adj, ref = "None")
red_EU_5km_rev$Predation<- relevel(red_EU_5km_rev$Predation, ref = "None")


#### Model # -------------------------------------------------------------------
#### 5-km

gam5km_rev<- mgcv::gam(log(Deer_density) ~ hunting + Predation_adj + s(Human_influence_index, by = Predation_adj)+ s(NPP, by = Predation_adj)+ s(Tree_canopy_cover)+ s(NDSI_Snow_Cover) + s(Palmer_drought_summer) + Protected, data = red_EU_5km_rev)

sim_gam_rev<-simulateResiduals(gam5km_rev) # Tested and looks good
x11()
plot(sim_gam_rev)

anova(gam5km_rev)
summary(gam5km_rev)

