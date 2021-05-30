##### Final analysis - replicates figures and results

### Load required packages
library(ggplot2)
library(tidyr)
require(dplyr)
library(ggridges)
library(magrittr)
library(caret)
require(vegan)
require(MuMIn)
require(scales)
require(berryFunctions)
require(cowplot)
require(grid)


# Univariate and Multivariate habitat use analyses ####
  focal <- read.csv("./focal_habitat_data.csv") # Read in data
  
# Descriptive analysis of diel activity and habitat use patterns
## Format data
  focal$Species <- factor(focal$Species)
  focal$Activity <- factor(focal$Activity)
  focal$Substrate <- factor(focal$Substrate, ordered = TRUE)
  focal$Focal_V[focal$Focal_V < 0] <- 0
  
## Univariate test of activity between different species in each time period ####
## First break into separate data sets for each time period
  focal_day <- focal %>% filter(., Time=="Day")
  focal_night <- focal %>% filter(., Time=="Night")
  focal_dusk <- focal %>% filter(., Time=="Dusk")
  
## Function to calculate the proportion of resting vs. foraging fish and test equality of proportions
  prop_foraging <- function(x){
    rb_forage <- sum(x$Species == "Rainbow" & x$Activity=="Foraging")
    rb_total <- sum(x$Species == "Rainbow")
    bt_total <- sum(x$Species == "Bull trout")
    bt_forage <- sum(x$Species == "Bull trout" & x$Activity=="Foraging")
    test_result <- prop.test(x = c(rb_forage, bt_forage) , n = c(rb_total,bt_total))
    return(test_result)
  }
## Run equality of proportion tests - are species doing different activites on average at different times
  day_prop_foraging <-  prop_foraging(focal_day)
  dusk_prop_foraging <- prop_foraging(focal_dusk)
  night_prop_foraging <- prop_foraging(focal_night)
  
## Tables of raw proportions to report in text 
  prop_day <- with(focal_day, table(Species, Activity)) %>% prop.table(margin = 1)
  prop_dusk <- with(focal_dusk, table(Species, Activity)) %>% prop.table(margin = 1)
  
## Tests of differences in activity over time by each species
  rb <- focal %>% filter(., Species=="Rainbow")
  bt <- focal %>% filter(., Species=="Bull trout")
  table(rb$Activity, rb$Time) # rainbow (day = 58/95, dusk = 93/96, night = 18/74)
  table(bt$Activity, bt$Time) # bull trout (day = 14/54, dusk = 60/74, night = 10/64)
  
## Proportion tests 
  rb_time_proportion <- prop.test(x = c(58, 93, 18), n=c(95, 96, 74))
  bt_time_proportion <- prop.test(x = c(14, 60, 10), n=c(54, 74, 64))
  
## Proportion tables  
  prop_rb <- with(rb, table(Time, Activity)) %>% prop.table(margin = 1)
  prop_bt <- with(bt, table(Time, Activity)) %>% prop.table(margin = 1)
  
## Tests of cover use proportions by species and through time
  cover_prop <- function(x){
    rb_use <- sum(x$Species=="Rainbow" & x$Cover_yn=="Cover")
    bt_use <- sum(x$Species=="Bull trout" & x$Cover_yn=="Cover")
    rb_total <- sum(x$Species=="Rainbow")
    bt_total <- sum(x$Species=="Bull trout")
    test_result <- prop.test(x=c(rb_use, bt_use), n=c(rb_total, bt_total))
    return(test_result)
  }
  
## Tests of cover use patterns between species and time periods within species
  fisher.test(matrix(c(82, 96-82, 54, 54-54), ncol=2)) 
  cover_prop(focal_night) #X-squared = 0.45422, df = 1, p-value = 0.5003 
  cover_prop(focal_dusk) #X-squared = 2.8964, df = 1, p-value = 0.08878 
  table(rb$Time, rb$Cover_yn) # rainbow (day = 82/95, dusk = 19/96, night = 26/74) 
  table(bt$Time, bt$Cover_yn) # bull trout (day = 54/54, dusk = 24/74, night = 27/64) 
  rb_cover_proportion <- prop.test(x = c(82, 19, 26), n=c(95, 96, 74)) 
  bt_cover_proportion <- prop.test(x = c(54, 24, 27), n=c(54, 74, 64)) 
  
  
## Proportion tables of activity
  prop_rbc <- with(rb, table(Time, Cover_yn)) %>% prop.table(margin = 1)
  prop_btc <- with(bt, table(Time, Cover_yn)) %>% prop.table(margin = 1)
  
  
## Plot of proportions of activity and cover use through time (Figure 2)
  lab <- c("Bull Trout", "Rainbow")
  names(lab) <- c("Bull trout", "Rainbow")
  g <- ggplot(focal, aes(Time)) + 
    geom_bar(aes(fill = Activity), color="black")+theme_classic() + scale_fill_manual(values=c("red","black"))+
    facet_grid(~Species, labeller = labeller(Species=lab))+ylab("")+theme(axis.text = element_text( size = 12 ),
                                                                          axis.title = element_text( size = 12 ),
                                                                          axis.text.x = element_text( size = 12 ),
                                                                          legend.text = element_text( size = 12 ),
                                                                          legend.title = element_text( size = 12 ),
                                                                          panel.border = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          strip.background = element_rect(colour="white", fill="white"),
                                                                          strip.text = element_text(size = 18, hjust = 0))
  
  
  c <- ggplot(focal, aes(Time)) +
    geom_bar(aes(fill = Cover_yn), color="black")+theme_classic() + scale_fill_manual(values=c("blue","grey"), name="Cover Presence")+
    facet_grid(~Species, labeller=labeller(Species=lab))+ylab("")+  theme(axis.text = element_text( size = 12 ),
                                                                          axis.title = element_text( size = 12 ),
                                                                          axis.text.x = element_text( size = 12 ),
                                                                          legend.text = element_text( size = 12 ),
                                                                          legend.title = element_text( size = 12 ),
                                                                          strip.text = element_blank())
  
  plot_grid(g,c, align="v", nrow=2)  # Replicates Figure 2 (without the fish images)
  
  
# Univariate tests of diel differences in habitat use ####

## Kruskal wallis tests run for species-activity groups during each time period ####
## Aggregated medians to report
  medians <- aggregate(cbind(Focal_V, Depth) ~ Species_Activity + Time, data=focal, FUN=median)
  means <- aggregate(cbind(Focal_V, Depth, Focal_D) ~ Species_Activity + Time, data=focal, FUN=mean)
  
### Focal velocity
### Day
  kruskal.test(Focal_V ~ Species_Activity, data=focal[focal$Time=="Day",]) 
  pairwise.wilcox.test(focal$Focal_V[focal$Time=="Day"], focal$Species_Activity[focal$Time=="Day"],
                       p.adjust.method = "holm") 
  
### Dusk
  kruskal.test(Focal_V ~ Species_Activity, data=focal[focal$Time=="Dusk",]) 
  pairwise.wilcox.test(focal$Focal_V[focal$Time=="Dusk"], focal$Species_Activity[focal$Time=="Dusk"],
                       p.adjust.method = "holm") 
  
### Night
  kruskal.test(Focal_V ~ Species_Activity, data=focal[focal$Time=="Night",]) 
  pairwise.wilcox.test(focal$Focal_V[focal$Time=="Night"], focal$Species_Activity[focal$Time=="Night"],
                       p.adjust.method = "holm") 
  
### Total depth use
### Day
  kruskal.test(Depth~Species_Activity, data=focal[focal$Time=="Day",]) 
  pairwise.wilcox.test(focal$Depth[focal$Time=="Day"], focal$Species_Activity[focal$Time=="Day"],
                       p.adjust.method = "holm") 
### Dusk
  kruskal.test(Depth~Species_Activity, data=focal[focal$Time=="Dusk",]) 
  pairwise.wilcox.test(focal$Depth[focal$Time=="Dusk"], focal$Species_Activity[focal$Time=="Dusk"],
                       p.adjust.method = "holm") 
  
### Night
  kruskal.test(Depth~Species_Activity, data=focal[focal$Time=="Night",]) 
  pairwise.wilcox.test(focal$Depth[focal$Time=="Night"], focal$Species_Activity[focal$Time=="Night"],
                       p.adjust.method = "holm") 


### Multi-panel plot of habitat use for each species and time period (Figure 3)
  
palette(c("lightblue","darkblue","yellow", "darkorange")) # Set colour palette

#### Velocity
  v1 <- ggplot(aes(y = Focal_V, x = Time, fill = Species:Activity), data = focal) + geom_boxplot() + labs(y=expression(paste('Velocity (m s'^'-1'~")")), x="")+ theme_bw() +
    scale_fill_manual(values = c("lightblue","darkblue","yellow", "darkorange")) + geom_jitter(aes(x= Time, fill=Species:Activity), alpha=0.1, size=2, position=position_jitterdodge(0.2))+theme(legend.position = "none")+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),plot.margin = margin(0, 0, 0, 0,"cm"))

### Depth
  ### Focal depth % 
  d1 <- ggplot(aes(y = Depth, x = Time, fill = Species:Activity), data = focal) + geom_boxplot() + labs(y=expression(paste(' Depth (m)')), x="")+ theme_bw() +
    scale_fill_manual(values = c("lightblue","darkblue","yellow", "darkorange"), name="", labels = c("Bull trout - foraging", "Bull trout - resting","Rainbow - foraging","Rainbow - resting")) + geom_jitter(aes(x= Time, fill=Species:Activity),alpha=0.1, size=2, position=position_jitterdodge(0.2))+theme(legend.position = "bottom", legend.title= element_blank())+ theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), plot.margin = margin(0, 0, 0, 0,"cm"))
 
  plot_grid(v1,d1, align = "v", nrow=2) # Recreates Figure 3
  
# Multivariate analysis ####
## Format data for input into ordination
  focal$Species_Activity <- paste(focal$Species, focal$Activity, sep = "-")
  focal$Species_Activity <- factor(focal$Species_Activity)
  focal_ord <- focal %>% select(., Species_Activity, Species, Time, Length, Activity, Depth, V_60, Focal_D, Focal_V, Substrate, Cover_yn)
  focal_ord <- na.omit(focal_ord)
## Make categorical variables numeric and standardize focal depth %
  focal_ord$Cover_yn <- ifelse(focal_ord$Cover_yn=="Cover", 1, 0)
  focal_ord$Substrate <- as.numeric(focal_ord$Substrate)
  focal_ord$Focal_D <-  focal_ord$Focal_D/100

##### CCA approach for each time period separately ####
# Day  
  day_env <- focal_ord  %>% filter(., Time=="Day") %>% select(., Depth, Focal_D, V_60, Focal_V, Substrate, Cover_yn)
  day_fish <- focal_ord  %>% filter(., Time=="Day") %>% select(., Species, Activity, Species_Activity)
  day_scaled <- data.frame(scale(day_env, scale=TRUE))
  cap_day <- capscale(day_scaled~Species + Activity, day_fish, distance="gower")
## Extract CAP scores and point locations for plotting
  scores_day <- scores(cap_day)$sites
  axes_day <- data.frame(scores(cap_day)$species)
## Test model and variance explained
  anova.cca(cap_day, by="margin", permutations = 10000)
  summary(cap_day)

# Dusk
  dusk_env <- focal_ord  %>% filter(., Time=="Dusk") %>% select(., Depth, Focal_D, V_60, Focal_V, Substrate, Cover_yn)
  dusk_fish <- focal_ord  %>% filter(., Time=="Dusk") %>% select(., Species, Activity, Species_Activity)
  dusk_scaled <- data.frame(scale(dusk_env, scale=TRUE))
  cap_dusk <- capscale(dusk_scaled~Species + Activity, dusk_fish, distance="gower")
## Extract CAP scores and point locations for plotting 
  scores_dusk <- scores(cap_dusk)$sites
  axes_dusk <- scores(cap_dusk)$species
## Test model and variance explained
  anova.cca(cap_dusk, by="margin", permutations = 10000)
  summary(cap_dusk)

# Night 
  night_env <- focal_ord  %>% filter(., Time=="Night") %>% select(., Depth, Focal_D, V_60, Focal_V, Substrate, Cover_yn)
  night_fish <- focal_ord  %>% filter(., Time=="Night") %>% select(., Species, Activity, Species_Activity)
  night_scaled <- data.frame(scale(night_env, scale=TRUE))
  cap_night <- capscale(night_scaled~Species + Activity, night_fish, distance="gower")
## Extract CAP scores and point locations for plotting 
  scores_night <- scores(cap_night)$sites
  axes_night <-  data.frame(scores(cap_night)$species)
## Test model and variance explained  
  anova.cca(cap_night, by="margin", permutations = 10000)
  summary(cap_night)

# 3-panel CAP plot (replicates Figure 4)
## Set plotting parameters
par(mfrow=c(1,3), mar=c(1,2,0,0), oma=c(4,2,3,0.1))
palette(c("lightblue","darkblue","yellow", "darkorange"))
## Day
  plot(CAP2~CAP1, data=scores_day, col="grey",bg=day_fish$Species_Activity, pch=c(21:24)[as.numeric(day_fish$Species_Activity)], yaxt="n", xlim=c(-1.8,1.5), ylim=c(-3.5,3.2))
  with(day_fish, ordiellipse(cap_day, Species_Activity,  kind = "sd", label=FALSE, conf = 0.90,alpha=0.3, draw="polygon", border=FALSE,col=c("lightblue","darkblue","yellow", "darkorange")))
  points(CAP2~CAP1, data=scores_day, col="grey",bg=day_fish$Species_Activity, pch=c(21:24)[as.numeric(day_fish$Species_Activity)])  ## Overlay points
  arrows(0,0, axes_day[1,1], axes_day[1,2], lwd=2, length=0.1)
  arrows(0,0, axes_day[2,1], axes_day[2,2], lwd=2,length=0.1)
  arrows(0,0, axes_day[3,1], axes_day[3,2], lwd=2, length=0.1)
  arrows(0,0, axes_day[4,1], axes_day[4,2], lwd=2, length=0.1)
  arrows(0,0, axes_day[5,1], axes_day[5,2], lwd=2, length=0.1)
  arrows(0,0, axes_day[6,1], axes_day[6,2], lwd=2, length=0.1)
  textField(-1.5,0.7, "Focal Depth", fill="white", cex=0.8)
  textField(-0.3,0.9, "Cover", fill="white", border = "grey", cex = 0.8)
  textField(-1.5,-0.6, "Substrate", fill="white",  cex = 0.8)
  textField(-1, 0.5, "Avg. Velocity", fill="white",  cex = 0.8, border="grey")
  textField(0.75,1, "Depth", fill="white",  cex = 0.8, border = "grey")
  textField(1.2,0.1, "Focal Velocity", fill="white",  cex = 0.8,border = "grey")
  legend("topleft", pch=c(21:24), legend=c("Bull trout - foraging", "Bull trout - resting", "Rainbow - foraging", "Rainbow - resting"), pt.bg=c("lightblue","darkblue","yellow", "darkorange"), bty="n")
  axis(side=2, las=1)
  box(lwd=2)
  mtext(side=2, line=2.5, "CAP2")
  mtext(side=3, line=0.7, "Day")

## Dusk
  plot(CAP2~CAP1, data=scores_dusk, bg=dusk_fish$Species_Activity, yaxt="n", pch=c(21:24)[as.numeric(dusk_fish$Species_Activity)], xlim=c(-1.5,2), ylim=c(-4.75,4.5))
  with(dusk_fish, ordiellipse(cap_dusk, Species_Activity,  kind = "sd", label=FALSE, conf = 0.90,alpha=0.3, draw="polygon", border=FALSE,col=c("lightblue","darkblue","yellow", "darkorange")))
  points(CAP2~CAP1, data=scores_dusk, col="grey", bg=dusk_fish$Species_Activity, pch=c(21:24)[as.numeric(dusk_fish$Species_Activity)])
  arrows(0,0, axes_dusk[1,1], axes_dusk[1,2], lwd=2, length=0.1)
  arrows(0,0, axes_dusk[2,1], axes_dusk[2,2], lwd=2, length=0.1)
  arrows(0,0, axes_dusk[3,1], axes_dusk[3,2], lwd=2, length=0.1)
  arrows(0,0, axes_dusk[4,1], axes_dusk[4,2], lwd=2, length=0.1)
  arrows(0,0, axes_dusk[5,1], axes_dusk[5,2], lwd=2, length=0.1)
  arrows(0,0, axes_dusk[6,1], axes_dusk[6,2], lwd=2, length=0.1)
  textField(1.5,0.7, "Focal Depth", fill="white", cex=0.8, border="grey")
  textField(1.0,0.7, "Depth", fill="white", cex=0.8, border="grey")
  textField(-0.75, 0.6, "Avg. Velocity", fill="white", cex=0.8, border="grey")
  textField(-1.1,-0.1, "Focal Velocity", fill="white", cex=0.8, border="grey")
  textField(-0.25,1, "Substrate", fill="white", cex=0.8, border="grey")
  textField(0.35,0.75, "Cover", fill="white", cex=0.8, border="grey")
  axis(side=2, las=1)
  box(lwd=2)
  mtext(side=1, line=2.5, "CAP1")
  mtext(side=3, line=0.7, "Dusk")

## Night
  plot(CAP2~CAP1, data=scores_night, col="grey",bg=night_fish$Species_Activity,yaxt="n", pch=c(21:24)[as.numeric(night_fish$Species_Activity)], xlim=c(-1.5,2.5), ylim=c(-2.2,2.2))
  with(night_fish, ordiellipse(cap_night, Species_Activity,  kind = "sd", label=FALSE, conf = 0.90,alpha=0.3, draw="polygon",border=FALSE, col=c("lightblue","darkblue","yellow", "darkorange")))
  points(CAP2~CAP1, data=scores_night, col="grey",bg=night_fish$Species_Activity, pch=c(21:24)[as.numeric(night_fish$Species_Activity)])
  arrows(0,0, axes_night[1,1], axes_night[1,2], lwd=2, length=0.1)
  arrows(0,0, axes_night[2,1], axes_night[2,2], lwd=2, length=0.1)
  arrows(0,0, axes_night[3,1], axes_night[3,2], lwd=2, length=0.1)
  arrows(0,0, axes_night[4,1], axes_night[4,2], lwd=2, length=0.1)
  arrows(0,0, axes_night[5,1], axes_night[5,2], lwd=2, length=0.1)
  arrows(0,0, axes_night[6,1], axes_night[6,2], lwd=2, length=0.1)
  textField(-0.2,-1, "Depth", fill="white", cex=0.8, border="grey")
  textField(-1.2,0, "Focal Depth", fill="white", cex=0.8, border="grey")
  textField(1.2,0.1, "Avg. Velocity", fill="white", cex=0.8, border="grey")
  textField(1.4,-0.6, "Focal Velocity", fill="white", cex=0.8, border="grey")
  textField(0,0.8, "Substrate", fill="white", cex=0.8, border="grey")
  textField(-0.3,0.25, "Cover", fill="white", cex=0.8, border="grey")
  axis(side=2, las=2)
  mtext(side=3, line=0.7, "Night")
  box(lwd=2)




# Cover addition experiment ####
# Input data includes fish observations and NREI model output from BioenergeticHSC program
  cover_hab <-  read.csv("./Cover_experiment_data.csv") # Read in data
  cover_fish <- read.csv("./Cover_fish.csv") # Data for individual fish observed in cover experiment
  obs <- read.csv("./Raw_data/focal_habitat_data.csv") # Focal data to compare fish sizes 

# Binomial GLM model testing whether average NREI under cover box predicts probability of fish occupancy
  cover_glm <- glm(colonized~nrei, family="binomial",data=cover_hab)
  null_glm <- glm(colonized~1, family="binomial",data=cover_hab)
  drop1(cover_glm, test = "Chi")
  
# Plot model predictions
  nrei_range <- seq(-0.04, 0.15, 0.001)
  colonized_predicted <- data.frame(predict(cover_glm, list(nrei=nrei_range),type="link", se.fit = TRUE))
  critval <- 1.96 ## approx 95% CI
  upr.ci <- colonized_predicted$fit + (critval * colonized_predicted$se.fit)
  lwr.ci <- colonized_predicted$fit - (critval * colonized_predicted$se.fit)
  mean_predicted <- colonized_predicted$fit
  fit.glm1 <- cover_glm$family$linkinv(mean_predicted)
  uprb <- cover_glm$family$linkinv(upr.ci)
  lwrb <- cover_glm$family$linkinv(lwr.ci)
  cover_pred <- cbind.data.frame(nrei=nrei_range, fit.glm1, lwrb, uprb) # Combine predictions into dataframe
  
# Plot of GLM model
  ggplot(cover_hab, aes(nrei, colonized)) +
    geom_jitter(width = 0.01, height= 0.005, size=2.5, pch=21, color="green", fill="black") +
    geom_ribbon(data=cover_pred, aes(y=fit.glm1, ymin = lwrb, ymax = uprb), alpha = .25)+
    geom_line(data=cover_pred, aes(y = fit.glm1), size = 1)+
    labs(x="Net Rate of Energy Intake (J/s)", y = "Colonization Probability") + theme_classic()
  
# Examine differences in fish size distributions 
  obs_dusk <- obs %>% filter(Species=="Rainbow" & Time=="Dusk" & Activity=="Foraging") %>% select(., Time, Length)
  obs_day <- obs %>% filter(Species=="Rainbow" & Time=="Day" & Activity=="Foraging") %>% select(., Time, Length)
  cover_lengths <- cover_fish %>% select(., Length) 
  cover_lengths$Time <- "Added Cover"
  length_combined <- rbind(obs_dusk, obs_day, cover_lengths)

  
# Density plots of fork lengths with lines indicating median values
  ggplot(length_combined, aes(y=Time, x = Length)) + stat_density_ridges(quantile_lines = TRUE, quantiles=2, alpha=0.7) + labs(x="Estimated Fork Length (mm)", y="") + theme_classic()
  
# K-S tests if size distributions are different
  ks.test(obs_dusk$Length, cover_lengths$Length)
  ks.test(obs_day$Length, cover_lengths$Length)
  
#########
  
# Habitat Suitability Models ####

# Scaled habitat availability data - combined from ADV software. "Available" is the total number of points for given habitat conditions
  hab_pref_total <- read.csv("./Habitat_Preferences.csv") ## Read in data
  hab_pref$Substrate <- factor(hab_pref$Substrate, ordered = TRUE) ## Make substrate an ordered factor
  
# Function to compute preference as used relative to available for each species and time period
  pref_fun <- function(x){ ## Function to calculate std. preferences
    x_pref <- x %>% mutate("rb_day_pref" = rainbow_day/Available, "rb_dusk_pref" = rainbow_dusk/Available, "rb_night_pref" = rainbow_night/Available, "bt_day_pref" = char_day/Available, "bt_dusk_pref" = char_dusk/Available, "bt_night_pref" = char_night/Available)
    return(x_pref)
  }
  
### Velocity HSCs

# Velocity function to run logisitic GLMs and return data frame of predictions.
  velocity_fit <- function(x){
    model.x <- glm(x ~ poly(Velocity,2), weights=velocity_pref$Available ,data = velocity_pref, family="binomial")
    model.x2 <- glm(x ~ Velocity, weights=velocity_pref$Available ,data = velocity_pref, family="binomial")
    model_list <- list(model.x, model.x2)
    model.best <- model_list[[which.min(sapply(1:length(model_list),function(x)AICc(model_list[[x]])))]]
    v_range <- seq(0, 1.5, by=0.05)
    predictions <- (predict(model.best, list(Velocity=v_range), type="response"))
    pred <- cbind.data.frame(v_range, predictions) ## Combine predictions with new data
    pred$predictions <- rescale(pred$predictions, to=c(0,1))
    return(pred$predictions)
  }
  
# Data frame of velocity preferences
  velocity_hsc <-  aggregate(cbind(Available, rainbow_day, rainbow_dusk, rainbow_night, char_day, char_dusk, char_night)~Velocity, data=hab_pref_total, FUN=sum)
  velocity_pref <- pref_fun(velocity_hsc) ## data frame with unstandardized preferences
  
# Run GLM model to get scaled HSC predictions for velocity for each species and time period. Turn into long data frame for plotting  
  Velocity <- seq(0, 1.5, by=0.05)
  rb_v_day <- velocity_fit(velocity_pref$rb_day_pref)
  rb_v_dusk <- velocity_fit(velocity_pref$rb_dusk_pref)
  rb_v_night <- velocity_fit(velocity_pref$rb_night_pref)
  bt_v_day <- velocity_fit(velocity_pref$bt_day_pref)
  bt_v_dusk <- velocity_fit(velocity_pref$bt_dusk_pref)
  bt_v_night <- velocity_fit(velocity_pref$bt_night_pref)
  v_predictions <- cbind.data.frame(Velocity, rb_day=rb_v_day,rb_dusk=rb_v_dusk,rb_night=rb_v_night,bt_day=bt_v_day, bt_dusk=bt_v_dusk,bt_night=bt_v_night)
# Long data frame
  hsc_long <- gather(v_predictions, key = "Species_time", value = "HSI", -Velocity)
  hsc_long$Species <- c(rep("Rainbow", 93), rep("Bull Trout", 93))
  hsc_long$Time <- c(rep("Day",31), rep("Dusk", 31), rep("Night", 31),rep("Day",31), rep("Dusk", 31), rep("Night", 31))
  
### Depth HSCs - Same process for depth
  
# Function to run GLM and extract scaled HSC predictions for depth. *Required predictions over a different range than velocity
  
  depth_fit <- function(x){ ## Function to compute glm predictions for depth
    model.x <- glm(x ~ poly(Depth,2), weights=depth_pref$Available ,data = depth_pref, family="binomial")
    model.x2 <- glm(x ~ Depth, weights=depth_pref$Available ,data = depth_pref, family="binomial")
    model_list <- list(model.x, model.x2)
    model.best <- model_list[[which.min(sapply(1:length(model_list),function(x)AICc(model_list[[x]])))]]
    d_range <- seq(0, 2, by=0.05)
    predictions <- (predict(model.best, list(Depth=d_range), type="response"))
    pred <- cbind.data.frame(d_range, predictions) ## Combine predictions with new data
    pred$predictions <-  rescale(pred$predictions, to=c(0,1))
    return(pred$predictions)
    #return(pred)
  }
  
# Compute preferences and run GLM function for depth (same process as velocity)
  Depth <- seq(0, 2, by=0.05)
  depth_hsc <-  aggregate(cbind(Available, rainbow_day, rainbow_dusk, rainbow_night, char_day, char_dusk, char_night)~Depth, data=hab_pref_total, FUN=sum)
  depth_pref <- pref_fun(depth_hsc) ## data frame with unstandardized preferences
  rb_day_d <- depth_fit(depth_pref$rb_day_pref)
  bt_day_d <- depth_fit(depth_pref$bt_day_pref)
  rb_dusk_d <- depth_fit(depth_pref$rb_dusk_pref)
  bt_dusk_d <- depth_fit(depth_pref$bt_dusk_pref)
  rb_night_d <- depth_fit(depth_pref$rb_night_pref)
  bt_night_d <- depth_fit(depth_pref$bt_night_pref)
  d_predictions <- cbind.data.frame(Depth, rb_day_d, rb_dusk_d, rb_night_d, bt_day_d, bt_dusk_d, bt_night_d)
  
# Long form data frame for plotting
  d_long <- gather(d_predictions, key = "Species_time", value = "HSI", -Depth)
  d_long$Species <- c(rep("Rainbow", 123), rep("Bull Trout", 123))
  d_long$Time <- c(rep("Day",41), rep("Dusk", 41), rep("Night", 41),rep("Day",41), rep("Dusk", 41), rep("Night", 41))
  
### For cover and substrate, we are just showing raw habitat preferences
# Substrate
  
  sub_hsc <- aggregate(cbind(Available, rainbow_day, rainbow_dusk, rainbow_night, char_day, char_dusk, char_night)~Substrate, data=hab_pref_total, FUN=sum)
  sub_pref <- data.frame(pref_fun(sub_hsc))
  sub_long <- sub_pref %>% dplyr::select(., Substrate, ends_with("pref")) %>% 
    gather(., key="Species_time", value="HSI", -Substrate)
  sub_long$Species <- c(rep("Rainbow", 15), rep("Bull Trout", 15))
  sub_long$Time <- c(rep("Day", 5), rep("Dusk", 5), rep("Night", 5), rep("Day", 5), rep("Dusk", 5), rep("Night", 5))
  sub_long$Species_time <- factor(sub_long$Species_time)
  for(i in sub_long$Species_time){ ## Rescale between 0 and 1 for each group
    sub_long$HSI[sub_long$Species_time==i] <- rescale(sub_long$HSI[sub_long$Species_time==i], to=c(0,1))
  }
  
# Cover (presence or absence)
  cover_hsc <-  aggregate(cbind(Available, rainbow_day, rainbow_dusk, rainbow_night, char_day, char_dusk, char_night)~Cover_yn, data=hab_pref_total, FUN=sum)
  c_pref <- data.frame(pref_fun(cover_hsc))
  c_pref <- 
    cov_long <- c_pref %>% dplyr::select(., Cover_yn, ends_with("pref")) %>%
    gather(., key = "Species_Time", value = "HSI", -Cover_yn)
  cov_long$Species <- c(rep("Rainbow", 6), rep("Bull Trout", 6))
  cov_long$Time <- c(rep("Day", 2), rep("Dusk", 2), rep("Night", 2),rep("Day", 2), rep("Dusk", 2), rep("Night", 2))
  for(i in cov_long$Species_Time){ ## Rescale between 0 and 1 for each group
    cov_long$HSI[cov_long$Species_Time==i] <- cov_long$HSI[cov_long$Species_Time==i]/max(cov_long$HSI[cov_long$Species_Time==i])
  }
  
# Multi-panel plots of preference from logistic models 
### Velocity   
  hsc_v <- ggplot(hsc_long, aes(x=Velocity, y=HSI, group=Time, color=Time)) +
    geom_line(aes(color=Time), size=1.2)+
    facet_grid(~Species, scales = 'free')+theme_classic()+scale_color_manual(values=c("lightblue","blue", "darkblue"))+theme(legend.position = "none") + labs(y="", x=expression("Velocity (m s"^"-1"~")")) + theme(strip.background = element_rect(colour="white", fill="white"),
                                                                                                                                                                                                                    strip.text = element_text(size = 18, hjust = 0))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd=1.2)
  
### Depth
  hsc_d <- ggplot(d_long, aes(x=Depth, y=HSI, group=Time, color=Time)) +
    geom_line(aes(color=Time), size=1.2)+
    facet_grid(~Species)+theme_classic()+scale_color_manual(values=c("lightblue","blue", "darkblue"))+theme(legend.position="none") + labs(y="", x="Depth (m)")+
    theme(strip.background = element_rect(colour="white", fill="white"),
          strip.text = element_text(size = 18, hjust = 0))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd=1.2)
  
### Substrate
  hsc_s <- ggplot(sub_long, aes(x=Substrate, fill=Time))+
    geom_bar(aes(x=Substrate, y=HSI),color="black",position="dodge", stat="identity")+
    facet_grid(~Species)+theme_classic()+scale_fill_manual(values=c("lightblue","blue", "darkblue"))+theme(legend.position = "none") + labs(y="", x = "Substrate Class")+
    theme(strip.text = element_blank())+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd=1.2)
  
### Cover
  hsc_c <- ggplot(cov_long, aes(x=Cover_yn, fill=Time))+
    geom_bar(aes(x=Cover_yn, y=HSI),color="black",position="dodge", stat="identity")+facet_grid(~Species)+theme_classic()+scale_fill_manual(values=c("lightblue","blue", "darkblue"))+labs(y="", x="Cover Presence")+
    theme(strip.text = element_blank())+annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd=1.2)
  
  ## Make shared legend
  legend <- get_legend(hsc_c + theme(legend.position = "bottom")+theme(legend.title = element_blank())
  )
  
### Final multi-panel plot
  hsc_grid <- plot_grid(hsc_v, hsc_d, hsc_s, hsc_c+theme(legend.position = "none"))
  plot_grid(hsc_grid, legend, nrow=2, rel_heights = c(1,.1))
  grid.text("HSI (Standardized Use:Availability)", x=0.01, y=0.5, just="center", rot=90)
  