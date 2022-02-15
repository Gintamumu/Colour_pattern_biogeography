####### GLS (generalized least-squares) analysis ########
####### GLS script reference paper #########
####### Vrettos M, Reynolds C, Amar A. Malar stripe size and prominence in peregrine falcons vary #######
####### positively with solar radiation: support for the solar glare hypothesis[J]. Biology letters, 2021, 17(6): 20210116.#######


setwd("/data/martin/genomics/analyses/Danaus_SDM/04_nlm/01_correlation/")


##################################################
##### a. Read in data and extract scores #####
#################################################



#### NA frequency of each phenotype of file ought to be removed#####

perfal <- read.csv("03_Research_citizen_combine_17094.4DegreeBlocks_rmEnvValue.frequency_processed.csv") 

#environment variables
dat <- perfal[c(12:15)]

#Phenotypes
scores <- perfal[c(21:23)]

#####b.Correlation coefficient and correlation test in R
##https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

#environment variables
round(cor(dat),
  digits = 2 # rounded to 2 decimals
)

jnk=round(cor(na.omit(dat)),digits = 2)
write.csv(jnk, file = "Correlation_4v.csv")

#Phenotypes
as.scoresa.frame(na.omit(scores))
cor(na.omit(scores))
round(cor(na.omit(scores)),
  digits = 2 # rounded to 2 decimals
)
jnk=round(cor(na.omit(scores)),digits = 2)
write.csv(jnk, file = "Correlation_traitsv2.csv")




##################################################
##### b. Correlation plot #####
##################################################


# Correlations between variables

library(PerformanceAnalytics)

# Produce correlations plots using the chart.Correlation function in
# PerformanceAnalytics - edited here to remove asterisks indicating significance
# and density lines from histograms.

chart.Correlation <-
  function (R, histogram = TRUE, method=c("pearson", "kendall", "spearman"), ...)
  { # @author R Development Core Team
    # @author modified by Peter Carl
    # Visualization of a Correlation Matrix. On top the (absolute) value of the
    # correlation plus the result of the cor.test as stars. On bottom, the
    # bivariate scatterplots, with a fitted line
    
    x = checkData(R, method="matrix")
    
    if(missing(method)) method=method[1] #only use one
    cormeth <- method
    
    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", method=cormeth, cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use=use, method=method) # MG: remove abs here
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      
      test <- cor.test(as.numeric(x),as.numeric(y), method=method)
      # borrowed from printCoefmat
      # MG: add abs here and also include a 30% buffer for small numbers
      text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
    }
    f <- function(t) {
      dnorm(t, mean=mean(x), sd=sd.xts(x) )
    }
    
    #remove method from dotargs
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)
    
    hist.panel = function (x, ...=NULL ) {
      par(new = TRUE)
      hist(x,
           col = "light gray",
           probability = TRUE,
           axes = FALSE,
           main = "",
           breaks = "FD")
      #lines(f, col="blue", lwd=1, lty=1) how to add gaussian normal overlay?
      rug(x)
    }
    
    # Draw the chart
    if(histogram)
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=hist.panel)
    else
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor) 
  }


######################################################
# The NA can actually be due to 2 reasons. One is that there is a NA in your data. 
# Another one is due to there being one of the values being constant. 
# This results in standard deviation being equal to zero and hence the cor function returns NA.
######################################################

# Correlations between predictor variables

chart.Correlation(dat)

# Correlations between malar stripe scores
chart.Correlation(scores) 

######Save environment variables correlation plot into pdf file ####
# Open a pdf file
pdf("01Correlations_4variables.pdf") 
# 2. Create a plot
plot(chart.Correlation(dat)
# Close the pdf file
dev.off() 
######

######Save phenotype correlation plot into pdf file ####
# Open a pdf file
pdf("01Correlations_three_phenotypes.pdf") 
# 2. Create a plot
plot(chart.Correlation(scores))
# Close the pdf file
dev.off() 
#####################################################




#load the package
library(AICcmodavg)
library(ape)
library(ggplot2)
library(gridExtra)
library(lme4)
library(nlme)
library(PerformanceAnalytics)
library(raster)
library(RColorBrewer)
library(rworldxtra)
library(rworldmap)
library(scales)
library(viridis)
library(maptools)
library(corrplot)
library(fmsb)

setwd("/data/martin/genomics/analyses/Danaus_SDM/04_nlm/02_AIC/")

     
     
     
     
##################################################
####### c. Checking for spatial autocorrelation #######
##################################################
     

# Standardize variables

#Environmetal variables
perfal$Bio01.x <- scale(perfal$Bio01)
perfal$Bio12.x <- scale(perfal$Bio12)
perfal$Bio20.x <- scale(perfal$Bio20)
perfal$Bio28.x <- scale(perfal$Bio28)

#Phenotypes
perfal$groundColour_bb.x <- scale(perfal$groundColour_bb)
groundColour_n <- perfal$groundColour_n
groundColour_n

########### Basic linear models in pairwise comparison between phenotype and environmental variables###########
#groundColour
#Bio01
groundColour.global <- lm(groundColour_bb ~ Bio01.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

# Check the residuals of these basic models for spatial autocorrelation
library(ape) 
perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
# point with the lat and long
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$Bio01.x) + abline(groundColour.global)




#Bio12
groundColour.global <- lm(groundColour_bb ~ Bio12.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
# point with the lat and long
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$Bio12.x) + abline(groundColour.global)



#Bio20
groundColour.global <- lm(groundColour_bb ~ Bio20.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
# point with the lat and long
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$Bio20.x) + abline(groundColour.global)


#Bio28
groundColour.global <- lm(groundColour_bb ~ Bio28.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
# point with the lat and long
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)

plot(perfal$groundColour_bb.x ~ perfal$Bio28.x) + abline(groundColour.global)



#Bio01+Bio12+Bio20+Bio28
groundColour.global <- lm(groundColour_bb ~ Bio01.x + Bio12.x + Bio20.x + Bio28.x, data=perfal, weights = groundColour_n)
summary(groundColour.global)

perfal$groundColour.model.residuals <- groundColour.global$residuals
Latitude <- perfal$block_midpoint_lat
Longitude <- perfal$block_midpoint_long
perfal = cbind(perfal, Latitude&Longitude) # combine the model residual for each 
# point with the lat and long
dists <- as.matrix(dist(cbind(perfal$block_midpoint_lat, perfal$block_midpoint_long)))
dists.inv <- (1/(dists + 0.000000000000000001)) # add a small constant so not dividing by zero
diag(dists.inv) <- 0
Moran.I(perfal$groundColour.model.residuals, dists.inv, scaled=T, na.rm=T)


##################################################
####### d. GLS models #######
##################################################
     

library(nlme)
library(lme4)
library(AICcmodavg)

# groundColour

Cand.mod.groundColour_bb <- list()

Cand.mod.groundColour_bb[[1]] <- gls(groundColour_bb ~ 1, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[1]])

Cand.mod.groundColour_bb[[2]] <- gls(groundColour_bb ~ Bio01.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[2]])
Cand.mod.groundColour_bb[[3]] <- gls(groundColour_bb ~ Bio12.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[3]])
Cand.mod.groundColour_bb[[4]] <- gls(groundColour_bb ~ Bio20.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[4]])
Cand.mod.groundColour_bb[[5]] <- gls(groundColour_bb ~ Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[5]])
Cand.mod.groundColour_bb[[6]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[6]])
Cand.mod.groundColour_bb[[7]] <- gls(groundColour_bb ~ Bio01.x + Bio20.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[7]])
Cand.mod.groundColour_bb[[8]] <- gls(groundColour_bb ~ Bio01.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[8]])
Cand.mod.groundColour_bb[[9]] <- gls(groundColour_bb ~ Bio12.x + Bio20.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[9]])
Cand.mod.groundColour_bb[[10]] <- gls(groundColour_bb ~ Bio12.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[10]])
Cand.mod.groundColour_bb[[11]] <- gls(groundColour_bb ~ Bio20.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[11]])
Cand.mod.groundColour_bb[[12]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + Bio20.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[12]])
Cand.mod.groundColour_bb[[13]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[13]])
Cand.mod.groundColour_bb[[14]] <- gls(groundColour_bb ~ Bio01.x + Bio20.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[14]])
Cand.mod.groundColour_bb[[15]] <- gls(groundColour_bb ~ Bio12.x + Bio20.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[15]])
Cand.mod.groundColour_bb[[16]] <- gls(groundColour_bb ~ Bio01.x + Bio12.x + Bio20.x + Bio28.x, method = "ML", data=perfal,
                            correlation=corExp(form = ~Latitude+Longitude), weights=~1/groundColour_n)
summary(Cand.mod.groundColour_bb[[16]])

########################################

Model.names <- c("Null", "Bio01.x", "Bio12.x", "Bio20.x", "Bio28.x", "Bio01.x + Bio12.x", "Bio01.x + Bio20.x", "Bio01.x + Bio28.x", "Bio12.x + Bio20.x", "Bio12.x + Bio28.x", "Bio20.x + Bio28.x", "Bio01.x + Bio12.x + Bio20.x", "Bio01.x + Bio12.x + Bio28.x", "Bio01.x + Bio20.x + Bio28.x", "Bio12.x + Bio20.x + Bio28.x", "Bio01.x + Bio12.x + Bio20.x + Bio28.x")

aictabSUM <- aictab(cand.set = Cand.mod.groundColour_bb, modnames = Model.names)

write.csv(aictabSUM, file = "AIC02_weight.csv")


# install.packages("MuMIn")
library(MuMIn)

# Model-averaged estimates
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio20.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio12.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio20.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x + Bio20.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio20.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio12.x + Bio20.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio12.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio20.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x + Bio20.x + Bio28.x", conf.level = 0.95)
modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x + Bio12.x + Bio20.x + Bio28.x", conf.level = 0.95)




##################################################
###### e. Forest plot showing GLS model outputs ######
##################################################
     

library(ggplot2)

# Extract model-averaged estimates and confidence intervals

# groundColour
#Top seven variables

groundColour.Bio01 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio01.x", conf.level = 0.95)
####If it does not work, to annotate the upper "Model-averaged estimates".##########

groundColour.Bio12 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio12.x", conf.level = 0.95)

groundColour.Bio20 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio20.x", conf.level = 0.95)

groundColour.Bio28 <- modavg(cand.set = Cand.mod.groundColour_bb, modnames = Model.names, parm = "Bio28.x", conf.level = 0.95)


groundColour.means <- c(groundColour.Bio01$Mod.avg.beta, groundColour.Bio12$Mod.avg.beta, groundColour.Bio20$Mod.avg.beta, groundColour.Bio28$Mod.avg.beta)

groundColour.mins <- c(groundColour.Bio01$Lower.CL, groundColour.Bio12$Lower.CL, groundColour.Bio20$Lower.CL, groundColour.Bio28$Lower.CL)

groundColour.maxes <- c(groundColour.Bio01$Upper.CL, groundColour.Bio12$Upper.CL, groundColour.Bio20$Upper.CL, groundColour.Bio28$Upper.CL)

predictors <- c("Bio01", "Bio12", "Bio20", "Bio28")
var.name <- c("groundColour", "groundColour", "groundColour", "groundColour")

groundColour.avgs <- data.frame(Variable = var.name, Mean=groundColour.means, Upper=groundColour.maxes, Lower=groundColour.mins,
                         Predictor=predictors)

#####
perfal.avgs <- rbind(groundColour.avgs)


perfal.avgs$Variable <- factor(perfal.avgs$Variable, levels = c("groundColour"))

# Create forest plot - first four variables

forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.6,0.6), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(face="bold")
axis.title=element_text(size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270,face="bold")
axis.text.y=element_text(hjust=0,vjust = 1,angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_06.pdf')
forest.plot.main
dev.off()




############## Create forest plot - first four variables ###########################################################
######If the plot show no 95% interval of some variables, please change range value by limits=c(-0.5,0.5)###########

forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.5,0.5), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(face="bold")
axis.title=element_text(size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270,face="bold")
axis.text.y=element_text(hjust=0,vjust = 1,angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_05.pdf')
forest.plot.main
dev.off()

# Create forest plot - first four variables

forest.plot.main=ggplot(perfal.avgs, aes(y = Predictor, x = Mean, xmin=Lower, xmax=Upper))+
  geom_point(color = 'black', size=4)+
  geom_errorbarh(aes(xmin=Lower, xmax=Upper, height=0.2))+
  scale_x_continuous(limits=c(-0.4,0.4), name='Estimate')+
  ylab('')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_wrap(~Variable,strip.position="right",nrow=4,scales = "free_y") +
  theme()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(),
        text=element_text(size=24,face="bold"),
        legend.position='none')
axis.ticks.y=element_blank()
axis.text.x=element_text(face="bold")
axis.title=element_text(size=24,face="bold")
strip.text.y = element_text(hjust=0,vjust = 1,angle=270,face="bold")
axis.text.y=element_text(hjust=0,vjust = 1,angle=0)
forest.plot.main

pdf(file='forestplot_seven_v_04.pdf')
forest.plot.main
dev.off()



####################################################################################################
#################### f.Scatterplots showing linear relationships ###################
####################################################################################################
library(ggplot2)
library(scales)
library(gridExtra)
library(ggpubr)

# groundColour

########################

groundColour.plot <- ggplot(aes(x=Bio01, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("Bio01 (C of V)")), y = "groundColour_bb (Score)", col=expression(paste("Bio01 (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson")
groundColour.plot

pdf(file='Scatterplots_Bio01_03.pdf')
groundColour.plot
dev.off()

########################

########################

groundColour.plot <- ggplot(aes(x=Bio12, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("Bio12 (C of V)")), y = "groundColour_bb (Score)", col=expression(paste("Bio12 (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson")
groundColour.plot

pdf(file='Scatterplots_Bio12_03.pdf')
groundColour.plot
dev.off()

########################

########################

groundColour.plot <- ggplot(aes(x=Bio20, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("Bio20 (C of V)")), y = "groundColour_bb (Score)", col=expression(paste("Bio20 (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson")
groundColour.plot

pdf(file='Scatterplots_Bio20_03.pdf')
groundColour.plot
dev.off()

########################

########################

groundColour.plot <- ggplot(aes(x=Bio28, y=groundColour_bb), data=perfal) +
  geom_jitter(shape=16, alpha=0.2, position=position_jitter(0.2))+
  labs(x = expression(paste("Bio28 (C of V)")), y = "groundColour_bb (Score)", col=expression(paste("Bio28 (C of V)"))) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x.top = element_line(color = "black"),
        axis.line.x.bottom = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size=16),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
groundColour.plot <- groundColour.plot + geom_smooth(method="lm", fill="lightblue", formula = y ~ x) + stat_cor(method = "pearson")
groundColour.plot

pdf(file='Scatterplots_Bio28_03.pdf')
groundColour.plot
dev.off()

########################
