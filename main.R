## Pre-processing data ##

Obesity <- read.csv("obesity.csv", header = TRUE)
Names <- read.csv("UARegions.csv", header = TRUE)
Names <- Names[c(-105,-242,-248),]  # Removing three UAs which had no observations 

Obesity <- merge(Obesity, Names, all=TRUE) # Merging both files
Obesity <- Obesity[order(Obesity$ID),] # Order the data according to the ID rank
rownames(Obesity) <- Obesity[,2]  # Using ID as the row number


colnames(Obesity) <- c("UA", "ID", "Year", "Obesity_count", "Pop_count", "Pupil_absence", "Violent_offences",
                       "Falls_old_people", "Fuel_poverty", "Winter_death", "Sexual_offences",
                       "Pop_under_18", "Pop_over_65", "Inactivity_rate", "Air_pollution", "Home_affordability", 
                       "Gender_pay_gap","Average_earnings", "Region")        # Renaming the different variables 

Obesity[Obesity$Obesity_count==(-1),4] <- NA  # Replacing the missing values by NA's
Year_categ <- factor(Obesity$Year, levels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017")) # Convert year into a categorical variable
Obesity_rate <- Obesity$Obesity_count/Obesity$Pop_count # Computing rates
Obesity <- data.frame(Obesity, Year_categ, Obesity_rate) # New data file containing rates

# Splitting the data into two files

ObesityModel <- Obesity[1:1785,]   # Create a file for model building 
ObesityPred <- Obesity[1786:2232,] # Create a file for the predictions

## Exploratory part ##
library(ggplot2)

tapply(ObesityModel$Obesity_count, INDEX = ObesityModel$Region, FUN=mean)  # Computing the mean of obesity counts over the regions

Cor_offences <- cor(ObesityModel$Violent_offences, ObesityModel$Sexual_offences) # Computing the correlation coefficient
Cor_earnings <- cor(ObesityModel$Average_earnings, ObesityModel$Home_affordability)

# Boxplot of obesity count according to the different regions 
First_plot <- ggplot(data=ObesityModel, mapping=aes(x=Region, y=Obesity_count)) +        
  geom_boxplot() +labs(x="Regions", y="Obesity counts",title="Obesity per regions in England") +   # Labelling the axis and adding a title
  stat_summary(fun.y=mean, geom="point", col="red", shape=3, size=2) + coord_trans(y="log10") +    # Adding the mean for each boxplot and apply log on the y-axis
  scale_x_discrete(labels = c("East Midlands" = "E. Mid", "East of England" = "E. Eng", "London" = "Ldn",   # Abbreviate names of each region 
                                  "Yorkshire and the Humber"="Y Humb", "North West" = "N.W.", "South East"="S.E.",
                                  "South West"="S.W.", "West Midlands"= "W. Mid", "North East"="N.E.")) 
First_plot 
dev.copy(pdf, "Box_obesity_regions.pdf", width = 6, height = 6)  # Copying the graph window into a file 
dev.off()  # Stop copying 

# Boxplot of obesity count over the years
Second_plot <- ggplot(data=ObesityModel, mapping=aes(x=Year_categ, y=Obesity_count)) +
  geom_boxplot() +labs(x="Years", y="Obesity counts", title="Obesity across years") +
  stat_summary(fun.y=mean, aes(colour="mean"), geom="point", shape=3, size=2) + coord_trans(y="log10") 
Second_plot
dev.copy(pdf, "Boxplot_obesity.pdf", width = 6, height = 6)
dev.off()

# Plotting sexual offences against violent offences 
Third_plot <- ggplot(data=ObesityModel, mapping=aes(x=Violent_offences, y=Sexual_offences)) + 
              geom_point()+labs(x="Violent offences", y="Sexual offences",  
                                title="Sexual offences against violent offences")
Third_plot
dev.copy(pdf, "Offences.pdf")
dev.off()

# Plotting average earnings against home affordability
Fourth_plot <- ggplot(data=ObesityModel, mapping=aes(x=sqrt(Home_affordability), y=Average_earnings)) + 
              geom_point()+labs(y="Average earnings", x="Home affordability",  
                                title="Average earnings against home affordability")
Fourth_plot
dev.copy(pdf, "Earnings.pdf")
dev.off()


# Plotting obesity counts and log obesity counts against population over 65 and inactivity rate 
par(mfrow=c(2,2))

plot(ObesityModel$Pop_over_65, ObesityModel$Obesity_count, xlab = "Population over 65", ylab = "Obesity count", pch=20)  
plot(ObesityModel$Pop_over_65, log(ObesityModel$Obesity_count), xlab = "Population over 65", ylab = "Log of obesity count", pch=20) # log transformation on obesity count
plot(ObesityModel$Inactivity_rate, ObesityModel$Obesity_count, xlab = "Inactivity rate", ylab = "Obesity count", pch=20)
plot(ObesityModel$Inactivity_rate, log(ObesityModel$Obesity_count), xlab = "Inactivity rate", ylab = "Log of obesity count", pch=20) # same 
mtext("Effects of covariates on log obesity ", side = 3, line = -2, outer=TRUE)  # Adding a title 

dev.copy(pdf, "LogObesity.pdf")
dev.off()


## Model building ##
library(mgcv)

Model_all <- lm (Obesity_rate~UA+Region+Year
                 +Average_earnings+Gender_pay_gap+Home_affordability+Fuel_poverty 
                 +Violent_offences+Falls_old_people+Winter_death+Sexual_offences
                 +Inactivity_rate+Pupil_absence
                 +Pop_under_18+Pop_over_65
                 +Air_pollution, data=ObesityModel)    # First linear model including all covariates 
                 
summary(Model_all)   

## Hierarchical clustering for UAs ##

NumVars <- 5:18 # Selecting columns containing numeric covariates
UASummaries <- # Means & SDs of all numeric covariates
  aggregate(ObesityModel[,NumVars], # for each group
            by=list(ObesityModel$UA), FUN=mean)
rownames(UASummaries) <- UASummaries[,1]
UASummaries <- scale(UASummaries[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(UASummaries) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering 

# Plotting the cluster dendrogram
par(mfrow=c(1,1))
plot(ClusTree, xlab="Unitary Authority", ylab="Separation")  
abline(h=7.6, col="red", lty=2)  # Adds a cutting line

NewGroups <- paste("UAGrp", cutree(ClusTree, h=7.6), sep="")  
table(Names$Region[match(rownames(UASummaries), Names$UA)], NewGroups)  # Table comparing distribution of UA's over their new groups & regions

Obesity_group <- cbind(Names, NewGroups)  # Associating each UA to its new group
ObesityModel <- merge(ObesityModel, Obesity_group, all=TRUE)  # Adding the new variable to the table 

Model_all_HC <- update(Model_all, .~. -UA-Region+NewGroups)  # Linear model with only the new groups as geographical covariate
Model_all_Region <- update(Model_all, .~. -UA)  # Linear model with only region as geographical covariate 

# Comparing both output model
summary(Model_all_Region)
summary(Model_all_HC)
# Keeping the model with regions 

## Covariate selection ##
SecondModel <- update(Model_all_Region, . ~ . -Fuel_poverty) # Linear model without fuel poverty
anova(Model_all_Region, SecondModel)  # F-test on fuel poverty

# Removing fuel poverty

ThirdModel <- update(SecondModel, . ~ . -Falls_old_people)  # Linear model without fall in old people
anova(SecondModel, ThirdModel)  # F-test on fall in old people 

# Removing falls in old people

FourthModel <- update(ThirdModel, . ~ . -Pop_under_18) # Linear model without pop under 18
anova(ThirdModel, FourthModel)  # F-test on pop under 18

# Removing pop under 18

FifthModel <- update(FourthModel, . ~ . -Sexual_offences) # Linear model without sexual offences
anova(FourthModel, FifthModel) # F-test on sexual offences

# Removing sexual offences

summary(FifthModel)

# Looking for possible interactions

SixthModel <- update(FifthModel, . ~ . +Average_earnings:Violent_offences) # Adding an interaction term between average earnings and violent offences
summary(SixthModel) 

par(mfrow=c(2,2))
plot(SixthModel)  # residual plots

par(mfrow=c(1,1))
Stan_res <-rstandard(SixthModel)  # Computing the standardized residuals 
qqnorm(Stan_res, ylab="Standardized Residuals", xlab="Theoritical quantiles", main="Normal Q-Q")  # Q-Q plot of standardized errors
qqline(Stan_res)
dev.copy(pdf, "QQplot.pdf")
dev.off()

# Switching to a glm with a log link function & poisson distribution

GLM_all <- glm (Obesity_count~Region+Year
              +Average_earnings+Gender_pay_gap+Home_affordability+Fuel_poverty 
              +Violent_offences+Falls_old_people+Winter_death+Sexual_offences
              +Inactivity_rate+Pupil_absence
              +Pop_under_18+Pop_over_65+Air_pollution
              +offset(log(Pop_count)),   # Using the offset to model rates instead of counts
              data=ObesityModel, family=poisson(link="log"))  # Poisson distribution

summary(GLM_all)  # GLM output

par(mfrow=c(2,2))
plot(GLM_all)  # Residuals plot

Pearson_res1 <- sum(resid(GLM_all,type="pearson")^2) / GLM_all$df.residual # Computing the variance of pearson residuals 

# Trying to explain the excess variance by looking at interactions 

SecondGLM <- update(GLM_all, . ~ . +Average_earnings:Violent_offences) # Adding interaction average earnings & violent offences 
summary(SecondGLM)

par(mfrow=c(2,2))
plot(SecondGLM)   # Residual plots

Pearson_res2 <- sum(resid(SecondGLM,type="pearson")^2 ) / SecondGLM$df.residual # Computing the new variance of pearson residuals 
diff_res <- Pearson_res1- Pearson_res2  # Difference between both variance of the pearson residuals

# Switching to a quasipoisson distribution 

ThirdGLM <- glm (Obesity_count~Region+Year
                +Average_earnings+Home_affordability+Fuel_poverty 
                +Violent_offences+Falls_old_people+Winter_death+Sexual_offences+Gender_pay_gap
                +Inactivity_rate+Pupil_absence
                +Pop_under_18+Pop_over_65+Air_pollution
                +offset(log(Pop_count)),
                data=ObesityModel, family=quasipoisson(link="log"))  # Quasi-poisson distribution

summary(ThirdGLM)

# Comparing model with year as continuous and as categorical covariate

ThirdGLM2 <- update(ThirdGLM, . ~ . -Year+Year_categ)  
summary(ThirdGLM2)  # Output of the model with year as a categorical covariate 
# Keeping year as categorical

# Trying PCA analysis 

PCs <- prcomp(formula =~Average_earnings+Home_affordability+Violent_offences+Sexual_offences,  
                 scale.=TRUE, data=ObesityModel, rank. =2)  # Generating two PCs from a subset of four covariates

PCs  # Getting the coefficients 
par(mfrow=c(1,1))
biplot(PCs)  # Visualising the PCs

Offences <- PCs$x[,1]  # Assigning names to PCs
Earnings <- PCs$x[,2]

ModelPC <- update(ThirdGLM2, .~.-Average_earnings-Home_affordability-Violent_offences-Sexual_offences
                                +Offences+Earnings) # Replacing covariates by their PCs

summary(ModelPC)
anova(ThirdGLM2, ModelPC, test="Chi") # Comparing model with PCA and without
# Keeping the model without PCA

# Covariate selection

FourthGLM <- update(ThirdGLM2, . ~ . -Fuel_poverty) # Model without fuel poverty

FifthGLM <- update(FourthGLM, . ~ . -Falls_old_people) # Model without falls in old people
anova(FifthGLM, FourthGLM, test ="Chi")  # Chi-squared test on falls in old people

# Removing falls in old people

SixthGLM <- update(FifthGLM, . ~ . -Pop_under_18) # Model without pop under 18
anova(SixthGLM, FifthGLM, test="Chi")    # Chi-squared test on pop under 18

# Removing pop under 18

SeventhGLM <- update(SixthGLM, . ~ . -Sexual_offences) # Model without sexual offences
anova(SeventhGLM, SixthGLM, test="Chi")  # Chi-squared test on sexual offences

SeventhGLM2 <- update(SixthGLM, . ~ . -Violent_offences) # Model without violent offences
anova(SeventhGLM2, SixthGLM, test="Chi") # Chi-squared test on violent offences

# Removing sexual offences

summary(SeventhGLM)   
anova(SeventhGLM, test="Chi") # Sequential sum of squares table

EighthGLM <- update(SeventhGLM, . ~ . -Winter_death) # Model without winter death
anova(EighthGLM, SeventhGLM, test="Chi")  # Chi-squared test on winter death

# Removing winter death

summary(EighthGLM)

# Looking for interactions

NinethGLM <- update(EighthGLM, .~. +Average_earnings:Violent_offences)  # Adding interaction term between average earnings & violent offences
summary(NinethGLM)

TenthModel <- update(NinethGLM, .~.+Air_pollution:Region)  # Adding interaction term between air pollution & region
summary(TenthModel)  # Output of the model containing both interactions

anova(TenthModel, NinethGLM, test="Chi")  # Chi-squared on the previous interaction

# Keeping both interactions

# Looking for non parametric term to include

FirstGAM <- gam (Obesity_count~Region+Year
                 +Average_earnings
                 +s(Home_affordability) # gam with home affordability as non-parametric term
                 +Violent_offences+Gender_pay_gap
                 +Inactivity_rate+Pupil_absence
                 +Pop_over_65+Air_pollution
                 +offset(log(Pop_count))
                 +Air_pollution:Region
                 +Average_earnings:Violent_offences,
                 data=ObesityModel, family=quasipoisson(link="log"))  

par(mfrow=c(1,2))

# Plotting the estimated smooth  

plot(FirstGAM, shade = TRUE, xlab = "Home affordability")  # Plotting estimated smooth for home affordability

SecondGAM <- update(FirstGAM, .~. -s(Home_affordability)+s(log(Home_affordability))) # Gam with a log transformation on home affordability  

plot(SecondGAM, shade=TRUE, xlab = "Log of home affordability") # Plotting estimated smooth for log of home affordability
mtext("Estimate smooth of home affordability and its log ", side = 3, line = -2, outer=TRUE)  # Adding a title
dev.copy(pdf, "Home_affordability_gam.pdf")
dev.off()

# Keeping a GLM and switching to a log transformation on home affordability

# Final model 

Final_model <- glm (Obesity_count~Region+Year_categ
               +Pupil_absence+Gender_pay_gap +Inactivity_rate+Average_earnings
               +log(Home_affordability)+Violent_offences+Air_pollution+Pop_over_65
               +offset(log(Pop_count)) 
               +Average_earnings:Violent_offences+Region:Air_pollution,
               family=quasipoisson(link="log"), data= ObesityModel)

summary(Final_model)  # Output of the final model

## Checking the model assumptions ##

par(mfrow=c(2,2))
plot(Final_model)  # Residual plots
dev.copy(pdf, "Diagnostic_plot.pdf")
dev.off()


# Plotting standardized residuals against some covariates 

Std_Resid <- rstandard(Final_model)  # Computing standardized residuals 

ggplot(mapping=aes(x=Pupil_absence, y=Std_Resid), data=ObesityModel) + geom_point() + 
  labs(x="Pupil absence", y="Standardized residuals", 
       title="Standardized residuals against pupil absence")   # Standardized residuals against pupil absent
dev.copy(pdf, "SDres_pupil.pdf")
dev.off()

ggplot(mapping=aes(x=Inactivity_rate, y=Std_Resid), data=ObesityModel)+ geom_point() + 
  labs(x="Inactivity rate", y="Standardized residuals", 
       title="Standardized residuals against inactivity rate")  # Standardized residuals against inactivity rate
dev.copy(pdf, "SDres_inact_rate.pdf")
dev.off()

ggplot(mapping=aes(x=Average_earnings, y=Std_Resid), data=ObesityModel) + geom_point() + 
  labs(x="Average earnings", y="Standardized residuals", 
       title="Standardized residuals against average earnings") # Standardized residuals against average earnibgs
dev.copy(pdf, "SDres_aver_earnings.pdf")
dev.off()

## Predictions ##

Pred <- predict(Final_model, newdata = ObesityPred, type="response", se.fit = TRUE)  # Computing predictions and their associated standard errors
Pred_errors <- sqrt(Pred$fit+(Pred$se.fit)^2) # Then computing standard deviation of the prediction errors

Predictions <- data.frame(Pred$fit, Pred_errors)   # Create a table containing the predictions and the standard deviation of the prediction errors 
write.table(Predictions, file="ICA2_N.dat", col.names = FALSE)  # Create table without header and save it to a text file


