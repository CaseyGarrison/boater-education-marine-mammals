###################################Single Location Analysis using Distance Score###################################

#Read data file
Survey1Location_Campaign <- read.csv("Survey1Location_Campaign.csv")

#Check distribution of distance score
hist(SurveyMersLocation_Campaign$DistanceScore, 
     main = "", 
     xlab = "Distance Score", 
     ylab = "Frequency",
     breaks = seq(0, 3, by = 1)) 

hist(SurveyMersLocation_Campaign$DistanceScore,
     main = "",
     xlab = "Distance Score",
     ylab = "Frequency",
     breaks = seq(-0.5, 3.5, by = 1),  # Ensures bins: [-0.5, 0.5), [0.5, 1.5), etc.
     col = "lightgray",
     border = "black",
     xaxt = "n")  # Turn off default x-axis labels

axis(1, at = 0:3)

# Compute mean and variance of Distance Score to check dispersion
mean_score <- mean(Survey1Location_Campaign$DistanceScore)
variance_score <- var(Survey1Location_Campaign$DistanceScore)
mean_score
variance_score

#Rename campaign column (typo in table)
colnames(Survey1Location_Campaign)[colnames(Survey1Location_Campaign) == "Campagin"] <- "Campaign"

#Convert predictor variables to factors
Survey1Location_Campaign$Location <- as.factor(Survey1Location_Campaign$Location)
Survey1Location_Campaign$Vessel <- as.factor(Survey1Location_Campaign$Vessel)
Survey1Location_Campaign$Role <- as.factor(Survey1Location_Campaign$Role)
Survey1Location_Campaign$Certification <- as.factor(Survey1Location_Campaign$Certification)
Survey1Location_Campaign$Experience <- as.factor(Survey1Location_Campaign$Experience)
Survey1Location_Campaign$Campaign <- as.factor(Survey1Location_Campaign$Campaign)

#Re level location reference category so it is the same across both single location analyses
Survey1Location_Campaign$Location <- relevel(Survey1Location_Campaign$Location, ref = "CC")

#Load required package
library(glmmTMB)

#Run models
modeld9 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Vessel, 
                   family = betabinomial(),
                         data = Survey1Location_Campaign)
modeld10 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
modeld11 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
modeld12 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Vessel, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
modeld13 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Vessel + Campaign, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
modeld14 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Campaign, 
                         family = betabinomial(),
                         data = Survey1Location_Campaign)
modeld15 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Campaign, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
modeld16 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Vessel + Campaign, 
                         family = betabinomial(), 
                         data = Survey1Location_Campaign)
null_model<- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ 1, 
                              family = betabinomial(), 
                              data = Survey1Location_Campaign)

#Named list of models
models <- list(
  "modeld9" = modeld9,
  "modeld10" = modeld10,
  "modeld11" = modeld11,
  "modeld12" = modeld12,
  "modeld13" = modeld13,
  "modeld14" = modeld14,
  "modeld15" = modeld15,
  "modeld16" = modeld16,
  "null_model" = null_model
)

#View model summaries
model_summaries <- lapply(models, summary)
model_summaries

#Load required package
library(MuMIn)

#View ranked AIC of models 
aic_results <- model.sel(models, rank = AIC)
print(aic_results)

#Calculate RVI for variables in the top two models
rvi_top <- sw(list(modeld14, modeld15))  
print(rvi_top)

#Test for outliers in top model (modeld14) using DHARMa 
library(DHARMa)

#simulate residuals
simulation_output <- simulateResiduals(fittedModel = modeld14)

# Run outlier test with bootstrap 
outlier_test <- testOutliers(simulation_output, type = "bootstrap")

# Plot outlier test results
plot(outlier_test)

#View outlier test results
str(outlier_test)

#Test for over dispersion in top model (modeld14)

# Test for over dispersion
overdispersion_test <- testDispersion(simulation_output)

# print test
print(overdispersion_test)

#Create coefficient plot
coefficients <- summary(modeld14)$coefficients$cond  

# Get the confidence intervals
conf_int <- confint(modeld14)

# Create a data frame 
coefficients_df <- data.frame(
  variable = rownames(coefficients),  
  estimate = coefficients[, "Estimate"],  
  conf.low = conf_int[, 1],  
  conf.high = conf_int[, 2]  
)

coefficients_df <- coefficients_df %>%
  filter(variable != "(Intercept)")

# Identify if confidence intervals overlap zero
coefficients_df <- coefficients_df %>%
  mutate(overlap_zero = ifelse(conf.low <= 0 & conf.high >= 0, "Overlap Zero", "No Overlap Zero"))

coefficients_df <- coefficients_df %>%
  mutate(distance_from_zero = abs(estimate)) %>% 
  arrange(desc(estimate > 0), ifelse(estimate > 0, -distance_from_zero, distance_from_zero))

coefficients_df$variable <- factor(coefficients_df$variable, levels = coefficients_df$variable)

ggplot(coefficients_df, aes(x = estimate, y = variable, color = overlap_zero)) +
  geom_point(size = 2) +  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("No Overlap Zero" = "black", "Overlap Zero" = "darkgrey")) + 
  scale_y_discrete(labels = c(
    "LocationNEVI" = "Location: NEVI", 
    "LocationUSA" = "Location: USA",
    "LocationHG" = "Location: HG",
    "LocationNC" = "Location: NC",
    "LocationSEVI" = "Location: SEVI",
    "LocationSWVI" = "Location: SWVI",
    "LocationOTHER" = "Location: Other",
    "CertificationOTHER" = "Certification: Other",
    "CertificationPCOC" = "Certification: PCOC",
    "CertificationPRO" = "Certification: PRO",
    "CertificationSVOP/MED" = "Certification: SVOP/MED",
    "Experiencelt10" = "Experience: 6-10 yrs",
    "Experiencelt15" = "Experience: 11-15 yrs",
    "Experiencelt5" = "Experience: ≤ 5 yrs",
    "VesselSailboat" = "Vessel: Sailboat",
    "VesselMotorized-lt8" = "Vessel: Motorized < 8m",
    "VesselSelf-propelled" = "Vessel: Self-propelled",
    "CampaignYes" = "Campaign: Yes"
  )) +  
  theme_minimal() +
  labs(
    x = "Coefficient Estimate", 
    y = "Predictor"
  ) +
  theme(
    legend.position = "none",  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 16)   
  )

# Filter the data to only include location categories
location_categories <- c("LocationNEVI", "LocationUSA", "LocationHG", "LocationNC", 
                         "LocationSEVI", "LocationSWVI", "LocationOTHER")

coefficients_location_df <- coefficients_df %>% 
  filter(variable %in% location_categories)

# Create the plot using the filtered data
ggplot(coefficients_location_df, aes(x = estimate, y = variable, color = overlap_zero)) +
  geom_point(size = 2) +  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("No Overlap Zero" = "blue", "Overlap Zero" = "skyblue")) +  
  scale_y_discrete(labels = c(
    "LocationNEVI" = "Location: NEVI", 
    "LocationUSA" = "Location: USA",
    "LocationHG" = "Location: HG",
    "LocationNC" = "Location: NC",
    "LocationSEVI" = "Location: SEVI",
    "LocationSWVI" = "Location: SWVI",
    "LocationOTHER" = "Location: Other"
  )) +  
  theme_minimal() +
  labs(
    x = "Coefficient Estimate", 
    y = "Location"
  ) +
  theme(
    legend.position = "none",  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()   
  )



