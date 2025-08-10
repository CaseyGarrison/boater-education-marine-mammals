###################################NEVI Location Analysis using Distance Score################################### 

#Read data file
SurveyMersLocation_Campaign <- read.csv("SurveyMersLocation_Campaign.csv")

#Check distribution of distance score
hist(SurveyMersLocation_Campaign$DistanceScore, 
     main = "", 
     xlab = "Distance Score", 
     ylab = "Frequency",
     breaks = seq(0, 3, by = 1), 
     xaxt = "n")  # Suppress x-axis
axis(side = 1, at = 0:3)

# Compute mean and variance of Distance Score to check dispersion
mean_score <- mean(SurveyMersLocation_Campaign$DistanceScore)
variance_score <- var(SurveyMersLocation_Campaign$DistanceScore)
mean_score
variance_score

#Convert predictor variables to factors
SurveyMersLocation_Campaign$Location <- as.factor(SurveyMersLocation_Campaign$Location)
SurveyMersLocation_Campaign$Vessel <- as.factor(SurveyMersLocation_Campaign$Vessel)
SurveyMersLocation_Campaign$Role <- as.factor(SurveyMersLocation_Campaign$Role)
SurveyMersLocation_Campaign$Certification <- as.factor(SurveyMersLocation_Campaign$Certification)
SurveyMersLocation_Campaign$Experience <- as.factor(SurveyMersLocation_Campaign$Experience)
SurveyMersLocation_Campaign$Campaign <- as.factor(SurveyMersLocation_Campaign$Campaign)

#Re level location reference category so it is the same across both MERS location analyses
SurveyMersLocation_Campaign$Location <- relevel(SurveyMersLocation_Campaign$Location, ref = "Other")

#Load required package
library(glmmTMB)

#Run models
modeld1 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Vessel, 
                   family = betabinomial(),
                   data = SurveyMersLocation_Campaign)
modeld2 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience, 
                    family = betabinomial(), 
                    data = SurveyMersLocation_Campaign)
modeld3 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role, 
                    family = betabinomial(),
                    data = SurveyMersLocation_Campaign)
modeld4 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Vessel, 
                    family = betabinomial(), 
                    data = SurveyMersLocation_Campaign)
modeld5 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Vessel + Campaign, 
                    family = betabinomial(), 
                    data = SurveyMersLocation_Campaign)
modeld6 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Campaign, 
                    family = betabinomial(), 
                    data = SurveyMersLocation_Campaign)
modeld7 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Role + Campaign, 
                    family = betabinomial(),
                    data = SurveyMersLocation_Campaign)
modeld8 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Vessel + Campaign, 
                    family = betabinomial(), 
                    data = SurveyMersLocation_Campaign)
null_model <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ 1,  
                      family = betabinomial(),  
                      data = SurveyMersLocation_Campaign)

#Named list of models
models <- list(
  "modeld1" = modeld1,
  "modeld2" = modeld2,
  "modeld3" = modeld3,
  "modeld4" = modeld4,
  "modeld5" = modeld5,
  "modeld6" = modeld6,
  "modeld7" = modeld7,
  "modeld8" = modeld8,
  "null_model" = null_model
)

#view summaries of models
model_summaries <- lapply(models, summary)
model_summaries

#view ranked AICs of models 
library(MuMIn)
aic_results <- model.sel(models, rank = AIC)
print(aic_results)

#Calculate RVI for variables in the top two models
rvi_top <- sw(list(modeld6, modeld8))  
print(rvi_top)


#Test for outliers in top model (modeld6) using DHARMa 
library(DHARMa)

#simulate residuals
simulation_output <- simulateResiduals(fittedModel = modeld6)

# Run outlier test with bootstrap 
outlier_test <- testOutliers(simulation_output, type = "bootstrap")

# Plot outlier test results
plot(outlier_test)

#View outlier test results
str(outlier_test)

#Test for over dispersion in top model (modeld6)

# Test for over dispersion
overdispersion_test <- testDispersion(simulation_output)

# print test
print(overdispersion_test)

#Create coefficient plot
coefficients <- summary(modeld6)$coefficients$cond  

# Get the confidence intervals
conf_int <- confint(modeld6)

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
  scale_color_manual(values = c("No Overlap Zero" = "black", "Overlap Zero" = "darkgray")) +  
  scale_y_discrete(labels = c(
    "LocationNEVI" = "Location: NEVI", 
    "CertificationOTHER" = "Certification: Other",
    "CertificationPCOC" = "Certification: PCOC",
    "CertificationPRO" = "Certification: PRO",
    "CertificationSVOP/MED" = "Certification: SVOP or MED",
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



