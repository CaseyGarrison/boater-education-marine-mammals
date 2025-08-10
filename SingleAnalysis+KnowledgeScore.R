###################################Single Location Analysis using Knowledge Score################################### 

#Read data file
Survey1Location_Campaign <- read.csv("Survey1Location_Campaign.csv")

#Check distribution of knowledge score
hist(SurveyMersLocation_Campaign$KnowledgeScore)

# Compute mean and variance of Knowledge Score to check dispersion
mean_score <- mean(Survey1Location_Campaign$KnowledgeScore)
variance_score <- var(Survey1Location_Campaign$KnowledgeScore)
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
model9 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Vessel, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model10 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model11 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model12 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model13 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Campaign, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model14 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Campaign, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model15 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel + Campaign, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
model16 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Vessel + Campaign, 
                  family = betabinomial(), 
                  data = Survey1Location_Campaign)
null_model <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ 1, 
                      family = betabinomial(), 
                      data = Survey1Location_Campaign)

#Named list of models
models <- list(
  "model9" = model9,
  "model10" = model10,
  "model11" = model11,
  "model12" = model12,
  "model13" = model13,
  "model14" = model14,
  "model15" = model15,
  "model16" = model16,
  "null_model" = null_model
)

#View model summaries
model_summaries <- lapply(models, summary)
model_summaries

#Load required package
library(MuMIn)

#view ranked AICs of models 
aic_results <- model.sel(models, rank = AIC)
print(aic_results)

#Calculate RVI for variables in the top two models
rvi_top <- sw(list(model15, model16))  
print(rvi_top)

#Test for outliers in top model (modeld14) using DHARMa 
library(DHARMa)

#simulate residuals
simulation_output <- simulateResiduals(fittedModel = model15)

# Run outlier test with bootstrap 
outlier_test <- testOutliers(simulation_output, type = "bootstrap")

# Plot outlier test results
plot(outlier_test)

#View outlier test results
str(outlier_test)

#Test for over dispersion in top model (model15)

overdispersion_test <- testDispersion(simulation_output)

# print test
print(overdispersion_test)

#Create coefficient plot
coefficients <- summary(model15)$coefficients$cond  

# Get the confidence intervals
conf_int <- confint(model15)

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
  scale_color_manual(values = c("No Overlap Zero" = "black", "Overlap Zero" = "darkgrey")) +  
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


