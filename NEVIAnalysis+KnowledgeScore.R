###################################NEVI Analysis using Knowledge Score###################################
#Read data file
SurveyMersLocation_Campaign <- read.csv("SurveyMersLocation_Campaign.csv")

#Check distribution of knowledge score
hist(SurveyMersLocation_Campaign$KnowledgeScore,
     main = "", 
     xlab = "Knowledge Score", 
     ylab = "Frequency",
     breaks = seq(0, 24, by = 1),  # Breaks from 0 to 24 (23 bins)
     xaxt = "n")
axis(side = 1, at = 0:23)
# Compute mean and variance of knowledge score to check dispersion
mean_knowledgescore <- mean(SurveyMersLocation_Campaign$KnowledgeScore)
variance_knowledgescore <- var(SurveyMersLocation_Campaign$KnowledgeScore)
mean_knowledgescore
variance_knowledgescore

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
model1 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Vessel, 
                  family = betabinomial(), 
                  data = SurveyMersLocation_Campaign)
model2 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model3 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model4 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model5 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Campaign, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model6 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Campaign, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model7 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel + Campaign, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
model8 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Role + Vessel + Campaign, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)
null_model <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ 1, 
                      family = betabinomial(), 
                      data = SurveyMersLocation_Campaign)


#Named list of models
models <- list(
  "model1" = model1,
  "model2" = model2,
  "model3" = model3,
  "model4" = model4,
  "model5" = model5,
  "model6" = model6,
  "model7" = model7,
  "model8" = model8,
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
rvi_top <- sw(list(model7, model8))  
print(rvi_top)

#Calculate Cramer's V to check for correlation between variables

library(vcd)

cat_vars <- c("Location", "Certification", "Experience", "Vessel", "Campaign", "Role")

cramers_v_matrix <- matrix(NA, nrow = length(cat_vars), ncol = length(cat_vars))

for (i in 1:length(cat_vars)) {
  for (j in i:length(cat_vars)) {
    # Create a contingency table for the pair of variables
    tbl <- table(SurveyMersLocation_Campaign[[cat_vars[i]]], SurveyMersLocation_Campaign[[cat_vars[j]]])
    
    # Compute Cramér's V and store it in the matrix
    cramers_v_matrix[i, j] <- assocstats(tbl)$cramer
    cramers_v_matrix[j, i] <- cramers_v_matrix[i, j]  # Symmetric matrix
  }
}

rownames(cramers_v_matrix) <- cat_vars
colnames(cramers_v_matrix) <- cat_vars

print(cramers_v_matrix)

#Test for outliers using in top model (model7) using DHARMa
library(DHARMa)

#simulate residuals
simulation_output <- simulateResiduals(fittedModel = model7)

# Run outlier test with bootstrap 
outlier_test <- testOutliers(simulation_output, type = "bootstrap")

# Plot outlier test results
plot(outlier_test)

#View outlier test results
str(outlier_test)

#Test for over dispersion in top model (model7)

# Test for over dispersion
overdispersion_test <- testDispersion(simulation_output)

# print test
print(overdispersion_test)

#Create coefficient plot
coefficients <- summary(model7)$coefficients$cond  

# Get the confidence intervals
conf_int <- confint(model7)

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
  scale_color_manual(values = c("No Overlap Zero" = "black", "Overlap Zero" = "lightgrey")) +  
  scale_y_discrete(labels = c(
    "LocationNEVI" = "Location: NEVI", 
    "CertificationOTHER" = "Certification: Other",
    "CertificationPCOC" = "Certification: PCOC",
    "CertificationPRO" = "Certification: PRO",
    "CertificationSVOP/MED" = "Certification: SVOP or MED",
    "Experiencelt10" = "Experience: 6-10 years",
    "Experiencelt15" = "Experience: 11-15 years",
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







