###################################NEVI Analysis using Distance Score Figures###################################
#Read data file
SurveyMersLocation_Campaign <- read.csv("SurveyMersLocation_Campaign.csv")

#Load required package
library(ggeffects)
library(ggplot2)
library(dplyr)

#make predictor variables factors
SurveyMersLocation_Campaign$Location <- as.factor(SurveyMersLocation_Campaign$Location)
SurveyMersLocation_Campaign$Vessel <- as.factor(SurveyMersLocation_Campaign$Vessel)
SurveyMersLocation_Campaign$Role <- as.factor(SurveyMersLocation_Campaign$Role)
SurveyMersLocation_Campaign$Certification <- as.factor(SurveyMersLocation_Campaign$Certification)
SurveyMersLocation_Campaign$Experience <- as.factor(SurveyMersLocation_Campaign$Experience)
SurveyMersLocation_Campaign$Campaign <- as.factor(SurveyMersLocation_Campaign$Campaign)

modeld6 <- glmmTMB(cbind(DistanceScore, 3 - DistanceScore) ~ Location + Certification + Experience + Campaign, 
                   family = betabinomial(), 
                   data = SurveyMersLocation_Campaign)

############EXPERIENCE

#Define fixed values for Experience
fixed_values <- data.frame(
  Experience = c("lt5", "lt10", "lt15", "gt15"),  # Categories for Experience
  Location = "Other",  # Fixed Location
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Campaign = "No",  # Fixed Campaign
  Certification = "NONE"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(modeld6, terms = "Experience")

#Order the variables
preds$x <- factor(preds$x, levels = c("lt5", "lt10", "lt15", "gt15"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    x = "Experience",
    y = "Predicted Distance Score"
  ) +
  scale_fill_manual(values = c("lt5" = "#1E90FF", "lt10" = "#1E90FF", "lt15" = "#1E90FF", "gt15" = "#1E90FF")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")


####################LOCATION

#Define fixed values for Location
fixed_values <- data.frame(
  Location = c("NEVI", "Other"),  # Categories for Location
  Experience = "gt15",  # Fixed Experience
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Campaign = "No",  # Fixed Campaign
  Certification = "NONE"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(modeld6, terms = "Location")

#Order the variables
preds$x <- factor(preds$x, levels = c("Other", "NEVI"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    x = "Location",
    y = "Predicted Distance Score"
  ) +
  scale_fill_manual(values = c("Other" = "#1E90FF", "NEVI" = "#1E90FF")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

############CERTIFICATION

#Define fixed values for Certification
fixed_values <- data.frame(
  Certification = c("NONE", "OTHER", "PCOC", "SVOP/MED", "PRO"),  # Categories for Experience
  Location = "Other",  # Fixed Location
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Campaign = "No",  # Fixed Campaign
  Experience = "gt15"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(modeld6, terms = "Certification")

#Order the variables
preds$x <- factor(preds$x, levels = c("NONE", "OTHER", "PCOC", "SVOP/MED", "PRO"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    x = "Certification",
    y = "Predicted Distance Score"
  ) +
  scale_fill_manual(values = c("NONE" = "#1E90FF", "OTHER" = "#1E90FF", "PCOC" = "#1E90FF", "SVOP/MED" = "#1E90FF", "PRO" = "#1E90FF")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")


###############CAMPAIGN

#Define fixed values for Campaign
fixed_values <- data.frame(
  Campaign = c("No", "Yes"),  # Categories for Location
  Experience = "gt15",  # Fixed Experience
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Location = "Other",  # Fixed Campaign
  Certification = "NONE"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(modeld6, terms = "Campaign")

#Order the variables
preds$x <- factor(preds$x, levels = c("No", "Yes"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    x = "Campaign",
    y = "Predicted Distance Score"
  ) +
  scale_fill_manual(values = c("No" = "#1E90FF", "Yes" = "#1E90FF")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")
