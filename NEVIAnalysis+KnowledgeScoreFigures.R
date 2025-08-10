###################################NEVI  Analysis using Knowledge Score Figures###################################
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

model7 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel + Campaign, 
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

preds <- ggeffect(model7, terms = "Experience")
preds$predicted <- preds$predicted * 23

# Reorder x variable for plot
preds$x <- factor(preds$x, levels = c("lt5", "lt10", "lt15", "gt15"))

ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +  
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low * 23, ymax = conf.high * 23), width = 0.2) + 
  labs(
    x = "Experience",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = rep("grey70", length(unique(preds$x)))) +  
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 23, by = 2)
  ) +
  coord_cartesian(ylim = c(10, 16.5)) +
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 40),
    plot.background = element_rect(color = NA),
    panel.border = element_blank(),
    clip = "off",
    axis.text = element_text(size = 14),  # Bigger axis text
    axis.title = element_text(size = 16)  # Bigger axis titles
  )








                               
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
# Obtain predictions from the model
preds <- ggeffect(model7, terms = "Location")
preds$predicted <- preds$predicted * 23

#Order the variables
preds$x <- factor(preds$x, levels = c("Other", "NEVI"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low * 23, ymax = conf.high * 23), width = 0.2) + 
  labs(
    x = "Location",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = c("Other" = "grey70", 
                               "NEVI" = "grey70")) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 23, by = 2)
  ) +
  coord_cartesian(ylim = c(10, 16.5)) +
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 40),
    plot.background = element_rect(color = NA),
    panel.border = element_blank(),
    clip = "off",
    axis.text = element_text(size = 14),  # Bigger axis text
    axis.title = element_text(size = 16)
  )


###############CERTIFICATION

#Define fixed values for Certification
fixed_values <- data.frame(
  Certification = c("NONE", "OTHER", "PCOC", "SVOP/MED", "PRO"),  # Categories for Experience
  Location = "Other",  # Fixed Location
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Campaign = "No",  # Fixed Campaign
  Experience = "gt15"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(model7, terms = "Certification")
preds$predicted <- preds$predicted * 23
#Order the variables
preds$x <- factor(preds$x, levels = c("NONE", "OTHER", "PCOC", "SVOP/MED", "PRO"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low * 23, ymax = conf.high * 23), width = 0.2) + 
  labs(
    x = "Certification",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = c("NONE" = "grey70", 
                               "OTHER" = "grey70", 
                               "PCOC" = "grey70", 
                               "SVOP/MED" = "grey70", 
                               "PRO" = "grey70")) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 23, by = 2)
  ) +
  coord_cartesian(ylim = c(10, 16.5)) +
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 40),
    plot.background = element_rect(color = NA),
    panel.border = element_blank(),
    clip = "off",
    axis.text = element_text(size = 12),  # Bigger axis text
    axis.title = element_text(size = 16)
  )





##############VESSEL

#Define fixed values for Vessel
fixed_values <- data.frame(
  Vessel = c("Motorized-gt8", "Motorized-lt8", "Sailboat", "Self-propelled"),  # Categories for Experience
  Location = "Other",  # Fixed Location
  Certification = "NONE",  # Fixed Vessel
  Campaign = "No",  # Fixed Campaign
  Experience = "gt15"  # Fixed Certification
)

#Generate predictions
preds <- ggeffect(model7, terms = "Vessel")
preds$predicted <- preds$predicted * 23
#Order the variables
preds$x <- factor(preds$x, levels = c("Motorized-gt8", "Motorized-lt8", "Sailboat", "Self-propelled"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low * 23, ymax = conf.high * 23), width = 0.2) + 
  labs(
    x = "Vessel",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = c("Motorized-gt8" = "grey70", 
                               "Sailboat" = "grey70", 
                               "Motorized-lt8" = "grey70", 
                               "Self-propelled" = "grey70")) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 23, by = 2)
  ) +
  coord_cartesian(ylim = c(10, 16.5)) +
  theme_minimal() + 
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 40),
    plot.background = element_rect(color = NA),
    panel.border = element_blank(),
    clip = "off",
    axis.text = element_text(size = 11),  # Bigger axis text
    axis.title = element_text(size = 16)
  )






####################CAMPAIGN

#Define fixed values for Campaign
fixed_values <- data.frame(
  Campaign = c("No", "Yes"),  # Categories for Location
  Experience = "gt15",  # Fixed Experience
  Vessel = "Motorized-gt8",  # Fixed Vessel
  Location = "Other",  # Fixed Campaign
  Certification = "NONE"  # Fixed Certification
)
# Generate predictions using ggeffect()
preds <- ggeffect(model7, terms = "Campaign")

preds$predicted <- preds$predicted * 23  # Scaling to 0-23 range

# Now we order the variables for consistency in plotting
preds$x <- factor(preds$x, levels = c("No", "Yes"))

install.packages("ggh4x")  
library(ggplot2)
library(ggh4x)

# install.packages("ggh4x")  # if you haven't already
library(ggplot2)
library(ggh4x)

library(ggplot2)

ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low * 23, ymax = conf.high * 23), width = 0.2) + 
  labs(
    x = "Campaign",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = c("Yes" = "grey70", 
                               "No" = "grey70")) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(10, 23, by = 2)
  ) +
  coord_cartesian(ylim = c(10, 16.5)) +
  annotate("text", x = 0.35, y = 10, label = "0", vjust = 1.5, size = 4) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 40),
    plot.background = element_rect(color = NA),
    panel.border = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 16)
  )





#####
  



































































