###################################Single Location Analysis using Knowledge Score Figures###################################
#Read data file
Survey1Location_Campaign <- read.csv("Survey1Location_Campaign.csv")

#Load required package
library(ggeffects)
library(ggplot2)
library(dplyr)

#Rename campaign column (typo in table)
colnames(Survey1Location_Campaign)[colnames(Survey1Location_Campaign) == "Campagin"] <- "Campaign"

#make predictor variables factors
Survey1Location_Campaign$Location <- as.factor(Survey1Location_Campaign$Location)
Survey1Location_Campaign$Vessel <- as.factor(Survey1Location_Campaign$Vessel)
Survey1Location_Campaign$Role <- as.factor(Survey1Location_Campaign$Role)
Survey1Location_Campaign$Certification <- as.factor(Survey1Location_Campaign$Certification)
Survey1Location_Campaign$Experience <- as.factor(Survey1Location_Campaign$Experience)
Survey1Location_Campaign$Campaign <- as.factor(Survey1Location_Campaign$Campaign)

model15 <- glmmTMB(cbind(KnowledgeScore, 23 - KnowledgeScore) ~ Location + Certification + Experience + Vessel + Campaign, 
                   family = betabinomial(), 
                   data = Survey1Location_Campaign)

#Define fixed values for Location
fixed_values <- data.frame(
  Location = c("SEVI", "SWVI", "NEVI","CC", "NC", "HG", "USA", "OTHER"),  
  Vessel = "Motorized-gt8",  
  Campaign = "No",  
  Certification = "NONE"  
)

preds <- ggpredict(model15, terms = "Location", type = "fixed")  # gets predicted PROPORTIONS

# Scale up to actual number of correct responses (out of 23)
preds$predicted <- preds$predicted * 23
preds$conf.low <- preds$conf.low * 23
preds$conf.high <- preds$conf.high * 23

# Preserve your factor order (optional)
preds$x <- factor(preds$x, levels = c("SEVI", "SWVI", "NEVI", "CC", "NC", "HG", "USA", "OTHER"))

#create plot
ggplot(preds, aes(x = reorder(x, predicted), 
                  y = predicted, 
                  fill = x)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    x = "Location",
    y = "Predicted Knowledge Score"
  ) +
  scale_fill_manual(values = c("SEVI"= "grey", "SWVI"= "grey", "NEVI"= "grey",
                               "CC"= "grey", "NC"= "grey", "HG"= "grey", 
                               "USA"= "grey", "OTHER" = "grey")) +
  theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14),  # Slightly smaller titles
    axis.text = element_text(size = 12)    # Slightly smaller tick labels
  )



