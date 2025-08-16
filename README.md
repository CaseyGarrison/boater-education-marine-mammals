# boater-education-marine-mammals
Code and data for the analysis in "Recipients of boater education have greater knowledge of marine mammal regulations in coastal British Columbia"
#############################################################################################################

THIS DATA PACKAGE ACCOMPANIES:

Garrison, Casey, McMillan, Christie J., Birdsall, Caitlin, Hildering, Jackie, and Darimont, Chris T. Recipients of boater education have greater knowledge of marine mammal regulations among boaters in coastal British Columbia.

Please contact Casey Garrison (caseyrgarrison@gmail.com) with questions about the database or if you find any errors in the file. Please let us know if you use the data and we would be happy to provide assistance. 

This data package contains the original dataset as well as R code used to generate figures and analyses in the manuscript. 

##############################################################################################################

CONTENTS

1. READ—ME.md —This file, including a description of variables.

2. Mihalikdata.csv--A data file used in all of the figures and analyses that appear in the paper. It contains prices advertised online for guided hunts of 15 North American big game species used in our study, and the jurisdictions for the offered hunts. It also includes data collected on our predictor variables: average male body mass, NatureServe conservation status, classification (ungulate or carnivore), and presence or absence of ‘difficult' or 'dangerous’ hunt descriptions. 

3. NEVIAnalysis+DistanceScore.R — Code for the NEVI and Distance Score Analysis in the manuscript. 

4.  NEVIAnalysis+DistanceScoreFigures.R — Code for the NEVI and Distance Score figures in the manuscript. 

5. NEVIAnalysis+KnowledgeScore.R — Code for the NEVI and Knowledge Score Analysis in the manuscript. 

6. NEVIAnalysis+KnowledgeScoreFigures.R — Code for the NEVI and Knowledge Score figures in the manuscript. 

7. SingleAnalysis+DistanceScore.R — Code for the Single Location and Distance Score Analysis in the manuscript. 

8.  SingleAnalysis+DistanceScoreFigures.R — Code for the Single Location and Distance Score figures in the manuscript. 

9. SingleAnalysis+KnowledgeScore.R — Code for the Single Location and Knowledge Score Analysis in the manuscript. 

10. SingleAnalysis+KnowledgeScoreFigures.R — Code for the Single Location and Knowledge Score figures in the manuscript. 

11. NEVIAnalysis.xlsx — Cleaned survey results for NEVI Analysis.

12. SingleAnalysis.xlsx — Cleaned survey results for Single Location Analysis.

13. Survey Marine Mammals Questions and Answer Key.docx — Answer key to the MERS survey of recreational boaters.

14. Survey Marine Mammals and Recreational Boating Data - no identifiers RAW.csv — Raw data from the results of the MERS survey of recreational boaters.

###############################################################################################################

VARIABLE NAMES AND DESCRIPTIONS

FILES: SingleAnalysis.xlsx and NEVIAnalysis.xlsx

VARIABLES:

Role: Primary role of the boater aboard the vessel. 
-Operator: Mainly operates the vessel.
-Passenger: Mainly a passenger on the vessel.

Vessel: Type of vessel do you most often used when boating recreationally off the coast of British Columbia.
-Motorized-lt8: less than eight metres long
-Motorized-gt8: Motorized vessel less than eight metres long
-Sailboat: Sailboat
-Self-propelled: Any vessel not powered by an engine (e.g., kayak, dinghy, etc.)

Experience: How many years experience the boater has with the above type of vessel.
-lt5: Less than five years 
-lt10: Six to 10 years experience
-lt15: 11 to 15 years experience
-gt15: Greater than 15 years experience

Location: Location(s) where the boater regularily goes on the above type of vessel.
-NEVI: Northeast Vancouver Island
-HG: Haida Gwaii
-NC: North Coast
-CC: Central Coast
-SEVI: Southeast Vancouver Island
-SWVI: Southwest Vancouver Island
-USA: Alaska or Washington state
-Other: Any other location not listed as an option in the survey (for Single Location Analysis) or any location other than NEVI (for NEVI analysis)

Certification: What level of certification the boater has obtained
-PCOC: Pleasure Craft Operator Card
-SVOP/MED: Small Vessel Operator Proficiency or Marine Emergency Duties
-PRO: Any professional mariner certification
-OTHER: Any other certification not listed as an option in the survey
-NONE: No certification

Campaign: Whether or not the boater was exposed to MERS’ See a Blow? Go Slow! education campaign
-Yes: Yes, seen the campaign.
-No: No, has not seen the campaign.

DistanceScore: Total score obtained by the survey respondent on the questions pertaining to legal distances boaters must remain from marine mammals. Scored out of three possible points.

KnowledgeScore: Total score obtained by the survey respondent on all of the knowledge testing questions from the MERS survey. Scored out of 23 possible points.

##############################################################################################################

ACKNOWLEDGEMENTS

This database was compiled by Casey Garrison. 
