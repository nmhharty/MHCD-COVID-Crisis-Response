#Initial data cleaning and prep of 2020 COVID-19 Crisis Response Survey v2 launched May 7 2020
#Loading qualtrics data, joining to Staff Summary, preparing for analysis of survey data and response rate

library(qualtRics)
library(tidyverse)

#Connect to BI01 server and retrieve data
library(RODBC)
BI01 <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")
#bring in SQL tables
StaffSummary <- sqlQuery(BI01, 'SELECT * FROM Staging.[dbo].[COVID19SurveyStaffListHistory]')
#change Staff ID column name to be shorter and a factor and Clinical Staff columns
StaffSummary <- StaffSummary %>%
  rename(StaffID="Most Recent Active Directory Staff ID") %>%
  mutate(ClinicalYN = ifelse(`Total Staff Clinical Team FTE`>0,"Yes","No")) %>%
  filter(Week>5)

StaffSummary$StaffID <- as.factor(StaffSummary$StaffID)

#connect to Qualtrics
qualtrics_api_credentials(api_key = "9b9tIGyWZ2PhxDNF97qs9rHC2aIjv2rEJw70SDOw", 
                          base_url = "https://mhcd.co1.qualtrics.com",
                          install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
all_surveys() %>%
  filter(str_detect(name,"COVID"))
#COVID Survey id = SV_38EBgL3jCbTmRvL
COVIDall_v2 <- fetch_survey("SV_5BXQdGNYWEh0nNr",
                         label = TRUE, force_request = TRUE)

#trim columns not needed, add new
colnames(COVIDall_v2)
COVIDtrim_v2 <- COVIDall_v2 %>%
  filter(DistributionChannel!="preview") %>%
  select(c(RecordedDate, RecipientEmail, RecipientLastName, RecipientFirstName, Q16, Q17, Q11, Q11_1, Q11_2, Q11_3, Q11_4, Q11_5, Q11_6, Q13_1, 
           Q13_2, Q13_3, Q13_11, Q13_12, Q13_13, Q15, `Q13_x11 - Topics`, `Q11 - Topics`, `Q13_x1 - Topics`, `Q13_x2 - Topics`, `Q13_x3 - Topics`, 
          `Q13_x12 - Topics`, `Q13_x13 - Topics`, `Q15 - Topics`
    #Not using Parent Codes (May 8 2020)
        # , `Q13_x11 - Parent Topics`
        # , `Q11 - Parent Topics`
        # , `Q13_x1 - Parent Topics`
        # , `Q13_x2 - Parent Topics`
        # , `Q13_x3 - Parent Topics`
        # , `Q13_x12 - Parent Topics`
        # , `Q13_x13 - Parent Topics`
        # , `Q15 - Parent Topics`
          )) %>%
  rename("WorkLocation" = Q16, "WorkInPersonYN" = Q17, "ResponseWentWellAppreciate" = Q11, "WBrank" = Q11_1, "TechRank" = Q11_2, 
         "ProcessRank" = Q11_3, "WFHrank" = Q11_4, "InPersonRank" = Q11_5, "OtherRank" = Q11_6, "ChallWB_Full" = Q13_1, "ChallTech_Full" = Q13_2, 
         "ChallProcess_Full" = Q13_3, "ChallWFH_Full" = Q13_11, "ChallInPerson_Full" = Q13_12, "ChallOther_Full" = Q13_13, "Resources_Full" = Q15, 
         "ResponseWentWellAppreciate_Codes" = `Q11 - Topics`, "ChallWB_Codes" = `Q13_x1 - Topics`, "ChallTech_Codes" = `Q13_x2 - Topics`, 
         "ChallProcess_Codes" = `Q13_x3 - Topics`, "ChallWFH_Codes" = `Q13_x11 - Topics`, "ChallInPerson_Codes" = `Q13_x12 - Topics`, 
         "ChallOther_Codes" = `Q13_x13 - Topics`, "Resources_Codes" = `Q15 - Topics`
    #Not using Parent Codes (May 8 2020)
         # , "ResponseWentWellAppreciate_ParentCodes" = `Q11 - Parent Topics`
         # , "ChallWB_ParentCodes" = `Q13_x1 - Parent Topics`
         # , "ChallTech_ParentCodes" = `Q13_x2 - Parent Topics`
         # , "ChallProcess_ParentCodes" = `Q13_x3 - Parent Topics`
         # , "ChallWFH_ParentCodes" = `Q13_x11 - Parent Topics`
         # , "ChallInPerson_ParentCodes" = `Q13_x12 - Parent Topics`
         # , "ChallOther_ParentCodes" = `Q13_x13 - Parent Topics`
         # , "Resources_ParentCodes" = `Q15 - Parent Topics`
         ) %>%
#calculate new time column to convert from wrong timezone  
  mutate(LocalTime = with_tz(RecordedDate, "America/Denver")) %>%
#create column for count of weeks since survey initiated
##update this code to be dynamic by counting # weeks since survey launched
  mutate(Week = ifelse(LocalTime<"2020-05-21",7,
                      ##*Skipped second distribution because Employee Engagement survey went out, but week 1 survey wasn't closed in time
                        ifelse(LocalTime<"2020-06-04"&LocalTime>"2020-05-20",NA,
                         ifelse(LocalTime<"2020-06-18"&LocalTime>"2020-06-03",11,
                #          ifelse(LocalTime<"2020-07-02"&LocalTime>"2020-06-17",13,
                #            ifelse(LocalTime<"2020-07-16"&LocalTime>"2020-07-01",15,
                       NA))))

#Use COVIDtrim_v2 for other joins. COVIDtrimDemos_v2 is for all reporting


COVIDtrimDemos_v2 <- COVIDtrim_v2 %>%
#Join up Staff Summary data to add demographics to survey data
  left_join(StaffSummary, by = c("RecipientFirstName" = "First Name", "RecipientLastName" = "Last Name", "Week"="Week")) %>%
  mutate("Length of Employment" = ifelse(difftime(LocalTime,`Current Hire Date`, units = "weeks")<26,"Probationary",
                                  ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<52,"Under One Year",
                                         ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<156,"Under Three Years",
                                                ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<260,"Under Five Years",
                                                       ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<520,"Under Ten Years","Ten Years or More")))))) %>%
  select(-`MHCD Email`) %>%
  mutate(Week=ifelse(Week==7,((Week-5)/2),((Week-7)/2)))

COVIDtrimDemos_v2$`Length of Employment` <- as_factor(COVIDtrimDemos_v2$`Length of Employment`) %>%
  fct_relevel("Probationary", "Under One Year", "Under Five Years", "Under Ten Years", "Ten Years or More")

#Add clinical team groupings
OPT <- c("222 - Outpatient at Humboldt", "226 - Evergreen", "227 - Aspen Outpatient Team", "230 - Birch-OP", "725 - Living & Learning with HIV", 
         "750 - Wellshire Behavioral Services", "911 - Access Center") 
CTT <- c("844 - SGF Expansion-Ctt", "845 - Medicaid Expansion-CTT", "854 - 21st Century Grant", "224 - Downing Community Treatment Team", 
         "300 - Adult Spectrum Care")  

HITT <- c("221 - Rehab-Assertive Community", "622 - HIIATT", "676 - Vine-SLT", "805 - Vine Community Treatment Team", "808 - FUSE", 
                "809 - Denver's Road Home", "812 - Welcome Home", "813 - PHASE Project", "816 - Bridges", "830 - Independent Living Team", 
                "840 - Humboldt Community Treatment Team", "851 - Downing High Intensity Treatment Team", "853 - Downing-EHITT", "882 - Social Impact Bond", 
                "883 - OBH Competency Restoration")
Rehabilitation <- c("240 - Resource Center", "250 - 2Succeed in Education", "260 - 2Succeed in Employment")

Residential <- c("612 - Monaco House", "614 - Monroe House", "623 - David House", "626 - New Visions", "628 - Ash House", "630 - Franklin House", 
                 "634 - Vallegos House", "638 - Lincoln House", "650 - Beeler House", "652 - Park Place", "660 - Second Street", "670 - Miller House", 
                 "672 - Grant Street", "674 - Vine Apts", "682 - Narcissus House", "684 - Roanoke Apts", "686 - Humboldt Apartments", "690 - Sanderson Apts", 
                 "695 - Housing Peer Specialists", "698 - Rental Admin", "699 - Rental Assistance")

CF <- c("401 - School Based Services", "422 - Intensive Day Treatment", "446 - Child & Family Outpatient Serv", "447 - Home-Based Intensive Care", 
        "449 - Right Start for Infant Mental Health", "450 - Healthy Living", "462 - Care Management", "464 - Family,Friends,Neighbors", 
        "499 - Enhancing Access to Child Psychiatry", "720 - El Centro de las Familias", "724 - DEAF COUNSELING SERVICES", "727 - Early Childhood Consultation",
        "728 - ABC Play")
Emerson <-  "255 - Emerson"
MedProviders <- "131 - Medical/ Clinical Office"

COVIDtrimDemos_v2 <- COVIDtrimDemos_v2 %>%
  mutate(TeamGrouping = ifelse(`Primary Team` %in% OPT,"OPT",
                               ifelse(`Primary Team` %in% CTT,"CTT",
                                      ifelse(`Primary Team` %in% HITT,"HITT",
                                             ifelse(`Primary Team` %in% Rehabilitation,"Rehab",
                                                    ifelse(`Primary Team` %in% Residential,"Residential",
                                                           ifelse(`Primary Team` %in% CF,"CF",
                                                                  ifelse(`Primary Team` %in% Emerson,"Emerson",
                                                                         ifelse(`Primary Team` %in% MedProviders,"MedProviders",NA)))))))))
StaffSummary <- StaffSummary %>%
  mutate(TeamGrouping = ifelse(`Primary Team` %in% OPT,"OPT",
                               ifelse(`Primary Team` %in% CTT,"CTT",
                                      ifelse(`Primary Team` %in% HITT,"HITT",
                                             ifelse(`Primary Team` %in% Rehabilitation,"Rehab",
                                                    ifelse(`Primary Team` %in% Residential,"Residential",
                                                           ifelse(`Primary Team` %in% CF,"CF",
                                                                  ifelse(`Primary Team` %in% Emerson,"Emerson",
                                                                         ifelse(`Primary Team` %in% MedProviders,"MedProviders",NA)))))))))



#Separate codes and pivot into long form --> one dataframe per question
colnames(COVIDtrimDemos_v2)
ResponseWentWellAppreciate_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ResponseWentWellAppreciate_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ResponseWentWellAppreciate, ResponseWentWellAppreciate_Codes)
ChallWB_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallWB_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallWB_Full, ChallWB_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

ChallTech_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallTech_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallTech_Full, ChallTech_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

ChallProcess_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallProcess_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallProcess_Full, ChallProcess_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

ChallInPerson_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallInPerson_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallInPerson_Full, ChallInPerson_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

ChallWFH_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallWFH_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallWFH_Full, ChallWFH_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

ChallOther_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'ChallOther_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, ChallOther_Full, ChallOther_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))

Resources_Codes <- separate_rows(COVIDtrimDemos_v2, sep = ",", 'Resources_Codes', convert = TRUE) %>%
  select(1:6,8:13,29:47, Resources_Full, Resources_Codes) %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community"))



##***Need a way to link parent codes to the sub-codes. ONLY IF USING PARENT CODES


#prepping theme groupings for Went Well and Resources Questions
ResponseWentWellAppreciate_Codes <- ResponseWentWellAppreciate_Codes %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community")) %>%
  mutate(`Theme Grouping`=ifelse(str_sub(`ResponseWentWellAppreciate_Codes`,1,28)=="Organizational Communication","Organizational Communication",
                                 ifelse(str_sub(`ResponseWentWellAppreciate_Codes`,1,17)=="Resource Delivery","Resource Delivery",
                                        ifelse(str_sub(`ResponseWentWellAppreciate_Codes`,1,7)=="Support","Support",
                                               ifelse(str_sub(`ResponseWentWellAppreciate_Codes`,1,8)=="Benefits","Benefits",NA))))) %>%
  mutate(FilterOut=str_detect(`ResponseWentWellAppreciate_Codes`,"_"))

#PTO, Benefits, and Productivity Expectations for Resources question
Resources_Codes <- Resources_Codes %>%
  mutate(InPersonSummary=ifelse(WorkInPersonYN=="Yes","Yes","No"), WorkLocSummary=ifelse(WorkLocation=="From home","From home","MHCD or Community")) %>%
  mutate(`Theme Grouping`=ifelse(str_sub(`Resources_Codes`,1,3)=="PTO","PTO",
                                 ifelse(str_sub(`Resources_Codes`,1,25)=="Productivity Expectations","Productivity Expectations",
                                       ifelse(str_sub(`Resources_Codes`,1,8)=="Benefits","Benefits",NA)))) %>%
  mutate(FilterOut=str_detect(`Resources_Codes`,"_"))



QuantData <- COVIDtrimDemos_v2 %>%
  select(1:6, 8:13,29:46)


#response rate data prep
RRdemos_v2 <- StaffSummary %>%
  left_join(COVIDtrim_v2, by = c("First Name" = "RecipientFirstName", "Last Name" = "RecipientLastName", "Week"="Week")) %>%
  mutate(Response = ifelse(is.na(RecordedDate),"No","Yes")) %>%
  mutate(ResponseNum = ifelse(Response=="Yes",1,0)) %>% 
  select(1:22,48:49)

OverallResponsePercent <- RRdemos_v2 %>%
  select(-Week) %>%
  distinct() %>%
  summarise("Unique Respondents" = sum(ResponseNum), Total = n_distinct(`Full Name`), 
            "Percent Responded" = scales::percent(`Unique Respondents`/Total)) %>%
  select(`Percent Responded`)


#we exported codes for Sarah for v1 survey. May not need to do that here.

# #exporting files for Sarah
# codesLong %>%
#   filter(Week!=1, CodeWW %in% c("New Work Space - Routines", "Self-Care - Personal Accomplishment","Work accomplishment", "Healthy - Safe", 
#                                 "Organizational communication")) %>%
#   select(`StaffID`, Week, CodeWW, Q3) %>%
#   distinct() %>%
#   group_by(CodeWW, Week, Q3) %>%
#   summarise(n()) %>%
#   select(Theme = CodeWW, Week, Response = Q3) %>%
#   write.csv(file = "WorksWellCodesSarah.csv")
# 

