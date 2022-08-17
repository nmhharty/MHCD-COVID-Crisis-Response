#Initial data cleaning and prep of 2020 COVID-19 Crisis Response Survey
#Loading qualtrics data, joining to Staff Summary, preparing for analysis of survey data and response rate

library(qualtRics)
library(tidyverse)

#load Maarch2020 staff distribution list
DistributionList <- read_csv("/home/Public/COVID19 Crisis Response/March2020StaffDistribution.csv")
#join to Reporting DB to get staff details

#*****3/31/2020: Staff Summary not a good pull because drops staff info on those who leave org**###
##***Use CSV downloaded**#
#Connect to BI01 server and retrieve data
library(RODBC)
BI01 <- odbcConnect("BI01", uid = "randd", pwd = "Mhcd6770")
#bring in SQL tables
StaffSummary <- sqlQuery(BI01, 'SELECT * FROM Staging.[dbo].[COVID19SurveyStaffListHistory]')
#change Staff ID column name to be shorter and a factor and Clinical Staff columns
StaffSummary <- StaffSummary %>%
  rename(StaffID="Most Recent Active Directory Staff ID") %>%
  mutate(ClinicalYN = ifelse(`Total Staff Clinical Team FTE`>0,"Yes","No"))
  
StaffSummary$StaffID <- as.factor(StaffSummary$StaffID)

# #Load StaffSummary csv
# SS3.31.2020 <- read_csv("/home/Public/COVID19 Crisis Response/March31.2020StaffSummary.csv")
# #recode NAs
# SS3.31.2020 <- SS3.31.2020 %>%
#   na_if("NULL")
# 
# SS3.31.2020$`Current Hire Date` <- as.POSIXlt.character(SS3.31.2020$`Current Hire Date`, format = "%m/%d/%Y")
# SS3.31.2020$`Current Hire Date` <- as.POSIXct(SS3.31.2020$`Current Hire Date`)


# DistributionList <- DistributionList %>%
#   left_join(SS3.31.2020, by = c("FirstName" = "First Name", "LastName" = "Last Name")) %>%
#   mutate(ClinicalYN = ifelse(`Total Staff Clinical Team FTE`>0,"Yes","No"))

#connect to Qualtrics
qualtrics_api_credentials(api_key = "9b9tIGyWZ2PhxDNF97qs9rHC2aIjv2rEJw70SDOw", 
                          base_url = "https://mhcd.co1.qualtrics.com",
                          install = TRUE, overwrite = TRUE)

#COVID Survey id = SV_38EBgL3jCbTmRvL
#May 4 2020: this is just the survey used for the first 5 weeks
COVIDall <- fetch_survey("SV_38EBgL3jCbTmRvL",
                         label = TRUE, force_request = TRUE)

#trim columns not needed, add new
colnames(COVIDall)
COVIDtrim <- COVIDall %>%
  filter(DistributionChannel!="preview") %>%
  select(c(RecordedDate, RecipientEmail, RecipientLastName, RecipientFirstName, Q3, Q4, Q5, Q6, `Q3 - Topics`, `Q3 - Parent Topics`,
           `Q4 - Topics`, `Q4 - Parent Topics`, `Q5 - Topics`, `Q5 - Parent Topics`, `Q6 - Topics`, `Q6 - Parent Topics`)) %>%
  rename("What Went Well Topics" = `Q3 - Topics`, "What Went Well Parent Topics" =  `Q3 - Parent Topics`, "Challenges Topics" = `Q4 - Topics`, 
         "Challenges Parent Topics" = `Q4 - Parent Topics`, "SelfCare Topics" = `Q5 - Topics`,"SelfCare Parent Topics" = `Q5 - Parent Topics`, 
         "MHCD Support Topics" = `Q6 - Topics`, "MHCD Support Parent Topics" = `Q6 - Parent Topics`) %>%
#calculate new time column to convert from wrong timezone  
  mutate(LocalTime = with_tz(RecordedDate, "America/Denver")) %>%
#create column for count of weeks since survey initiated
##update this code to be dynamic by counting # weeks since survey launched
  mutate(Week = ifelse(LocalTime<"2020-04-01",1,
                        ifelse(LocalTime<"2020-04-07"&LocalTime>"2020-04-01",2,
                         ifelse(LocalTime<"2020-04-14"&LocalTime>"2020-04-08",3,
                          ifelse(LocalTime<"2020-04-21"&LocalTime>"2020-04-15",4,
                            ifelse(LocalTime<"2020-05-06"&LocalTime>"2020-04-22",5,
                       NA)))))) 

#***APRIL 15: creating separate COVIDtrim and COVIDtrimDemos becuase have good StaffSummary now.***
#***Using COVIDtrim for other joins. COVIDtrimDemos is for all reporting - change all references in RMD file!
COVIDtrimDemos <- COVIDtrim %>%
#Join up Staff Summary data to add demographics to survey data
  #4/15/2020 switch to joining to StaffSummary since we have a table that's updated with the weekly distribution list + demos
  left_join(StaffSummary, by = c("RecipientFirstName" = "First Name", "RecipientLastName" = "Last Name", "Week"="Week")) %>%
  mutate("Length of Employment" = ifelse(difftime(LocalTime,`Current Hire Date`, units = "weeks")<26,"Probationary",
                                  ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<52,"Under One Year",
                                         ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<156,"Under Three Years",
                                                ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<260,"Under Five Years",
                                                       ifelse(difftime(LocalTime,`Current Hire Date`,units = "weeks")<520,"Under Ten Years","Ten Years or More")))))) %>%
  select(-`MHCD Email`)

COVIDtrimDemos$`Length of Employment` <- as_factor(COVIDtrimDemos$`Length of Employment`) %>%
  fct_relevel("Probationary", "Under One Year", "Under Five Years", "Under Ten Years", "Ten Years or More")

#Separate code columns to one code per columns
##*****UPDATE TO INCLUDE SUFFICIENT COLUMNS FOR # UNIQUE CODES****
#April 20, 2020: increased to 10 codes per question
codesWide <- COVIDtrimDemos %>%
  separate(`What Went Well Topics`, c("WentWell1", "WentWell2", "WentWell3", "WentWell4", "WentWell5", "WentWell6", "WentWell7", "WentWell8", "WentWell9", "WentWell10"), 
           sep = ",") %>%
  separate(`Challenges Topics`, c("Challenges1", "Challenges2", "Challenges3", "Challenges4", "Challenges5", "Challenges6", "Challenges7", "Challenges8", "Challenges9",
                                  "Challenges10"), sep = ",") %>%
  separate(`SelfCare Topics`, c("SelfCare1", "SelfCare2", "SelfCare3", "SelfCare4", "SelfCare5", "SelfCare6", "SelfCare7", "SelfCare8", "SelfCare9", "SelfCare10")
           , sep = ",") %>%
  separate(`MHCD Support Topics`, c("MHCDSupport1", "MHCDSupport2", "MHCDSupport3", "MHCDSupport4", "MHCDSupport5", "MHCDSupport6", "MHCDSupport7", "MHCDSupport8",
                                    "MHCDSupport9", "MHCDSupport10"), sep = ",")

colnames(codesWide)
#April 20, 2020: edited column indices to account for increased number of code columns
codesLong <- codesWide %>%
  group_by(RecipientEmail) %>%
  gather(MHCDSupport, CodeMHCD, 42:51) %>%
  gather(SelfCare, CodeSC, 31:40) %>%
  gather(Challenges, CodeChall, 20:29) %>%
  gather(WentWell, CodeWW, 9:18) %>%
#filter out rows with no codes
  filter(!is.na(CodeMHCD), !is.na(CodeSC), !is.na(CodeChall), !is.na(CodeWW))

##***Need a way to link parent codes to the sub-codes. ONLY IF USING PARENT CODES
##Can probably create a vector for each parent code, but will need the list of codes associated with each
# "Technology Infrastructure" <- c("","","","","","","")



#response rate data prep
RRdemos <- StaffSummary %>%
  left_join(COVIDtrim, by = c("First Name" = "RecipientFirstName", "Last Name" = "RecipientLastName", "Week"="Week")) %>%
  mutate(Response = ifelse(is.na(RecordedDate),"No","Yes")) %>%
  mutate(ResponseNum = ifelse(Response=="Yes",1,0)) %>%
  select(c(1:19,35:36))


OverallResponsePercent <- RRdemos %>%
  select(-Week) %>%
  distinct() %>%
  summarise("Unique Respondents" = sum(ResponseNum), Total = n_distinct(`Full Name`), 
            "Percent Responded" = scales::percent(`Unique Respondents`/Total)) %>%
  select(`Percent Responded`)


#exporting files for Sarah
codesLong %>%
  filter(Week!=1, CodeWW %in% c("New Work Space - Routines", "Self-Care - Personal Accomplishment","Work accomplishment", "Healthy - Safe", 
                                "Organizational communication")) %>%
  select(`StaffID`, Week, CodeWW, Q3) %>%
  distinct() %>%
  group_by(CodeWW, Week, Q3) %>%
  summarise(n()) %>%
  select(Theme = CodeWW, Week, Response = Q3) %>%
  write.csv(file = "WorksWellCodesSarah.csv")

codesLong %>%
  filter(Week!=1, CodeChall %in% c("Mental & Physical Health - Well-being", "Work life balance", "New workflows - systems - processes",
                                   "Communication issues", "Intakes")) %>%
  select(`StaffID`, Week, CodeChall, Q4) %>%
  distinct() %>%
  group_by(CodeChall, Week, Q4) %>%
  summarise(n()) %>%
  select(Theme = CodeChall, Week, Response = Q4) %>%
  write.csv(file = "ChallengesCodesSarah.csv")

codesLong %>%
  filter(Week!=1, CodeSC %in% c("Work-life balance", "Work breaks", "Community-connection", "Outdoors")) %>%
  select(`StaffID`, Week, CodeSC, Q5) %>%
  distinct() %>%
  group_by(CodeSC, Week, Q5) %>%
  summarise(n()) %>%
  select(Theme = CodeSC, Week, Response = Q5) %>%
  write.csv(file = "SelfCareCodesSarah.csv")

codesLong %>%
  filter(Week!=1, CodeMHCD %in% c("Information on procedures - expectations", "Opportunities for connection", "Share successes - positive info",
                                  "Provide encouragement", "Training", "Patience - Understanding")) %>%
  select(`StaffID`, Week, CodeMHCD, Q6) %>%
  distinct() %>%
  group_by(CodeMHCD, Week, Q6) %>%
  summarise(n()) %>%
  select(Theme = CodeMHCD, Week, Response = Q6) %>%
  write.csv(file = "MHCDcodesSarah.csv")


##DON"T SEEM TO NEED THIS
# AnyResponse <- RRdemos %>%
#   select(StaffID,Response) %>%
#   filter(Response=="Yes") %>%
#   distinct() %>%
#   summarise(Response=n()) %>%
#   mutate(TotalPossible=n_distinct(StaffSummary$StaffID), OverallPercent=scales::percent(Response/TotalPossible))
# 
# 
