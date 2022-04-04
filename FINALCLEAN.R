#©2022 Integrated Musculoskeletal Care, Inc./ Confidential & Proprietary / All content associated with this
#communication is considered confidential, proprietary, and/or legally privileged information. 
#If this document has reached someone other than the intended recipient(s), please notify Steve McClellan 
#immediately at steve.mcclellan@imcpt.com and immediately delete this document. 
#Any unauthorized retention, disclosure, copying, distribution, or use of this proprietary, 
#confidential, and/or legally privileged material is strictly prohibited.

#set the working directory manually

##SET DIRECTORY HERE###
DIREC <- "D:/Internship"
##SET DIRECTORY HERE###


require(magrittr)
require(tidyverse)
require(dplyr)
require(tidyr)
setwd(DIREC)
D <- read.csv("INPUT.csv",header = T,stringsAsFactors = F)
#show header names
names(D)

#Transform Dates to best format
#since date is 1/1/2020
D$Notification.Date<-as.Date(D$Notification.Date,"%m/%d/%Y")
D$Waiver.Date<-as.Date(D$Waiver.Date,"%m/%d/%Y")
D$DOB<-as.Date(D$DOB,"%m/%d/%Y")
#BCBSSC is expecting the end of waiver date which is 6 months after issue date
D$Waiver.Date <- D$Waiver.Date+180
#return DOB to 01/01/2022 format with leading zeros
yearD<-substr(D$DOB,1,4)
monthD<-substr(D$DOB,6,7)
dayD<-substr(D$DOB,9,10)
D$DOB<-as.character(paste(monthD,dayD,yearD,sep="/"))

#return Notification.Date to 01/01/2022 format with leading zeros
yearN<-substr(D$Notification.Date,1,4)
monthN<-substr(D$Notification.Date,6,7)
dayN<-substr(D$Notification.Date,9,10)
D$Notification.Date<-as.character(paste(monthN,dayN,yearN,sep="/"))

#return Waiver.Date to 01/01/2022 format with leading zeros
yearW<-substr(D$Waiver.Date,1,4)
monthW<-substr(D$Waiver.Date,6,7)
dayW<-substr(D$Waiver.Date,9,10)
D$Waiver.Date<-as.character(paste(monthW,dayW,yearW,sep="/"))

#convert values to Upper case
names <- as.list(names(D))
names(D) <- lapply(names, toupper)


#convert fields to characters
D$MEMBER.ID<-as.character(D$MEMBER.ID)
D$ZIP<-as.character(D$ZIP)
D$PHONE.NUMBER<-as.character(D$PHONE.NUMBER)
D$CPT.CODES<-as.character(D$CPT.CODES)


#If variable name has '.', replace with " "
D <- D %>% 
  mutate(
    "First Name" = FIRST.NAME,
    "Last Name" = LAST.NAME,
    "Phone Number" = PHONE.NUMBER,
    "ICD10 Codes" = ICD10.CODES,
    "CPT Codes" = CPT.CODES,
    "Body Region" = BODY.REGION,
    "Notification Date" = NOTIFICATION.DATE,
    "Waiver Date" = WAIVER.DATE,
    "Waiver Reason" = WAIVER.REASON,
    "Member Id" = MEMBER.ID
  )
drop <- c("FIRST.NAME","LAST.NAME", "PHONE.NUMBER", "ICD10.CODES", "CPT.CODES", "BODY.REGION",
          "NOTIFICATION.DATE", "WAIVER.DATE", "WAIVER.REASON", "MEMBER.ID",
          "ICD10")
D = D[,!(names(D) %in% drop)]

#Make Gender M or F
D <- D %>% 
  mutate(
    GENDER = ifelse(GENDER %in% c("M", "male"), "M", "F")
  )

#make SSN null
D$SSN<-"Null"

#we will have a row in the source file for every CASE

#create UMID WITH CASE!!!
D$caseID<-paste(D$`First Name`,D$`Last Name`,D$DOB,D$`Body Region`,sep = "-")

dups<-duplicated(D$caseID)
D<-subset(D,dups==F)

#here is where we need to split the csv arrays into "|" delimited with 12 cells for each ICD and 40 cells for associated CPTs
D1<-subset(D,sel=c("Member Id","caseID","SSN","Last Name","First Name","DOB","GENDER","ADDRESS","CITY","STATE","ZIP","Phone Number","PLAN","Body Region","SIDE"))
D2<-subset(D,sel=c("ICD10 Codes"))
D3<-subset(D,sel=c("CPT Codes"))
D4<-subset(D,sel=c("Notification Date","Waiver Date","Waiver Reason"))


#Lets try a little something with tidydata
DICD <- D2 %>% 
  separate("ICD10 Codes", into = c("Code1", "Code2", "Code3", "Code4", "Code5", "Code 6",
                                   "Code7", "Code8", "Code9", "Code10", "Code11", "Code12"))

DCPT <- D3 %>% 
  separate("CPT Codes", into = c("CPT1", "CPT2", "CPT3", "CPT4", "CPT5", "CPT6", "CPT7",
                                 "CPT8", "CPT9", "CPT10", "CPT11", "CPT12", "CPT13",
                                 "CPT14", "CPT15", "CPT16", "CPT17", "CPT18", "CPT19",
                                 "CPT20","CPT21", "CPT22", "CPT23", "CPT24", "CPT25", "CPT26", "CPT27",
                                 "CPT28", "CPT29", "CPT30", "CPT31", "CPT32", "CPT33",
                                 "CPT34", "CPT35", "CPT36", "CPT37", "CPT38", "CPT39",
                                 "CPT40"))
##Setting up the necessary extra frames to be used

##nullblock is a single "stack" of 1 ICD code and 40 CPT codes that get binned when we run out of
##Available ICD codes for that row

##FINALFR is what we will fill up with our ICD and CPT codes to then be re-merged with 
##the other information

nullblock <- as.data.frame(matrix(nrow = 1, ncol = 41))
FINALFR <- as.data.frame(matrix(nrow = nrow(D), ncol = 492))



########## How The Custom Function Works #####################

#Step 1) Create empty data frame called 'empty' with nrow = number of available ICD codes
#for our first observation

#Step 2) Also create data frame with one row and zero columns in which to bind
#our blocks of ICD codes and CPT codes to have them stacked how client likes

#Step 3) 1st loop assigns each ICD code to its own row in 'empty'. This loop also
#stacks all available CPT codes as well as the necessary empty columns onto each row

#Step 4) Bind all the rows end to end for available codes

#Step 5) Figure out how many to stack empty blocks so final frame of codes is 492 columns wide


fill_row <- function(ICDCODE, CPTCODE){
  empty <- as.data.frame(matrix(nrow = sum(!is.na(ICDCODE)), ncol = 41))
  blankfill = as.data.frame(matrix(nrow = 1, ncol = 0))
  for(i in 1:sum(!is.na(ICDCODE))){
    empty[i,] <- ICDCODE[[i]] %>% 
      cbind(CPTCODE)
    
  }
  for(w in 1:nrow(empty)){
    blankfill <- blankfill %>% 
      cbind(empty[w,])
    
  }
  
  for(j in 1:(12-sum(!is.na(ICDCODE)))) {
    
    blankfill <- blankfill %>%  
      cbind(nullblock)
  }
  print(blankfill)
}



#Basically, the next loop will assign the n'th row our ICD codes and n'th row of 
#CPT codes to objects DDICD and DDCPT.

#Those vectors will be assigned to the arguments "ICDCODE" and "CPTCODE" of our fill_row
#function and looped over our whole data frame

#Finally, each time the loop runs it assigns that 492 column long complete observation
#to its corresponding row in FINALFR to be re-merged with the other data in the next step

for(y in 1:nrow(D)){
  DDICD <- as.vector(DICD[y,])
  DDCPT <- as.vector(DCPT[y,])
  for(z in 1:1){
    FINALFR[y,] <- fill_row(ICDCODE = DDICD, CPTCODE = DDCPT)
    
  }
}

##Now to bring it all back
##When we split our raw data into D1, D2 D3, D4, to handle codes, now we must loop to 
#combine to set back together with our complete ICD & CPT code wrangling
DONE <- as.data.frame(matrix(nrow = nrow(D), ncol = 510))
for(b in 1:nrow(D)){
  half <- D1[b,] %>% 
    cbind(FINALFR[b,]) %>% 
    cbind(D4[b,])
  DONE[b,] = half
}

##Lastly we replace all those NA's with blanks so output text file looks right
DONE[is.na(DONE)] <- ""


##Write the table
write.table(DONE,"FINALCLEAN.txt",sep="|",row.names = F,col.names = F, quote = F)
