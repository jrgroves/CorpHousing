#Uses Regex to determine the type of owner within the ownership data.
#Reads in the Parcel files and extracts IDs and Ownership Information

#Jeremy R. Groves
#May 30, 2024

#UPDATES
#June 27:  Add in 2018-2020 but used slightly different code due to different data.
#July 3:   Modified code to create a full ownership (fown) set and the non-owner occupied (noo) set
#August 1: Changed the original attempt to use regex expressions. Includes all owners without regard to tenure
#September 8: Modified to run using parallel processing
#September 17: Added combination into since OWN file long with cleaning up missing values that can be known.
#April 8, 2025: Added data up to 2024 EOY files. Also set to load off external drive for zip file extract.

rm(list=ls())

library(tidyverse)
library(foreign)
library(foreach)
library(doParallel)

#Set Up Cores
  n_cores <- detectCores()

#Register Cluster
  cluster <- makeCluster(n_cores - 1)
  registerDoParallel(cluster)
  
#Define Values#####

y<-seq(2001,2024)
  
#Corporate Names#####
CORP <- c("\\s(ASHFIELD ACTIVE LIVING)+\\s+",
          "\\s(CASTLE POINT LIVING)+\\s+",
          "\\s(BRYANTS TOWING)+\\s+",
          "\\s(REHAB)+.*\\s+",
          "\\s(PRECISION PLUMBING SOLUTIONS)+\\s+",
          "\\s(WIRELESS)+\\s+",
          "\\s(AERO CHARTER)+\\s+",
          "\\s(AMERITECH)+\\s+",
          "\\s(AUTOMATIC ICE SYSTEMS)+\\s+",
          "\\s(BABY HAVEN)+\\s+",
          "\\s(CENTURY ALARM)+\\s+",
          "\\s(CRESTWOOD EXECUTIVE CENTER)+\\s+",
          "\\s(DOUGLASS LAND LIMITED)+\\s+",
          "\\s(DRURY INN)+\\s+",
          "\\s(DUBLEN HOMES)+\\s+",
          "\\s(EDYS GRAND ICE CREAM)+\\s+",
          "\\s(EIGHTEEN INESTMENTS)+\\s+",
          "\\s(EVERGREEN EQUITIES)+\\s+",
          "\\s(HICKORY CREST)+\\s+",
          "\\s(NORTH MERAMEC)+\\s+",
          "\\s(CAR WASH)+\\s+",
          "\\s(CARWASH)+\\s+",
          "\\s(FOUR SEVENTY SEVEN)+\\s+",
          "\\s(GALE BUILDING & REMODELING)+\\s+",
          "\\s(ATLAS TRUCK SALES)+\\s+",
          "\\s(YOUDE FAMILY)+\\s+",
          "\\s(H & M MACHINE SERVICE)+\\s+",
          "\\s(HGS DESIGN LTD)+\\s+",
          "\\s(I-44 MARINE)+\\s+",
          "\\s(JIFFY LUBE)+\\s+",
          "\\s(KINGS FOOD MARKET)+\\s+",
          "\\s(LADUE INNERBELT EXECUTIVE CENTER)+\\s+",
          "\\s(LECHNER & LECHNER LTD)+\\s+",
          "\\s(CUSTOM HOMES)+\\s+",
          "\\s(LONE STAR STEAKHOUSE)+\\s+",
          "\\s(MIDAMERICA CENTER)+\\s+",
          "\\s(NORFOLK & WESTERN RAILROAD)+\\s+",
          "\\s(OUTDOOR SYSTEMS)+\\s+",
          "\\s(P & A DRYWALL SUPPLY)+\\s+",
          "\\s(PARK CRESTWOOD LTD)+\\s+",
          "\\s(PHOENIX INTERNATIONAL)+\\s+",
          "\\s(PHOENIX INTTERNATIONAL)+\\s+",
          "\\s(PINE LAWN DENTAL)+\\s+",
          "\\s(PIZZA INN)+\\s+",
          "\\s(PLEASANT HOLLOW LTD)+\\s+",
          "\\s(R MITCHELL RENOVATIONS)+\\s+",
          "\\s(RED LOBSTER INNS OF AMERICA)+\\s+",
          "\\s(SHERWOODS FOREST NURSERY)+\\s+",
          "\\s(SPRINT PCS)+\\s+",
          "\\s(REFUSE DISPOSAL SERVICE)+\\s+",
          "\\s(SUTTON RESOURCES LTD)+\\s+",
          "\\s(TOWNHOUSES LTD)+\\s+",
          "\\s(TUCKEY & ASSOCS PHYSICAL)+\\s+",
          "\\s(UNION PACIFIC SYSTEMS)+\\s+",
          "\\s(WINTER BROTHERS MATERIALS)+\\s+",
          "\\s(WOLFF SHOE MANUFACTURING)+\\s+",
          "\\s(MARYVILLE CENTRE HOTEL)+\\s+",
          "\\s(HIGHLAND MINI STORAGE)+\\s+")

#Loop Parallel Code####

    test<-  foreach(i=y) %dopar% {
        library(foreign)
        library(tidyverse)
        
      i<-2003
        #Calculate the total living units in each census tract
      
      ifelse(i < 2009,{
        load(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                 filename = "pardat.RData"))
        load(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                         filename = "owndat.RData"))
        temp <- left_join(pardat, owndat, by="parid", relationship = "many-to-many")
        
        
        parcel <- temp %>%
          mutate(PROP_ADD = paste(adrno, adrdir, adrstr, adrsuf, sep = " "),
                 PROP_ADD = gsub("\\s+", " ", str_trim(PROP_ADD)),
                 OWN_ADD = paste(o_adrno, o_adrdir, o_adrstr, o_adrsuf, sep = " "),
                 OWN_ADD = gsub("NA", "", OWN_ADD),
                 OWN_ADD = gsub("\\s+", " ", str_trim(OWN_ADD)),
                 OWNER_NAME = o_name1,
                 OWN_CITY = o_city,
                 OWN_STATE = o_statecode,
                 OWN_ZIP = o_zip,
                 PROP_ZIP = zip1,
                 TENURE = case_when(PROP_ADD == OWN_ADD ~ "OWNER",
                                    PROP_ADD != OWN_ADD ~ "NOT OWNER"))
        names(parcel) <- toupper(names(parcel))
      },{
        file.nm  <- ifelse(i < 2013, "PRIMARY_PARCEL.txt", "primary_parcel.csv")
        
        
        temp <- read.csv(unz(paste0("F:/Data/Saint Louis County Assessor Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"),
                             filename = file.nm), sep = "|", header = TRUE, stringsAsFactors = FALSE)
      
        colnames(temp) <- gsub("\\.", "_", colnames(temp))
      
        parcel <- temp %>%
           mutate(PROP_ADD = paste(TAX_ADRNO, TAX_DIR, TAX_STREET, TAX_SUFFIX, sep = " "),
                  PROP_ADD = gsub("\\s+", " ", str_trim(PROP_ADD)),
                  OWN_ADD = paste(OWNER_ADRNO, OWNER_DIR, OWNER_STREET, OWNER_SUFFIX, sep = " "),
                  OWN_ADD = gsub("NA", "", OWN_ADD),
                  OWN_ADD = gsub("\\s+", " ", str_trim(OWN_ADD)),
                  OWNER_NAME = OWN1,
                  OWN_CITY = OWNER_CITY,
                  OWN_STATE = OWNER_STATE,
                  OWN_ZIP = OWNER_ZIP,
                  PROP_ZIP = TAX_ZIP,
                  TENURE = case_when(PROP_ADD == OWN_ADD ~ "OWNER",
                                     PROP_ADD != OWN_ADD ~ "NOT OWNER"))
      })
      
      
      #CODE Names##### 
      own_dat <- parcel %>%
          filter(CLASS == "R" & 
                 LIVUNIT > 0 &
                !is.na(LIVUNIT)) %>%
          select(PARID, OWNER_NAME, OWN_ADD, OWN_CITY, OWN_STATE, OWN_ZIP,
                 PROP_ADD, PROP_ZIP, LIVUNIT, TENURE) %>%
          mutate(
            OWNER_NAME = gsub("\\s+", " ", OWNER_NAME),
            Corporate = case_when(str_detect(OWNER_NAME, "\\s+LLC+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+INC+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+LTD+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+RENTAL+\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+ACQUI+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+INVEST+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+MANAG+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+COMPA+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s*CORP+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+REALT+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+DEVEL+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+EQUITY+.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+LEASING.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+ENTER+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s*PROPER+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+REAL EST+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, "\\s+CROW MIDWEST+.*.*\\s*") ~ 1,
                                  str_detect(OWNER_NAME, str_c(CORP, collapse = "|"))~ 1,
                                  TRUE ~ 0),
            Trustee = case_when(str_detect(OWNER_NAME, "\\s*(TRUST)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(TRS)+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s.*REVOC+.*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*ESTATE OF+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s ITAF \\s") ~ 1,
                                str_detect(OWNER_NAME, "\\s*FUND+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*INDENT+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*REPRESENT+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*HEIRS+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*.*RETIREMENT+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*SHARING+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*FAMILY+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*PARTNER+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*BUILDERS+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*PTNSP+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s+GROUP+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s+VENTURE+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s+LP+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*GROUP+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*PROJECT+\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*LIMITED+\\s*") ~ 1,
                                TRUE ~ 0),
            Bank = case_when(str_detect(OWNER_NAME, "\\s*(BANK)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(BANKS)+\\s*") ~ 0,
                             str_detect(OWNER_NAME, "\\s*(MORTGAGE)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(CREDIT)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(MUTUAL)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(MTGE)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(LENDER)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(LOAN)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(SAVINGS)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(FINANCIAL)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(SECURITIES)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(LEND)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s(EQUICREDIT)+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(CREDIT)+\\s*") ~ 1,
                               TRUE ~ 0),
            Muni = case_when(str_detect(OWNER_NAME, "\\s*(HOUSING & URBAN)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(MISSOURI)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(SCHOOL DIST)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(STATE OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(CITY OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(TOWN OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(VILLAGE OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(VIL OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(DEPARTMENT OF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(DISTRICT)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\sDIST\\s+\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(DEPARTMENT)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(SEWER)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(LEVEE)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(AUTHORITY)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(RECREATION)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(RIVERS GREENWAY)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(ST LOUIS)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(UNITED STATES)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(LAND CLEARANCE)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(TRANSPORTATION)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(PROTECTION)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(CHILD & FAMILY)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(CITIZENS)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(R-2)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(JOHN BURROUGHS SCHOOL)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(SOUTH TRAILS SEC)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(STATE CA)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(STATE PF)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(UNITED POSTAL)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(ADMINIST)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*(ADMINST)+.*\\s*") ~ 1,
                             str_detect(OWNER_NAME, "\\s*REDEV\\s*\\s*") ~ 1,
                             TRUE ~ 0),
            Nonprof = case_when(str_detect(OWNER_NAME, "\\s*(CHURCH)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(CATHOLIC)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(FELLOWSHIP)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(BAPTIST)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(SCHOOL SISTER)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(FOUNDATION)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(HOME CARE)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(ARCHBISHOP)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(UNIVERSITY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(UNITED CHRISTIAN)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(ASSEMBLY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*MEDICAL+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(BEYOND HOUSING)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(LUTHERAN)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(SISTERS OF)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(HOLY TRINITY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(HABITAT FOR)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(MINISTRIES)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(ASSISTED LIVING)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(HEALTH CARE)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(DAUGHTERS OF)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(CHRISTIAN CENTER)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(SOCIETY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(WORSHIP CENTER)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(MISSIONARY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(SALVATION ARMY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(EPISCOPAL)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(EVANGELICAL)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(CHRISTIAN ACADEMY)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(SCHOOL OF METAPHYSICS)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(ST MARYS INSTITUTE)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(APOSTOLIC FAITH)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(URBAN LEAGUE OF)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(LODGE)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(CHILD CENTER)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(METHODIST)+.*\\s*") ~ 1,
                                str_detect(OWNER_NAME, "\\s*(NEHEMIAH PROGRAM)+.*\\s*") ~ 1,
                                TRUE ~ 0),
            Hoa = case_when(str_detect(OWNER_NAME, "\\s*(HOMEOWNER)+.*\\s*") ~ 1,   #Includes Builders
                            str_detect(OWNER_NAME, "\\s*(CONDOMINIUM)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(CLUB\\s)+\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(OWNERSASSOCIATION)+.*\\s*") ~ 1, 
                            str_detect(OWNER_NAME, "\\s*(RESIDENTS ASS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(WOODSRESIDENTSASSOC)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(ESTATES)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(OF GOVERNORS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(OFGOVERNORS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(NEIGHBORHOOD)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(ESTATES RESIDENTS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(LAKES AT)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(MANORS OF)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(MANOR DIRECTORS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(FORESTRESIDENTSASS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(RESERVE AT)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(SUBDIVISION)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(SUGARTREE HOMES)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(HOMES ASS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(DIRECTORS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(VILLAGES AT)+.*\\s*") ~ 1, 
                            str_detect(OWNER_NAME, "\\s*(VILLAGE AT)+.*\\s*") ~ 1, 
                            str_detect(OWNER_NAME, "\\s*(VILLAS AT)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(PLAT)\\s+") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(CASA ROYALE)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(MAYER HOMES)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(ORTMANN OVERLAND HOME)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(R B PLATT HOMES)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(SHENANDOAH RESIDENT ASSOC)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(VASEL ACRES IMPROVEMENT ASSN)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(VILLAGE THREE PKS)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(WEST LAKE ACRES FACILITIES ASSN)+.*\\s*") ~ 1,
                            str_detect(OWNER_NAME, "\\s*(WOODCHASE PLAZA ASSOCS)+.*\\s*") ~ 1,
                            TRUE ~ 0),
            
            Corporate = case_when(Bank == 1 & Corporate == 1 ~ 0,
                                  Corporate == 1 & Trustee == 1 & str_detect(OWNER_NAME, "\\s*LP\\s+") ~ 0,
                                  Corporate == 1 & Trustee == 1 & str_detect(OWNER_NAME, "\\s*PARTN\\s*") & 
                                                                  !str_detect(OWNER_NAME, "\\s*LLC\\s*") ~ 0,
                                  Corporate == 1 & Trustee == 1 & str_detect(OWNER_NAME, "\\s*PARTN\\s*") & 
                                                                  str_detect(OWNER_NAME, "\\s*LLC\\s*") ~ 1,
                                  TRUE ~ Corporate),
            Trustee = case_when(Bank == 1 & Trustee == 1 ~ 0,
                                Corporate == 1 & Trustee == 1 & str_detect(OWNER_NAME, "\\s*LLC\\s*") ~ 0,
                                Muni == 1 & Trustee == 1 ~ 0,
                                TRUE ~ Trustee),
            Muni = case_when(Muni == 1 & Nonprof == 1 ~ 0,
                             Muni == 1 & Corporate == 1 ~ 0,
                             Bank == 1 & Muni == 1 ~ 0,
                             TRUE ~ Muni),
            Nonprof = case_when(Bank == 1 & Nonprof == 1 ~ 0,
                                TRUE ~ Nonprof),
            Hoa = case_when(Bank == 1 & Hoa == 1 ~ 0,
                            Corporate == 1 & Hoa == 1 ~ 0,
                            TRUE ~ Hoa)) %>%
         distinct(., PARID, .keep_all = TRUE ) %>%
          mutate(key = Corporate+Bank+Trustee+Muni+Nonprof+Hoa,
                 private = case_when(key==0 ~ 1,
                                     TRUE ~ 0),
                 year = i) %>%
          arrange(PROP_ADD, .keep_all = TRUE) 
        #return(own_dat)
       
    }
  
#Pull out from list and create own_dat files####
 
   k<-1
  for(i in y){
    c<-as.data.frame(test[k])
    assign(paste0("own_dat",i), c)
    k<-k+1
  }

#Clean and save environment with OWN_DAT files####
  
  rm(i, k, test, n_cores, CORP, cluster)

  save.image(file="./Build/Output/owners2.RData")

#Merge of individual years into single frame.#####

for(i in y){
  own<-get(paste0("own_dat",i))
  
  own <- own %>%
    mutate(PARID = LOCATOR,
           year = i) 
  ifelse(i==2001,
         OWN <- own,
         OWN <- rbind(OWN, own ))
}

#Cleanup of Owner Data

fix1<-c("BALLWIN","BRIDGETON","CHESTERFIELD","EUREKA", "FENTON", "FLORISSANT","GLENCOE","GROVER",
        "HAZELWOOD","MARYLAND HEIGHTS","PACIFIC","SAINT ANN","SAINT LOUIS","VALLEY PARK")

OWN <- OWN %>%
  mutate(OWN_STATE = case_when(is.na(OWN_STATE) & OWN_CITY %in% fix1 ~ "MO",
                               TRUE ~ OWN_STATE),
         OWN_ZIP = as.character(OWN_ZIP),
         OWN_ZIP = case_when(OWN_ZIP == "630" ~ "63017",
                             OWN_ZIP == "6319" ~ "63019",
                             OWN_ZIP == "2829" ~ "63144",
                             OWN_ZIP == "6314" ~ "63144",
                             OWN_ZIP == "6332" ~ "63132",
                             OWN_ZIP == "9136" ~ "91367",
                             TRUE ~ OWN_ZIP),
         zip = substr(OWN_ZIP, 1, 2),
         OWN_STATE = case_when(is.na(OWN_STATE) & zip == "63" ~ "MO",
                               TRUE ~ OWN_STATE),
         PROP_ZIP = case_when(TENURE == "OWNER" ~ OWN_ZIP,
                              PROP_ADD == OWN_ADD ~ OWN_ZIP,
                              TRUE ~ PROP_ZIP))

#Fix missing or incomplete Zip Codes for Owners####
  temp <- OWN %>%
    filter(is.na(PROP_ZIP)) %>%
    select(PARID) %>%
    distinct()
  
  temp2 <- OWN %>%
    filter(PARID %in% temp$PARID) %>%
    select(PARID, PROP_ZIP) %>%
    filter(!is.na(PROP_ZIP)) %>%
    distinct(PARID, .keep_all = TRUE)
  
    inds<-match(OWN$PARID, temp2$PARID)
    OWN$PROP_ZIP<-ifelse(is.na(OWN$PROP_ZIP), temp2$PROP_ZIP[inds], OWN$PROP_ZIP)

#Fix missing or incomplete addresses for OWNERS
    temp <- OWN %>%
      filter(is.na(PROP_ADD)) %>%
      select(PARID) %>%
      distinct()
    temp2 <- OWN %>%
      filter(PARID %in% temp$PARID) %>%
      select(PARID, PROP_ADD) %>%
      filter(!is.na(PROP_ADD)) %>%
      distinct(PARID, .keep_all = TRUE)
    
    inds<-match(OWN$PARID, temp2$PARID)
    OWN$PROP_ADD<-ifelse(is.na(OWN$PROP_ADD), temp2$PROP_ADD[inds], OWN$PROP_ADD)

  #This section MUST use an external file.#####
  
  temp <- OWN %>%
    filter(is.na(OWN_ADD)) %>%
    select(OWNER_NAME) %>%
    distinct()

    #Hidden Code: Code to create the necessary external file####
          #temp2 <- OWN %>%
          #  filter(OWNER_NAME %in% temp$OWNER_NAME) %>%
          #  select(OWNER_NAME, OWN_ADD, OWN_CITY, OWN_STATE, OWN_ZIP) %>%
          #  filter(!is.na(OWN_ADD)) %>%
          #  filter(OWN_ADD != "") %>%
          #  mutate(OWN_ADD = str_squish(OWN_ADD)) %>%
          #  distinct(OWNER_NAME, OWN_ADD, .keep_all = TRUE)
  #Using External File to fix some addresses####
    
    temp2<-read.csv("./Build/Input/ownadd_fixed.csv", header = TRUE)
    temp2 <- temp2 %>%
      distinct(OWNER_NAME, OWN_ADD, .keep_all = TRUE)
    
    inds<-match(OWN$OWNER_NAME, temp2$OWNER_NAME)
        OWN$OWN_ADD<-ifelse(is.na(OWN$OWN_ADD), temp2$OWN_ADD[inds], OWN$OWN_ADD)
        OWN$OWN_CITY<-ifelse(is.na(OWN$OWN_CITY), as.character(temp2$OWN_CITY[inds]), as.character(OWN$OWN_CITY))
        OWN$OWN_STATE<-ifelse(is.na(OWN$OWN_STATE), temp2$OWN_STATE[inds], OWN$OWN_STATE)
        OWN$OWN_ZIP<-ifelse(is.na(OWN$OWN_ZIP), temp2$OWN_ZIP[inds], OWN$OWN_ZIP)

rm(own, temp, temp2)
rm(list = ls(pattern="^own_dat"))

save(OWN, file="./Build/Output/Own.RData")

