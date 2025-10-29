library(dplyr)

#raw_dat <- read.csv('I:/BulkFilesAQI/batch_example/bc_env_batch_example.csv')
raw_dat <- read.csv('C:/Users/jkrogh/Downloads/bc_env_batch_example.csv')

#notes on EAs
#Ministry contact on field visit 
min_contact <- "1c24fcf6-07b7-43c1-8a77-ed1d7b06603d"

#Sampling Agency on field visit 
sampling_age <- "65d94fac-aac5-498f-bc73-b63a322ce350"

#Depth Lower on activity 
depth_lower <- "2cb342c4-5b66-4ef7-b417-e61ff4209738"

#Lab arrival temp specimen
lab_temp <- "c1a72675-fd82-4ab1-8126-8a0102f60384"

#work order number on specimen
work_order_no <- "e3721c59-0caf-47a8-954c-92f0e925394a"

#tissue type on specimen
tissue_type <- "6f7d5be0-f91a-4353-9d31-13983205cbe0"

#biological Life stage Obs
bio_life_stage <- "d82927fd-e63b-4eff-84be-10a8dbe7cf64"

#observation comp stat
obs_composite_stat <- "1652c9fc-5694-4b57-820f-68d2827fc337"

#observation lab batch ID
lab_batch_id <- "4d423aad-c133-4e61-aa03-297be54cd3b9"

#Function to get the EAs from field visits
get_ea_value_fv <- function(df, ea_id, type) {
  if (type == "text") {
    df$pos_ea <- sapply(strsplit(df$fieldVisitExtendedAttributeAttributeIds, 
                                ",\\s*"), function(x) {
                                    match(ea_id, x)
                                 })
    
    #extract the ministry contact
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(str, ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$fieldVisitExtendedAttributeTexts, df$pos_ea)
  }
  
  if (type == "list") {
    df$pos_ea <- sapply(strsplit(df$fieldVisitExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the ministry contact
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(str, ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$fieldVisitExtendedAttributeListItemCustomIds, df$pos_ea)
  }
  
  return(df$return_var)
}

raw_dat$Ministry_Contact <- get_ea_value_fv(raw_dat, "1c24fcf6-07b7-43c1-8a77-ed1d7b06603d", "text")
raw_dat$Sampling_Agency <- get_ea_value_fv(raw_dat, "65d94fac-aac5-498f-bc73-b63a322ce350", "list")

#function to get EA on Activities
get_ea_value_fa <- function(df, ea_id, type) {
  if (type == "number") {
    df$pos_ea <- sapply(strsplit(df$activityExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(str, ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$activityExtendedAttributeNumbers, df$pos_ea)
  }
  
  if (type == "text") {
    df$pos_ea <- sapply(strsplit(df$activityExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the text based ea
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(as.character(str), ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$activityExtendedAttributeTexts, df$pos_ea)
  }
  
  return(df$return_var)
}

raw_dat$Depth_Lower <- get_ea_value_fa(raw_dat, depth_lower, "text")

#function to get EA on Specimens
get_ea_value_specimen <- function(df, ea_id, type) {
  if (type == "number") {
    df$pos_ea <- sapply(strsplit(df$specimenExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(str, ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$specimenExtendedAttributeNumbers, df$pos_ea)
  }
  
  if (type == "text") {
    df$pos_ea <- sapply(strsplit(df$specimenExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the text based ea
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(as.character(str), ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$specimenExtendedAttributeTexts, df$pos_ea)
  }
  
  if (type == "list") {
    df$pos_ea <- sapply(strsplit(df$specimenExtendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the text based ea
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(as.character(str), ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$specimenExtendedAttributeListItemCustomIds, df$pos_ea)
  }
  
  return(df$return_var)
}

raw_dat$Tissue_Type <- get_ea_value_specimen(raw_dat, tissue_type, "list")
raw_dat$Work_Order_Number <- get_ea_value_specimen(raw_dat, work_order_no, "text")
raw_dat$Lab_Arrival_Temp <- get_ea_value_specimen(raw_dat, lab_temp, "number")

#function to get result EAs
get_ea_value_obs <- function(df, ea_id, type) {
  if (type == "number") {
    df$pos_ea <- sapply(strsplit(df$extendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(str, ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$extendedAttributeNumbers, df$pos_ea)
  }
  
  if (type == "text") {
    df$pos_ea <- sapply(strsplit(df$extendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the text based ea
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(as.character(str), ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$extendedAttributeTexts, df$pos_ea)
  }
  
  if (type == "list") {
    df$pos_ea <- sapply(strsplit(df$extendedAttributeAttributeIds, 
                                 ",\\s*"), function(x) {
                                   match(ea_id, x)
                                 })
    
    #extract the list based ea
    df$return_var <- mapply(function(str, pos) {
      parts <- strsplit(as.character(str), ",\\s*")[[1]]
      if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
    }, df$extendedAttributeListItemCustomIds, df$pos_ea)
  }
  
  return(df$return_var)
}

raw_dat$Lab_Batch_ID <- get_ea_value_obs(raw_dat, lab_batch_id, "text") 
raw_dat$Composite_Stat <- get_ea_value_obs(raw_dat, obs_composite_stat, "list") 

#add the following missing values
#raw_dat$Field_Visit_Comments <- "Field visit comments as text."
raw_dat$Observed_Property_Name <- "OP Name as text."
#raw_dat$Lab_Arrival_Date <- "2025-09-16"

#Now just select?
cleaner_dat <- raw_dat %>% select(Ministry_Contact,
                                Sampling_Agency,
                                fieldVisitProjectCustomId,
                                fieldVisitProjectName,
                                Work_Order_Number,
                                samplingLocationCustomId,
                                samplingLocationName,
                                samplingLocationTypeCustomId,
                                samplingLocationLatitude,
                                samplingLocationLongitude,
                                samplingLocationElevationValue,
                                samplingLocationElevationUnitCustomId,
                                samplingLocationGroupNames,
                                fieldVisitStartTime,
                                fieldVisitEndTime,
                                fieldVisitParticipants,
                                fieldVisitNotes,
                                #Field_Visit_Comments,
                                specimenFiltered,
                                specimenFiltrationComment,
                                specimenPreservative,
                                deviceCustomId,
                                deviceType,
                                samplingContextTagNames,
                                collectionMethodCustomId,
                                mediumCustomId,
                                taxonomicResultValueScientificName,
                                taxonomicResultValueCommonName,
                                depthValue,
                                Depth_Lower,
                                depthUnitCustomId,
                                observedTime,
                                activityStartTime,
                                activityEndTime,
                                observedPropertyCustomId,
                                observedPropertyDescription,
                                observedPropertyAnalysisType,
                                observedPropertyResultType,
                                Observed_Property_Name,
                                observedPropertyCasNumber,
                                normalizedResultValue,
                                numericResultMethodDetectionLevelValue,
                                numericResultLowerMethodReportingLimitValue,
                                numericResultMdlUnitCustomId,
                                numericResultDetectionConditionCustomId,
                                Composite_Stat,
                                numericResultSampleFraction,
                                dataClassification,
                                labResultDetailsLaboratoryCustomId,
                                labResultDetailsLaboratoryName,
                                analysisMethodMethodId,
                                #analysisMethodName,
                                resultTime,
                                labResultDetailsDateReceived, #why was this commented out?
                                resultStatusCustomId,
                                resultGradeCustomId,
                                activityCustomId,
                                Tissue_Type,
                                Lab_Arrival_Temp,
                                specimenName,
                                labResultDetailsQualityFlag,
                                labResultDetailsDatePrepared,
                                labResultDetailsDateReceived,
                                labResultDetailsLabSampleId,
                                labResultDetailsDilutionFactor,
                                labResultDetailsAnalysisComment,
                                Lab_Batch_ID,
                                activityType,
                                activityReplicateSourceActivityId
                                )


#rename the columns to match final versions
clean_dat <- cleaner_dat %>% rename(
  Project = fieldVisitProjectCustomId,
  Project_Name = fieldVisitProjectName,
  Location_ID = samplingLocationCustomId,
  Location_Name = samplingLocationName,
  Location_Type = samplingLocationTypeCustomId,
  Location_Latitude = samplingLocationLatitude,
  Location_Longitude = samplingLocationLongitude,
  Location_Elevation = samplingLocationElevationValue,
  Location_Elevation_Unit = samplingLocationElevationUnitCustomId,
  Location_Groups = samplingLocationGroupNames,
  Field_Visit_Start_Time = fieldVisitStartTime,
  Field_Visit_End_Time = fieldVisitEndTime,
  Field_Visit_Participants = fieldVisitParticipants,
  Field_Visit_Comments = fieldVisitNotes,
  Field_Filtered = specimenFiltered,
  Field_Filtered_Comment = specimenFiltrationComment,
  Field_Preservative = specimenPreservative,
  Field_Device_ID = deviceCustomId,
  Field_Device_Type = deviceType,
  Sampling_Context_Tag = samplingContextTagNames,
  Collection_Method = collectionMethodCustomId,
  Medium = mediumCustomId,
  Taxonomy = taxonomicResultValueScientificName,
  Taxonomy_Common_Name = taxonomicResultValueCommonName,
  Depth_Upper = depthValue,
  Depth_Unit = depthUnitCustomId,
  Observed_Date_Time = observedTime,
  Observed_Date_Time_Start = activityStartTime,
  Observed_Date_Time_End = activityEndTime,
  Observed_Property_ID = observedPropertyCustomId,
  Observed_Property_Description = observedPropertyDescription,
  Observed_Property_Analysis_Type = observedPropertyAnalysisType,
  Observed_Property_Result_Type = observedPropertyResultType,
  CAS_Number = observedPropertyCasNumber,
  Result_Value = normalizedResultValue,
  Method_Detection_Limit = numericResultMethodDetectionLevelValue,
  Method_Reporting_Limit = numericResultLowerMethodReportingLimitValue,
  Result_Unit = numericResultMdlUnitCustomId,
  Detection_Condition = numericResultDetectionConditionCustomId,
  Fraction = numericResultSampleFraction,
  Data_Classification = dataClassification,
  Analyzing_Agency = labResultDetailsLaboratoryCustomId,
  Analyzing_Agency_Full_Name = labResultDetailsLaboratoryName,
  Analysis_Method = analysisMethodMethodId,
  #Analyzed_Method_Name = analysisMethodName,
  Analyzed_Date_Time = resultTime,
  Result_Status = resultStatusCustomId,
  Result_Grade = resultGradeCustomId,
  Activity_Name = activityCustomId,
  Specimen_Name = specimenName,
  Lab_Quality_Flag = labResultDetailsQualityFlag,
  Lab_Arrival_Date_Time =  labResultDetailsDateReceived,
  Lab_Prepared_Date_Time = labResultDetailsDatePrepared,
  Lab_Sample_ID = labResultDetailsLabSampleId,
  Lab_Dilution_Factor = labResultDetailsDilutionFactor,
  Lab_Comment = labResultDetailsAnalysisComment,
  QC_Type = activityType,
  QC_Source_Activity_Name = activityReplicateSourceActivityId
)

x<-na.omit(clean_dat)

write.csv(clean_dat, "AQI_Test_File_Oct_29_2025-v2.csv", row.names = F)

----
#Sampling Agency Stuff
#get the location of the sampling agency ea
fv_ea$pos_sampling_age <- sapply(strsplit(fv_ea$fieldVisitExtendedAttributeAttributeIds, 
                                         ",\\s*"), function(x) {
                                           match(sampling_age, x)
                                         })

#extract the ministry contact
fv_ea$ministry_contact <- mapply(function(str, pos) {
  parts <- strsplit(str, ",\\s*")[[1]]
  if (!is.na(pos) && pos <= length(parts)) parts[pos] else NA
}, fv_ea$fieldVisitExtendedAttributeTexts, fv_ea$pos_min_contact)






strsplit(fv_min_contact$fieldVisitExtendedAttributeAttributeIds, ",")[[1]] == "1c24fcf6-07b7-43c1-8a77-ed1d7b06603d"


df_wide <- fv_min_contact %>%
  separate(fieldVisitExtendedAttributeAttributeIds, into = paste0("col1_", 1:2), sep = ",", fill = "right") |>
  separate(fieldVisitExtendedAttributeListItemCustomIds, into = paste0("col2_", 1:2), sep = ",", fill = "right") |>
  separate(fieldVisitExtendedAttributeTexts, into = paste0("col3_", 1:2), sep = ",", fill = "right")

ix_1 <- df_wide$col1_1 == '1c24fcf6-07b7-43c1-8a77-ed1d7b06603d'
ix_2 <- df_wide$col1_2 == '1c24fcf6-07b7-43c1-8a77-ed1d7b06603d'

df_wide$col3_1[ix_1]
df_wide$col3_2[ix_2]

clean_dat <- raw_dat %>% select(
  
)