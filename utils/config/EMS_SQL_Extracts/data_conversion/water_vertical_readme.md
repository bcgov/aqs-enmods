# Water & Vertical Profile Data Conversion

SQL script for converting EMS (Environmental Management System) water quality and vertical profile data into a standardized format for external data systems.

## Overview

This Oracle SQL script (`conversion_main_20251208_water_vertical.sql`) extracts and transforms environmental monitoring data from the EMS database, focusing on water quality measurements and vertical profile observations. The script handles data classification, unit conversions, quality control metadata, and proper formatting for downstream systems.

## Features

- Water quality data extraction and transformation
- Vertical profile data identification and processing
- Automated unit conversion for method detection limits
- Data classification (LAB, FIELD_RESULT, VERTICAL_PROFILE, etc.)
- Duplicate observation handling with unique identifiers
- Quality control type mapping (Replicate, Blank, Spike, etc.)
- Specimen name generation with deduplication logic
- Preservation of field visit metadata and lab information

## Data Sources

The script queries multiple EMS tables including:
- `ems_samples` - Sample collection information
- `ems_results` - Laboratory analysis results
- `ems_monitoring_locations` - Sampling location details
- `ems_parameters` - Parameter definitions
- `ems_client_locations` - Agency information
- `OBSERVED_PROPERTIES_FOR_ETL` - Property mapping crosswalk
- `VERTICAL_PROFILES` - Vertical profile identification
- `unit_conversions_temp` - Unit conversion factors

## Key Components

### Core Data CTE
Extracts base sample and result information with all necessary joins to reference tables. Includes logic for:
- Field visit time aggregation (earliest start, latest end per location/day)
- Collection method mapping
- Date/time formatting (ISO 8601 with timezone offset)
- Detection condition handling (NOT_DETECTED, NOT_SAMPLED, etc.)

### Sample Data CTE
Provides sample-level metadata without result values, used for activity-level measurements.

### Main Query
Combines core data with observed property mappings and performs:
- Unit conversion for method detection limits
- Vertical profile identification
- Data classification assignment
- Specimen name generation
- Duplicate row numbering for temporal sequences

## Recent Changes

### NULL Character Handling (Line 20)
```sql
REPLACE(smpl.field_comment, CHR(0), '') AS "Activity Comments"
```
Removes NULL characters from activity comments to prevent data quality issues.

### Duplicate Row Logic (Lines 631-649)
Updated partition logic to exclude Activity Name from duplication counting for vertical profiles. This ensures vertical profile observations at different depths share the same specimen name and are properly grouped as a single vertical activity.

**Partition Key:**
- Location ID
- Field Visit Start Time
- Medium
- Depth Upper
- Activity Name (excluded for FIELD_RESULT and VERTICAL_PROFILE)
- Specimen Name
- Data Classification
- QC Type (excluded for FIELD_RESULT and VERTICAL_PROFILE)
- Observed Property ID

### Vertical Profile Identification (Lines 780-786, 823-825)
Complex left join to `VERTICAL_PROFILES` table to properly identify and classify vertical profile data:
```sql
CASE
    WHEN core."Depth Upper" is not null 
        AND core."Depth Lower" is not null 
        AND core."Location ID" = to_char(v.EMS_ID)
        AND core."Observed DateTime" = to_char(v.collection_date_time, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
        AND core.parm_cd = v.parm_cd
    THEN 'VERTICAL_PROFILE'
    ELSE ed.Classification 
END AS "Data Classification"
```

### Activity Name Handling (Lines 694, 695, 704, 709)
Activity Name is set to NULL for FIELD_RESULT and VERTICAL_PROFILE classifications to prevent improper specimen name generation and maintain proper data hierarchy.

### Specimen Name Convention (Lines 541-546)
```sql
CASE
    WHEN "Specimen Name" IS NULL AND "Data Classification" IN ('LAB', 'SURROGATE_RESULT') 
        THEN to_char("Activity Name")
    WHEN "Specimen Name" IS NULL THEN ''
    WHEN duplicate_row_number > 1 
        THEN "Specimen Name" || '-' || duplicate_row_number
    ELSE "Specimen Name"
END AS "Specimen Name"
```

## Output Format

The script produces a dataset with the following key fields:
- **Location & Visit Info**: Location ID, Field Visit Start/End Time, Participants
- **Sample Details**: Specimen Name, Observed Property ID, Collection Method, Medium, Depth (Upper/Lower), Observed DateTime
- **Result Data**: Result Value, Detection Condition, Method Detection Limit, Result Unit
- **Lab Information**: Analyzing Agency, Analysis Method, Lab Sample ID, Lab Batch ID
- **Quality Control**: QC Type, Result Status, Result Grade
- **Classification**: Data Classification

## Filters & Exclusions

- Excludes discharge (D%) and permit (P%) location types
- Filters out specific collection methods marked as 'DELETE'
- Requires valid Observed Property ID
- Requires either a result value or a NOT_SAMPLED condition

## Usage

```sql
-- Execute the script to generate the conversion output
@conversion_main_20251208_water_vertical.sql
```

The script can be filtered by uncommenting the location-specific WHERE clauses for testing individual locations or date ranges.

## Dependencies

- Oracle Database with PL/SQL support
- Access to EMS schema and tables
- `OBSERVED_PROPERTIES_FOR_ETL` lookup table
- `VERTICAL_PROFILES` reference table
- `unit_conversions_temp` conversion factors

## Notes

- All timestamps are formatted in ISO 8601 with Pacific Time offset (-08:00)
- Location IDs are zero-padded to 7 digits
- Method detection limits are converted to match result units when necessary
- Depth measurements default to 'metre' units when present
- End times are omitted when equal to start times per business requirements

## Commented Sections

The script includes commented-out sections for:
- Air quality data (separate conversion file)
- Continuous monitoring data (average, maximum, minimum)
- Taxonomic data (bio sample area, volume, weight)
- Fish data (chemistry, fork length, weight)

These sections are maintained for reference but should be processed through their respective specialized conversion files.

## Contact

For questions about data mappings, business rules, or technical issues, refer to the data conversion project documentation or contact the EnMoDS data team.