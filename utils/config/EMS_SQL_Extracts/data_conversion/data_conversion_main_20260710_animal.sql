set define off;
WITH tax_cw_tokenized AS (
  SELECT
    tax_cw.*,
    '|' || REGEXP_REPLACE(
             REGEXP_REPLACE(
               REGEXP_REPLACE(
                REGEXP_REPLACE(
                   REGEXP_REPLACE(
                   UPPER(tax_cw.parm_code),
                   '\s+', ''              -- NEW: strip all whitespace first
                 ),
                '[/]', '@'
                ), 
               '\.', '#'
               ),
               ',', '|'
             ),
             '[^A-Z0-9#|@]+', '#'
           ) || '|' AS parm_code_tokens
  FROM ems.ems_etl_tax_crosswalk_temp tax_cw
),
result_tokenized AS (
  SELECT
    results.*,
     '|' || REGEXP_REPLACE(
             REGEXP_REPLACE(
               REGEXP_REPLACE(
                REGEXP_REPLACE(
                  REGEXP_REPLACE(
                    UPPER(results.tax_nm_cd), 
                    '\s+', ''              -- NEW: strip all whitespace first
                    ),
                    '[/]', '@'
                    ), 
              '\.', '#'
               ),
               ',', '|'
             ),
             '[^A-Z0-9#|@]+', '#'
           ) || '|' AS tax_nm_cd_tokens
  FROM ems_results results
),
core_data AS (
    SELECT DISTINCT
        ps.first_name || ' ' || ps.last_name                         AS "Ministry Contact",
        cl.id || ' - ' || cl.name                                    AS "Sampling Agency",
        case when aqs_project.EMS_CODE is null then null else 'BCLMN' end AS "Project",
        smpl.requisition_id                                          AS "Work Order Number",
        smpl.mon_locn_id                                             AS "Location ID",
        to_char(eal.earlieststarttime, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'    AS "Field Visit Start Time",
        case -- if earlieststarttime = latestendtime, Jeremy requested that we don't display the end date and time
            when eal.earlieststarttime = eal.latestendtime then null
            --ADDED 20260508
            when eal.earlieststarttime > eal.latestendtime then null
            else
				CASE 
					WHEN to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
                -- to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
        end AS "Field Visit End Time",
        smpl.sampler                                                 AS "Field Visit Participants",
        REPLACE(smpl.field_comment , CHR(0), '') AS "Activity Comments",
        NULL                                                         AS "Field Filtered", -- blank, doesn't exist in ems
        NULL                                                         AS "Field Filtered Comment", -- blank, doesn't exist in ems
        epc.description                                              AS "Field Preservative",-- updated to use descrsiption  note that only 3800 records of ~ 2 million records have a field preservative
        NULL                                                         AS "Sampling Context Tag", -- blank, doesn't exist in ems
        smpl.clct_methd_cd,
                CASE
            WHEN cm.code = '25' THEN 'Autosampler: Peristaltic Pump'
            WHEN cm.code = '025' THEN 'Autosampler: Peristaltic Pump'
            WHEN cm.code = 'FCFLOW' THEN 'Flow Proportional Composite'
            WHEN cm.code = 'FCTIME' THEN 'Flow Proportional Composite'
            WHEN cm.code = 'GRB' THEN 'Grab'
            WHEN cm.code = 'GEL' THEN 'Grab'
            WHEN cm.code = '16' THEN 'Grab'
            WHEN cm.code = '016' THEN 'Grab'
            WHEN cm.code = '8' THEN 'Grab'
            WHEN cm.code = '008' THEN 'Grab'
            WHEN cm.code = 'ELECTR' THEN 'Electrofishing'
            WHEN cm.code = 'IVKICK' THEN 'Invertebrate Kicknetting'
            WHEN cm.code = 'MNWTRP' THEN 'Minnow Trapping'
            WHEN cm.code = 'NET' THEN 'Netting (gill net or other)'
            WHEN cm.code = 'C04' THEN 'Paper Weighed on Scale'
            WHEN cm.code = 'SCPOOL' THEN 'Spatial Composite'
            WHEN cm.code = 'SCSRAM' THEN 'Spatial Composite: Simple Random'
            WHEN cm.code = 'SCHTOW' THEN 'Spatial Composite: Horizontal Tow'
            WHEN cm.code = 'SCTRAN' THEN 'Spatial Composite: Transect'
            WHEN cm.code = 'SCVERT' THEN 'Spatial Composite: Vertical'
            WHEN cm.code = 'TCDIS' THEN 'Time Composite: Discrete'
            WHEN cm.code = '31' THEN 'Time Composite: Discrete'
            WHEN cm.code = '031' THEN 'Time Composite: Discrete'
            WHEN cm.code = '14' THEN 'Time Composite: Discrete'
            WHEN cm.code = '014' THEN 'Time Composite: Discrete'
            WHEN cm.code = 'H01' THEN 'Time Composite: Discrete'
            WHEN cm.code = 'H02' THEN 'Time Composite: Discrete'
            WHEN cm.code = 'CMON' THEN 'Time Composite: Continuous Monitor'
            WHEN cm.code = 'C03' THEN 'Time Composite: Continuous Monitor'
            WHEN cm.code = 'C01' THEN 'Time Composite: Continuous Monitor'
            WHEN cm.code = 'TCCON' THEN 'Time Composite: HiVol'
            WHEN cm.code = 'TCSEG' THEN 'Time Composite: Segmented Discrete'
            WHEN cm.code = 'VRBL' THEN 'Variable Well Sampling'
            WHEN cm.code = '29' THEN 'Variable Well Sampling'
            WHEN cm.code = '32' THEN 'Variable Well Sampling'
            WHEN cm.code = '029' THEN 'Variable Well Sampling'
            WHEN cm.code = '032' THEN 'Variable Well Sampling'
            WHEN cm.code = 'SCOBLQ' THEN 'DELETE' -- don't display this row
            WHEN cm.code = 'CFLOW' THEN 'DELETE' -- don't display this row
            WHEN cm.code = '30' THEN 'DELETE' -- don't display this row
            WHEN cm.code = '030' THEN 'DELETE' -- don't display this row
            WHEN cm.code = 'PDW' THEN 'DELETE' -- don't display this row
            WHEN cm.code = 'SCHTRL' THEN 'DELETE' -- don't display this row
            WHEN NULLIF(cm.code, '') IS NULL THEN 'Unknown'
            ELSE cm.code
        END AS "Collection Method", -- still need to find source
        CASE 
            WHEN m.enmods_medium = 'Animal Tissue - Zooplankton' THEN 'Animal - Zooplankton'
            ELSE m.enmods_medium
        END AS "Medium",
        smpl.depth_upper                                             AS "Depth Upper",
        smpl.depth_lower                                             AS "Depth Lower",
        case 
            when smpl.depth_upper is null and smpl.depth_lower is null then null
            else 'metre'                                                          
        end AS "Depth Unit",
        to_char(smpl.collection_start_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00' AS "Observed DateTime",
        case -- if earlieststarttime = latestendtime, Jeremy requested that we don't display the end date and time
            when smpl.collection_start_date = smpl.collection_end_date then null
            --ADDED 20260508
            when smpl.collection_start_date > smpl.collection_end_date then null
            else
				CASE 
					WHEN to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
                --to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
        end AS "Observed Date Time End",        
        CASE 
			WHEN result.result_text IN ('C', '"C"') THEN null
            WHEN result.result_numeric IS NULL AND REGEXP_LIKE(REGEXP_REPLACE(TRIM(result.result_text), ',', ''), '^-?\d+(\.\d+)?$') THEN TO_NUMBER(REGEXP_REPLACE(TRIM(result.result_text), ',', ''))
            --WHEN result.result_numeric is NULL AND REGEXP_LIKE(result.result_text, '^-?\d+(\.\d+)?$') THEN TO_NUMBER(result.result_text)
            --WHEN result.result_text LIKE '<%' THEN CAST(REPLACE(REPLACE(result.result_text, '< ', ''), '<',  '') AS FLOAT)
            WHEN result.result_text LIKE '%<%' THEN -999
            WHEN result.result_text LIKE 'M%' THEN CAST(REPLACE(REPLACE(result.result_text, 'M ', ''), 'M',  '') AS FLOAT)
			--WHEN result.result_numeric = 'NA' THEN ''
            ELSE result.result_numeric 
        END AS "Result Value",
        result.method_detect_limit                                   AS "Method Detection Limit",
        --UPDATE 20260402 MOVED OUTSIDE CORE SINCE PARM_CD NEEDS UPDATING SO CANNOT USE JOINS on it yet d.METHOD_DETECT_LIMIT   AS "Method Detection Limit Source 2",        
        NULL                                                         AS "Method Reporting Limit", -- leave as blank
        --UPDATE 20260402 MOVED OUTSIDE CORE SINCE PARM_CD NEEDS UPDATING SO CANNOT USE JOINS on it yet
--		CASE
--			WHEN aqs_units.AQS_NAME_ON_IMPORT is null then 'None'
--			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'ug/g wet' then 'ug/g' 
--			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'C' then 'degC'
--			ELSE aqs_units.AQS_NAME_ON_IMPORT
--		END AS "Result Unit",
         --UPDATE 20260402 MOVED OUTSIDE CORE SINCE PARM_CD NEEDS UPDATING SO CANNOT USE JOINS on it yet mu.short_name AS "EMS Result Unit",
        mu_mdl.short_name                                            AS "MDL Unit",
        result.result_text,
        CASE
            WHEN result.result_letter = '<' THEN 'NOT_DETECTED'
            WHEN result.result_text LIKE '%<%' THEN 'NOT_DETECTED'
            WHEN result.result_text = '''C''' then 'NOT_SAMPLED'
            ELSE NULL
        END                                                          AS "Detection Condition",
        NULL                                                         AS "Limit Type",
        NULL                                                         AS "Source of Rounded Value", -- can be blank
        NULL                                                         AS "Rounded Value", -- can be blank
        NULL                                                         AS "Rounding Specification", -- can be blank
        cl2.short_name                                               AS "Analyzing Agency",
        CASE 
            WHEN mu_mdl.short_name = 'cells/mL' THEN 'TAXA'
            WHEN mu_mdl.short_name = 'cells/cm2' THEN 'TAXA'
            WHEN mu_mdl.short_name = 'No. Org.' THEN 'TAXA'
            WHEN mu_mdl.short_name = 'to/s' THEN 'TAXA'
            WHEN mu_mdl.short_name = 'ug/ml' THEN 'TAXA'
            ELSE result.anal_method_cd
        END AS "Analysis Method",
        CASE 
            WHEN result.analytical_date IS NULL THEN NULL
            ELSE 
				CASE 
					WHEN to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
				--to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
        END AS "Analyzed Date Time",
        'Preliminary'                                                AS "Result Status",
        'Ungraded'                                                   AS "Result Grade",
        NULL                                                         AS "Activity ID",
        smpl.id                                                      AS "Activity Name",
        tt.description                                               AS "Tissue Type", -- blank for this query, but not necessarily true for tax. and air
        smpl.tissue_typ_cd,
        esp.DESCRIPTION AS "Species",
        result.tax_nm_cd,
        result.tax_nm_cd_tokens,
        smpl.lab_arrival_temperature                                 AS "Lab Arrival Temperature",
        result.result_comment AS "Lab Quality Flag",
        --NULL                                                         AS "Lab Quality Flag",-- leave blank
        CASE 
            WHEN smpl.lab_arrival_date IS NULL THEN NULL
            ELSE 
				CASE 
					WHEN to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
				--to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'      
        END AS "Lab Arrival Date and Time",        
        NULL                                                         AS "Lab Prepared DateTime",-- leave blank
        result.lab_sample_id                                         AS "Lab Sample ID",
        NULL                                                         AS "Lab Dilution Factor",-- leave blank
        smpl.lab_comment                                             AS "Lab Comment",
        result.lab_batch_id                                          AS "Lab Batch ID",
        CASE
            WHEN sc.description IN ( 'Replicate', 'Replicate-First', 'Replicate-Second', 'Replicate-Third' ) THEN
                'Replicate'
            WHEN upper(sc.description) LIKE '%BLANK%' THEN
                'Blank'
            WHEN upper(sc.description) LIKE '%SPIKE%' THEN
                'Spike'
            ELSE
                ''
        END                                                          AS "QC Type",
        NULL                                                         AS "QC Source Activity Name",-- leave as blank
        NULL                                                         AS "Composite Stat",-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes
--      CASE
--			WHEN aqs_units.AQS_NAME_ON_IMPORT is null then 'None'
--			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'ug/g wet' then 'ug/g' 
--			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'C' then 'degC'
--			ELSE aqs_units.AQS_NAME_ON_IMPORT
--		END AS "Result Unit",
        CASE 
            WHEN result.parm_cd is NULL AND mu_mdl.short_name = 'cells/mL' THEN '-111'
            WHEN result.parm_cd is NULL AND mu_mdl.short_name = 'cells/cm2' THEN '-222'
            WHEN result.parm_cd is NULL AND mu_mdl.short_name = 'No. Org.' THEN '-333'
            WHEN result.parm_cd is NULL AND mu_mdl.short_name = 'to/s' THEN '-444'
            WHEN result.parm_cd is NULL AND mu_mdl.short_name = 'ug/ml' THEN '-555'
            ELSE result.parm_cd 
        END as parm_cd,
        --UPDATE 20260402 MOVED OUTSIDE CORE SINCE PARM_CD NEEDS UPDATING SO CANNOT USE JOINS on it yet d.meas_unit_cd as result_unit_code,
        result.meas_unit_cd as mdl_unit_code,
        smpl.flow as "Air Flow Volume", -- convert to rows and add to activity result (data classification) - "ACTIVITY_RESULT" - no method, no 
        flow_unit.short_name as "Air Flow Unit Code",
        smpl.filter_size as "Air Filter Size",
        smpl.BIO_SAMPLE_AREA as "Bio Sample Area",
        au.short_name as "Bio Sample Area Unit",
        smpl.BIO_SAMPLE_VOLUME as "Bio Sample Volume",
        vu.short_name as "Bio Sample Volume Unit",
        --smpl.BIO_SAMPLE_WEIGHT,
        --wu.short_name as BIO_SAMPLE_WEIGHT_CODE,
        result.CONTINUOUS_MINIMUM,
        result.CONTINUOUS_MAXIMUM,
        result.CONTINUOUS_AVERAGE,
        smpl.SIZE_FROM,
        smpl.SIZE_TO,
        smpl.WEIGHT_FROM,
        smpl.WEIGHT_TO,
        --UPDATE 20260511 ls and lfs carry the life stage description
        --Use to update life stage value
        CASE 
            WHEN ls.description is NULL AND lfs.description is NOT NULL THEN lfs.description
            WHEN ls.description is NOT NULL AND lfs.description is NULL THEN ls.description
            WHEN ls.description is NOT NULL AND lfs.description is NOT NULL and ls.description = lfs.description THEN ls.description
            ELSE ''
        END AS "Biological Life Stage"
    FROM
        ems_samples smpl
        LEFT OUTER JOIN result_tokenized result ON smpl.id = result.smpl_id
        LEFT JOIN ems_monitoring_locations mloc ON smpl.mon_locn_id = mloc.id
        LEFT JOIN ems_location_types elt ON mloc.locntyp_cd = elt.code
        LEFT JOIN ems_location_purposes p ON mloc.locnpurpose_cd = p.code
        LEFT JOIN (
            SELECT
                pa.mon_locn_id,
                MAX(pa.permit_id) AS permit
            FROM
                ems_permit_assoc pa
            GROUP BY
                pa.mon_locn_id
        ) max_pa ON max_pa.mon_locn_id = mloc.id
        LEFT JOIN ems_permit_relationships pr ON pr.code = (
            SELECT
                pa.perm_rltn_code
            FROM
                ems_permit_assoc pa
            WHERE
                pa.mon_locn_id = mloc.id
            FETCH FIRST 1 ROWS ONLY
        )
        LEFT JOIN ems_discharge_medias dm ON mloc.dismedia_cd = dm.code
        LEFT JOIN ems_user_details eud ON smpl.ministry_contact = eud.id
        LEFT JOIN print.staffs ps ON eud.staff_id = ps.id
        LEFT JOIN ems_client_locations cl ON cl.id = smpl.cliloc_id_smpl_by
        LEFT JOIN ems_client_locations cl2 ON cl2.id = smpl.cliloc_id_anal_by
        LEFT JOIN ems_collection_methods cm ON smpl.clct_methd_cd = cm.code
        LEFT JOIN ems_sample_classes sc ON smpl.smpl_cls_cd = sc.code
        LEFT JOIN ems_sample_states ss ON smpl.smpl_st_cd = ss.code
        LEFT JOIN ems_sample_descriptors sd ON smpl.smpl_desc_cd = sd.code
        LEFT JOIN ems.ems_locn_state_descriptor_export_map m ON mloc.locntyp_cd = m.type
                                                                AND smpl.smpl_st_cd = m.state
                                                                AND smpl.smpl_desc_cd = m.descriptor
        --UPDATE 20260409 DOES NOT GET USED ANYWHERE SO COMMENTING IT
        --LEFT JOIN ems_parameters param ON result.parm_cd = param.code
        --LEFT JOIN ems_anal_methods am ON result.anal_method_cd = am.code
        LEFT JOIN ems.AQS_UNITS_TEMP aqs_project ON to_char(aqs_project.EMS_CODE) = to_char(smpl.requisition_id)
        LEFT JOIN ems_measurment_units mu_mdl ON mu_mdl.code = result.meas_unit_cd
        LEFT JOIN ems_tides tide ON smpl.tide_cd = tide.code
        LEFT JOIN ems_measurment_units flow_unit ON flow_unit.code = smpl.flow_unit_cd
        LEFT JOIN ems_tissue_types tt ON smpl.tissue_typ_cd = tt.code
        LEFT JOIN ems_species sp ON smpl.species_cd = sp.code
        LEFT JOIN ems_sexes es ON smpl.sex_cd = es.code
        LEFT JOIN ems_life_stages ls ON smpl.life_stg_cd = ls.code
        LEFT JOIN ems_measurment_units vu ON vu.code = smpl.bio_smpl_vol_units_cd
        LEFT JOIN ems_measurment_units au ON au.code = smpl.bio_smpl_area_units_cd
        LEFT JOIN ems_measurment_units su ON su.code = smpl.size_units_cd
        LEFT JOIN ems_measurment_units wu ON wu.code = smpl.bio_smpl_weight_units_cd
        LEFT JOIN ems_species esp ON result.tax_nm_cd = esp.code
        LEFT JOIN ems_life_stages lfs ON result.life_stg_cd = lfs.code
        LEFT JOIN ems_preservatives epc ON smpl.preservative_cd = epc.code
        -- Inline subquery to get the earliest start and latest end times for each MON_LOCN_ID per day
        LEFT JOIN (
            SELECT
                mon_locn_id,
                trunc(collection_start_date) AS dateonly,
                MIN(collection_start_date)   AS earlieststarttime,
                MAX(collection_end_date)     AS latestendtime
            FROM
                ems_samples
            GROUP BY
                mon_locn_id,
                trunc(collection_start_date)
        ) eal ON smpl.mon_locn_id = eal.mon_locn_id
                 AND trunc(smpl.collection_start_date) = eal.dateonly
    WHERE
        mloc.locntyp_cd NOT LIKE 'D%' -- needed for all queries
        AND mloc.locntyp_cd NOT LIKE 'P%' 
        --AND smpl.when_created <= TIMESTAMP '2026-01-15 17:15:00'
        --AND smpl.id = '3515886'
)
select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        'Animal tissue' as "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Activity Comments" as "Field Visit Comments",
        "Activity ID" as "Activity Comments",
        'FALSE' as "Field Filtered",
        "Field Filtered Comment",
--        WHEN UPPER("Tissue Type") = 'ROOT' THEN ' - ' || "Tissue Type"
        CASE 
        WHEN UPPER("Field Preservative") IN 
        ('SULFURIC_ACID', 'NITRIC_ACID', 'HYDROCHLORIC_ACID', 'SODIUM_HYDROXIDE', 'ICE', 'ISOPROPYL_ALCOHOL', 
        'MERCURIC_CHLORIDE', 'LIQUID_NITROGEN', 'FORMALIN', 'SODIUM_AZIDE', 'FIELD_FREEZE', 'KEEP_DARK') THEN UPPER("Field Preservative")
        ELSE ''
        END AS "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        '' as "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        "Observed Property ID",
        "Result Value",
        CASE WHEN "Data Classification" = 'LAB' AND "Method Detection Limit" is NULL THEN -99 ELSE "Method Detection Limit" END AS "Method Detection Limit",
        '' as "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        CASE 
          WHEN "Detection Condition" = 'NOT_DETECTED' THEN 'LOWER'
            ELSE ''
        END AS "Limit Type",
        '' as "Fraction",
        "Data Classification",
        '' as "Source of Rounded Value",
        '' as "Rounded Value",
        '' as "Rounding Specification",
        CASE 
            WHEN "Data Classification" = 'LAB' AND "Analyzing Agency" is NULL THEN 'UNK' 
                ELSE "Analyzing Agency"
        END AS "Analyzing Agency",
        CASE 
            WHEN "Data Classification" = 'LAB' AND "Analysis Method" is NULL THEN 'UNK' 
                ELSE "Analysis Method"
        END AS "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        'Preliminary' as "Result Status",
        'Ungraded' as "Result Grade",
        '' as "Activity ID",
        CASE 
            WHEN UPPER("Medium") LIKE '%WATER%' THEN to_char("Activity Name")||'A'
            ELSE to_char("Activity Name")
        END AS "Activity Name",
         "Tissue Type",
        "Lab Arrival Temperature",
    "Medium" ||
    CASE 
        WHEN cnt > 1 AND rn >= 1 THEN ' ' || TO_CHAR(rn)
        ELSE ''
    END ||
    CASE 
        WHEN UPPER("Tissue Type") = 'ROOT' THEN ' - ' || "Tissue Type"
        ELSE ''
    END AS "Specimen Name",
    "Lab Quality Flag",
        "Lab Arrival Date and Time",
        '' as "Lab Prepared DateTime",
    "Medium" ||
    CASE 
        WHEN cnt > 1 AND rn >= 1 THEN ' ' || TO_CHAR(rn)
        ELSE ''
    END ||
    CASE 
        WHEN UPPER("Tissue Type") = 'ROOT' THEN ' - ' || "Tissue Type"
        ELSE ''
    END AS "Lab Sample ID",    
--        'Fish 1' ||
--    CASE 
--        WHEN cnt > 1 AND rn > 1 THEN ' - ' || rn
--        ELSE ''
--    END ||
--    CASE 
--        WHEN UPPER("Tissue Type") IS NOT NULL AND UPPER("Tissue Type") <> 'WHOLE BODY' THEN ' - ' || "Tissue Type"
--        ELSE ''
--    END AS "Lab Sample ID",
        --"Limit Type",-- doesn't exist in ems  
        --"Fraction",
        --"Lab Prepared DateTime",
        '' as "Lab Dilution Factor",
        CASE 
            WHEN "Method Detection Limit" = -99 THEN "Lab Comment" || '; Missing MDL set to -99 for EMS to EnMoDS migration' 
            WHEN "Data Classification" = 'LAB' AND "Method Detection Limit" is NULL THEN "Lab Comment" ||'; Missing MDL set to -99 for EMS to EnMoDS migration' 
            ELSE "Lab Comment"
        END AS "Lab Comment",
        "Lab Batch ID",
        CASE 
            WHEN "QC Type" is NULL THEN 'REGULAR'
            ELSE "QC Type"
        END AS "QC Type",
        --'' as "QC Type",
        '' as "QC Source Activity Name",
        --'' as "QC Source Sample ID",
        '' as "Composite Stat",
        "Biological Life Stage",
       "DEBUGGING TAX_NM_CD",
             COUNT(*) OVER (PARTITION BY "Work Order Number", "Location ID", "Activity Name", "Observed DateTime", "Observed Property ID", "Lab Quality Flag", "Result Value") AS cnt_dup,
        ROW_NUMBER() OVER (PARTITION BY "Work Order Number", "Location ID", "Activity Name", "Observed DateTime", "Observed Property ID", "Result Value" ORDER BY "Lab Quality Flag") AS rn_dup--,     
       -- COUNT(*) OVER (PARTITION BY "Observed Property ID", "DEBUGGING TAX_NM_CD" ORDER BY "Observed DateTime") AS cnt_op_tax_nm_cd,
       -- ROW_NUMBER() OVER (PARTITION BY "Observed Property ID", "DEBUGGING TAX_NM_CD" ORDER BY "Observed DateTime") AS rn_op_tax_nm_cd       
from(select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        CASE 
            WHEN "Observed Property ID" IN ('Extinction Depth (len.)') THEN 'Water - Unknown'
            WHEN LOWER("Observed Property ID") LIKE '%fl. conc.%' THEN 'Water - Unknown'
            WHEN (LOWER("Observed Property ID") LIKE '%coliform%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%temperature%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%microb%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%acidity%'  AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%oxygen%'  AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%phosphorus%'  AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            --OR LOWER("Observed Property ID") LIKE '%mass%'
            --OR LOWER("Observed Property ID") LIKE '%volume%'
            --OR LOWER("Observed Property ID") LIKE '%chlorophyll%'
            --OR LOWER("Observed Property ID") LIKE '%moisture%'
            --OR LOWER("Observed Property ID") LIKE '%phaeophytin%'
            OR (LOWER("Observed Property ID") LIKE '%turbidity%'  AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%specific conductance%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            THEN 'Water - Unknown' 
            ELSE "Medium"
        END AS "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        --"Observed Property ID",-- based on the analytical method and parameter code and unit
        --"Result Value",
        "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Specimen Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        CASE 
            WHEN UPPER("Field Preservative") NOT IN 
                ('SULFURIC_ACID', 'NITRIC_ACID', 'HYDROCHLORIC_ACID', 'SODIUM_HYDROXIDE', 'ICE', 'ISOPROPYL_ALCOHOL', 
                'MERCURIC_CHLORIDE', 'LIQUID_NITROGEN', 'FORMALIN', 'SODIUM_AZIDE', 'FIELD_FREEZE', 'KEEP_DARK') THEN "Lab Comment" || '; Field Preservative: ' || UPPER("Field Preservative")
            ELSE "Lab Comment"
        END AS "Lab Comment",
--        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        "Observed Property ID",
        "Result Value",
             COUNT(*) OVER (PARTITION BY "Location ID", "Field Visit Start Time", "Activity Name", "Observed Property ID", "Lab Batch ID") AS cnt,
        ROW_NUMBER() OVER (PARTITION BY "Location ID", "Field Visit Start Time", "Activity Name", "Observed Property ID" ORDER BY "Lab Batch ID") AS rn,
        "DEBUGGING TAX_NM_CD"
from(select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        --"Observed Property ID",-- based on the analytical method and parameter code and unit
        --"Result Value",
        "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method",
        CASE
            WHEN UPPER("Data Classification") IN ('LAB', 'SURROGATE_RESULT') AND "DEBUGGING TAX_NM_CD" is NOT NULL
                THEN MIN("Analyzed Date Time") 
                OVER (
                PARTITION BY
                    "Work Order Number",
                    "DEBUGGING TAX_NM_CD"
            )
            ELSE ''
        END AS "Analyzed Date Time",
        --"Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Specimen Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        CASE 
            WHEN "Data Classification" IN ('FIELD_RESULT', 'ACTIVITY_RESULT') THEN '' 
            ELSE "Lab Quality Flag"
        END AS "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        CASE 
            WHEN "Data Classification" IN ('FIELD_RESULT', 'ACTIVITY_RESULT') THEN '' 
            WHEN "DEBUGGING TAX_NM_CD" = 'UID JU' THEN 'Juvenile'
            ELSE "Biological Life Stage"
        END AS "Biological Life Stage",
        "Observed Property ID",
        "Result Value",
        "DEBUGGING TAX_NM_CD"
from(select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        --"Observed Property ID",-- based on the analytical method and parameter code and unit
        --"Result Value",
        "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
--         CASE 
--            WHEN "Observed Property ID 2" = 'Taxonomy' AND "Result Value" = 0 THEN 'PRESENT'
--            ELSE "Detection Condition"
--        END AS "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method",
        "Analyzed Date Time",
        --"Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Specimen Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND "Lab Quality Flag" LIKE '%ICP%' THEN ''
            ELSE "Lab Quality Flag"
        END AS "Lab Quality Flag",
        --"Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        "Observed Property ID",
        "Result Value",
--        CASE 
--            WHEN "Observed Property ID 2" = 'Taxonomy' AND "Result Value" = 0 THEN ''
--            ELSE "Result Value"
--        END AS "Result Value",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' THEN "DEBUGGING TAX_NM_CD"
            ELSE ''
        END AS "DEBUGGING TAX_NM_CD"
from(select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        CASE 
            WHEN "Location ID" IS NOT NULL THEN LPAD("Location ID", 7, '0')
            ELSE NULL
        END AS "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        --"Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
       CASE 
            WHEN ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN NULL
            WHEN TRIM("Result Value") IS NOT NULL AND "Method Detection Limit" is NULL THEN -99
            ELSE "Method Detection Limit"
       END AS "Method Detection Limit", 
        "Method Reporting Limit",
        --"Result Unit",
        CASE
            WHEN ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN 'MC'
          --ELSE NULL
            ELSE "Result Unit"            
        END AS "Result Unit",
       CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN 'PRESENT'
            ELSE "Detection Condition"
        END AS "Detection Condition",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN NULL
            ELSE "Limit Type"
        END AS "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        case when "Data Classification" IN ('FIELD_RESULT', 'ACTIVITY_RESULT') then null else "Analysis Method" end AS "Analysis Method",
        CASE 
            WHEN upper("Data Classification") IN ('LAB', 'SURROGATE_RESULT') AND "Observed Property ID 2" <> 'Taxonomy'
                THEN COALESCE(
                    "Analyzed Date Time",
                    "Observed DateTime"
                )
            WHEN upper("Data Classification") IN ('LAB', 'SURROGATE_RESULT') AND "Observed Property ID 2" = 'Taxonomy'
                THEN COALESCE(
                    "Observed DateTime",
                    "Analyzed Date Time"
                )
            ELSE "Analyzed Date Time"
        END AS "Analyzed Date Time",
        --"Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        case when "Data Classification" = 'FIELD_RESULT' then null else "Activity Name" end AS "Activity Name",
        "Specimen Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        "Observed Property ID",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND "Result Value" = -999 THEN ''
            ELSE "Result Value"
        END AS "Result Value",
        "Observed Property ID 2",
        "DEBUGGING TAX_NM_CD",
        "PARM_CD"
from(
select distinct "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        --"Observed Property ID",-- based on the analytical method and parameter code and unit
        --"Result Value",
       "Method Detection Limit", 
       "Method Reporting Limit",
        CASE 
--          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-111' THEN 'cells/mL'
          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-111' THEN 'No. Org.'
          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-222' THEN 'cells/cm2'
          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-333' THEN 'No. Org.'
          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-444' THEN 'to/s'
          WHEN "Observed Property ID 2" = 'Taxonomy' AND "PARM_CD" = '-555' THEN 'um3/mL'
          WHEN "Observed Property ID 2" = 'Biological Sample Volume (vol.)' THEN "Bio Sample Volume Unit"
          WHEN "Observed Property ID 2" = 'Biological Sample Area (area)' THEN "Bio Sample Area Unit"
          WHEN "Observed Property ID 2" = 'Taxonomy' AND ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN 'MC'
          --ELSE NULL
          ELSE "Result Unit"            
        END AS "Result Unit",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN 'PRESENT'
            WHEN "Observed Property ID 2" IN ('Biological Sample Volume (vol.)', 'Biological Sample Area (area)') THEN NULL
            ELSE "Detection Condition"
        END AS "Detection Condition",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' AND ("Result Value" is NULL OR TRIM("Result Value") IS NULL) THEN NULL
            ELSE "Limit Type"
        END AS "Limit Type",-- doesn't exist in ems  
        "Fraction",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' THEN 'LAB'
            WHEN "Observed Property ID 2" = 'Biological Sample Volume (vol.)' THEN 'ACTIVITY_RESULT'
            WHEN "Observed Property ID 2" = 'Biological Sample Area (area)' THEN 'ACTIVITY_RESULT'
        END AS "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' THEN 'TAXA'
            ELSE "Analysis Method" 
        END AS "Analysis Method",        
        --"Analyzed Date Time", -- add date/time mask
        "Analyzed Date Time",
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Specimen Name",
        "Lab Quality Flag",
--        CASE 
--          WHEN "Observed Property ID" IS NOT NULL THEN '' 
--          WHEN "Observed Property ID 2" = 'Taxonomy' THEN "Lab Quality Flag"
--          ELSE ''
--        END AS "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        CASE 
            --WHEN "Observed Property ID 2" = 'Taxonomy' THEN "Lab Comment" || '; EMS TAXA: ' || "DEBUGGING TAX_NM_CD"
            WHEN "Observed Property ID 2" = 'Taxonomy' THEN "Lab Comment" || '; EMS TAXA: ' || "Taxonomy"
            ELSE "Lab Comment"
        END AS "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        CASE 
            WHEN "Observed Property ID 2" = 'Taxonomy' THEN TO_CHAR("VALUE")
            WHEN "Observed Property ID 2" = 'Biological Sample Area (area)' AND "Bio Sample Area Unit" = 'g' THEN 'Biological Sample Mass (wet) (mass)'
            ELSE "Observed Property ID 2" 
        END AS "Observed Property ID",
        CASE 
            WHEN "Observed Property ID 2" <> 'Taxonomy' THEN TO_CHAR("VALUE")
            WHEN "Observed Property ID" is not NULL THEN NULL
            ELSE TO_CHAR("Result Value")
        END AS "Result Value",
         "Taxonomy",
        "Observed Property ID 2",
        "DEBUGGING TAX_NM_CD",
        "PARM_CD"
from(
select DISTINCT "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        "Observed Property ID",-- based on the analytical method and parameter code and unit
        "Result Value",
        "Method Detection Limit", 
        "Method Reporting Limit",
        --TO_NUMBER('') as "Method Detection Limit", 
        --TO_NUMBER('') as "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        '' as "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Specimen Name",
 /*       CASE
            WHEN "Specimen Name" IS NULL AND "Data Classification" IN ('LAB', 'SURROGATE_RESULT') THEN to_char("Activity Name")
            WHEN "Specimen Name" IS NULL THEN ''
            WHEN duplicate_row_number > 1 THEN "Specimen Name" || '-' || duplicate_row_number
            ELSE "Specimen Name"
        END AS "Specimen Name", */
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        '' AS "Lab Sample ID",
        "Lab Dilution Factor" as "Lab Dilution Factor",
        "Lab Comment",
        '' AS "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        --"Bio Sample Area",
        "Bio Sample Area Unit",
        --"Bio Sample Volume",
        "Bio Sample Volume Unit",
        --"Taxonomy OP",
        "Taxonomy",
        "Observed Property ID 2",
        value,
        "PARM_CD",
        "DEBUGGING TAX_NM_CD"
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
from (select DISTINCT "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        "Observed Property ID",-- based on the analytical method and parameter code and unit
        "Result Value",
        CASE
            WHEN "Observed Property ID" is NULL AND "Method Detection Limit" is null THEN "Method Detection Limit Source 2"
            WHEN "Observed Property ID" is NULL AND "MDL Unit" <> "Result Unit" THEN
                "Method Detection Limit" / "Conversion_Factor"
                --unit_conversion.conversion_factor
            WHEN "Observed Property ID" is not NULL THEN NULL
        ELSE "Method Detection Limit" -- No conversion needed
        END AS "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        CASE 
            WHEN "Observed Property ID" is not NULL THEN NULL
            ELSE "Detection Condition"
        END AS "Detection Condition",
        CASE 
            WHEN "Observed Property ID" is not NULL THEN NULL
            ELSE "Limit Type"
        END AS "Limit Type",-- doesn't exist in ems  
        '' as "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Specimen Name",
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
      '' AS "Lab Sample ID",
        "Lab Dilution Factor" as "Lab Dilution Factor",
        "Lab Comment",
        '' AS "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Taxonomy OP",
        "Taxonomy",
        "Bio Sample Area",
        "Bio Sample Area Unit",
        "Bio Sample Volume",
        "Bio Sample Volume Unit",
        --"Fish Fork Length (len.).Unit",            
        --"Fish Weight (mass).Unit",
        "Composite Stat",
        "Biological Life Stage",
        "PARM_CD",
        "DEBUGGING TAX_NM_CD"
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
FROM (
SELECT DISTINCT
        ''  as "Observation ID",
        core_updated."Ministry Contact",
        core_updated."Sampling Agency",
        core_updated."Project",
        core_updated."Work Order Number",
        core_updated."Location ID",
        core_updated."Field Visit Start Time", -- required
        core_updated."Field Visit End Time",
        core_updated."Field Visit Participants",
        '' as "Field Visit Comments",
        core_updated."Activity Comments",
        core_updated."Field Filtered",
        core_updated."Field Filtered Comment" AS "Field Filtered Comment",
        core_updated."Field Preservative",
        NULL AS "Field Device ID",-- leave as blank
        ed.Device_Type as "Field Device Type",
        core_updated."Sampling Context Tag",
        core_updated."Collection Method",
        core_updated."Medium" as "Medium",
        core_updated."Depth Upper",
        core_updated."Depth Lower",
        core_updated."Depth Unit",
        core_updated."Observed DateTime",
        core_updated."Observed Date Time End",
        ed.NewNameID AS "Observed Property ID",-- based on the analytical method and parameter code and unit
        core_updated."Result Value",
        --core."Method Detection Limit" as "UNCONVERTED_MDL",-- the unit may not be accurate.  Conversion may be needed.  This is the lab based limit.  If the result from the lab is missing, we can get from the analytical methods table
        --unit_conversion.source_unit_id,
        --unit_conversion.target_unit_id,
        --unit_conversion.conversion_factor,
        core_updated."Method Detection Limit", 
        core_updated."Method Detection Limit Source 2",
--        CASE
--            WHEN core_updated."Method Detection Limit" is null THEN core_updated."Method Detection Limit Source 2"
--            WHEN core_updated."MDL Unit" <> core_updated."Result Unit" THEN
--                core_updated."Method Detection Limit" / unit_conversion.conversion_factor
--                --unit_conversion.conversion_factor
--        ELSE core_updated."Method Detection Limit" -- No conversion needed
--        END AS "Method Detection Limit", 
        --core."Method Detection Limit" as "Method Detection Limit OG", -- debugging
        --unit_conversion.conversion_factor, -- debugging
        --core."MDL Unit", core."Result Unit", -- debugging
        core_updated."Method Reporting Limit",
        core_updated."MDL Unit",
        core_updated."Result Unit",
        core_updated."Detection Condition",
        core_updated."Limit Type",-- doesn't exist in ems  
        --ed.Fraction as "Fraction",
        CASE
			WHEN ed.Fraction is null then ''
			WHEN ed.Fraction = 'Extractable' then ''
			ELSE ed.Fraction 
		END AS "Fraction",
        ed.Classification as "Data Classification",
        core_updated."Source of Rounded Value",
        core_updated."Rounded Value",
        core_updated."Rounding Specification",
        core_updated."Analyzing Agency",
        core_updated."Analysis Method",
        --logic changed to match the logic in water extracts
        --CASE
        --    WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') and core."Analyzed Date Time" is null then core."Observed DateTime"
        --    ELSE core."Analyzed Date Time"
        --END as "Analyzed Date Time",
        core_updated."Analyzed Date Time",
--        CASE 
--            WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT')
--                THEN COALESCE(
--                    "Analyzed Date Time",
--                    core_updated."Observed DateTime"
--                )
--            ELSE core_updated."Analyzed Date Time"
--        END AS "Analyzed Date Time",
        core_updated."Result Status",
        core_updated."Result Grade",
        core_updated."Activity ID",
        core_updated."Activity Name",
        --logic changed to match the logic in water extracts
        core_updated."Tissue Type",
        --COALESCE(core."Tissue Type", 'Unknown') AS "Tissue Type",
        --logic commented to match the logic in water extracts
        --core.tissue_typ_cd,
        core_updated."Lab Arrival Temperature",
     /*   CASE 
            WHEN ed.Classification IN ('FIELD_RESULT', 'VERTICAL_PROFILE', 'FIELD_SURVEY') 
             THEN '' 
           -- WHEN duplicate_row_number > 1 THEN RTRIM(ed.OP_Group, '; ') || '-' || duplicate_row_number
             ELSE RTRIM(ed.OP_Group, '; ') 
        END AS "Specimen Name",*/
        core_updated."Medium" as "Specimen Name",
        core_updated."Lab Quality Flag",
        core_updated."Lab Arrival Date and Time",
        core_updated."Lab Prepared DateTime",
        core_updated."Lab Sample ID",
        core_updated."Lab Dilution Factor" as "Lab Dilution Factor",
         CASE 
            WHEN core_updated."Bio Sample Area Unit" = 'cm3' THEN "Lab Comment" || '; EMS Sample Area Unit cm3 updated to cm2' 
            WHEN core_updated."Bio Sample Area Unit" is NULL AND "Bio Sample Area" is NOT NULL THEN "Lab Comment" || '; EMS Sample Area Unit missing assumed cm2'
            WHEN core_updated."Bio Sample Volume Unit" = 'cm2' AND "Bio Sample Volume" is NOT NULL THEN "Lab Comment" || '; EMS Sample Volume Unit cm2 updated to cm3'
            WHEN core_updated."Bio Sample Volume Unit" is NULL AND "Bio Sample Volume" is NOT NULL THEN "Lab Comment" || '; EMS Sample Volume Unit unclear; reported Sample Volume is ' || to_char(core_updated."Bio Sample Volume Unit")
            ELSE "Lab Comment"
        END as "Lab Comment",
        core_updated."Lab Batch ID",
        core_updated."QC Type",
        core_updated."QC Source Activity Name",
        core_updated."Species" as "Taxonomy",
        tax_cw."NEWNAMEID" AS "Taxonomy OP",
        CASE 
            WHEN core_updated."Bio Sample Area Unit" = 'cm3' THEN 'cm2'
            WHEN core_updated."Bio Sample Area Unit" is NULL THEN 'cm2'
            ELSE core_updated."Bio Sample Area Unit"
        END AS "Bio Sample Area Unit",
        CASE 
            WHEN core_updated."Bio Sample Volume Unit" = 'cm2' THEN 'cm3'
            ELSE core_updated."Bio Sample Volume Unit"
        END AS "Bio Sample Volume Unit",
        to_char(core_updated."Bio Sample Area") AS "Bio Sample Area",
        CASE 
            WHEN "Bio Sample Volume Unit" IS NOT NULL THEN to_char(core_updated."Bio Sample Volume") 
            ELSE ''
        END AS "Bio Sample Volume",
        --core_updated."Fish Species",
        --core_updated."Fish Life Stage",
        --core_updated."Fish Sex",
        --core_updated."Fish Fork Length",
        --core_updated."Fish Weight",
        core_updated."Composite Stat",--,-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes.
        core_updated."Biological Life Stage",
        core_updated."PARM_CD",
        unit_conversion.conversion_factor AS "Conversion_Factor",
        core_updated.tax_nm_cd AS "DEBUGGING TAX_NM_CD"
        -- debugging likely leadinig to duplicates
        -- core.parm_cd as "DEBUGGING PARM_CD",
        -- core."Analysis Method" as "DEBUGGING ANALYSIS METHOD",
        -- core."EMS Result Unit" as "DEBUGGING_EMS_RESULT_UNIT"
--core.parm_cd, -- for troubleshooting
        --core."Analysis Method", -- for troubleshooting
        --core."Result Unit", -- for troubleshooting     
        --core."MDL Unit", -- for troubleshooting,
        --core.mdl_unit_code,
        --core.result_unit_code
FROM -- fish data
    (
        (
            select 
            core.*, 
            d.METHOD_DETECT_LIMIT AS "Method Detection Limit Source 2", 
            d.meas_unit_cd AS result_unit_code,
            mu.short_name AS "EMS Result Unit",
            CASE
                WHEN aqs_units.AQS_NAME_ON_IMPORT is null then 'None'
                --Seems like AQS UNITS TEMP WAS UPDATED at some point
                --WHEN aqs_units.AQS_NAME_ON_IMPORT = 'ug/g wet' then 'ug/g' 
                --WHEN aqs_units.AQS_NAME_ON_IMPORT = 'C' then 'degC'
                ELSE aqs_units.AQS_NAME_ON_IMPORT
            END AS "Result Unit"
            from core_data core
            LEFT JOIN ems_parm_dicts d on d.parm_cd = core.parm_cd
                AND d.anal_method_cd = core."Analysis Method"
            LEFT JOIN ems.AQS_UNITS_TEMP aqs_units ON aqs_units.EMS_CODE = d.meas_unit_cd
            LEFT JOIN ems_measurment_units mu ON mu.code = d.meas_unit_cd
            WHERE upper(core."Medium") LIKE '%ANIMAL%'
            AND upper(core."Medium") <> 'ANIMAL - FISH'
            ) core_updated
            left outer JOIN OBSERVED_PROPERTIES_FOR_ETL ed on core_updated.parm_cd = ed.Parm_code
                AND core_updated."Analysis Method" = ed.Analysis_Method_Code
                AND core_updated."EMS Result Unit" = ed.Unit
            left outer join ems.unit_conversions_temp unit_conversion on core_updated.result_unit_code = unit_conversion.target_unit_id 
                AND core_updated.mdl_unit_code = unit_conversion.source_unit_id
            left outer join tax_cw_tokenized tax_cw ON
            --left outer join ems.ems_etl_tax_crosswalk_temp tax_cw ON core_updated.tax_nm_cd IS NOT NULL 
                core_updated.tax_nm_cd IS NOT NULL 
                AND tax_cw.parm_code is NOT NULL
                AND INSTR(tax_cw.parm_code_tokens, core_updated.tax_nm_cd_tokens) > 0
--left outer join ems.ems_etl_tax_crosswalk_temp tax_cw on core_updated.tax_nm_cd = tax_cw.parm_code
                )
WHERE upper(core_updated."Medium") LIKE '%ANIMAL%'
            AND upper(core_updated."Medium") <> 'ANIMAL - FISH'
order by "Location ID" asc, "Observed DateTime" asc))
UNPIVOT (
    value FOR "Observed Property ID 2" IN (
       "Taxonomy OP" AS 'Taxonomy', 
       "Bio Sample Area" AS 'Biological Sample Area (area)', 
       "Bio Sample Volume" AS 'Biological Sample Volume (vol.)')
       )
)))) 
UNION ALL
select DISTINCT "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        CASE 
            WHEN "Location ID" IS NOT NULL THEN LPAD("Location ID", 7, '0')
            ELSE NULL
        END AS "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        TO_CHAR (
            TO_TIMESTAMP (
                SUBSTR ("Observed DateTime", 1, 19),
                'YYYY-MM-DD"T"HH24:MI:SS'
            ) + NUMTODSINTERVAL (duplicate_row_number - 1, 'SECOND'),
            'YYYY-MM-DD"T"HH24:MI:SS'
        ) || '-08:00' AS "Observed DateTime",
        "Observed Date Time End",
        "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        case when "Data Classification" = 'FIELD_RESULT' then null else "Analysis Method" end as "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        case when "Data Classification" = 'FIELD_RESULT' then null else "Activity Name" end as "Activity Name",
        "Tissue Type",
        /* CASE
            WHEN "Specimen Name" IS NULL AND "Data Classification" IN ('LAB', 'SURROGATE_RESULT') THEN to_char("Activity Name")
            WHEN "Specimen Name" IS NULL THEN ''
            WHEN duplicate_row_number > 1 THEN "Specimen Name" || '-' || duplicate_row_number
            ELSE "Specimen Name"
        END AS "Specimen Name",*/
        NVL("Specimen Name", "Medium") AS "Specimen Name",
        "Lab Arrival Temperature",
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor" as "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        "Observed Property ID" AS  "Observed Property ID",-- based on the analytical method and parameter code and unit
        TO_CHAR("Result Value") AS "Result Value",
        NULL AS "DEBUGGING TAX_NM_CD"     
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
from(
select DISTINCT "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        "Field Visit Comments",
        "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        "Observed Property ID",-- based on the analytical method and parameter code and unit
        "Result Value",
        "Method Detection Limit", 
        "Method Reporting Limit",
        "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        "Analyzing Agency",
        "Analysis Method", -- removed as per request from Jeremy.  The METHOD name was moved to field device type column
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        "Specimen Name",
        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor" as "Lab Dilution Factor",
        "Lab Comment",
        --'Lab Comment: ' || "Lab Comment" || ' EMS Result ID: ' || "Result ID" AS "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Biological Life Stage",
        ROW_NUMBER() OVER (
            PARTITION BY 
                "Location ID", 
                "Field Visit Start Time", 
                "Medium", 
                "Depth Upper", 
                CASE 
                    WHEN "Data Classification" IN ('FIELD_RESULT') THEN ''
                    ELSE COALESCE(to_char("Activity Name"), '')
                END, 
                COALESCE("Specimen Name", ''),
                "Data Classification", 
                CASE 
                    WHEN "Data Classification" IN ('FIELD_RESULT', 'VERTICAL_PROFILE') THEN null
                    ELSE "QC Type"
                END, 
                "Observed Property ID"
            ORDER BY TO_TIMESTAMP(SUBSTR("Observed DateTime", 1, 19), 'YYYY-MM-DD"T"HH24:MI:SS')
        ) AS duplicate_row_number,
        "DEBUGGING TAX_NM_CD"
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
from (
-- fish data - chemistry data
SELECT DISTINCT 
        ''  as "Observation ID",
        core_updated."Ministry Contact",
        core_updated."Sampling Agency",
        core_updated."Project",
        core_updated."Work Order Number",
        core_updated."Location ID",
        core_updated."Field Visit Start Time", -- required
        core_updated."Field Visit End Time",
        core_updated."Field Visit Participants",
        '' as "Field Visit Comments",
        core_updated."Activity Comments",
        core_updated."Field Filtered",
        core_updated."Field Filtered Comment",
        core_updated."Field Preservative",
        NULL AS "Field Device ID",-- leave as blank
        ed.Device_Type as "Field Device Type",
        core_updated."Sampling Context Tag",
        core_updated."Collection Method",
        core_updated."Medium" as "Medium",
        core_updated."Depth Upper",
        core_updated."Depth Lower",
        core_updated."Depth Unit",
        core_updated."Observed DateTime",
        core_updated."Observed Date Time End",
        ed.NewNameID AS "Observed Property ID",-- based on the analytical method and parameter code and unit
        core_updated."Result Value",
        --core_updated."Method Detection Limit" as "UNCONVERTED_MDL",-- the unit may not be accurate.  Conversion may be needed.  This is the lab based limit.  If the result from the lab is missing, we can get from the analytical methods table
        --unit_conversion.source_unit_id,
        --unit_conversion.target_unit_id,
        --unit_conversion.conversion_factor,
        CASE
            WHEN core_updated."Method Detection Limit" is null THEN core_updated."Method Detection Limit Source 2"
            WHEN core_updated."MDL Unit" <> core_updated."Result Unit" THEN
                core_updated."Method Detection Limit" / unit_conversion.conversion_factor
                --unit_conversion.conversion_factor
        ELSE core_updated."Method Detection Limit" -- No conversion needed
        END AS "Method Detection Limit", 
        --core_updated."Method Detection Limit" as "Method Detection Limit OG", -- debugging
        --unit_conversion.conversion_factor, -- debugging
        --core_updated."MDL Unit", core_updated."Result Unit", -- debugging
        core_updated."Method Reporting Limit",
        core_updated."Result Unit",
        core_updated."Detection Condition",
        core_updated."Limit Type",-- doesn't exist in ems  
        ed.Fraction as "Fraction",
        ed.Classification as "Data Classification",
        core_updated."Source of Rounded Value",
        core_updated."Rounded Value",
        core_updated."Rounding Specification",
        core_updated."Analyzing Agency",
        core_updated."Analysis Method",
        --logic changed to match the logic in water extracts
        --CASE
        --    WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') and core_updated."Analyzed Date Time" is null then core_updated."Observed DateTime"
        --    ELSE core_updated."Analyzed Date Time"
        --END as "Analyzed Date Time",
        CASE 
            WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') 
            and core_updated."Analyzed Date Time" is null then core_updated."Observed DateTime"
                --THEN COALESCE(
                --    "Analyzed Date Time",
                --    core_updated."Observed DateTime"
                --)
            ELSE core_updated."Analyzed Date Time"
        END AS "Analyzed Date Time",
        core_updated."Result Status",
        core_updated."Result Grade",
        core_updated."Activity ID",
        core_updated."Activity Name",
        --logic changed to match the logic in water extracts
        core_updated."Tissue Type",
        --COALESCE(core_updated."Tissue Type", 'Unknown') AS "Tissue Type",
        --logic commented to match the logic in water extracts
        --core_updated.tissue_typ_cd,
        core_updated."Lab Arrival Temperature",
       /* CASE 
        WHEN ed.Classification IN ('FIELD_RESULT', 'VERTICAL_PROFILE', 'FIELD_SURVEY') 
           THEN '' 
           -- WHEN duplicate_row_number > 1 THEN RTRIM(ed.OP_Group, '; ') || '-' || duplicate_row_number
           ELSE RTRIM(ed.OP_Group, '; ') 
        END AS "Specimen Name", */
        core_updated."Medium" as "Specimen Name",
        core_updated."Lab Quality Flag",
        core_updated."Lab Arrival Date and Time",
        core_updated."Lab Prepared DateTime",
        core_updated."Lab Sample ID",
        core_updated."Lab Dilution Factor" as "Lab Dilution Factor",
        core_updated."Lab Comment" as "Lab Comment",
        --core_updated."Result ID" as "Result ID",
        core_updated."Lab Batch ID",
        core_updated."QC Type",
        core_updated."QC Source Activity Name",
        core_updated."Composite Stat",
        core_updated."Biological Life Stage",--,-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes.
        core_updated.tax_nm_cd as "DEBUGGING TAX_NM_CD"--,-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes.
        -- debugging likely leadinig to duplicates
        -- core_updated.parm_cd as "DEBUGGING PARM_CD",
        -- core_updated."Analysis Method" as "DEBUGGING ANALYSIS METHOD",
        -- core_updated."EMS Result Unit" as "DEBUGGING_EMS_RESULT_UNIT"
--core_updated.parm_cd, -- for troubleshooting
        --core_updated."Analysis Method", -- for troubleshooting
        --core_updated."Result Unit", -- for troubleshooting     
        --core_updated."MDL Unit", -- for troubleshooting,
        --core_updated.mdl_unit_code,
        --core_updated.result_unit_code
FROM -- fish data
    (
        (
        
           select 
            core.*, 
            d.METHOD_DETECT_LIMIT AS "Method Detection Limit Source 2", 
            d.meas_unit_cd AS result_unit_code,
            mu.short_name AS "EMS Result Unit",
            CASE
                WHEN aqs_units.AQS_NAME_ON_IMPORT is null then 'None'
                --WHEN aqs_units.AQS_NAME_ON_IMPORT = 'ug/g wet' then 'ug/g' 
                --WHEN aqs_units.AQS_NAME_ON_IMPORT = 'C' then 'degC'
                ELSE aqs_units.AQS_NAME_ON_IMPORT
            END AS "Result Unit"
            from core_data core
            LEFT JOIN ems_parm_dicts d on d.parm_cd = core.parm_cd
                AND d.anal_method_cd = core."Analysis Method"
            LEFT JOIN ems.AQS_UNITS_TEMP aqs_units ON aqs_units.EMS_CODE = d.meas_unit_cd
            LEFT JOIN ems_measurment_units mu ON mu.code = d.meas_unit_cd
            WHERE upper(core."Medium") LIKE '%ANIMAL%' 
                    AND upper(core."Medium") <> 'ANIMAL - FISH'
        ) core_updated
          left outer JOIN OBSERVED_PROPERTIES_FOR_ETL ed on core_updated.parm_cd = ed.Parm_code
                AND core_updated."Analysis Method" = ed.Analysis_Method_Code
                AND core_updated."EMS Result Unit" = ed.Unit
          left outer join ems.unit_conversions_temp unit_conversion on core_updated.result_unit_code = unit_conversion.target_unit_id 
                AND core_updated.mdl_unit_code = unit_conversion.source_unit_id
            --left outer join ems.ems_etl_tax_crosswalk_temp tax_cw ON 
          left outer join tax_cw_tokenized tax_cw ON
                core_updated.tax_nm_cd IS NOT NULL
                AND tax_cw.parm_code is NOT NULL
                AND INSTR(tax_cw.parm_code_tokens, core_updated.tax_nm_cd_tokens) > 0
--left outer join ems.ems_etl_tax_crosswalk_temp tax_cw on core_updated.tax_nm_cd = tax_cw.parm_code
    )
--    core_data core
--    left outer JOIN OBSERVED_PROPERTIES_FOR_ETL ed on core.parm_cd = ed.Parm_code
--        and core."Analysis Method" = ed.Analysis_Method_Code
--        and core."EMS Result Unit" = ed.Unit 
where core_updated.result_unit_code is not null 
and core_updated.mdl_unit_code is not null 
AND upper(core_updated."Medium") LIKE '%ANIMAL%' 
AND upper(core_updated."Medium") <> 'ANIMAL - FISH'
--removing System Calculations
AND core_updated."Analysis Method" NOT IN ('CS00', 'CS01', 'CS02', 'CS03', 'CS14', 'CS15', 'CS30', 'CS34', 'CS44')
-- end fish data
 AND ed.NewNameID is not null
    and ((core_updated."Result Value" is not null) or (core_updated.result_text = '''C'''))
                order by core_updated."Location ID" asc, core_updated."Observed DateTime" asc
    ))  --where duplicate_row_number = 1 AND
    -- WHERE "Observed Property ID" is not null
    -- AND "Work Order Number" = '3845'
    -- AND "Location ID" = '410054'
    )
    WHERE "Observed Property ID" is not null
    AND ("Result Unit" IS NULL OR "Result Unit" <> 'None')
    )
    -- WHERE "Work Order Number" = '50033615'
order by "Activity Name" asc, "Observed Property ID" asc, "Observed DateTime" asc

--where "Work Order Number" = '10070091'
--AND core.result_unit_code is not null 
--and core.mdl_unit_code is not null 
--removing System Calculations
--AND core."Analysis Method" NOT IN ('CS00', 'CS01', 'CS02', 'CS03', 'CS14', 'CS15', 'CS30', 'CS34', 'CS44')
-- end fish data
 --AND ed.NewNameID is not null
    --and ((core."Result Value" is not null) or (core.result_text = '''C'''))
    --            order by core."Location ID" asc, core."Observed DateTime" asc
--AND BIO_SAMPLE_VOLUME is not NULL
-- THESE HAVE VALUES
--BIO_SAMPLE_VOLUME is not NULL 
--OR BIO_SAMPLE_VOLUME_CODE is not NULL 
--OR BIO_SAMPLE_AREA is not NULL
--OR BIO_SAMPLE_AREA_CODE is not NULL

-- BIO_SAMPLE_WEIGHT, Composite Stat, CONTINUOUS_MAXIMUM, Air Flow Volume, Air Filter Size, LIFE_STG_CD, WEIGHT_FROM, and SIZE_FROM are all NULLS for Animal planktons
--AND (LIFE_STG_CD is not NULL 
--OR WEIGHT_FROM is not NULL 
--OR SIZE_FROM is not NULL)

--pending checks
--20260331: 5 observations when both PARM_CD and SPECIES are NULL 