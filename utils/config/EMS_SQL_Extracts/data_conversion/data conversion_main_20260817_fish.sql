--ALTER SESSION SET CURRENT_SCHEMA = ems;
set define off; --This is an SQL Developer thing. Cannot be set in PL SQL.
WITH core_data AS (
    SELECT DISTINCT
        TRIM(ps.first_name || ' ' || ps.last_name)                         AS "Ministry Contact",
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
                -- to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				CASE 
					WHEN to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(eal.latestendtime, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
        end AS "Field Visit End Time",
        smpl.sampler                                                 AS "Field Visit Participants",
        REPLACE(smpl.field_comment , CHR(0), '') AS "Activity Comments",
--        smpl.field_comment                                           AS "Activity Comments",
        NULL                                                         AS "Field Filtered", -- blank, doesn't exist in ems
        NULL                                                         AS "Field Filtered Comment", -- blank, doesn't exist in ems
        epc.description                                              AS "Field Preservative",-- updated to use description; note that only 3800 records of ~ 2 million records have a field preservative
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
        m.enmods_medium                                             AS "Medium",
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
                -- to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
                CASE 
					WHEN to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(smpl.collection_end_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
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
--        case 
--            when result.result_text = '''C''' then null
--            else result.result_numeric 
--        end AS "Result Value",
        result.method_detect_limit                                   AS "Method Detection Limit",
        d.METHOD_DETECT_LIMIT   AS "Method Detection Limit Source 2",        
        NULL                                                         AS "Method Reporting Limit", -- leave as blank
        --aqs_units.AQS_NAME_ON_IMPORT                                                AS "Result Unit",
        CASE
			WHEN aqs_units.AQS_NAME_ON_IMPORT is null then 'None'
			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'ug/g wet' then 'ug/g' 
			WHEN aqs_units.AQS_NAME_ON_IMPORT = 'C' then 'degC'
			ELSE aqs_units.AQS_NAME_ON_IMPORT
		END AS "Result Unit",
        mu.short_name AS "EMS Result Unit",
        mu_mdl.short_name                                            AS "MDL Unit",
        result.result_text,
        CASE
            WHEN result.result_letter = '<' THEN 'NOT_DETECTED'
            WHEN result.result_text LIKE '%<%' THEN 'NOT_DETECTED'
            WHEN result.result_text = '''C''' then 'NOT_SAMPLED'
            ELSE NULL
        END                                                          AS "Detection Condition",
--        CASE
--            WHEN result.result_letter = '<' THEN
--                'NOT_DETECTED'
--            WHEN result.result_text = '''C''' then
--                'NOT_SAMPLED'
--            ELSE
--                NULL
--        END                                                          AS "Detection Condition",
        NULL                                                         AS "Limit Type",
        NULL                                                         AS "Source of Rounded Value", -- can be blank
        NULL                                                         AS "Rounded Value", -- can be blank
        NULL                                                         AS "Rounding Specification", -- can be blank
        cl2.short_name                                               AS "Analyzing Agency",
        result.anal_method_cd                                        AS "Analysis Method",
        CASE 
            WHEN result.analytical_date IS NULL THEN NULL
            --ELSE to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
            ELSE 
				CASE 
					WHEN to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(result.analytical_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
        END AS "Analyzed Date Time",
        'Preliminary'                                                AS "Result Status",
        'Ungraded'                                                   AS "Result Grade",
        NULL                                                         AS "Activity ID",
        smpl.id                                                      AS "Activity Name",
         
        smpl.tissue_typ_cd,
        esp.DESCRIPTION AS "SPECIES",
        result.tax_nm_cd,
        smpl.lab_arrival_temperature                                 AS "Lab Arrival Temperature",
        result.result_comment AS "Lab Quality Flag",
--        NULL                                                         AS "Lab Quality Flag",-- leave blank
        CASE 
            WHEN smpl.lab_arrival_date IS NULL THEN NULL
            --ELSE to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00' 
            ELSE 
				CASE 
					WHEN to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') = 'NA' THEN ''
					ELSE to_char(smpl.lab_arrival_date, 'YYYY-MM-DD"T"HH24:MI:SS') || '-08:00'
				END
        END AS "Lab Arrival Date and Time",        
        NULL                                                         AS "Lab Prepared DateTime",-- leave blank
        result.lab_sample_id                                         AS "Lab Sample ID",
        --result.id as "Result ID",
        NULL                                                         AS "Lab Dilution Factor",-- leave blank
        smpl.lab_comment AS "Lab Comment",
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
        result.parm_cd ,
        d.meas_unit_cd as result_unit_code,
        result.meas_unit_cd as mdl_unit_code,
        smpl.flow as "Air Flow Volume", -- convert to rows and add to activity result (data classification) - "ACTIVITY_RESULT" - no method, no 
        flow_unit.short_name as "Air Flow Unit Code",
        smpl.filter_size as "Air Filter Size",
        smpl.BIO_SAMPLE_AREA,
        au.short_name as BIO_SAMPLE_AREA_CODE,
        smpl.BIO_SAMPLE_VOLUME,
        vu.short_name as BIO_SAMPLE_VOLUME_CODE,
        smpl.BIO_SAMPLE_WEIGHT,
        wu.short_name as BIO_SAMPLE_WEIGHT_CODE,
        result.CONTINUOUS_MINIMUM,
        result.CONTINUOUS_MAXIMUM,
        result.CONTINUOUS_AVERAGE,

       CASE 
            WHEN tt.description is NOT NULL THEN tt.description
            WHEN (
            
            CASE WHEN tt.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%LIVE%' 
            OR UPPER(smpl.field_comment) LIKE '%LIVE%') THEN 1 ELSE 0 END +
            
            CASE WHEN tt.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%MUSC%'
            OR UPPER(smpl.field_comment) LIKE '%MUSC%') THEN 1 ELSE 0 END +
            
            CASE WHEN tt.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%WHOLE%' 
            OR UPPER(smpl.lab_comment) LIKE '%WB%'
            OR UPPER(smpl.field_comment) LIKE '%WHOLE%'
            OR UPPER(smpl.field_comment) LIKE '%WB%') THEN 1 ELSE 0 END +
            
            CASE WHEN tt.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%STOMACH%'
            OR UPPER(smpl.field_comment) LIKE '%STOMACH%') THEN 1 ELSE 0 END +
            
            CASE WHEN tt.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%EGG%'
            OR UPPER(smpl.field_comment) LIKE '%EGG%') THEN 1 ELSE 0 END
            
            ) >=2 THEN ''
            
           WHEN tt.description is NULL 
           AND (UPPER(smpl.lab_comment) LIKE '%LIVE%' 
           OR UPPER(smpl.field_comment) LIKE '%LIVE%') THEN 'Liver'
           
           WHEN tt.description is NULL 
           AND (UPPER(smpl.lab_comment) LIKE '%MUSC%'
           OR UPPER(smpl.field_comment) LIKE '%MUSC%') THEN 'Muscle'
           
           WHEN tt.description is NULL 
           AND (UPPER(smpl.lab_comment) LIKE '%WHOLE%' 
           OR UPPER(smpl.lab_comment) LIKE '%WB%'
           OR UPPER(smpl.field_comment) LIKE '%WHOLE%'
           OR UPPER(smpl.field_comment) LIKE '%WB%') THEN 'Whole Body'
           
           WHEN tt.description is NULL 
           AND (UPPER(smpl.lab_comment) LIKE '%STOMACH%'
           OR UPPER(smpl.field_comment) LIKE '%STOMACH%') THEN 'Stomach'  
           
           WHEN tt.description is NULL 
           AND (UPPER(smpl.lab_comment) LIKE '%EGG%' 
           OR UPPER(smpl.field_comment) LIKE '%EGG%') THEN 'Egg'
           
           ELSE 'Unknown'
        END AS "Tissue Type", -- blank for this query, but not necessarily true for tax. and air
        
        CASE 
            WHEN sp.DESCRIPTION = 'Oncorhynchus clarki (Cutthroat trout)' THEN 'Oncorhynchus clarkii'
            WHEN sp.DESCRIPTION = 'Onchorhynchus clarki' THEN 'Oncorhynchus clarkii'
            WHEN sp.DESCRIPTION = 'Onchorhynchus kisutch' THEN 'Oncorhynchus kisutch'
            WHEN sp.DESCRIPTION = 'Onchorhynchus nerka' THEN 'Oncorhynchus nerka'
            WHEN sp.DESCRIPTION = 'Salvelinus malma (Dolly Varden Trout)' THEN 'Salvelinus malma'
            WHEN sp.DESCRIPTION is NOT NULL THEN sp.DESCRIPTION
            
            WHEN (
            
            CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%DOLLY%' 
                OR UPPER(smpl.field_comment) LIKE '%DOLLY%'
                OR smpl.field_comment LIKE '%DV%') THEN 1 ELSE 0 END +
                
            CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%RAINBOW%'
                OR UPPER(smpl.field_comment) LIKE '%RAINBOW%'
                OR UPPER(smpl.field_comment) LIKE '%RBW TROUT%'
                OR UPPER(smpl.field_comment) LIKE '%RBT%'
                OR smpl.field_comment LIKE '%RB %'
                OR UPPER(smpl.field_comment) LIKE '%O.MYKI%') THEN 1 ELSE 0 END +
                
            CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%CUTTHROAT%'
                OR UPPER(smpl.field_comment) LIKE '%CUTTHROAT%'
                OR smpl.field_comment LIKE '%WCT%'
                OR smpl.field_comment LIKE '%WTC%' 
                OR smpl.lab_comment LIKE '% CT%'
                OR smpl.field_comment LIKE '%CT%'
                ) THEN 1 ELSE 0 END +
                
            CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%LONG NOSE SUCKER%'
                OR UPPER(smpl.field_comment) LIKE '%LS SUCKER%') THEN 1 ELSE 0 END +

            CASE WHEN sp.DESCRIPTION is NULL AND 
            (UPPER(smpl.lab_comment) LIKE '%PIKE MINNOW%'
            OR UPPER(smpl.field_comment) LIKE '%PIKE MINNOW%') THEN 1 ELSE 0 END +
            
            CASE  WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%WALLEYE%' 
                OR UPPER(smpl.field_comment) LIKE '%WALL EYE%') THEN 1 ELSE 0 END +
                
              CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%LAKE TROUT%'
                OR REGEXP_LIKE(upper(smpl.field_comment), '(^|[^AUB])LT') --'(?<![UB])LT')
                OR UPPER(smpl.lab_comment) LIKE '%LAKE TROUT%'
                OR REGEXP_LIKE(upper(smpl.lab_comment), '(^|[^AUB])LT')) --'(?<![UB])LT')
                THEN 1 ELSE 0 END +
                
              CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%SNAKE PRICKLEBACK%'
                OR UPPER(smpl.field_comment) LIKE '%SNAKE PICKLEBACK%') THEN 1 ELSE 0 END +  
              
              CASE WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%MOUNTAIN WHITEFISH%' 
                OR smpl.field_comment LIKE '%MW%'
                OR UPPER(smpl.field_comment) LIKE '%MNWF%') THEN 1 ELSE 0 END + 
              
              CASE WHEN sp.DESCRIPTION is NULL 
                AND (REGEXP_LIKE(upper(smpl.field_comment), '(^|[^R])BT') 
                OR UPPER(smpl.field_comment) LIKE '%BLTR%') THEN 1 ELSE 0 END + 
              
              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SHINER PERCH%' THEN 1 ELSE 0 END + 
              
              CASE WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(UPPER(smpl.field_comment), '(^|[^R ])PERCH') THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STAGHORN SCULPIN%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(UPPER(smpl.field_comment), '(^|[^H ])SCULPIN') THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%PACIFIC SANDAD%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SANDAB%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SANDDAB%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND smpl.field_comment LIKE '%CCG%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND smpl.field_comment LIKE '%CBA%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, 'CH[0-9]') THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, '\(CM\)') THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SOCKEYE%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STURGEON%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%CARP%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%RED SIDE SHINER%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SPINE STICKLE%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%BURBOT%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%LARGE SCALE SUCKER%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, '(^|[^N])WF') THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%BUTTERSOLE%' THEN 1 ELSE 0 END + 

              CASE WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STARRY FLOUNDER%' THEN 1 ELSE 0 END
                
                ) >= 2 THEN ''
              
            --dealing with data with Result Unit set to None
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%DOLLY%' 
                OR UPPER(smpl.field_comment) LIKE '%DOLLY%'
                OR smpl.field_comment LIKE '%DV%') THEN 'Salvelinus malma-COMMENT'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%RAINBOW%'
                OR UPPER(smpl.field_comment) LIKE '%RAINBOW%'
                OR UPPER(smpl.field_comment) LIKE '%RBW TROUT%'
                OR UPPER(smpl.field_comment) LIKE '%RBT%'
                OR smpl.field_comment LIKE '%RB %'
                OR UPPER(smpl.field_comment) LIKE '%O.MYKI%') THEN 'Oncorhynchus mykiss'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.lab_comment) LIKE '%CUTTHROAT%'
                OR UPPER(smpl.field_comment) LIKE '%CUTTHROAT%'
                OR smpl.lab_comment LIKE '% CT%'
                OR smpl.field_comment LIKE '%WCT%'
                OR smpl.field_comment LIKE '%WTC%'
                OR smpl.field_comment LIKE '%CT%') THEN 'Oncorhynchus clarkii-COMMENT' 
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%LONG NOSE SUCKER%'
                OR UPPER(smpl.field_comment) LIKE '%LS SUCKER%') THEN 'Catostomus catostomus'
                
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.lab_comment) LIKE '%PIKE MINNOW%' THEN 'Ptychocheilus' 
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%WALLEYE%' 
                OR UPPER(smpl.field_comment) LIKE '%WALL EYE%') THEN 'Sander vitreus'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%LAKE TROUT%'
                OR REGEXP_LIKE(upper(smpl.field_comment), '(^|[^AUB])LT') --'(?<![UB])LT')
                OR UPPER(smpl.lab_comment) LIKE '%LAKE TROUT%'
                OR REGEXP_LIKE(upper(smpl.lab_comment), '(^|[^AUB])LT')) --'(?<![UB])LT') 
                THEN 'Salvelinus namaycush'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%SNAKE PRICKLEBACK%'
                OR UPPER(smpl.field_comment) LIKE '%SNAKE PICKLEBACK%') THEN 'Lumpenus sagitta'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (UPPER(smpl.field_comment) LIKE '%MOUNTAIN WHITEFISH%' 
                OR smpl.field_comment LIKE '%MW%'
                OR UPPER(smpl.field_comment) LIKE '%MNWF%') THEN 'Prosopium williamsoni'
                
                WHEN sp.DESCRIPTION is NULL 
                AND (REGEXP_LIKE(smpl.field_comment, '(^|[^R])BT') 
                OR UPPER(smpl.field_comment) LIKE '%BLTR%') THEN 'Salvelinus confluentus'                              
 
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SHINER PERCH%' THEN 'Cymatogaster aggregata'
                WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(UPPER(smpl.field_comment), '(^|[^R ])PERCH') THEN 'Percidae'
                
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STAGHORN SCULPIN%' THEN 'Leptocottus armatus'
                WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(UPPER(smpl.field_comment), '(^|[^H ])SCULPIN') THEN 'Cottidae'       

                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%PACIFIC SANDAD%' THEN 'Citharichthys sordidus'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SANDAB%' THEN 'Citharichthys'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SANDDAB%' THEN 'Citharichthys'

                WHEN sp.DESCRIPTION is NULL AND smpl.field_comment LIKE '%CCG%' THEN 'Cottus cognatus'               
                WHEN sp.DESCRIPTION is NULL AND smpl.field_comment LIKE '%CBA%' THEN 'Cottus bairdii'               

                WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, 'CH[0-9]') THEN 'Oncorhynchus tshawytscha'               
                WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, '\(CM\)') THEN 'Oncorhynchus keta'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SOCKEYE%' THEN 'Oncorhynchus nerka'            
 
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STURGEON%' THEN 'Acipenseridae'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%CARP%' THEN 'Cyprinus carpio'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%RED SIDE SHINER%' THEN 'Richardsonius balteatus'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%SPINE STICKLE%' THEN 'Gasterosteus aculeatus'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%BURBOT%' THEN 'Lota lota'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%LARGE SCALE SUCKER%' THEN 'Catostomus macrocheilus'
                WHEN sp.DESCRIPTION is NULL AND REGEXP_LIKE(smpl.field_comment, '(^|[^N])WF') THEN 'Salmonidae'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%BUTTERSOLE%' THEN 'Isopsetta isolepis'
                WHEN sp.DESCRIPTION is NULL AND UPPER(smpl.field_comment) LIKE '%STARRY FLOUNDER%' THEN 'Platichthys stellatus'
                
            ELSE NULL
        END AS "Fish Species", 
        
    CASE
        WHEN ls.description IS NOT NULL THEN ls.description
        -- If 2 or more keywords match, return blank
        WHEN (
            CASE WHEN ls.description IS NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%FRY%'
            OR UPPER(smpl.field_comment) LIKE '%FRY%') THEN 1 ELSE 0 END +
            
            CASE WHEN ls.description IS NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%JUVENILE%' 
            OR UPPER(smpl.field_comment) LIKE '%JUVENILE%') THEN 1 ELSE 0 END +
            
            CASE WHEN ls.description IS NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%ADULT%' 
            OR UPPER(smpl.field_comment) LIKE '%ADULT%') THEN 1 ELSE 0 END +
            
            CASE WHEN ls.description IS NULL
            AND (UPPER(smpl.lab_comment) LIKE '%MATURE%' 
            OR UPPER(smpl.field_comment) LIKE '%MATURE%') THEN 1 ELSE 0 END +
            
            CASE WHEN ls.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%EGG%'
            OR UPPER(smpl.field_comment) LIKE '%EGG%') THEN 1 ELSE 0 END
            
            ) >= 2 THEN ''

        -- Otherwise assign the single match (if any)
        WHEN ls.description IS NULL 
        AND (UPPER(smpl.lab_comment) LIKE '%FRY%'
        OR UPPER(smpl.field_comment) LIKE '%FRY%') THEN 'Fry'
        
        WHEN ls.description IS NULL
        AND (UPPER(smpl.lab_comment) LIKE '%JUVENILE%' 
        OR UPPER(smpl.field_comment) LIKE '%JUVENILE%') THEN 'Juvenile'
        
        WHEN ls.description IS NULL
        AND (UPPER(smpl.lab_comment) LIKE '%ADULT%' 
        OR UPPER(smpl.field_comment) LIKE '%ADULT%') THEN 'Adult'
        
        WHEN ls.description IS NULL
        AND (UPPER(smpl.lab_comment) LIKE '%MATURE%' 
        OR UPPER(smpl.field_comment) LIKE '%MATURE%') THEN 'Mature'
        
        WHEN ls.description IS NULL 
        AND (UPPER(smpl.lab_comment) LIKE '%EGG%'
        OR UPPER(smpl.field_comment) LIKE '%EGG%') THEN 'Egg'
        
        ELSE NULL
    END AS "Fish Life Stage",

        CASE 
            WHEN es.description is NOT NULL THEN es.description
            
            WHEN (
            CASE WHEN es.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%FEMALE%'
            OR UPPER(smpl.field_comment) LIKE '%FEMALE%') THEN 1 ELSE 0 END +
            
            CASE WHEN es.description is NULL 
            AND (REGEXP_LIKE(smpl.lab_comment, '(^|[^E])MALE')
            OR REGEXP_LIKE(smpl.field_comment, '(^|[^E])MALE')) THEN 1 ELSE 0 END
            
            ) >=2 THEN ''
            
            WHEN es.description is NULL 
            AND (UPPER(smpl.lab_comment) LIKE '%FEMALE%'
            OR UPPER(smpl.field_comment) LIKE '%FEMALE%') THEN 'Female'
            
            WHEN es.description is NULL
            AND (REGEXP_LIKE(smpl.lab_comment, '(^|[^E])MALE')
            OR REGEXP_LIKE(smpl.field_comment, '(^|[^E])MALE')) THEN 'Male'
            
           ELSE NULL
        END AS "Fish Sex",  
        
        CASE 
          WHEN SIZE_FROM IS NOT NULL AND SIZE_TO IS NOT NULL THEN (SIZE_FROM + SIZE_TO) / 2
          WHEN SIZE_FROM IS NOT NULL THEN SIZE_FROM 
          WHEN SIZE_TO IS NOT NULL THEN SIZE_TO 
          
          -- Check if there are multiple matches (>=2), if so return NULL
        WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL AND (
        CASE WHEN smpl.lab_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') 
        THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +
        
        CASE WHEN smpl.field_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') 
        THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +

        CASE WHEN smpl.lab_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') 
        THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END +
        
        CASE WHEN smpl.field_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') 
        THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END
    ) >= 2 THEN NULL
    
          
          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL THEN 
        CASE
            -- Check if it's a range like "25-36 CM"
            WHEN REGEXP_LIKE(
                REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(CM|MM)',
                'i'
            ) THEN
                -- Extract both numbers and average them
                (TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(CM|MM)',
                            1, 1, 'i'
                        ),
                        '^(\d{1,6}(\.\d*)?).*$',
                        '\1',
                        1, 1, 'i'
                    )
                ) + 
                TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(CM|MM)',
                            1, 1, 'i'
                        ),
                        '^.*-\s*(\d{1,6}(\.\d*)?)\s*(CM|MM).*$',
                        '\1',
                        1, 1, 'i'
                    )
                )) / 2
            
            -- Single value like "52.0 CM"
            ELSE
                TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*(CM|MM)',
                            1, 1, 'i'
                        ),
                        '\s*(CM|MM).*$',
                        '',
                        1, 1, 'i'
                    )
                )
        END
                      
--          WHEN (
--                   CASE WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL AND smpl.lab_comment IS NOT NULL
--                    AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +
--                    
--                    CASE WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL AND smpl.field_comment IS NOT NULL
--                    AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +
--            
--                    CASE WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL AND smpl.lab_comment IS NOT NULL
--                    AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END +
--                    
--                    CASE WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL AND smpl.field_comment IS NOT NULL
--                    AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END            
--            
--          ) >=2 THEN NULL
--            
--          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL
--                 AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') THEN TO_NUMBER(REPLACE(
--                     REGEXP_SUBSTR(smpl.lab_comment, '[-+]?\d{1,6}(\.\d*)?(?=\s*CM)', 1, 1, 'i'), 'TM9'), '9999D9', 'NLS_NUMERIC_CHARACTERS=.,')
--            
--          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL
--                 AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') THEN TO_NUMBER(REPLACE(
--                     REGEXP_SUBSTR(smpl.field_comment, '[-+]?\d{1,6}(\.\d*)?(?=\s*CM)', 1, 1, 'i'), 'TM9'), '9999D9', 'NLS_NUMERIC_CHARACTERS=.,')
--
--          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL
--                 AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') THEN TO_NUMBER(REPLACE(
--                     REGEXP_SUBSTR(smpl.lab_comment, '[-+]?\d{1,6}(\.\d*)?(?=\s*MM)', 1, 1, 'i'), 'TM9'), '9999D9', 'NLS_NUMERIC_CHARACTERS=.,')
--
--          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL
--                 AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') THEN TO_NUMBER(REPLACE(
--                     REGEXP_SUBSTR(smpl.field_comment, '[-+]?\d{1,6}(\.\d*)?(?=\s*MM)', 1, 1, 'i'), 'TM9'), '9999D9', 'NLS_NUMERIC_CHARACTERS=.,')        
            
            ELSE NULL 
        END AS "Fish Fork Length",
                  
        CASE 
          WHEN WEIGHT_FROM IS NOT NULL AND WEIGHT_TO IS NOT NULL THEN (WEIGHT_FROM + WEIGHT_TO) / 2
            WHEN WEIGHT_FROM IS NOT NULL THEN WEIGHT_FROM 
              WHEN WEIGHT_TO IS NOT NULL THEN WEIGHT_TO 
              
                  -- Check if there are multiple matches (>=2), if so return NULL
        WHEN WEIGHT_FROM IS NULL AND WEIGHT_TO IS NULL AND (
        CASE WHEN smpl.lab_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*g','i') 
        THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*g', 1,'i') ELSE 0 END +
        
        CASE WHEN smpl.field_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*g','i') 
        THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*g', 1,'i') ELSE 0 END +

        CASE WHEN smpl.lab_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb','i') 
        THEN REGEXP_COUNT(smpl.lab_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb', 1,'i') ELSE 0 END +
        
        CASE WHEN smpl.field_comment IS NOT NULL
        AND REGEXP_LIKE(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb','i') 
        THEN REGEXP_COUNT(smpl.field_comment,'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb', 1,'i') ELSE 0 END
    ) >= 2 THEN NULL
    
          
          WHEN SIZE_FROM IS NULL AND SIZE_TO IS NULL THEN 
        CASE
            -- Check if it's a range like "25-36 CM"
            WHEN REGEXP_LIKE(
                REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(g|lb)',
                'i'
            ) THEN
                -- Extract both numbers and average them
                (TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(g|lb)',
                            1, 1, 'i'
                        ),
                        '^(\d{1,6}(\.\d*)?).*$',
                        '\1',
                        1, 1, 'i'
                    )
                ) + 
                TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*-\s*\d{1,6}(\.\d*)?\s*(g|lb)',
                            1, 1, 'i'
                        ),
                        '^.*-\s*(\d{1,6}(\.\d*)?)\s*(g|lb).*$',
                        '\1',
                        1, 1, 'i'
                    )
                )) / 2
            
            -- Single value like "52.0 CM"
            ELSE
                TO_NUMBER(
                    REGEXP_REPLACE(
                        REGEXP_SUBSTR(
                            REPLACE(NVL(smpl.field_comment, smpl.lab_comment), CHR(160), ' '),
                            '\d{1,6}(\.\d*)?\s*(g|lb)',
                            1, 1, 'i'
                        ),
                        '\s*(g|lb).*$',
                        '',
                        1, 1, 'i'
                    )
                )
        END
              
                ELSE NULL 
        END AS "Fish Weight",
                  
        smpl.SIZE_FROM,
        smpl.SIZE_TO,
        smpl.WEIGHT_FROM,
        smpl.WEIGHT_TO,
        smpl.life_stg_cd--,
--        CASE 
--            WHEN ls.description is NULL AND lfs.description is NOT NULL THEN lfs.description
--            WHEN ls.description is NOT NULL AND lfs.description is NULL THEN ls.description
--            WHEN ls.description is NOT NULL AND lfs.description is NOT NULL and ls.description = lfs.description THEN ls.description
--            ELSE ''
--        END AS "Biological Life Stage"
FROM
        ems_samples smpl
        LEFT JOIN ems_results result ON smpl.id = result.smpl_id
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
        LEFT JOIN ems_parameters param ON result.parm_cd = param.code
        LEFT JOIN ems_anal_methods am ON result.anal_method_cd = am.code
        LEFT JOIN ems_parm_dicts d on d.parm_cd = result.parm_cd
                AND d.anal_method_cd = result.anal_method_cd
        LEFT JOIN ems.AQS_UNITS_TEMP aqs_units ON aqs_units.EMS_CODE = d.meas_unit_cd
        LEFT JOIN ems.AQS_UNITS_TEMP aqs_project ON to_char(aqs_project.EMS_CODE) = to_char(smpl.requisition_id)
        LEFT JOIN ems_measurment_units mu ON mu.code = d.meas_unit_cd
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
        -- picking these filters up from RG (SQL code: ems_sample_results_FISH_TISSUE FIELD_SURVEY select) 
        -- and JK (R code: fish_code_sept05) 
        --AND smpl.species_cd is not null
)
select DISTINCT
    "Observation ID",
        "Ministry Contact",
        "Sampling Agency",
        'Fish Tissue' as "Project",
        "Work Order Number",
        "Location ID",
        "Field Visit Start Time", -- required
        "Field Visit End Time",
        "Field Visit Participants",
        --"Field Visit Comments",
        CASE 
            WHEN UPPER("Ministry Contact") IN ('EMS', 'CHRIS SWAN') AND "Medium" = 'Water - Unknown' THEN 'These results may be reporting concentration of a digest extract rather than the tissue itself or represent a water quality result. The metadata is stored by the Ministry Program responsible for the samples' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211024', '4211025', '4211026', '4211027', '4211028') THEN 'Creek 661-01' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211034', '4211035', '4211036', '4211037', '4211038') THEN 'Creek 661-05' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211014', '4211015', '4211016', '4211017', '4211018') THEN 'Creek 661-10' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211019', '4211020', '4211021', '4211022', '4211023') THEN 'Davidson Creek DC-05' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211010', '4211039', '4211040', '4211041', '4211042') THEN 'Davidson Creek DC-15' || ' ' || "Activity Comments"
            WHEN UPPER("Ministry Contact") = 'ERIN RAINEY' AND 
            "Activity Name" IN ('4211009', '4211011', '4211012', '4211013', '4211043') THEN 'Fawnie Creek FC-01' || ' ' || "Activity Comments"
            WHEN UPPER(TRIM("Ministry Contact")) LIKE '%EMS%'
         OR UPPER(TRIM("Ministry Contact")) IN (
                'BOB GRACE',
                'BRUCE CARMICHAEL',
                'CHRIS SWAN',
                'YVONNE PREDIGER',
                'CHRISTINA BALL',
                'DAVE SUTHERLAND',
                'DEB EPPS',
                'GREG TAMBLYN',
                'JAMES JACKLIN',
                'JASON MCCOY',
                'JODY FRENETTE',
                'JOHN DENISEGER',
                'JOHN LOVE',
                'JOLENE RAGGETT',
                'LES MCDONALD',
                'PAUL SASO',
                'ROBYN ROOME',
                'ROD SHEAD',
                'ROSIE BARLAK',
                'VIC JENSEN',
                'VIRGINIA STANFORD'
            )
    THEN
       'Metadata (taxonomy and physical measurements) for these results are currently stored by the Ministry program responsible for the samples. Field notes and specimen metadata uploads are pending.' || ' ' || "Activity Comments"
            ELSE "Activity Comments" 
        END AS "Field Visit Comments",
--        CASE
--    WHEN UPPER(TRIM("Ministry Contact")) LIKE '%EMS%'
--         OR UPPER(TRIM("Ministry Contact")) IN (
--                'BOB GRACE',
--                'BRUCE CARMICHAEL',
--                'CHRIS SWAN',
--                'YVONNE PREDIGER',
--                'CHRISTINA BALL',
--                'DAVE SUTHERLAND',
--                'DEB EPPS',
--                'GREG TAMBLYN',
--                'JAMES JACKLIN',
--                'JASON MCCOY',
--                'JODY FRENETTE',
--                'JOHN DENISEGER',
--                'JOHN LOVE',
--                'JOLENE RAGGETT',
--                'LES MCDONALD',
--                'PAUL SASO',
--                'ROBYN ROOME',
--                'ROD SHEAD',
--                'ROSIE BARLAK',
--                'VIC JENSEN',
--                'VIRGINIA STANFORD'
--            )
--    THEN
--       'Metadata (taxonomy and physical measurements) for these results are currently stored by the Ministry program responsible for the samples. Field notes and specimen metadata uploads are pending.'
--    ELSE
--        ''
--    END AS "Activity Comments",
--        CASE 
--            WHEN "Ministry Contact" IN ('%EMS%', 'Bob Grace', 'Bruce Carmichael', 'Chris Swan', 'Yvonne Prediger', 'Christina Ball', 'Dave Sutherland', 'Deb Epps', 'Greg Tamblyn', 'James Jacklin', 'Jason McCoy', 
--                        'Jody Frenette', 'John Deniseger', 'John Love', 'Jolene Raggett', 'Les McDonald', 'Paul Saso', 'Robyn Roome', 'ROD SHEAD', 'Rosie Barlak', 'Vic Jensen', 'Virginia Stanford') 
--                THEN "Activity Comments" || ' ' || 'Metadata (taxonomy and physical measurements) for these results are currently stored by the Ministry program responsible for the samples. Field notes and specimen metadata uploads are pending.' 
--            ELSE "Activity Comments"
--        END AS "Activity Comments",
        "Activity ID" as "Activity Comments",
        'FALSE' as "Field Filtered",
        '' as "Field Filtered Comment",
        'ICE' as "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        '' as "Sampling Context Tag",
        "Collection Method",
--Adding WO by WO changes recommended by Lisa for importing high quality data
        CASE
            WHEN "Work Order Number" = 'VA23C1874' THEN 'Animal - Benthic Invertebrates'
            WHEN UPPER("Ministry Contact") = 'ADRIANNA JOHNSON' THEN 'Plant - Periphyton' 
            WHEN UPPER("Ministry Contact") IN ('EMS', 'CHRIS SWAN') AND "Medium" = 'Water - Unknown' THEN 'Animal - Fish'
            ELSE "Medium"
        END AS "Medium",
--        "Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
        "Observed Date Time End",
        "Observed Property ID",
        "Result Value",
        CASE WHEN "Data Classification" = 'LAB' AND "Method Detection Limit" is NULL THEN -99 ELSE "Method Detection Limit" END AS "Method Detection Limit",
--        "Method Detection Limit", 
        '' as "Method Reporting Limit",
        "Result Unit",
        CASE 
         WHEN "Medium" = 'Animal - Fish' AND "Result Unit" is NULL THEN 'PRESENT'
         ELSE "Detection Condition"
        END AS "Detection Condition",
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
--        "Analyzing Agency",
--        "Analysis Method",    
        "Analyzed Date Time", -- add date/time mask
        'Preliminary' as "Result Status",
        'Ungraded' as "Result Grade",
        '' as "Activity ID",
        CASE 
            WHEN UPPER("Medium") LIKE '%WATER%' THEN to_char("Activity Name")||'F'
            WHEN UPPER("Medium") LIKE '%PLANT%' THEN to_char("Activity Name")||'F'
            WHEN UPPER("Medium") LIKE '%BENTHIC%' THEN to_char("Activity Name")||'F'
            ELSE to_char("Activity Name")
        END AS "Activity Name",
        --"Activity Name",
        CASE 
            WHEN "Medium" = 'Animal - Fish' AND "Tissue Type" is NULL THEN 'Unknown'
            ELSE "Tissue Type"
        END AS "Tissue Type",
        "Lab Arrival Temperature",
    'Fish' ||
    CASE 
        WHEN cnt > 1 AND rn >= 1 THEN ' ' || TO_CHAR(rn)
        ELSE ''
    END ||
    CASE 
        WHEN UPPER("Tissue Type") IS NOT NULL AND UPPER("Tissue Type") <> 'WHOLE BODY' THEN ' - ' || "Tissue Type"
        ELSE ''
    END AS "Specimen Name",
        '' as "Lab Quality Flag",
        "Lab Arrival Date and Time",
        '' as "Lab Prepared DateTime",
    'Fish' ||
    CASE 
        WHEN cnt > 1 AND rn >= 1 THEN ' ' || TO_CHAR(rn)
        ELSE ''
    END ||
    CASE 
        WHEN UPPER("Tissue Type") IS NOT NULL AND UPPER("Tissue Type") <> 'WHOLE BODY' THEN ' - ' || "Tissue Type"
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
--        "Lab Comment",
        "Lab Batch ID",
        CASE 
            WHEN "QC Type" is NULL THEN 'REGULAR'
            ELSE "QC Type"
        END AS "QC Type",
--        '' as "QC Type",
        '' as "QC Source Activity Name",
        --'' as "QC Source Sample ID",
        '' as "Composite Stat",
        '' AS "Biological Life Stage"
FROM (
select DISTINCT
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
        CASE 
            WHEN "Activity Name" IN ('2240153', '2240156') THEN "Activity Comments" || ' ' || 'FISH ID # KO-38'
            WHEN "Activity Name" IN ('2240154', '2240155') THEN "Activity Comments" || ' ' || 'FISH ID # KO-40'
            ELSE "Activity Comments"
        END AS "Activity Comments",
        "Field Filtered",
        "Field Filtered Comment",
        "Field Preservative",
        "Field Device ID",-- leave as blank
        "Field Device Type",
        "Sampling Context Tag",
        "Collection Method",
        CASE
            --WHEN UPPER("Ministry Contact") IN ('EMS', 'CHRIS SWAN') THEN "Medium"
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
            OR (LOWER("Observed Property ID") LIKE '%color%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            OR (LOWER("Observed Property ID") LIKE '%specific conductance%' AND LOWER("Observed Property ID") NOT LIKE '%mass%')
            THEN 'Water - Unknown' 
            WHEN LOWER("Observed Property ID") LIKE '%chlorophyll%' THEN 'Plant - Unknown'
            ELSE "Medium"
        END AS "Medium",
--       "Medium",
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
        CASE 
            WHEN "Activity Name" IN ('2240153', '2240155', '231415', '2533420', '2533421', '2533422', '2533423', '2533424') THEN 'Muscle'
            WHEN "Activity Name" IN ('2240154', '2240156') THEN 'Ovary'
            WHEN "Activity Name" IN ('231413') THEN 'Liver'
            ELSE "Tissue Type"
        END AS "Tissue Type",
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
        "Observed Property ID",
        "Result Value",
             COUNT(*) OVER (PARTITION BY "Location ID", "Field Visit Start Time", "Activity Name", "Observed Property ID", "Lab Batch ID") AS cnt,
        ROW_NUMBER() OVER (PARTITION BY "Location ID", "Field Visit Start Time", "Activity Name", "Observed Property ID" ORDER BY "Lab Batch ID") AS rn
FROM (
select DISTINCT
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
        CASE 
            WHEN "Observed Property ID" = 'Taxonomy' THEN ''
            ELSE "Field Device Type"
        END AS "Field Device Type",
--        "Field Device Type",
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
            WHEN LOWER("Observed Property ID") LIKE '%chlorophyll%' THEN 'Plant - Unknown'
            ELSE "Medium"
        END AS "Medium",
        --"Medium",
        "Depth Upper",
        "Depth Lower",
        "Depth Unit",
        "Observed DateTime",
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
        "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
        "Tissue Type",
        "Specimen Name",
        "Lab Arrival Temperature",
        CASE 
            WHEN "Data Classification" IN ('FIELD_RESULT', 'ACTIVITY_RESULT') THEN '' 
            ELSE "Lab Quality Flag"
        END AS "Lab Quality Flag",
--        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Observed Property ID",
        "Result Value"
FROM (
select DISTINCT
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
        --"Analysis Method",
        case when "Data Classification" IN ('FIELD_RESULT', 'FIELD_SURVEY') then null else "Analysis Method" end as "Analysis Method",
        "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        case when "Data Classification" = 'FIELD_RESULT' then null else "Activity Name" end as "Activity Name",
        --"Activity Name",
        "Specimen Name",
        "Tissue Type",
        "Lab Arrival Temperature",
        '' as "Lab Quality Flag",
--        CASE 
--            WHEN "Observed Property ID" = 'Taxonomy' AND "Lab Quality Flag" LIKE '%ICP%' THEN ''
--            ELSE "Lab Quality Flag"
--        END AS "Lab Quality Flag",
--        "Lab Quality Flag",
        "Lab Arrival Date and Time",
        "Lab Prepared DateTime",
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Observed Property ID",
        "Result Value"
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
          WHEN "Observed Property ID 2" = 'Taxonomy' THEN ''
          WHEN "Observed Property ID 2" = 'Biological Life Stage (cat.)' THEN ''
          WHEN "Observed Property ID 2" = 'Biological Sex (cat.)' THEN ''
          WHEN "Observed Property ID 2" = 'Fish Fork Length (len.)' THEN "Fish Fork Length (len.).Unit"
          WHEN "Observed Property ID 2" = 'Fish Weight (mass)' THEN "Fish Weight (mass).Unit"
          ELSE "Result Unit"            
        END AS "Result Unit",
        "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        "Fraction",
        'FIELD_SURVEY' AS "Data Classification",
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
        "Lab Sample ID",
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Composite Stat",
        "Observed Property ID 2" as "Observed Property ID",
        "VALUE" as "Result Value"
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
        --"Observed Property ID",-- based on the analytical method and parameter code and unit
        --"Result Value",
        TO_NUMBER('') as "Method Detection Limit", 
        TO_NUMBER('') as "Method Reporting Limit",
        "Result Unit",
        '' AS "Detection Condition",
        "Limit Type",-- doesn't exist in ems  
        '' as "Fraction",
        "Data Classification",
        "Source of Rounded Value",
        "Rounded Value",
        "Rounding Specification",
        '' AS "Analyzing Agency",
        "Analysis Method",
        --'' AS "Analysis Method",
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
        "Lab Dilution Factor",
        "Lab Comment",
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Fish Fork Length (len.).Unit",            
        "Fish Weight (mass).Unit",
        "Composite Stat",
        "Observed Property ID 2",
        value
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
        "Observed DateTime",
--        TO_CHAR (
--            TO_TIMESTAMP (
--                SUBSTR ("Observed DateTime", 1, 19),
--                'YYYY-MM-DD"T"HH24:MI:SS'
--            ) + NUMTODSINTERVAL (duplicate_row_number - 1, 'SECOND'),
--            'YYYY-MM-DD"T"HH24:MI:SS'
--        ) || '-08:00' AS "Observed DateTime",
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
        "Analysis Method",
--      case when "Data Classification" = 'FIELD_RESULT' then null else "Analysis Method" end as "Analysis Method",
        '' as "Analyzed Date Time", -- add date/time mask
        "Result Status",
        "Result Grade",
        "Activity ID",
        "Activity Name",
--        case when "Data Classification" = 'FIELD_RESULT' then null else "Activity Name" end as "Activity Name",
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
        CASE 
            WHEN "Fish Species" = 'Oncorhynchus clarkii-COMMENT' THEN 'Oncorhynchus clarkii'
            WHEN "Fish Species" = 'Salvelinus malma-COMMENT' THEN 'Salvelinus malma'
            ELSE "Fish Species" 
        END as "Taxonomy",
        "Fish Life Stage" as "Biological Life Stage (cat.)",
        "Fish Sex" as "Biological Sex (cat.)",
        to_char("Fish Fork Length") as "Fish Fork Length (len.).Result",
        CASE 
          WHEN "Fish Species" IN ('Oncorhynchus clarkii', 'Cottus', 'Salvelinus malma') THEN 'mm'
            WHEN "Fish Species" IN ('Oncorhynchus kisutch', 'Oncorhynchus nerka') THEN 'cm'
            --dealing with data with Result Unit set to None
                --WHEN "Fish Species" IN ('Catostomus catostomus', 'Catostomus macrocheilus') THEN 'mm'
                    --WHEN "Fish Species" IN 'Prosopium williamsoni' THEN 'cm'
                    --no need to built checks for the following two cases and lab and field comments are also empty
                        WHEN "Lab Comment" = 'Pike Minnow' THEN 'mm'
                            WHEN "Collection Method" = 'Minnow Trapping' THEN 'mm'
                              WHEN -- Check if there are multiple matches (>=2), if so return NULL
                              (
        CASE 
            WHEN "Lab Comment" IS NOT NULL
        AND REGEXP_LIKE("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') 
        THEN REGEXP_COUNT("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +
        
        CASE WHEN "Field Visit Comments" IS NOT NULL
        AND REGEXP_LIKE("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM','i') 
        THEN REGEXP_COUNT("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*CM', 1,'i') ELSE 0 END +

        CASE WHEN "Lab Comment" IS NOT NULL
        AND REGEXP_LIKE("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') 
        THEN REGEXP_COUNT("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END +
        
        CASE WHEN "Field Visit Comments" IS NOT NULL
        AND REGEXP_LIKE("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM','i') 
        THEN REGEXP_COUNT("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*MM', 1,'i') ELSE 0 END
    ) >= 2 THEN NULL
    
    ELSE
        lower(
            REGEXP_REPLACE(
                REGEXP_SUBSTR(
                    REPLACE(NVL("Field Visit Comments", "Lab Comment"), CHR(160), ' '),
                    '\d{1,6}(\.\d*)?\s*(CM|MM)',
                    1, 1, 'i'
                ),
                '.*\d{1,6}(\.\d*)?\s*',
                '',
                1, 1, 'i'
            )
        )
        END AS "Fish Fork Length (len.).Unit",            
        to_char("Fish Weight") as "Fish Weight (mass).Result",
        CASE 
          WHEN "Fish Species" IN ('Oncorhynchus clarkii', 'Cottus', 'Salvelinus malma') THEN 'g'
            WHEN "Fish Species" IN ('Oncorhynchus kisutch', 'Oncorhynchus nerka') THEN 'kg'
                WHEN "Fish Species" IN ('Catostomus catostomus', 'Catostomus macrocheilus') THEN 'g'
                    WHEN "Lab Comment" = 'Pike Minnow'  THEN 'g'
                        WHEN "Collection Method" = 'Minnow Trapping' THEN 'g'
                        
               WHEN -- Check if there are multiple matches (>=2), if so return NULL
                              (
        CASE 
            WHEN "Lab Comment" IS NOT NULL
        AND REGEXP_LIKE("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*g','i') 
        THEN REGEXP_COUNT("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*g', 1,'i') ELSE 0 END +
        
        CASE WHEN "Field Visit Comments" IS NOT NULL
        AND REGEXP_LIKE("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*g','i') 
        THEN REGEXP_COUNT("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*g', 1,'i') ELSE 0 END +

        CASE WHEN "Lab Comment" IS NOT NULL
        AND REGEXP_LIKE("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb','i') 
        THEN REGEXP_COUNT("Lab Comment",'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb', 1,'i') ELSE 0 END +
        
        CASE WHEN "Field Visit Comments" IS NOT NULL
        AND REGEXP_LIKE("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb','i') 
        THEN REGEXP_COUNT("Field Visit Comments",'(^|[^0-9])\d{1,6}(\.\d*)?\s*lb', 1,'i') ELSE 0 END
    ) >= 2 THEN NULL
    
    ELSE
        lower(
            REGEXP_REPLACE(
                REGEXP_SUBSTR(
                    REPLACE(NVL("Field Visit Comments", "Lab Comment"), CHR(160), ' '),
                    '\d{1,6}(\.\d*)?\s*(g|lb)',
                    1, 1, 'i'
                ),
                '.*\d{1,6}(\.\d*)?\s*',
                '',
                1, 1, 'i'
            )
        )
        END AS "Fish Weight (mass).Unit",
        "Composite Stat"
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
        "Activity Comments" as "Field Visit Comments",
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
        "Lab Batch ID",
        "QC Type",
        "QC Source Activity Name",
        "Fish Species",
        "Fish Life Stage",
        "Fish Sex",
        "Fish Fork Length",
        "Fish Weight",
        "Composite Stat"--,
--        ROW_NUMBER() OVER (
--            PARTITION BY 
--                "Location ID", 
--                "Field Visit Start Time", 
--                "Medium", 
--                "Depth Upper", 
--                CASE 
--                    WHEN "Data Classification" IN ('FIELD_RESULT') THEN ''
--                    ELSE COALESCE(to_char("Activity Name"), '')
--                END, 
--                COALESCE("Specimen Name", ''),
--                "Data Classification", 
--                CASE 
--                    WHEN "Data Classification" IN ('FIELD_RESULT', 'VERTICAL_PROFILE') THEN null
--                    ELSE "QC Type"
--                END, 
--                "Observed Property ID"
--            ORDER BY TO_TIMESTAMP(SUBSTR("Observed DateTime", 1, 19), 'YYYY-MM-DD"T"HH24:MI:SS')
--        ) AS duplicate_row_number 
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
from (
-- fish data - chemistry data
SELECT DISTINCT
        ''  as "Observation ID",
        core."Ministry Contact",
        core."Sampling Agency",
        core."Project",
        core."Work Order Number",
        core."Location ID",
        core."Field Visit Start Time", -- required
        core."Field Visit End Time",
        core."Field Visit Participants",
        '' as "Field Visit Comments",
        core."Activity Comments",
        core."Field Filtered",
        core."Field Filtered Comment",
        core."Field Preservative",
        NULL AS "Field Device ID",-- leave as blank
        ed.Device_Type as "Field Device Type",
        core."Sampling Context Tag",
        core."Collection Method",
        core."Medium",
        core."Depth Upper",
        core."Depth Lower",
        core."Depth Unit",
        core."Observed DateTime",
        core."Observed Date Time End",
        ed.NewNameID AS "Observed Property ID",-- based on the analytical method and parameter code and unit
        core."Result Value",
        --core."Method Detection Limit" as "UNCONVERTED_MDL",-- the unit may not be accurate.  Conversion may be needed.  This is the lab based limit.  If the result from the lab is missing, we can get from the analytical methods table
        --unit_conversion.source_unit_id,
        --unit_conversion.target_unit_id,
        --unit_conversion.conversion_factor,
        CASE
            WHEN core."Method Detection Limit" is null THEN core."Method Detection Limit Source 2"
            WHEN core."MDL Unit" <> core."Result Unit" THEN
                core."Method Detection Limit" / unit_conversion.conversion_factor
                --unit_conversion.conversion_factor
        ELSE core."Method Detection Limit" -- No conversion needed
        END AS "Method Detection Limit", 
        --core."Method Detection Limit" as "Method Detection Limit OG", -- debugging
        --unit_conversion.conversion_factor, -- debugging
        --core."MDL Unit", core."Result Unit", -- debugging
        core."Method Reporting Limit",
        core."Result Unit",
        core."Detection Condition",
        core."Limit Type",-- doesn't exist in ems  
        --ed.Fraction as "Fraction",
        CASE
			WHEN ed.Fraction is null then ''
			WHEN ed.Fraction = 'Extractable' then ''
			ELSE ed.Fraction 
		END AS "Fraction",
        ed.Classification as "Data Classification",
        core."Source of Rounded Value",
        core."Rounded Value",
        core."Rounding Specification",
        core."Analyzing Agency",
        core."Analysis Method",
        --logic changed to match the logic in water extracts
        --CASE
        --    WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') and core."Analyzed Date Time" is null then core."Observed DateTime"
        --    ELSE core."Analyzed Date Time"
        --END as "Analyzed Date Time",
        CASE 
            WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT')
                THEN COALESCE(
                    "Analyzed Date Time",
                    core."Observed DateTime"
                )
            ELSE core."Analyzed Date Time"
        END AS "Analyzed Date Time",
        core."Result Status",
        core."Result Grade",
        core."Activity ID",
        core."Activity Name",
        --logic changed to match the logic in water extracts
        core."Tissue Type",
        --COALESCE(core."Tissue Type", 'Unknown') AS "Tissue Type",
        --logic commented to match the logic in water extracts
        --core.tissue_typ_cd,
        core."Lab Arrival Temperature",
     /*   CASE 
            WHEN ed.Classification IN ('FIELD_RESULT', 'VERTICAL_PROFILE', 'FIELD_SURVEY') 
             THEN '' 
           -- WHEN duplicate_row_number > 1 THEN RTRIM(ed.OP_Group, '; ') || '-' || duplicate_row_number
             ELSE RTRIM(ed.OP_Group, '; ') 
        END AS "Specimen Name",*/
        '' as "Specimen Name",
        core."Lab Quality Flag",
        core."Lab Arrival Date and Time",
        core."Lab Prepared DateTime",
        core."Lab Sample ID",
        core."Lab Dilution Factor" as "Lab Dilution Factor",
        core."Lab Comment" as "Lab Comment",
        core."Lab Batch ID",
        core."QC Type",
        core."QC Source Activity Name",
        core."Fish Species",
        core."Fish Life Stage",
        core."Fish Sex",
        core."Fish Fork Length",
        core."Fish Weight",
        core."Composite Stat"--,-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes.
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
    core_data core
    left outer JOIN OBSERVED_PROPERTIES_FOR_ETL ed on core.parm_cd = ed.Parm_code
        and core."Analysis Method" = ed.Analysis_Method_Code
        and core."EMS Result Unit" = ed.Unit
left outer join ems.unit_conversions_temp unit_conversion on core.result_unit_code = unit_conversion.target_unit_id 
and core.mdl_unit_code = unit_conversion.source_unit_id      
where core."Medium" = 'Animal - Fish'
--AND core.result_unit_code is not null 
--and core.mdl_unit_code is not null 
--removing System Calculations
--AND core."Analysis Method" NOT IN ('CS00', 'CS01', 'CS02', 'CS03', 'CS14', 'CS15', 'CS30', 'CS34', 'CS44')
-- end fish data
 --AND ed.NewNameID is not null
    --and ((core."Result Value" is not null) or (core.result_text = '''C'''))
    --            order by core."Location ID" asc, core."Observed DateTime" asc
    ))  --where duplicate_row_number = 1 AND
    --WHERE "Observed Property ID" is not null
        order by "Location ID" asc, "Observed DateTime" asc )
UNPIVOT (
    value FOR "Observed Property ID 2" IN (
       "Taxonomy" AS 'Taxonomy',
        "Biological Life Stage (cat.)" AS 'Biological Life Stage (cat.)', 
        "Biological Sex (cat.)" AS 'Biological Sex (cat.)',
        "Fish Fork Length (len.).Result" as 'Fish Fork Length (len.)',
        "Fish Weight (mass).Result" as 'Fish Weight (mass)')
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
        '' as "Specimen Name",
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
        "Observed Property ID" AS  "Observed Property ID",-- based on the analytical method and parameter code and unit
        TO_CHAR("Result Value") AS "Result Value"      
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
        "Activity Comments" as "Field Visit Comments",
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
        "Fish Species",
        "Fish Life Stage",
        "Fish Sex",
        "Fish Fork Length",
        "Fish Weight",
        "Composite Stat",
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
        ) AS duplicate_row_number
        -- Debugging columns likely leading to duplicates,
        -- "DEBUGGING PARM_CD",
        -- "DEBUGGING ANALYSIS METHOD",
        -- "DEBUGGING_EMS_RESULT_UNIT"
from (
-- fish data - chemistry data
SELECT DISTINCT 
        ''  as "Observation ID",
        core."Ministry Contact",
        core."Sampling Agency",
        core."Project",
        core."Work Order Number",
        core."Location ID",
        core."Field Visit Start Time", -- required
        core."Field Visit End Time",
        core."Field Visit Participants",
        '' as "Field Visit Comments",
        core."Activity Comments",
        core."Field Filtered",
        core."Field Filtered Comment",
        core."Field Preservative",
        NULL AS "Field Device ID",-- leave as blank
        ed.Device_Type as "Field Device Type",
        core."Sampling Context Tag",
        core."Collection Method",
        core."Medium",
        core."Depth Upper",
        core."Depth Lower",
        core."Depth Unit",
        core."Observed DateTime",
        core."Observed Date Time End",
        ed.NewNameID AS "Observed Property ID",-- based on the analytical method and parameter code and unit
        core."Result Value",
        --core."Method Detection Limit" as "UNCONVERTED_MDL",-- the unit may not be accurate.  Conversion may be needed.  This is the lab based limit.  If the result from the lab is missing, we can get from the analytical methods table
        --unit_conversion.source_unit_id,
        --unit_conversion.target_unit_id,
        --unit_conversion.conversion_factor,
        CASE
            WHEN core."Method Detection Limit" is null THEN core."Method Detection Limit Source 2"
            WHEN core."MDL Unit" <> core."Result Unit" THEN
                core."Method Detection Limit" / unit_conversion.conversion_factor
                --unit_conversion.conversion_factor
        ELSE core."Method Detection Limit" -- No conversion needed
        END AS "Method Detection Limit", 
        --core."Method Detection Limit" as "Method Detection Limit OG", -- debugging
        --unit_conversion.conversion_factor, -- debugging
        --core."MDL Unit", core."Result Unit", -- debugging
        core."Method Reporting Limit",
        core."Result Unit",
        core."Detection Condition",
        core."Limit Type",-- doesn't exist in ems  
        ed.Fraction as "Fraction",
        ed.Classification as "Data Classification",
        core."Source of Rounded Value",
        core."Rounded Value",
        core."Rounding Specification",
        core."Analyzing Agency",
        core."Analysis Method",
        --logic changed to match the logic in water extracts
        --CASE
        --    WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') and core."Analyzed Date Time" is null then core."Observed DateTime"
        --    ELSE core."Analyzed Date Time"
        --END as "Analyzed Date Time",
        CASE 
            WHEN upper(ed.Classification) IN ('LAB', 'SURROGATE_RESULT') 
            and core."Analyzed Date Time" is null then core."Observed DateTime"
                --THEN COALESCE(
                --    "Analyzed Date Time",
                --    core."Observed DateTime"
                --)
            ELSE core."Analyzed Date Time"
        END AS "Analyzed Date Time",
        core."Result Status",
        core."Result Grade",
        core."Activity ID",
        core."Activity Name",
        --logic changed to match the logic in water extracts
        core."Tissue Type",
        --COALESCE(core."Tissue Type", 'Unknown') AS "Tissue Type",
        --logic commented to match the logic in water extracts
        --core.tissue_typ_cd,
        core."Lab Arrival Temperature",
       /* CASE 
        WHEN ed.Classification IN ('FIELD_RESULT', 'VERTICAL_PROFILE', 'FIELD_SURVEY') 
           THEN '' 
           -- WHEN duplicate_row_number > 1 THEN RTRIM(ed.OP_Group, '; ') || '-' || duplicate_row_number
           ELSE RTRIM(ed.OP_Group, '; ') 
        END AS "Specimen Name", */
        '' as "Specimen Name",
        core."Lab Quality Flag",
        core."Lab Arrival Date and Time",
        core."Lab Prepared DateTime",
        core."Lab Sample ID",
        core."Lab Dilution Factor" as "Lab Dilution Factor",
        core."Lab Comment" as "Lab Comment",
        --core."Result ID" as "Result ID",
        core."Lab Batch ID",
        core."QC Type",
        core."QC Source Activity Name",
        core."Fish Species",
        core."Fish Life Stage",
        core."Fish Sex",
        core."Fish Fork Length",
        core."Fish Weight",
        core."Composite Stat"--,-- ea on observation level in enmods.  "Minimum, mean, and average... not used for lakes, but will be required on other extracts).  This will be in the results table.  Blank for lakes.
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
    core_data core
    left outer JOIN OBSERVED_PROPERTIES_FOR_ETL ed on core.parm_cd = ed.Parm_code
        and core."Analysis Method" = ed.Analysis_Method_Code
        and core."EMS Result Unit" = ed.Unit
left outer join ems.unit_conversions_temp unit_conversion on core.result_unit_code = unit_conversion.target_unit_id 
and core.mdl_unit_code = unit_conversion.source_unit_id      
where core.result_unit_code is not null 
and core.mdl_unit_code is not null 
AND core."Medium" = 'Animal - Fish'
--removing System Calculations
AND core."Analysis Method" NOT IN ('CS00', 'CS01', 'CS02', 'CS03', 'CS14', 'CS15', 'CS30', 'CS34', 'CS44')
-- end fish data
 AND ed.NewNameID is not null
    and ((core."Result Value" is not null) or (core.result_text = '''C'''))
                order by core."Location ID" asc, core."Observed DateTime" asc
    ))  --where duplicate_row_number = 1 AND
    -- WHERE "Observed Property ID" is not null
    -- AND "Work Order Number" = '3845'
    -- AND "Location ID" = '410054'
    )
    WHERE "Observed Property ID" is not null
    AND ("Result Unit" IS NULL OR "Result Unit" <> 'None')
    )
    --WHERE "Work Order Number" = '50033616'
order by "Activity Name" asc, "Observed Property ID" asc, "Observed DateTime" asc
