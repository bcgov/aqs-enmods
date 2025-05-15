## Code Style Standards

Naming conventions based on the reference value (SNAKE CASE)

  1. For file names/sheets, we follow the format "Snake_Case" 
  2. For file names coming from EMS, we follow the format "EMS_<Data_Type>_Date"
  3. For variable names in coding pipelines, we follow:
	 1. the "snake_case" format if "case" is a version of the "snake"
	 2. the "snake.case" format if "case" is an element of the "snake"
	 3. Column names for tables also follow "snake_case" as dot notation can cause parsing issues in some environments
	 4. AQI uses "camelCase" so using formatting styles in 1 through 3 will also allow us to differentiate between variables created by BC Gov vs AQI
  
## Steps to configure an instance of EnMoDS - AQS

Starting from a clean environment with no configuration (see how to wipe an environment)...
  1. Run preprocessing steps
  2. Run configuration steps
     1. Support tables are here
     2. functions are here
  3. other steps 

## Steps to wipe an instance of EnMoDS - AQS
  1. Run delete * from all
