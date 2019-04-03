# DoLakesFeelTheBurn
Effects of fire on North American lake ecosystems #FireAndFury

McCullough, I. M., Cheruveli, K. S., Lapierre, J., Lottig, N., Moritz, M. A., Stachelek, J. and P. A. Soranno. Do lakes feel the burn? Ecological consequences of increasing exposure of lakes to fire in the continental US

Status: Revision under review at Global Change Biology (Apr 2019)

This manuscript:

1) documents increasing exposure of lakes to fire in the continental US
2) reviews past lake-fire research
3) synthesizes research from aquatic, terrestrial, landscape and fire ecology into a novel conceptual model for effects of fire on physical, chemical and biological properties of lakes
4) proposes research priorities for future lake-fire research

This repository:

ExportedData: contains saved data files and a metadata document for all data files

  lake_fire_history: contains lake-specific fire history files (fire type, area burned, ignition dates)

  lake_fire_history_severity: contains lake-specific fire history files by burn severity

  baileys_provinces_burned_lakes.csv: lake watershed fires by Bailey's provinces

  Burned1500mBuffs.csv: table of lagoslakeids (unique lake IDs) with at least 1 watershed fire, any type

  Burned1500mBuffs_Rx.csv: table of lagoslakeids (unique lake IDs) with at least 1 watershed prescribed fire

  Burned1500mBuffs_WF.csv: table of lagoslakeids (unique lake IDs) with at least 1 watershed wildfire

  fire_area_severity_year: total burn severity in lake watersheds by year (hectares by burn severity class)

  Metadata_forExportedData.docx: metadata focument for all data files, with text formatting

  Metadata_forExportedData.txt: metadata focument for all data files, without text formatting

  states_burned_lakes.csv: lake watershed fires by US state (lower 48; continental US)

GIS: contains GIS files small enough to be part of this repository, ArcGIS mxd mapping files

baileys_ecoregions:
  
  Baileys_ecoreg_map.mxd: used to create supplemental map of Bailey's provinces
  
  baileys_provinces_propBurned.shp: shapefile of proportion of lakes per Bailey's province with fire
  
  eco_us_province_dissolved.shp: shapefile of Bailey's provinces; see baileys_provinces_burned_lakes.csv for column descriptions  
  (but note auto-truncation of column names when exported from R using rgdal:writeOGR)

US_states:
  
  lower48.shp: shapefile of lower 48 US states (continental US)
  
  state_prop_lakes_burned.shp: shapefile of proportion of lakes per state with fire; see states_burned_lakes.csv for column       
  descriptions (but note auto-truncation of column names when exported from R using rgdal:writeOGR)
  
RCode: contains scripts and custom functions used for data analysis
  CompareIWS_toBuffers.R: compare watersheds to 1500m buffer areas based on 51000 lakes in LAGOSNE (Soranno et al. 2017). 
  Compare lake area to 1500 m buffer area for all US lakes in analysis (~137000)
  
  CumulativeWatershedBurnSeverity.R: calculate cumulative burn severity by lake watershed
  
  Fire_by_State_Ecoregion.R: analyzing watershed fire by US state and Bailey's provinces
  
  LakeFires_vs_AllFires.R: analyze recent fire activity in lake watersheds vs. background fire activity in continental US
  
  PercentWatershedBurned.R: executes percent_watershed_burned_cumulative.R (see functions) and produces output shapefile for 
  mapping in ArcGIS
  
  Watershed_FireSeverity.R: executes tabulate_burn_severity.R and Buffer_erase_lagoslakeid.R (see functions) to generate lake-
  specific fire histories based on burn severity class (ExportedData/lake_fire_history_severity/)
  
  WatershedAreaBurned.R: executes zones_that_burned_all.R to identify lakes with watershed fires. Executes 
  lagoslakeid_fire_history_watershed.R to generate lake-specific fire histories based on fire type 
  (ExportedData/lake_fire_history/). Executes_zones_that_burned_given_year.R to count number of watersheds with different fire 
  types by year. Executes number_fires_by_lake.R to count number of fires by type in individual years. Executes 
  annual_area_burn_firetype.R to calculate annual area burned by fire type in individual years, based on output of 
  lagoslakeid_fire_history_watershed.R. 

functions:
  
  annual_area_burn_firetype.R: calculates annual wildfire, prescribed, wildland fire use, unknown type and total area burned
  from pre-calculated fire histories (output of lagoslakeid_fire_history_watershed.R)
  
  Buffer_erase_lagoslakeid.R: buffers lakes, erases lake area (serving as lake watersheds)
  
  lagoslakeid_fire_history_watershed.R: calculates area burned through time in lake watersheds, uses instersection between fire 
  and watershed polygons (watershed polygons can be substituted by lake buffer polygons)
  
  number_fires_by_lake.R: counts number of fires by lake (lagoslakeid) based on pre-calculated lake-specific fire histories     
  (output of lagoslakeid_fire_history_watershed.R)
  
  percent_watershed_burned_cumulative.R: calculates % watershed burned by lagoslakeid using pre-calculated data (output of    
  lagoslakeid_fire_history_watershed.R)
  
  tabulate_burn_severity.R: returns table of hectares and percent of each severity class
  
  zones_that_burned_all.R: determines which lakes (by lagoslakeid) that have experienced fire (any fire type)
  
  zones_that_burned_given_year_fire_type.R: determines how many lakes (by lagoslakeid) that have experienced different fire 
  types (wildfire, prescribed, wildland fire use, unknown)
 
  
