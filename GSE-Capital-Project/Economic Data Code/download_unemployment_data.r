# Download the FRED Unemployment Data

# Load Necessary Packages for this analysis
if (!(require(quantmod))) install.packages ("quantmod")
if (!(require(reshape2))) install.packages ("reshape2")

# Set the file location
file_location	<- "C:/Users/robert/Desktop/Economic Data"
Result_location <- file.path( file_location, "Results" ) 

# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Read in the MSA/State code lookup table
MSA_codes	<- read.csv( file.path( file_location, "OMB MSA Codes.csv" ) )
State_codes	<- read.csv( file.path( file_location, "State Codes.csv" ) )

# Read in the csv file with the FRED Unemployment Series IDs
FRED_table	<- read.csv( file.path( file_location, "UR_50_112_113_116_Mannual_Selection_2016-07-15.csv" ) )

# Subset to the MSA and State seasonally adjusted data
FRED_table_MSA	<- subset( FRED_table, (Category.2 == "MSA") & (Series_Swasonal.Adj == "Smoothed Seasonally Adjusted") )
FRED_table_St	<- subset( FRED_table, (Category.2 == "State") & (Series_Swasonal.Adj == "Seasonally Adjusted") )


###################################
# Organize the data by MSA/State
###################################

download_organize_FRED_data <- function( FRED_table, type, type_codes=NULL )
{ # Download and organize the FRED unemployment data
  # Type:	"MSA" or "State"

  # Download the FRED Unemployment Data
  setDefaults(getSymbols, src='FRED')
  temp_Series		<- lapply( FRED_table$Series_ID, function(x) getSymbols.FRED(x, auto.assign=FALSE) )
  unemp_table		<- Reduce(function(...) merge(..., all=T), temp_Series)

  # Convert from wide to long format
  unemp_table		<- as.data.frame(unemp_table)
  unemp_table$date 	<- row.names(unemp_table)
  unemp_table		<- melt(unemp_table, id.vars="date")

  # Merge the region to the data
  unemp_table 		<- merge( x=unemp_table, y=FRED_table[,c("Series_ID", "Segement...Geo")], by.x="variable", by.y="Series_ID", all.x=TRUE )

  if( type == "MSA" )
  {
    # Merge the MSA codes to the data
    type_codes$Name	<- trimws( sub( " Micropolitan Statistical Area", "", type_codes$Name ) )
    type_codes$Name	<- trimws( sub( " Metropolitan Statistical Area", "", type_codes$Name ) )
    unemp_table[,"Segement...Geo"] <- trimws( as.character(unemp_table[,"Segement...Geo"]) )
    unemp_table		<- merge( x=unemp_table, y=type_codes, by.x="Segement...Geo", by.y="Name", all.x=TRUE )

    # Correct for the ones that were unmatched
    unemp_table[ unemp_table[,"Segement...Geo"] == "Anderson, IN", "MSA" ]					<- 26900	# Indianapolis-Carmel-Anderson, IN
    unemp_table[ unemp_table[,"Segement...Geo"] == "Anderson, SC", "MSA" ]					<- 24860	# Greenville-Anderson-Mauldin, SC
    unemp_table[ unemp_table[,"Segement...Geo"] == "Bradenton-Sarasota-Venice, FL", "MSA" ]			<- 35840	# North Port-Sarasota-Bradenton, FL
    unemp_table[ unemp_table[,"Segement...Geo"] == "Charleston-North Charleston-Summerville, SC", "MSA" ]	<- 16700	# Charleston-North Charleston, SC
    unemp_table[ unemp_table[,"Segement...Geo"] == "Holland-Grand Haven, MI", "MSA" ]				<- 26090	# Holland, MI
    unemp_table[ unemp_table[,"Segement...Geo"] == "Louisville-Jefferson County, KY-IN", "MSA" ]		<- 31140	# Louisville/Jefferson County, KY-IN
    unemp_table[ unemp_table[,"Segement...Geo"] == "Macon, GA", "MSA" ]						<- 31420	# Macon-Bibb County, GA
    unemp_table[ unemp_table[,"Segement...Geo"] == "Palm Coast, FL", "MSA" ]					<- 45540	# The Villages, FL
    unemp_table[ unemp_table[,"Segement...Geo"] == "Pascagoula, MS", "MSA" ]					<- 25060	# Gulfport-Biloxi-Pascagoula, MS
    unemp_table[ unemp_table[,"Segement...Geo"] == "Poughkeepsie-Newburgh-Middletown, NY", "MSA" ]		<- 35620	# New York-Newark-Jersey City, NY-NJ-PA
    unemp_table[ unemp_table[,"Segement...Geo"] == "Stockton, CA", "MSA" ]					<- 44700	# Stockton-Lodi, CA
    unemp_table[ unemp_table[,"Segement...Geo"] == "Texarkana, TX-Texarkana, AR", "MSA" ]			<- 45500	# Texarkana, TX-AR
    unemp_table[ unemp_table[,"Segement...Geo"] == "Trenton-Ewing, NJ", "MSA" ]					<- 45940	# Trenton, NJ

  } else if( type == "State" )
  {
    # Merge the state codes to the data
    unemp_table		<- merge( x=unemp_table, y=type_codes, by.x="Segement...Geo", by.y="Name", all.x=TRUE )
    unemp_table$State	<- as.character(unemp_table$State)

  } # end if

  # Return the unemployment data table
  return( unemp_table )

} # end function


# Download and organize the FRED unemployment data
MSA_unemp_table		<- download_organize_FRED_data( FRED_table_MSA, type="MSA",   MSA_codes )
State_unemp_table	<- download_organize_FRED_data( FRED_table_St,  type="State", State_codes )

# Save the MSA and state unemployment tables to file
save( MSA_unemp_table, file=file.path( Result_location, "MSA_Unemp_Table.RData" ) )
save( State_unemp_table, file=file.path( Result_location, "State_Unemp_Table.RData" ) )

######################################
# End Organize the data by MSA/State
######################################

