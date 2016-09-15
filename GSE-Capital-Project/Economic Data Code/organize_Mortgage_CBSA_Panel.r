# Organize the Mortgage CBSA Panel Data
# Note: The data is from Michael Ohlrogge in a dropbox folder

# Load Necessary Packages for this analysis
if (!(require(plyr))) install.packages ("plyr")


# Set the file location
file_location	<- "C:/Users/robert/Desktop/Economic Data"
Result_location <- file.path( file_location, "Results" ) 

# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Read in the MSA/State code lookup table
MSA_codes	<- read.csv( file.path( file_location, "OMB MSA Codes.csv" ) )
State_codes	<- read.csv( file.path( file_location, "State Codes.csv" ) )
CBSA_MSA_State	<- read.csv( file.path( file_location, "CBSA MSA Mapping.csv" ) )

# Merge the state abbreviations
CBSA_MSA_State$State.Name	<- as.character( CBSA_MSA_State$State.Name )
CBSA_MSA_State[ CBSA_MSA_State$State.Name == "District of Columbia", "State.Name" ] <- "the District of Columbia"
CBSA_MSA_State	<- merge( x=CBSA_MSA_State, y=State_codes, by.x="State.Name", by.y="Name", all.x=TRUE )

# Read in the mortgage data
mortgage_data	<- read.table( file.path( file_location, "Mortgage_CBSA_Panel.txt" ), header=TRUE, sep="\t" )
colnames(mortgage_data)[which( colnames(mortgage_data) == "CBSA" )] <- "MSA"

# Standardize missing values with the Fannie Mae data
mortgage_data[ mortgage_data[,"State"] == "XX", "State" ] 	<- NA
mortgage_data[ mortgage_data[,"MSA"] == "99997", "MSA" ] 	<- '00000'

# Confirm all of the MSA codes are in the correct state
# Note: seems like some of the mortgage panel data has wrong code/state values
unique_MSA	<- sort( unique( mortgage_data$MSA ) )
for( MSA in unique_MSA )
{
  if( MSA != '00000' )
  {
    temp		<- subset( CBSA_MSA_State, CBSA.Code == MSA )
    if( nrow(temp) == 0 ) { temp <- subset( CBSA_MSA_State, Metropolitan.Division.Code == MSA ) }
    if( nrow(temp) > 0 )
    {
      allowed_states	<- as.character( unique( temp$State ) )
      other_MSA		<- mortgage_data$MSA != MSA
      correct_vals	<- (mortgage_data$MSA == MSA) & (mortgage_data$State %in% allowed_states)
      mortgage_data	<- subset( mortgage_data, other_MSA | correct_vals )
    } # end inner if
  } # end outer if
} # end for

# Subset to the data with real state abbreviations
unique_states	<- as.character( State_codes$State )
mortgage_data	<- subset( mortgage_data, (State %in% unique_states) | is.na(State) )

# Correct the MSA codes (should end in 0)
# Note: the numbers that end in 4 are division codes
unique_MSA	<- sort( unique( mortgage_data$MSA ) )
wrong_MSA	<- unique_MSA[ ( as.numeric( unique_MSA ) %% 10 ) != 0 ]
for( MSA in wrong_MSA )
{
  temp		<- subset( CBSA_MSA_State, Metropolitan.Division.Code == MSA )
  MSA_code	<- unique( temp$CBSA.Code )
  if( length(MSA_code) != 1 ) { stop( "No unique MSA Code!" ) }
  mortgage_data[ mortgage_data$MSA == MSA, "MSA" ] <- MSA_code

} # end for

# Make sure there is one unique observation per State/MSA/Date
mortgage_data	<- ddply( mortgage_data, .(State, MSA, Date), 
			function(x) colSums( x[,c("OpenMortgages", "Delinquencies", "Foreclosures")] ) )

# Compute some ratios
mortgage_data$perc_delinquent			<- with( mortgage_data, Delinquencies / OpenMortgages )
mortgage_data$perc_foreclosure			<- with( mortgage_data, Foreclosures / OpenMortgages )
mortgage_data$foreclosure_delinquent_ratio	<- with( mortgage_data, Foreclosures / Delinquencies )
mortgage_data[ mortgage_data$Delinquencies == 0, "foreclosure_delinquent_ratio" ] <- 0


# Save the mortgage data to file
save( mortgage_data, file=file.path( Result_location, "mortgage_panel_data.RData" ) )
