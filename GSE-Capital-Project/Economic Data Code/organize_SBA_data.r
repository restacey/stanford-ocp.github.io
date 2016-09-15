# Organize the SBA Data
# Note: The data is from Michael Ohlrogge in a dropbox folder

# Load Necessary Packages for this analysis
if (!(require(plyr))) install.packages ("plyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(RcppRoll))) install.packages ("RcppRoll")

# Set the file location
file_location	<- "C:/Users/robert/Desktop/Economic Data"
Result_location <- file.path( file_location, "Results" ) 

# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Read in the MSA/Division Code code lookup table
CBSA_MSA_State	<- read.csv( file.path( file_location, "CBSA MSA Mapping.csv" ) )

# Read in the SBA data
SBA_data	<- read.table( file.path( file_location, "SBA/SBA_Panel.txt" ), header=TRUE, sep="\t" )
colnames(SBA_data)[which( colnames(SBA_data) == "CBSA" )] <- "MSA"

# Standardize missing values with the Fannie Mae data
SBA_data[ SBA_data[,"MSA"] == "99999", "MSA" ] 	<- '00000'

# Correct the MSA codes (should end in 0)
# Note: the numbers that end in 4 are division codes
unique_MSA	<- sort( unique( SBA_data$MSA ) )
wrong_MSA	<- unique_MSA[ ( as.numeric( unique_MSA ) %% 10 ) != 0 ]
for( MSA in wrong_MSA )
{
  temp		<- subset( CBSA_MSA_State, Metropolitan.Division.Code == MSA )
  MSA_code	<- unique( temp$CBSA.Code )
  if( MSA == '13644' ) { MSA_code <- '47900' }	# Montgomery County, MD
  if( MSA == '14484' ) { MSA_code <- '14460' }	# Boston-Cambridge-Newton, MA-NH
  if( MSA == '20764' ) { MSA_code <- '35620' }	# New York-Newark-Jersey City, NY-NJ-PA
  if( MSA == '35644' ) { MSA_code <- '35620' }	# New York-Newark-Jersey City, NY-NJ-PA
  if( MSA == '37764' ) { MSA_code <- '00000' }	# Unknown
  if( MSA == '42044' ) { MSA_code <- '31080' }	# Los Angeles-Long Beach-Anaheim, CA
  if( MSA == '47644' ) { MSA_code <- '19820' }	# Warren-Troy-Farmington Hills, MI

  if( length(MSA_code) != 1 ) { stop( "No unique MSA Code!" ) }
  SBA_data[ SBA_data$MSA == MSA, "MSA" ] <- MSA_code

} # end for

# Make sure there is one unique observation per MSA/Date
SBA_data	<- ddply( SBA_data, .(MSA, Date), function(x) colSums( x[,c("Orig_Combo", "Orig_7a", "Orig_504", 
								            "Default_Combo", "Default_7a", "Default_504")] ) )
SBA_data	<- as.data.table(SBA_data)

# Compute the running 5yr number of loans and 1 year number of defaults
SBA_data[, c("Orig_5yr_Loan_Combo", "Orig_5yr_Loan_7a", "Orig_5yr_Loan_504",
	     "Orig_1yr_Def_Combo",  "Orig_1yr_Def_7a",  "Orig_1yr_Def_504") :=
	   list( c( rep(NA, 59), roll_sum(Orig_Combo,    n=60, na.rm=FALSE) ),
		 c( rep(NA, 59), roll_sum(Orig_7a,       n=60, na.rm=FALSE) ),
		 c( rep(NA, 59), roll_sum(Orig_504,      n=60, na.rm=FALSE) ),
		 c( rep(NA, 11), roll_sum(Default_Combo, n=12, na.rm=FALSE) ),
		 c( rep(NA, 11), roll_sum(Default_7a,    n=12, na.rm=FALSE) ),
		 c( rep(NA, 11), roll_sum(Default_504,   n=12, na.rm=FALSE) )
		   ), by=MSA ]


# Compute the default ratio
SBA_data[, c("SBA_DefRate_Combo", "SBA_DefRate_7a", "SBA_DefRate_504") :=
	   list( Orig_1yr_Def_Combo/Orig_5yr_Loan_Combo, Orig_1yr_Def_7a/Orig_5yr_Loan_7a, Orig_1yr_Def_504/Orig_5yr_Loan_504 ) ]
SBA_data[, c("SBA_DefRate_Combo", "SBA_DefRate_7a", "SBA_DefRate_504") :=
	   list( ifelse( SBA_DefRate_Combo == Inf, 0, SBA_DefRate_Combo ),
		 ifelse( SBA_DefRate_7a    == Inf, 0, SBA_DefRate_7a ),
		 ifelse( SBA_DefRate_504   == Inf, 0, SBA_DefRate_504 ) ) ]


# Drop the unneeded columns
SBA_data[, setdiff( colnames(SBA_data), c( "MSA", "Date", "SBA_DefRate_Combo", "SBA_DefRate_7a", "SBA_DefRate_504" ) ) := NULL ]

# Drop the rows with missing default rates
SBA_data	<- subset( SBA_data, !is.na(SBA_DefRate_Combo) | !is.na(SBA_DefRate_7a) | !is.na(SBA_DefRate_504) )

# Save the SBA data to file
save( SBA_data, file=file.path( Result_location, "SBA_data.RData" ) )
