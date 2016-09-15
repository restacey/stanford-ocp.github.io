# Organize the Adjusted Gross Income (AGI) data from PowerLytics
# Note: The PowerLytics data is from Michael Ohlrogge

# Load Necessary Packages for this analysis
if (!(require(plyr))) install.packages ("plyr")

# Set the file location
file_location	<- "C:/Users/robert/Desktop/Economic Data"
Result_location <- file.path( file_location, "Results" ) 

# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Read in the MSA code lookup table
MSA_codes	<- read.csv( file.path( file_location, "OMB MSA Codes.csv" ) )

# Read in the csv file with the AGI data
AGI_data	<- read.csv( file.path( file_location, "Consumer_Research_Report-MSA_1040_created_Jul_21_2016_03_39_51_.csv" ) )
AGI_data	<- AGI_data[, c("Year", "MSA", "Number.of.Filers", "total_Adjusted_gross_income_AGI") ]

# Calculate the average AGI per MSA/Year
AGI_data	<- ddply( AGI_data, .(Year, MSA), function(x) sum(x$total_Adjusted_gross_income_AGI) / sum(x$Number.of.Filers) )
colnames(AGI_data)[which(colnames(AGI_data)=="V1")] <- "Avg_AGI"
colnames(AGI_data)[which(colnames(AGI_data)=="MSA")] <- "MSA_Name"

# Merge the MSA codes to the data
MSA_codes$Name		<- trimws( sub( " Micropolitan Statistical Area", "", MSA_codes$Name ) )
MSA_codes$Name		<- trimws( sub( " Metropolitan Statistical Area", "", MSA_codes$Name ) )
AGI_data$MSA_Name	<- trimws( sub( "Micro Area", "", AGI_data$MSA_Name ) )
AGI_data$MSA_Name	<- trimws( sub( "Metro Area", "", AGI_data$MSA_Name ) )
AGI_data		<- merge( x=AGI_data, y=MSA_codes, by.x="MSA_Name", by.y="Name", all.x=TRUE )


# Correct for the ones that were unmatched
AGI_data[ AGI_data[,"MSA_Name"] == "Ca??on City, CO", "MSA" ]		<- 15860	# Cañon City, CO
AGI_data[ AGI_data[,"MSA_Name"] == "Espa??ola, NM", "MSA" ]		<- 21580	# Española, NM
AGI_data[ AGI_data[,"MSA_Name"] == "Macon, GA", "MSA" ]			<- 31420	# Macon-Bibb County, GA


# Save the AGI data to file
save( AGI_data, file=file.path( Result_location, "AGI_data.RData" ) )
