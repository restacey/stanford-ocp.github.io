# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Windows laptop. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition, the following R packages were used in this program:
# package "Quandl" version 3.2.5
#
# This program will download from the internet and install the latest version of the above package if they are not installed in your R environment. 
# It is necessary to have internet connection to download these packages. 


#######################################################################################
# Code to get the Quandl Zillow data
#   Downloads from the Quandl website the Zillow website
#   Includes Monthly Median Sales Price by State, County, Metro Area,
#	City, Neighborhood, Zip
#   See https://www.quandl.com/data/ZILL/documentation/documentation
#   	for more information
# Each file is saved individually as an RData file
#######################################################################################

# Load the necessary package for this analysis
# If the package is not installed, the code will install it
if( !(require(Quandl)) ) install.packages( "Quandl" )

# Store where the files should be stored
# Note: This is should be changed to wherever you want the Quandl files saved
file_location	<- "C:/Users/robert/Desktop/Quandl"


# Set the API Key
# Note: Quandl limits to 50,000 calls per API key per day
#	If the key reaches its limit can use the commented out API Key
# Quandl.api_key("esCWKzHU12Fz9xy6yY2B")
Quandl.api_key("Q2dm7SBy3aPLVV-gL-Y5")


# Download the file with all of the data tables codes from Quandl
download_from_web	<- FALSE	# TRUE or FALSE depending on whether you want to download from web (TRUE) or use RData file (FALSE)
if( download_from_web )
{
  # Download the data from the web
  download.file( url="https://www.quandl.com/api/v3/databases/ZILL/codes",
 	destfile=file.path( file_location, "ZILL-datasets-codes.zip" ), mode="wb" )
  data_codes 	<- read.table( unz( file.path( file_location, "ZILL-datasets-codes.zip" ), "ZILL-datasets-codes.csv" ),
 			header=FALSE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, colClasses="character" )
  save( data_codes, file=file.path( file_location, "Zillow_Data_Codes.RData" ) )
  # Note: has 1,344,202 rows and 2 columns
} else
{
  # Load the Zillow data codes
  load( file.path( file_location, "Zillow_Data_Codes.RData" ) )
} # end if

# Extract the Area Category, Area Code, and Indicator Code
# Note: Some of the zip code area codes seem to give cities rather than numeric values
temp				<- data.frame(do.call('rbind', strsplit(data_codes$V1,'_',fixed=TRUE)))
temp2 				<- data.frame(do.call('rbind', strsplit(as.character(temp$X1),'/',fixed=TRUE)))
data_codes$Area_Name		<- sapply( strsplit( as.character(temp2$X2), "[^[:alpha:]]" ), function(x) x[[1]] )
data_codes$Area_Category	<- substr( data_codes$Area_Name, 1, 2 )
data_codes$Area_Category[ data_codes$Area_Category != "CO" ] <- substr( data_codes$Area_Name[ data_codes$Area_Category != "CO" ], 1, 1 ) 
data_codes$Area_Code		<- sapply( strsplit( as.character(temp2$X2), "[^[:digit:]]" ), function(x) x[[2]] )
data_codes$Indicator_Code	<- as.character( temp$X2 )


# Store the unique area categories and indicator codes
areas				<- unique( data_codes$Area_Category )
indicators			<- unique( data_codes$Indicator_Code )
# Note: Since Quandl limits to 50,000 calls per API key just download the relevant information
#	To download all of the Quandl data you can comment out the following two lines
indicators			<- c( "MSP" )
areas				<- sort(areas)

# Loop through the area categories and indicator codes
# Download the Quandl data and save to file
for( a in areas )		# for all area categories
{
  for( i in indicators )	# for all indicator codes
  {
    # Subset to the area / indicator data
    area_ind			<- subset( data_codes, ( Area_Category == a ) & ( Indicator_Code == i ) )

    # Get the area / indicator Quandli data and save it to file
    area_ind_data		<- Quandl(area_ind[,"V1"])
    save( area_ind_data, file=file.path( file_location, paste( a, "_", i, ".RData", sep="" ) ) )

  } # end inner for
} # end outer for
