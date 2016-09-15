# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Windows laptop. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".


#######################################################################################
# Code to get the Quandl Zillow Database descriptions
#   Downloads from the Quandl website the Zillow database descriptions
#   See https://www.quandl.com/data/ZILL/documentation/metadata
#   	for more information
# NOTE: Should have a folder called temp_data in the folder given as the file_location
#	Each file will be saved as a csv file in the temp_data folder and then combined
#	into a single RData file: Quandl_Zillow_Database_Description.RData
# WARNING: Can take 2-3 days to download all of the files
#######################################################################################

# Store the location to save the files
# Note: This is should be changed to wherever you want the files saved
file_location	<- "C:/Users/robert/Desktop/Quandl"

# Store the webpage locations
p	<- c(1:13443)		# Note: the number of pages is given on https://www.quandl.com/data/ZILL/documentation/metadata
				#	but may change in the future
str1	<- "https://www.quandl.com/api/v3/datasets.csv?database_code=ZILL&per_page=100&sort_by=id&page="
str2	<- "&api_key=Q2dm7SBy3aPLVV-gL-Y5"
pages	<- paste( str1, p, str2, sep="" )
temp	<- paste( "temp_data/temp_download", p, ".csv", sep="" )

# Loop through the webpages, downloading each page
lapply( p, function(i) download.file( url=pages[i], destfile=file.path( file_location, temp[i] ), quiet=TRUE ) )


# Read in the downloaded csv files and append to make a single file
dat	<- data.frame()
for( i in temp )
{
  # Read in the downloaded csv file
  temp_dat	<- read.csv( file.path( file_location, i ),
   			header=TRUE, na.strings="NA", dec=".", strip.white=TRUE, colClasses="character" )

  # Append the data
  dat		<- rbind( dat, temp_dat )
  rm(temp_dat)

} # end for

# Save the data to file
save( dat, file=file.path( file_location, "Quandl_Zillow_Database_Description.RData" ) )
