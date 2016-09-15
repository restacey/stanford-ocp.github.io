# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Windows laptop. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".


#######################################################################################
# Code to download zip files from Zillow
# http://files.zillowstatic.com/research/public/State.zip
# http://files.zillowstatic.com/research/public/Metro.zip
# http://files.zillowstatic.com/research/public/County.zip
# http://files.zillowstatic.com/research/public/City.zip
# http://files.zillowstatic.com/research/public/Zip.zip
# http://files.zillowstatic.com/research/public/Neighborhood.zip
#######################################################################################


# Store the location to save the files
# Note: This is should be changed to wherever you want the files saved
file_location	<- "C:/Users/robert/Desktop/Zillow"

# Store the web page urls
areas		<- c( "State", "Metro", "County", "City", "Zip", "Neighborhood" )
str1		<- "http://files.zillowstatic.com/research/public/"
str2		<- paste( areas, ".zip", sep="" )
pages		<- paste( str1, str2, sep="" )

# Download each of the Zillow area zip files
for( i in 1:length(areas) )
{
  download.file( url=pages[i], destfile=file.path( file_location, str2[i] ) )
}
