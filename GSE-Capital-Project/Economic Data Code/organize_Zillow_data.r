# Organize the Zillow sales data
# Note: The data is can be downloaded using "download_zillow_msa.r"

# Load Necessary Packages for this analysis
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")


# Set the file location
file_location	<- "C:/Users/robert/Desktop/Economic Data"
zillow_location	<- "C:/Users/robert/Desktop/Robert Tian Data/Posted to Mega Folder/Zillow"
Result_location <- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data/Results/Summary Tables"

# Create the output folder if it does not already exist
dir.create( Result_location, showWarnings = FALSE )

# Read in the MSA/State code lookup table
MSA_codes	<- read.csv( file.path( file_location, "OMB MSA Codes.csv" ) )
State_codes	<- read.csv( file.path( file_location, "State Codes.csv" ) )

# Read in the zillow data
zillow_msa 	<- read.table( unz( file.path( zillow_location, "Metro.zip" ), "Metro/Metro_MedianSoldPrice_AllHomes.csv" ),
 			header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, colClasses="character" )
zillow_state 	<- read.table( unz( file.path( zillow_location, "State.zip" ), "State/State_MedianSoldPrice_AllHomes.csv" ),
 			header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, colClasses="character" )


# Convert from wide to long format
zillow_msa		<- melt( zillow_msa, id.vars="RegionName", measure.vars=grep("X", colnames(zillow_msa), value=TRUE) )
colnames(zillow_msa) 	<- c( "RegionName", "ORIG_DTE", "W_ORIG_VAL" )
zillow_msa$ORIG_DTE	<- sub( "X", "", zillow_msa$ORIG_DTE )
zillow_msa$ORIG_DTE	<- as.Date( as.yearmon( zillow_msa$ORIG_DTE, "%Y.%m" ) )
zillow_msa		<- subset( zillow_msa, W_ORIG_VAL != "" )
zillow_msa$W_ORIG_VAL	<- as.numeric( zillow_msa$W_ORIG_VAL )
zillow_msa		<- subset( zillow_msa, RegionName != "United States" )

# Convert from wide to long format
zillow_state		<- melt( zillow_state, id.vars="RegionName", measure.vars=grep("X", colnames(zillow_state), value=TRUE) )
colnames(zillow_state) 	<- c( "RegionName", "ORIG_DTE", "W_ORIG_VAL" )
zillow_state$ORIG_DTE	<- sub( "X", "", zillow_state$ORIG_DTE )
zillow_state$ORIG_DTE	<- as.Date( as.yearmon( zillow_state$ORIG_DTE, "%Y.%m" ) )
zillow_state		<- subset( zillow_state, W_ORIG_VAL != "" )
zillow_state$W_ORIG_VAL	<- as.numeric( zillow_state$W_ORIG_VAL )


# Merge the MSA values
MSA_codes$Name		<- trimws( sub( " Micropolitan Statistical Area", "", MSA_codes$Name ) )
MSA_codes$Name		<- trimws( sub( " Metropolitan Statistical Area", "", MSA_codes$Name ) )
zillow_msa		<- merge( x=zillow_msa, y=MSA_codes, by.x="RegionName", by.y="Name", all.x=TRUE )

# Merge the state abbreviations
zillow_state		<- merge( x=zillow_state, y=State_codes, by.x="RegionName", by.y="Name", all.y=TRUE )
zillow_state		<- subset( zillow_state, !is.na(W_ORIG_VAL) )


# Correct for the MSAs that are unmatched
zillow_msa[ zillow_msa[,"RegionName"] == "Albany, NY", "MSA" ]					<- 10580	# Albany-Schenectady-Troy, NY
zillow_msa[ zillow_msa[,"RegionName"] == "Allentown, PA", "MSA" ]				<- 10900	# Allentown-Bethlehem-Easton, PA-NJ
zillow_msa[ zillow_msa[,"RegionName"] == "Atlanta, GA", "MSA" ]					<- 12060	# Atlanta-Sandy Springs-Roswell, GA
zillow_msa[ zillow_msa[,"RegionName"] == "Atlantic City, NJ", "MSA" ]				<- 12100	# Atlantic City-Hammonton, NJ
zillow_msa[ zillow_msa[,"RegionName"] == "Augusta, GA", "MSA" ]					<- 12260	# Augusta-Richmond County, GA-SC
zillow_msa[ zillow_msa[,"RegionName"] == "Baltimore, MD", "MSA" ]				<- 12580	# Baltimore-Columbia-Towson, MD
zillow_msa[ zillow_msa[,"RegionName"] == "Bend, OR", "MSA" ]					<- 13460	# Bend-Redmond, OR
zillow_msa[ zillow_msa[,"RegionName"] == "Boston, MA", "MSA" ]					<- 14460	# Boston-Cambridge-Newton, MA-NH
zillow_msa[ zillow_msa[,"RegionName"] == "Bremerton, WA", "MSA" ]				<- 14740	# Bremerton-Silverdale, WA
zillow_msa[ zillow_msa[,"RegionName"] == "Canton, OH", "MSA" ]					<- 15940	# Canton-Massillon, OH
zillow_msa[ zillow_msa[,"RegionName"] == "Cape Cod, MA", "MSA" ]				<- 12700	# Barnstable Town, MA
zillow_msa[ zillow_msa[,"RegionName"] == "Charleston, SC", "MSA" ]				<- 16700	# Charleston-North Charleston, SC
zillow_msa[ zillow_msa[,"RegionName"] == "Charlotte, NC", "MSA" ]				<- 16740	# Charlotte-Concord-Gastonia, NC-SC
zillow_msa[ zillow_msa[,"RegionName"] == "Chicago, IL", "MSA" ]					<- 16980	# Chicago-Naperville-Elgin, IL-IN-WI
zillow_msa[ zillow_msa[,"RegionName"] == "Cincinnati, OH", "MSA" ]				<- 17140	# Cincinnati, OH-KY-IN
zillow_msa[ zillow_msa[,"RegionName"] == "Cleveland, OH", "MSA" ]				<- 17460	# Cleveland-Elyria, OH
zillow_msa[ zillow_msa[,"RegionName"] == "Cumberland, MD", "MSA" ]				<- 19060	# Cumberland, MD-WV
zillow_msa[ zillow_msa[,"RegionName"] == "Dallas-Fort Worth, TX", "MSA" ]			<- 19100	# Dallas-Fort Worth-Arlington, TX
zillow_msa[ zillow_msa[,"RegionName"] == "Daytona Beach, FL", "MSA" ]				<- 19660	# Deltona-Daytona Beach-Ormond Beach, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Denver, CO", "MSA" ]					<- 19740	# Denver-Aurora-Lakewood, CO
zillow_msa[ zillow_msa[,"RegionName"] == "Des Moines, IA", "MSA" ]				<- 19780	# Des Moines-West Des Moines, IA
zillow_msa[ zillow_msa[,"RegionName"] == "Detroit, MI", "MSA" ]					<- 19820	# Detroit-Warren-Dearborn, MI
zillow_msa[ zillow_msa[,"RegionName"] == "Durham, NC", "MSA" ]					<- 20500	# Durham-Chapel Hill, NC
zillow_msa[ zillow_msa[,"RegionName"] == "Fort Myers, FL", "MSA" ]				<- 15980	# Cape Coral-Fort Myers, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Grand Rapids, MI", "MSA" ]				<- 24340	# Grand Rapids-Wyoming, MI
zillow_msa[ zillow_msa[,"RegionName"] == "Greensboro, NC", "MSA" ]				<- 24660	# Greensboro-High Point, NC
zillow_msa[ zillow_msa[,"RegionName"] == "Greenville, SC", "MSA" ]				<- 24860	# Greenville-Anderson-Mauldin, SC
zillow_msa[ zillow_msa[,"RegionName"] == "Hanford, CA", "MSA" ]					<- 25260	# Hanford-Corcoran, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Hartford, CT", "MSA" ]				<- 25540	# Hartford-West Hartford-East Hartford, CT
zillow_msa[ zillow_msa[,"RegionName"] == "Kahului, HI", "MSA" ]					<- 27980	# Kahului-Wailuku-Lahaina, HI
zillow_msa[ zillow_msa[,"RegionName"] == "Kalamazoo, MI", "MSA" ]				<- 28020	# Kalamazoo-Portage, MI
zillow_msa[ zillow_msa[,"RegionName"] == "Lakeland, FL", "MSA" ]				<- 29460	# Lakeland-Winter Haven, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Lansing, MI", "MSA" ]					<- 29620	# Lansing-East Lansing, MI
zillow_msa[ zillow_msa[,"RegionName"] == "Las Vegas, NV", "MSA" ]				<- 29820	# Las Vegas-Henderson-Paradise, NV
zillow_msa[ zillow_msa[,"RegionName"] == "Little Rock, AR", "MSA" ]				<- 30780	# Little Rock-North Little Rock-Conway, AR
zillow_msa[ zillow_msa[,"RegionName"] == "Manchester, NH", "MSA" ]				<- 31700	# Manchester-Nashua, NH
zillow_msa[ zillow_msa[,"RegionName"] == "Melbourne, FL", "MSA" ]				<- 37340	# Palm Bay-Melbourne-Titusville, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Memphis, TN", "MSA" ]					<- 32820	# Memphis, TN-MS-AR
zillow_msa[ zillow_msa[,"RegionName"] == "Miami-Fort Lauderdale, FL", "MSA" ]			<- 33100	# Miami-Fort Lauderdale-West Palm Beach, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Milwaukee, WI", "MSA" ]				<- 33340	# Milwaukee-Waukesha-West Allis, WI
zillow_msa[ zillow_msa[,"RegionName"] == "Minneapolis-St Paul, MN", "MSA" ]			<- 33460	# Minneapolis-St. Paul-Bloomington, MN-WI
zillow_msa[ zillow_msa[,"RegionName"] == "Mount Vernon, WA", "MSA" ]				<- 34580	# Mount Vernon-Anacortes, WA
zillow_msa[ zillow_msa[,"RegionName"] == "Naples, FL", "MSA" ]					<- 34940	# Naples-Immokalee-Marco Island, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Nashville, TN", "MSA" ]				<- 34980	# Nashville-Davidson--Murfreesboro--Franklin, TN
zillow_msa[ zillow_msa[,"RegionName"] == "New Haven, CT", "MSA" ]				<- 35300	# New Haven-Milford, CT
zillow_msa[ zillow_msa[,"RegionName"] == "New London, CT", "MSA" ]				<- 35980	# Norwich-New London, CT
zillow_msa[ zillow_msa[,"RegionName"] == "New Orleans, LA", "MSA" ]				<- 35380	# New Orleans-Metairie, LA
zillow_msa[ zillow_msa[,"RegionName"] == "New York, NY", "MSA" ]				<- 35620	# New York-Newark-Jersey City, NY-NJ-PA
zillow_msa[ zillow_msa[,"RegionName"] == "Olympia, WA", "MSA" ]					<- 36500	# Olympia-Tumwater, WA
zillow_msa[ zillow_msa[,"RegionName"] == "Orlando, FL", "MSA" ]					<- 36740	# Orlando-Kissimmee-Sanford, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Pensacola, FL", "MSA" ]				<- 37860	# Pensacola-Ferry Pass-Brent, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Philadelphia, PA", "MSA" ]				<- 37980	# Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
zillow_msa[ zillow_msa[,"RegionName"] == "Phoenix, AZ", "MSA" ]					<- 38060	# Phoenix-Mesa-Scottsdale, AZ
zillow_msa[ zillow_msa[,"RegionName"] == "Portland, OR", "MSA" ]				<- 38900	# Portland-Vancouver-Hillsboro, OR-WA
zillow_msa[ zillow_msa[,"RegionName"] == "Providence, RI", "MSA" ]				<- 39300	# Providence-Warwick, RI-MA      
zillow_msa[ zillow_msa[,"RegionName"] == "Riverside, CA", "MSA" ]				<- 40140	# Riverside-San Bernardino-Ontario, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Sacramento, CA", "MSA" ]				<- 40900	# Sacramento--Roseville--Arden-Arcade, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Salisbury, MD", "MSA" ]				<- 41540	# Salisbury, MD-DE
zillow_msa[ zillow_msa[,"RegionName"] == "San Diego, CA", "MSA" ]				<- 41740	# San Diego-Carlsbad, CA
zillow_msa[ zillow_msa[,"RegionName"] == "San Francisco, CA", "MSA" ]				<- 41860	# San Francisco-Oakland-Hayward, CA
zillow_msa[ zillow_msa[,"RegionName"] == "San Jose, CA", "MSA" ]				<- 41940	# San Jose-Sunnyvale-Santa Clara, CA
zillow_msa[ zillow_msa[,"RegionName"] == "San Luis Obispo, CA", "MSA" ]				<- 42020	# San Luis Obispo-Paso Robles-Arroyo Grande, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Seattle, WA", "MSA" ]					<- 42660	# Seattle-Tacoma-Bellevue, WA
zillow_msa[ zillow_msa[,"RegionName"] == "Santa Cruz, CA", "MSA" ]				<- 42100	# Santa Cruz-Watsonville, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Spokane, WA", "MSA" ]					<- 44060	# Spokane-Spokane Valley, WA
zillow_msa[ zillow_msa[,"RegionName"] == "St. Louis, MO", "MSA" ]				<- 41180	# St. Louis, MO-IL
zillow_msa[ zillow_msa[,"RegionName"] == "Stamford, CT", "MSA" ]				<- 14860	# Bridgeport-Stamford-Norwalk, CT
zillow_msa[ zillow_msa[,"RegionName"] == "Stockton, CA", "MSA" ]				<- 44700	# Stockton-Lodi, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Tampa, FL", "MSA" ]					<- 45300	# Tampa-St. Petersburg-Clearwater, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Utica, NY", "MSA" ]					<- 46540	# Utica-Rome, NY
zillow_msa[ zillow_msa[,"RegionName"] == "Vallejo, CA", "MSA" ]					<- 46700	# Vallejo-Fairfield, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Ventura, CA", "MSA" ]					<- 37100	# Oxnard-Thousand Oaks-Ventura, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Vero Beach, FL", "MSA" ]				<- 42680	# Sebastian-Vero Beach, FL
zillow_msa[ zillow_msa[,"RegionName"] == "Virginia Beach, VA", "MSA" ]				<- 47260	# Virginia Beach-Norfolk-Newport News, VA-NC
zillow_msa[ zillow_msa[,"RegionName"] == "Visalia, CA", "MSA" ]					<- 47300	# Visalia-Porterville, CA
zillow_msa[ zillow_msa[,"RegionName"] == "Washington, DC", "MSA" ]				<- 47900	# Washington-Arlington-Alexandria, DC-VA-MD-WV
zillow_msa[ zillow_msa[,"RegionName"] == "Worcester, MA", "MSA" ]				<- 49340	# Worcester, MA-CT
zillow_msa[ zillow_msa[,"RegionName"] == "York, PA", "MSA" ]					<- 49620	# York-Hanover, PA


# Only keep the necessary columns
zillow_msa	<- zillow_msa[,   c("ORIG_DTE", "MSA",   "W_ORIG_VAL") ]
colnames(zillow_state)	<- toupper( colnames(zillow_state) )
zillow_state	<- zillow_state[, c("ORIG_DTE", "STATE", "W_ORIG_VAL") ]

# Save the zillow data to file
zillow_msa	<- as.data.table( zillow_msa )
zillow_state	<- as.data.table( zillow_state )
save( zillow_msa,   file=file.path( Result_location, "zillow_MSA_SalePrice_data.RData" ) )
save( zillow_state, file=file.path( Result_location, "zillow_State_SalePrice_data.RData" ) )
