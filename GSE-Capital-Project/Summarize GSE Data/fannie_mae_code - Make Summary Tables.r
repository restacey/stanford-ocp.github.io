# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "zoo" version 1.7-7
# package "XLConnect" version 0.2-12

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 

# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# MAKE A TABLE BY ORIGINATION DATE, MSA, and STATE: SUM OF ORIGINATION AMOUNT, ORIGINATION RATE, FICO, DTI, ORIGINATION HOME VALUE
# NOTE: NEED TO RUN "fannie_mae_code - Performance Data.r" BEFORE RUNNING THIS CODE

# Load Necessary Packages for this analysis
# Note: Need to have Java installed for XLConnect to work
if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")
if (!(require(XLConnect))) install.packages ("XLConnect")


# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
RData_location	<- file.path( file_location, "RData" )
temp_location	<- file.path( file_location, "Summary Tables" )
Result_location	<- file.path( file_location, "Results" )

# Create the output folder if it does not already exist
dir.create( temp_location, showWarnings = FALSE )
dir.create( Result_location, showWarnings = FALSE )

# Get the files names 
fnames		<- list.files( RData_location, pattern = glob2rx("*RData"), full.names=TRUE)

# The "foreach" package contructs a loop so that R can iterate through the data files
n_files		<- length(fnames)


##################################################
# Create the Summary Tables by State and MSA
##################################################

Summary_Table_list <- foreach(k=1:n_files, .inorder=FALSE,
           .packages=c("data.table")) %do% {

# Load the data
load( fnames[k] )	# Combined_Data

# Keep only the rows with complete observations
Combined_Data	<- Combined_Data[, .SD, .SDcols=c("ORIG_DTE", "STATE", "MSA", "ORIG_AMT", "ORIG_RT", "CSCORE_MN", "DTI", "ORIG_VAL")]
Combined_Data[, MSA:= ifelse( is.na(MSA), '00000', MSA ) ]
Combined_Data	<- Combined_Data[ complete.cases(Combined_Data), ]

# Create the summary tables by MSA and State
State_Summary	<- Combined_Data[, list( sum(ORIG_AMT, na.rm=TRUE), sum(ORIG_AMT*ORIG_RT, na.rm=TRUE), sum(ORIG_AMT*CSCORE_MN, na.rm=TRUE), 
					 sum(ORIG_AMT*DTI, na.rm=TRUE), sum(ORIG_AMT*ORIG_VAL, na.rm=TRUE) ),
				by=list(ORIG_DTE, STATE)]
setnames(State_Summary, c("ORIG_DTE", "STATE", "Sum_ORIG_AMT", "W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL") )
MSA_Summary	<- Combined_Data[, list( sum(ORIG_AMT, na.rm=TRUE), sum(ORIG_AMT*ORIG_RT, na.rm=TRUE), sum(ORIG_AMT*CSCORE_MN, na.rm=TRUE),
					 sum(ORIG_AMT*DTI, na.rm=TRUE), sum(ORIG_AMT*ORIG_VAL, na.rm=TRUE) ),
				by=list(ORIG_DTE, MSA)]
setnames(MSA_Summary, c("ORIG_DTE", "MSA", "Sum_ORIG_AMT", "W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL") )

# Remove the Combined_Data from memory
rm(Combined_Data); gc()

# Return the summary tables
Summary_Table	<- list( State_Summary=State_Summary, MSA_Summary=MSA_Summary )
save( Summary_Table, file=file.path( temp_location, paste( "FNMA_Summary_Table_", k, ".RData", sep="" ) ) )
return( Summary_Table )

} # end for each

# Merge and organize the summary tables
State_Summary	<- lapply( Summary_Table_list, function(x) x$State_Summary )
MSA_Summary	<- lapply( Summary_Table_list, function(x) x$MSA_Summary )
State_Summary	<- rbindlist( State_Summary, use.names=TRUE, fill=TRUE )
State_Summary	<- aggregate( . ~ ORIG_DTE + STATE, sum, data=State_Summary )
MSA_Summary	<- rbindlist( MSA_Summary, use.names=TRUE, fill=TRUE )
MSA_Summary	<- aggregate( . ~ ORIG_DTE + MSA, sum, data=MSA_Summary )
# Note: The State and MSA summary tables should sum to the same value
# colSums(State_Summary[,c(3:7)])
# colSums(MSA_Summary[,c(3:7)])

# Compute the weighted average values
State_Summary	<- as.data.table(State_Summary)
MSA_Summary	<- as.data.table(MSA_Summary)
State_Summary[,c("W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL"):=
		list( W_ORIG_RT/Sum_ORIG_AMT, W_CSCORE_MN/Sum_ORIG_AMT, 
		      W_DTI/Sum_ORIG_AMT, W_ORIG_VAL/Sum_ORIG_AMT ) ]
MSA_Summary[,c("W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL"):=
		list( W_ORIG_RT/Sum_ORIG_AMT, W_CSCORE_MN/Sum_ORIG_AMT, 
		      W_DTI/Sum_ORIG_AMT, W_ORIG_VAL/Sum_ORIG_AMT ) ]
State_Summary[,Sum_ORIG_AMT:=NULL]
MSA_Summary[,Sum_ORIG_AMT:=NULL]


# Convert from long to wide format
State_Summary[,ORIG_DTE:= as.Date( as.yearmon( ORIG_DTE, "%m/%Y" ) ) ]
MSA_Summary[,  ORIG_DTE:= as.Date( as.yearmon( ORIG_DTE, "%m/%Y" ) ) ]
val_names	<- c("W_ORIG_RT", "W_CSCORE_MN", "W_DTI", "W_ORIG_VAL")
State_tables	<- lapply( val_names, function(x) dcast( State_Summary, STATE ~ ORIG_DTE, value.var=x ) )
MSA_tables	<- lapply( val_names, function(x) dcast( MSA_Summary,   MSA ~ ORIG_DTE, value.var=x ) )

# Save the long format summary tables as RData
save( State_Summary, file=file.path( Result_location, "FNMA_State_Summary_Table.RData" ) )
save( MSA_Summary,   file=file.path( Result_location, "FNMA_MSA_Summary_Table.RData" ) )

# Save the wide format summary tables as xlsx
State_xlsx	<- loadWorkbook( file.path( Result_location, "FNMA_State_Summary_Tables.xlsx" ), create = TRUE)
MSA_xlsx	<- loadWorkbook( file.path( Result_location, "FNMA_MSA_Summary_Tables.xlsx" ), create = TRUE)
for( i in 1:length(val_names) )
{
  createSheet( State_xlsx, name = val_names[i] )
  createSheet( MSA_xlsx,   name = val_names[i] )
  writeWorksheet(State_xlsx, State_tables[[i]], sheet =val_names[i], startRow = 1, startCol = 1, header=TRUE)
  writeWorksheet(MSA_xlsx,   MSA_tables[[i]],   sheet =val_names[i], startRow = 1, startCol = 1, header=TRUE)
} # end for
saveWorkbook(State_xlsx)
saveWorkbook(MSA_xlsx)


##################################################
# End the Summary Tables by State and MSA
##################################################

