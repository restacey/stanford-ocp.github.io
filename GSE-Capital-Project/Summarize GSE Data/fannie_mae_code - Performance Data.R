# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "foreach" version 1.4.0 
# package "data.table" version 1.9.6
# package "zoo" version 1.7-7

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 


# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# Load Necessary Packages for this analysis

if (!(require(foreach))) install.packages ("foreach")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(zoo))) install.packages ("zoo")

# You will need to download Fannie Mae's Single-Family Loan Performance Data from Fannie Mae's website at https://loanperformancedata.fanniemae.com/lppub/index.html.
# For more detail please refer to the accompanied presentation. After downloading the files you will need to unzip the files. 
# Though read.table function in R can read zipped files, we have used the "fread" function from data.table package 
# to read these files for efficiency and speed. Unfortunately, fread cannot read zipped files.
# While this program will run with any number of pairs of files, we encourage users to download the entire set of Acquisition and Performance 
# files. The naming of the files should remain the same after download and unzipping process so that the files are saved in order. 
# This program will process the first Acquisition file and then the first Performance file, merge them together, 
# and then repeat that process for all matching files. It will save the batched files into the "RData" folder.
# The folder will be created if it does not already exists.

# Set the path to where you have saved the downloaded files (should be in a folder called "Data")
file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
data_location	<- file.path( file_location, "Data" )
RData_location	<- file.path( file_location, "RData" )

# Create the output folder if it does not already exist
dir.create( RData_location, showWarnings = FALSE )

# Check the number of files downloaded (should be even, equal number of Acquisition and Performance Files).
numberoffiles	<- length( list.files( data_location, pattern = glob2rx("*txt"), full.names=TRUE) )

# The "foreach" package contructs a loop so that R can iterate through all pairs of related Acquisition and Performance files.
# Calculate the number of iterations/cores in parallel processing allowing each pair to be processed simultaneously.
numberofloops	<- (numberoffiles/2)


# Create function to handle missing Current UPBs in the last record by setting them to the record prior
na.lomf <- function(x) 
{
  
  na.lomf.0 <- function(x) 
  {
    non.na.idx <- intersect(which(!is.na(x)),which(x>0))
    if (is.na(x[1L]) || x[1L]==0) 
    {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  } # end function
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) 
  {
    na.lomf.0(x)
  } else 
  {
    apply(x, dim.len, na.lomf.0)
  }

} # end function


na.lomf_L <- function(x) 
{
  
  non.na.idx <- intersect(which(!is.na(x)),which(x[length(x)-1]>0))
  if (is.na(x[length(x)]) || x[length(x)]==0) 
  {
    XX <- c(x[1:length(x)-1], rep.int(x[length(x)-1], 1))
  } else 
  {
    XX <- x
  }
  
} # end function


####################################################################
# Data Preperation Step
####################################################################

# After defining the Acquisition and Performance variables and their classes, the files are read into R and then data manipulation is carried out. 
# Acquisition and Performance files (from one or many quarters) will be merged into an R dataframe called "Combined_Data."
Combined_Data <- foreach(k=1:numberofloops, .inorder=FALSE,
           .packages=c("data.table", "zoo")) %do% {

# Define Acquisition variables and classes, and read the files into R.
Acquisitions <- list.files(data_location, pattern = glob2rx("*Acquisition*txt"), full.names=TRUE)

Acquisitions_Variables = c("LOAN_ID", "ORIG_CHN", "Seller.Name", "ORIG_RT", "ORIG_AMT", "ORIG_TRM", "ORIG_DTE"
                          ,"FRST_DTE", "OLTV", "OCLTV", "NUM_BO", "DTI", "CSCORE_B", "FTHB_FLG", "PURPOSE", "PROP_TYP"
                          ,"NUM_UNIT", "OCC_STAT", "STATE", "ZIP_3", "MI_PCT", "Product.Type", "CSCORE_C")

Acquisition_ColClasses = c("character", "character", "character", "numeric", "numeric", "integer", "character", "character", "numeric",
                          "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", 
                          "character", "character", "numeric", "character", "numeric")

Data_A <- fread(Acquisitions[k], sep = "|", colClasses = Acquisition_ColClasses, showProgress=FALSE)
setnames(Data_A, Acquisitions_Variables)
setkey(Data_A, "LOAN_ID")

# Delete unnecessary Acquisition variables.
Data_A[,c("Seller.Name","Product.Type"):=NULL]

# Obtain the Minimum Fico Score of the Borrower and Co-Borrower, Calculate House Price, and Replace Missing OCLTV values with OLTV values where available
Data_A[, c("CSCORE_MN", "ORIG_VAL", "OCLTV"):= list(pmin(CSCORE_B,CSCORE_C, na.rm = TRUE),
                                                   (ORIG_AMT/(OLTV/100)),
                                                   ifelse(is.na(OCLTV), OLTV, OCLTV))]

# Remove not-needed Acquisition data from R environment.
rm('Acquisitions_Variables', 'Acquisition_ColClasses')

# Store the unique loan ids
num_batches		<- 2		#### NOTE: CAN INCREASE THIS IF MEMORY IS A PROBLEM ####
unique_IDs		<- unique( Data_A[,LOAN_ID] )
unique_IDs_split	<- vector( mode="list", length=num_batches )
id_breaks		<- ceiling( seq( from=0, to=length(unique_IDs), length.out=(num_batches+1) ) )
for( i in 2:length(id_breaks) )
{
  unique_IDs_split[[i-1]] <- unique_IDs[ ( id_breaks[i-1] + 1 ) : id_breaks[i] ]
}

# Define Performance variables and classes, and read the files into R.
Performance_Variables = c("LOAN_ID", "Monthly.Rpt.Prd", "Servicer.Name", "LAST_RT", "LAST_UPB", "Loan.Age", "Months.To.Legal.Mat"
                        , "Adj.Month.To.Mat", "Maturity.Date", "MSA", "Delq.Status", "MOD_FLAG", "Zero.Bal.Code", 
                        "ZB_DTE", "LPI_DTE", "FCC_DTE","DISP_DT", "FCC_COST", "PP_COST", "AR_COST", "IE_COST", "TAX_COST", "NS_PROCS",
                       "CE_PROCS", "RMW_PROCS", "O_PROCS", "NON_INT_UPB", "PRIN_FORG_UPB")

Performance_ColClasses = c("character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
                         "character", "character", "character", "character", "character", "character", "character", "character",
                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

Performance <- list.files(data_location, pattern = glob2rx("*Performance*txt"), full.names=TRUE)

# Read and Process the Performance loan id data (1st column)
Data_P 		<- fread( Performance[k], sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE, select=1 )

# Get the row numbers for the unique loan ID splits
ending_ids	<- sapply( unique_IDs_split, function(x) tail(x,1) )
split_id_rows 	<- Data_P[,.N, by=V1]
split_id_rows	<- split_id_rows[, end:= cumsum(N)][, N := NULL]
split_id_rows	<- subset( split_id_rows, V1 %in% ending_ids )
split_id_rows[1,"start"] <- 1
for( i in 2:num_batches )
{
  split_id_rows[i,"start"] 	<- split_id_rows[i-1,end] + 1 
}
split_id_rows	<- split_id_rows[, nrows:= end - start + 1 ]
rm(Data_P)
gc()

# Split the Performance data into batches based on the loan ids to help with memory management
for( split_num in 1:num_batches )
{
  # Read and Process Performance data
  Data_P	<-  fread( Performance[k], sep = "|", colClasses = Performance_ColClasses, showProgress=FALSE, 
		  	nrows=split_id_rows[split_num,nrows], skip=(split_id_rows[split_num,start]-1) )
  setnames(Data_P, Performance_Variables)

  # Convert character variables to Date type
  Data_P[, "Monthly.Rpt.Prd":= as.Date(Monthly.Rpt.Prd, "%m/%d/%Y") ]
  Data_P[, "DISP_DT":= as.Date(DISP_DT, "%m/%d/%Y") ]
  Data_P[, "FCC_DTE":= as.Date(FCC_DTE, "%m/%d/%Y") ]

  # Sort data by Loan ID and Monthly Reporting Period
  setorderv(Data_P, c("LOAN_ID", "Monthly.Rpt.Prd"))
  setkey(Data_P, "LOAN_ID")

  # Standardize Delinquency Status Codes
  Data_P[, "Delq.Status":= as.numeric( ifelse(Delq.Status=="X", "999", Delq.Status) ) ]

  # Add Original Rate from the Acquisitions Files
  Data_P[Data_A, ORIG_RT:=i.ORIG_RT, allow.cartesian=TRUE]

  # Apply function to backfill missing current UPBs and NON_INT_UPB
  Data_P[, c("LAST_UPB", "NON_INT_UPB") :=list(na.lomf(LAST_UPB), na.lomf(NON_INT_UPB)), by = "LOAN_ID"]


  Data_P[, c("MODTRM_CHNG", "NON_INT_UPB", "PRIN_FORG_UPB", "MODUPB_CHNG"):= list(max(ifelse(length(unique(Maturity.Date))>1 & MOD_FLAG =="Y", 1, 0), 0, na.rm = TRUE),
                                                                 -1*NON_INT_UPB,
                                                                 -1*PRIN_FORG_UPB,
                                                                 max(ifelse(!is.na(LAST_UPB) & !is.na(shift(LAST_UPB)) & MOD_FLAG =="Y" & LAST_UPB>shift(LAST_UPB), 1, 0), 0, na.rm = TRUE)), by = "LOAN_ID"]

  Data_P[, Fin_UPB := rowSums(.SD, na.rm = TRUE), .SDcols = c("LAST_UPB", "NON_INT_UPB", "PRIN_FORG_UPB")]

  Data_P[, c("modir_cost", "modfb_cost", "modfg_cost" ) := list(ifelse(MOD_FLAG =="Y", ((ORIG_RT - LAST_RT) / 1200) * LAST_UPB, 0),
                                                              ifelse(MOD_FLAG =="Y" & !is.na(NON_INT_UPB), -1*(LAST_RT / 1200) * NON_INT_UPB, 0),
                                                              ((-1*min(PRIN_FORG_UPB,0, na.rm = TRUE)) )), by = "LOAN_ID" ]
  Data_P[, c("C_modir_cost", "C_modfb_cost"):=list(cumsum(modir_cost),
                                                 cumsum(modfb_cost)), by = "LOAN_ID"]

  # Count the number of months a loan is active 
  Data_P[,Count:=1:.N, by="LOAN_ID"]

  # Obtain the date of the first time each loan was modified
  FMOD_DTE = Data_P[, .SD[MOD_FLAG =="Y"][,c("FMOD_DTE", "FMOD_UPB"):=list(Monthly.Rpt.Prd, LAST_UPB)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "FMOD_DTE", "FMOD_UPB"), with = FALSE, drop = FALSE]

  # Obtain the date and UPB of each loan's first credit event (i.e. 180 days SDQ, or Foreclosure or Default)
  First_CE = Data_P[, .SD[Zero.Bal.Code =="03" | Zero.Bal.Code =="09" 
                        | (Delq.Status<999 & Delq.Status>= 6)][,c("FCE_DTE", "FCE_UPB", "SPDelq1", "CountFC")
                                                               := list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "SPDelq1", "FCE_DTE", "FCE_UPB", "CountFC"), with = FALSE, drop = FALSE]

  # Obtain the date and UPB of each loan becoming 180 days delinquent 
  First_D180 = Data_P[, .SD[Delq.Status<999 & Delq.Status >=6][,c("F180_DTE", "F180_UPB", "SPDelq2", "CountF1"):= 
                                                               list(Monthly.Rpt.Prd, LAST_UPB, Delq.Status, Count)]][, .SD[1], by = "LOAN_ID"][,c("LOAN_ID", "F180_DTE", "F180_UPB", "SPDelq2", "CountF1"), with = FALSE, drop = FALSE]

  # Summarize Perfomance data by keeping only the last row of a loan's activity
  Data_P<-Data_P[, .SD[.N], by ="LOAN_ID"]

  # Define the last status of a loan and calculate the months between Last Paid Installment and Disposition date (for Lost Interest calculation)  
  Data_P[, c("LAST_STAT", "lpi2disp", "zb2disp"):= 
         list(ifelse(Zero.Bal.Code=='01','P',ifelse(Zero.Bal.Code=='03','S', ifelse(Zero.Bal.Code=='06', 'R', ifelse(Zero.Bal.Code=='09', 'F', ifelse(Delq.Status=='999','X',ifelse(Delq.Status >9, '9', ifelse(Delq.Status==0, 'C', as.character(Delq.Status)))))))),
            ifelse(Data_P$LPI_DTE!="" & !(is.na(Data_P$DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(LPI_DTE, "%m/%d/%Y")))*12+month(DISP_DT)-month(as.yearmon(LPI_DTE, "%m/%d/%Y"))), 0),
            ifelse(!(is.na(Data_P$ZB_DTE)) & !(is.na(Data_P$DISP_DT)),as.numeric((year(DISP_DT)-year(as.yearmon(ZB_DTE, "%m/%Y")))*12+month(DISP_DT)-month(as.yearmon(ZB_DTE, "%m/%Y"))), 0)
        )]

  # Calculate Interest Cost, total expenses and total proceeds
  Data_P[, c("INT_COST","total_expense", "total_proceeds") := 
       list(ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)),Fin_UPB *(((LAST_RT/100) - .0035)/12)*lpi2disp,0),
            ifelse((LAST_STAT =="F" | LAST_STAT =="S"),rowSums(Data_P[, list(FCC_COST,PP_COST,AR_COST,TAX_COST,IE_COST)], na.rm = TRUE),0),
            ifelse((LAST_STAT =="F" | LAST_STAT =="S"),(-1*rowSums(Data_P[, list(NS_PROCS,CE_PROCS,RMW_PROCS,O_PROCS)], na.rm = TRUE)),0))]

  # Calculate Net Loss, Net Severity, Total Costs, Total Proceeds, and Total Liquidation Expenses.  Define Last Date variable.
  Data_P[,c("NET_LOSS","NET_SEV", "Total_Cost", "Tot_Procs", "Tot_Liq_Ex", "LAST_DTE"):=
                list(ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)),rowSums(Data_P[, list(LAST_UPB,INT_COST,total_expense,total_proceeds)], na.rm=TRUE),0),
                     ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT)),(rowSums(Data_P[, list(LAST_UPB,INT_COST,total_expense,total_proceeds)], na.rm=TRUE)/LAST_UPB),0),
                     ifelse((LAST_STAT =="F" | LAST_STAT =="S"),rowSums(Data_P[, list(LAST_UPB, INT_COST,FCC_COST,PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE),0), 
                     ifelse((LAST_STAT =="F" | LAST_STAT =="S"),rowSums(Data_P[, list(NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS)], na.rm = TRUE),0),
                     ifelse((LAST_STAT =="F" | LAST_STAT =="S"),rowSums(Data_P[, list(FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST)], na.rm = TRUE),0),
                     as.Date(ifelse(!(is.na(Data_P$DISP_DT)), Data_P$DISP_DT, Data_P$Monthly.Rpt.Prd)))]


  # Merge new fields with full performance dataset to capture information on First Modification, First Credit Event, and First Default.
  Data_P[FMOD_DTE, c("FMOD_DTE", "FMOD_UPB"):=list(i.FMOD_DTE, i.FMOD_UPB)]
  Data_P[First_CE, c("FCE_DTE", "FCE_UPB", "SPDelq1", "CountFC"):=list(i.FCE_DTE, i.FCE_UPB, i.SPDelq1, i.CountFC)]
  Data_P[First_D180, c("F180_DTE", "F180_UPB", "SPDelq2", "CountF1"):=list(i.F180_DTE, i.F180_UPB, i.SPDelq2, i.CountF1)]

  # Delete Performance variables that are not needed.
  Data_P[, c("Count", "Monthly.Rpt.Prd", "ZB_DTE", "ORIG_RT", "Servicer.Name", "Loan.Age", "Months.To.Legal.Mat", "Adj.Month.To.Mat", "Maturity.Date", "Delq.Status","total_expense", "total_proceeds", "lpi2disp"):=NULL]

  # Remove not-needed data from R environment.
  rm("First_D180", "First_CE", "FMOD_DTE")


  # Merge together full Acquisition and Performance files.
  Combined_Data = as.data.table(merge(Data_A, Data_P, by.x = "LOAN_ID", by.y = "LOAN_ID", all = TRUE))

  # Create Vintage Year & Activity Year Attributes, set missing F180_UPB and FCE_UPB equal to ORIG_AMT if the loan goes to delinquency during the 
  # first six month of loan activity.
  Combined_Data[,c("VinYr", "ActYr", "DispYr", "F180_UPB", "FCE_UPB") :=list(format(as.yearmon(ORIG_DTE, format="%m/%Y"), "%Y"),
                                                                           format(as.yearmon(LAST_DTE, format="%m/%Y"), "%Y"),
                                                                           ifelse(!(is.na(DISP_DT)), format(as.yearmon(DISP_DT, format="%m/%Y"), "%Y"), 'NO DISP_DT'),
                                                                           ifelse((SPDelq2==6 & is.na(F180_UPB) & CountF1<=6), ORIG_AMT, 
                                                                                  ifelse(!(is.na(F180_UPB)),F180_UPB ,0)), 
                                                                           ifelse((SPDelq1==6 & CountFC <=6 & is.na(FCE_UPB)), ORIG_AMT, 
                                                                                  ifelse(!(is.na(FCE_UPB)),FCE_UPB ,0)))]

  # Calculate Modification Costs when loans default
  Combined_Data[,c("MODIR_COST","MODFB_COST"):=
         list((ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT) & MOD_FLAG =="Y"),zb2disp*((ORIG_RT - LAST_RT) / 1200) * LAST_UPB, 0))+C_modir_cost,
              (ifelse(((LAST_STAT =="F" | LAST_STAT =="S")& !is.na(DISP_DT) & !is.na(NON_INT_UPB) & MOD_FLAG =="Y"),zb2disp*(LAST_RT / 1200) * (-1*NON_INT_UPB), 0))+C_modfb_cost)]

  Combined_Data[, MODTOT_COST :=rowSums(.SD, na.rm = TRUE), .SDcols = c("modfg_cost", "MODIR_COST","MODFB_COST")]
  Combined_Data[,c("SPDelq1","SPDelq2", "CountF1", "CountFC", "modir_cost", "modfb_cost"):=NULL]

  # Save the Combined Data
  save(Combined_Data, file=file.path( RData_location, paste( "FNMA_Performance_Data", k, "_", split_num, ".RData", sep="" ) ) )

  # Delete the performance/combined data to save memory
  rm("Data_P", "Combined_Data")
  gc()

} # end for

return(NULL)

} # end for each

####################################################################
# End of Data Preperation Step
####################################################################

