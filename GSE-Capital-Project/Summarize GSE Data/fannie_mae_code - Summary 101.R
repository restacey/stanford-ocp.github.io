# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "data.table" version 1.9.6
# package "reshape2" version 1.2.1
# package "XLConnect" version 0.2-12

# This program will download from the internet and install the latest version of the above packages If they are not installed in your R environment. It is necessary to 
# have internet connection to download these packages. 


# If for any reason this program fails to run, please make sure that the above packages are installed, check the verion of the packages and 
# make sure the functions called in this program are still in use and are compatible with the Operating System you are using.

# A step-by-step description is provided throughout this code.

#######################################################################################################################################

# Load Necessary Packages for this analysis

# Note: Need to have Java installed for XLConnect to work
if (!(require(data.table))) install.packages ("data.table")
if (!(require(XLConnect))) install.packages ("XLConnect")
if (!(require(reshape2))) install.packages ("reshape2")

# Turn off scientific notation to prevent UPB round
options(scipen=999)


####################################################################
# Below various summary statistics are calculated and outputed to an .XLSX file. We use the XLConnect package to write the summary statistics to the .xlsx file.
# Summary statistics will be outputted as separate tabs in the xlsx file. The file will be saved as "Summary_File_101.xlsx" in the "Results" folder. 
# If the folder does not already exist, then it will be created. The "fannie_mae_code - Performance Data.r" code must be run before running this code. 
# This code requires the Performance Data to be in the RData folder. The Performance data will be combined into a single data table and saved in the "Results"
# folder as "FNMA_Performance_Data_101.RData"
####################################################################


# Set the path to where the performance RData is located (from the "fannie_mae_code - Performance Data.r" code)
file_location	<- "C:/Users/robert/Desktop/Fannie Mae Loan Level Data/Loan Acquisition and Performance Data"
RData_location	<- file.path( file_location, "RData" )
Result_location	<- file.path( file_location, "Results" )

# Create the output folders if they do not already exist
dir.create( Result_location, showWarnings = FALSE )


####################################################################
# Start of Part 1; Combine the Performance Data into a Single Data Table
####################################################################

fnames			<- list.files(RData_location, pattern = glob2rx("*Performance*RData"), full.names=TRUE)
n_files			<- length(fnames)

# Store the columns used in the summary analysis
used_cols <- c( "AR_COST", "CE_PROCS", "CSCORE_B", "CSCORE_C", "CSCORE_MN", "DISP_DT", "DispYr", "DTI",
		"FCC_COST", "IE_COST", "INT_COST", "LAST_STAT", "LAST_UPB", "MOD_FLAG", "NET_LOSS", "NS_PROCS", 
		"O_PROCS", "OCC_STAT", "OCLTV", "OLTV", "ORIG_AMT", "ORIG_RT", "PP_COST", "PURPOSE", "RMW_PROCS",
		"TAX_COST", "Tot_Liq_Ex", "Tot_Procs", "Total_Cost", "VinYr" )
Combined_Data_All	<- vector( mode="list", length=n_files )
for( i in 1:n_files )
{
  load( fnames[i] )
  Combined_Data			<- Combined_Data[, .SD, .SDcols=used_cols]

  # Subset to the data with a LAST_STAT value
  # Note: This seems to be what Fannie Mae did (Will cause the loan counts to match)
  Combined_Data			<- subset( Combined_Data, !is.na(LAST_STAT) )
  Combined_Data_All[[i]]	<- Combined_Data

} # end for

# Combined all of the data into a single data table
rm( Combined_Data ); gc()
Combined_Data		<- rbindlist( Combined_Data_All, fill=TRUE )

# Save a Copy to disk
save( Combined_Data, file=file.path( Result_location, "FNMA_Performance_Data_101.RData" ) )


# Remove all objects created besides the final data set.
rm(list= ls()[!(ls() %in% c('Combined_Data', 'Result_location'))])



####################################################################
# Start of Part 2; Summary Statistics Step
####################################################################

  # Create the output file
  Charts	<- loadWorkbook( file.path( Result_location, "Summary_File_101.xlsx" ), create = TRUE)

  # Create buckets for continuous attributes, Risk Flag, and group number of borrowers
  # Columns: CSCORE_MN
  Combined_Data[,FicoBkt
              :=as.character(cut(CSCORE_MN, breaks = c(-Inf, 0, 620, 660, 700, 740, 780, Inf), 
                                      labels = c('NA','[0-620)', '[620-660)', '[660-700)', '[700-740)', '[740-780)', '[780+)'), 
                                      right = FALSE, ordered = TRUE))]

  # Create 'Missing' buckets for continuous attributes
  Combined_Data[, FicoBkt:= ifelse( is.na(FicoBkt), 'MissingFICO', FicoBkt ) ]

  # The following section will produce tables that will help users tie out their loan counts to the loan counts in the webinar
  # Loan counts cut by origination vintage and purpose
  # Columns: PURPOSE, VinYr
  Vint.REFI.Counts	<- as.data.frame(addmargins(xtabs(~PURPOSE+VinYr, data=Combined_Data)))
  Vint.REFI.Counts	<- dcast(Vint.REFI.Counts,PURPOSE~VinYr,value.var = "Freq")
  createSheet(Charts, name = "Vint.REFI.Counts")
  writeWorksheet(Charts, Vint.REFI.Counts, sheet = "Vint.REFI.Counts", startRow = 1, startCol = 1)

  # Loan counts cut by origination vintage and occupancy
  # Columns: OCC_STAT, VinYr
  Vint.OCC.Counts	<- as.data.frame(addmargins(xtabs(~OCC_STAT+VinYr, data=Combined_Data)))
  Vint.OCC.Counts	<- dcast(Vint.OCC.Counts,OCC_STAT~VinYr,value.var = "Freq")
  createSheet(Charts, name = "Vint.OCC.Counts")
  writeWorksheet(Charts, Vint.OCC.Counts, sheet = "Vint.OCC.Counts", startRow = 1, startCol = 1)

  # Loan counts cut by last_status
  # Columns: LAST_STAT
  Vint.LAST_STAT.Counts	<- as.data.frame(addmargins(xtabs(~LAST_STAT, data=Combined_Data)))
  createSheet(Charts, name = "Vint.LAST_STAT.Counts")
  writeWorksheet(Charts, Vint.LAST_STAT.Counts, sheet = "Vint.LAST_STAT.Counts", startRow = 1, startCol = 1)

  # Summary Stats for Fico, Original Amount and OLTV
  # Columns: CSCORE_MN, OLTV, ORIG_AMT
  Summary		<- as.data.frame(unstack(as.data.frame(summary(Combined_Data[, list(CSCORE_MN, OLTV, ORIG_AMT)])), Freq~Var2))
  names(Summary)	<- c("CSCORE_MN", "OLTV", "ORIG_AMT")
  createSheet(Charts, name = "Summary Stats")
  writeWorksheet(Charts, Summary, sheet = "Summary Stats", startRow = 1, startCol = 1)

  # Loan counts by FICO bucket and origination vintage
  # Columns: FicoBkt (CSCORE_MN), VinYr
  Vint.Fico.Counts	<- as.data.frame(addmargins(xtabs(~FicoBkt+VinYr, data=Combined_Data)))
  Vint.Fico.Counts	<- dcast(Vint.Fico.Counts,FicoBkt~VinYr,value.var = "Freq")
  createSheet(Charts, name = "Vint.Fico.Counts")
  writeWorksheet(Charts, Vint.Fico.Counts, sheet = "Vint.Fico.Counts", startRow = 1, startCol = 1)


  # Acquisition Summary Statistics by Vintage
  # Columns: ORIG_AMT, CSCORE_B, CSCORE_C, OLTV, OCLTV, DTI, ORIG_RT
  Aqsn.Stat1		<- setorder(Combined_Data[, list(
    				"Loan Count"= .N,
    				"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
    				"Avg. Orig UPB($)"= round(mean(ORIG_AMT, na.rm = TRUE)),
    				"Borrower Credit Score"= round(weighted.mean(CSCORE_B, ORIG_AMT, na.rm=TRUE),0),
    				"Co-Borrower Credit Score"= round(weighted.mean(CSCORE_C, ORIG_AMT, na.rm=TRUE),0),
    				"LTV Ratio"= sprintf("%.3f",weighted.mean(OLTV, ORIG_AMT, na.rm=TRUE)),
    				"CLTV Ratio"= sprintf("%.3f",weighted.mean(OCLTV, ORIG_AMT, na.rm=TRUE)),
    				"DTI"= sprintf("%.3f",weighted.mean(DTI, ORIG_AMT, na.rm=TRUE)),
    				"Note Rate"= sprintf("%.3f",weighted.mean(ORIG_RT, ORIG_AMT, na.rm=TRUE))), by=list(Vintage=VinYr)], "Vintage")

  # Acquisition Stat Totals
  # Columns: ORIG_AMT, CSCORE_B, CSCORE_C, OLTV, OCLTV, DTI, ORIG_RT
  Aqsn.Stat2	<- Combined_Data[, list(
  				Vintage= "Total",
  				"Loan Count"= .N,
  				"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
  				"Avg. Orig UPB($)"= round(mean(ORIG_AMT, na.rm = TRUE)),
  				"Borrower Credit Score"= round(weighted.mean(CSCORE_B, ORIG_AMT, na.rm=TRUE),0),
  				"Co-Borrower Credit Score"= round(weighted.mean(CSCORE_C, ORIG_AMT, na.rm=TRUE),0),
  				"LTV Ratio"= sprintf("%.3f",weighted.mean(OLTV, ORIG_AMT, na.rm=TRUE)),
  				"CLTV Ratio"= sprintf("%.3f",weighted.mean(OCLTV, ORIG_AMT, na.rm=TRUE)),
  				"DTI"= sprintf("%.3f",weighted.mean(DTI, ORIG_AMT, na.rm=TRUE)),
  				"Note Rate"= sprintf("%.3f",weighted.mean(ORIG_RT, ORIG_AMT, na.rm=TRUE)))]

  # Merge Totals with breakout by Vintage for Full Acquisition Statistics Table
  Aqsn.Stat	<- rbind(Aqsn.Stat1,Aqsn.Stat2)
  createSheet(Charts, name = "Aqsn.Stat")
  writeWorksheet(Charts, Aqsn.Stat, sheet = "Aqsn.Stat", startRow = 1, startCol = 1)

  rm(Aqsn.Stat1, Aqsn.Stat2)

  # Performance Loan Counts by Vintage
  # Columns: ORIG_AMT, LAST_STAT, LAST_UPB, MOD_FLAG, DISP_DT, NET_LOSS, VinYr
  Perf.Stat1	<- setorder(Combined_Data[, list(
  			"Loan Count"= .N,
  			"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
  			"Loan Count (Active)"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), 1, 0), na.rm=TRUE),
  			"Active UPB"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), LAST_UPB, 0), na.rm=TRUE),
  			"Prepaid"= sum(ifelse(LAST_STAT =="P", 1, 0), na.rm=TRUE),
  			"Repurchased"= sum(ifelse(LAST_STAT =="R", 1, 0), na.rm=TRUE),
  			"Alternative Disposition"= sum(ifelse(LAST_STAT %chin% c("S","999"), 1, 0), na.rm=TRUE),  
  			"REO Disposition"= sum(ifelse(LAST_STAT=="F", 1, 0), na.rm=TRUE),
  			"Modified"= sum(ifelse(MOD_FLAG =="Y", 1, 0), na.rm=TRUE),
  			"Default UPB"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),LAST_UPB, 0) , na.rm=TRUE),  
  			"Net Loss Rate"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),NET_LOSS, 0), na.rm = TRUE)/sum(ORIG_AMT, na.rm = TRUE))
  			, by=list(Vintage=VinYr)], "Vintage")

  # Performance Loan Count Totals
  # Columns: ORIG_AMT, LAST_STAT, LAST_UPB, MOD_FLAG, DISP_DT, NET_LOSS
  Perf.Stat2	<- Combined_Data[, list(
  			Vintage= "Total",
  			"Loan Count"= .N,
  			"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
  			"Loan Count (Active)"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), 1, 0), na.rm=TRUE),
  			"Active UPB"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), LAST_UPB, 0), na.rm=TRUE),
  			"Prepaid"= sum(ifelse(LAST_STAT =="P", 1, 0), na.rm=TRUE), 
  			"Repurchased"= sum(ifelse(LAST_STAT =="R", 1, 0), na.rm=TRUE),
  			"Alternative Disposition"= sum(ifelse(LAST_STAT %chin% c("S","999"), 1, 0), na.rm=TRUE),
  			"REO Disposition"= sum(ifelse(LAST_STAT=="F", 1, 0), na.rm=TRUE),    
  			"Modified"= sum(ifelse(MOD_FLAG =="Y", 1, 0), na.rm=TRUE),
  			"Default UPB"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),LAST_UPB, 0) , na.rm=TRUE),
  			"Net Loss Rate"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),NET_LOSS, 0), na.rm = TRUE)/sum(ORIG_AMT, na.rm = TRUE))]

  # Merge Totals with breakout by Vintage for Full Performance Statistics Table of Loan Counts
  Perf.Stat	<- rbind(Perf.Stat1, Perf.Stat2)
  createSheet(Charts, name = "Perf.Stat.Counts")
  writeWorksheet(Charts, Perf.Stat, sheet = "Perf.Stat.Counts", startRow = 1, startCol = 1)

  rm(Perf.Stat1, Perf.Stat2)

  # Performance UPB broken out by Vintage
  # Columns: ORIG_AMT, LAST_STAT, LAST_UPB, MOD_FLAG, DISP_DT, NET_LOSS, VinYr
  Perf.Stat.Sums1 <- setorder(Combined_Data[, list(
  			"Loan Count"= .N,
  			"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
  			"Active UPB"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), LAST_UPB, 0), na.rm=TRUE),
  			"Prepaid UPB"= sum(ifelse(LAST_STAT =="P", LAST_UPB, 0), na.rm=TRUE),
  			"REO Disposition UPB"= sum(ifelse(LAST_STAT=="F", LAST_UPB, 0), na.rm=TRUE),
  			"Alternative Disposition"= sum(ifelse(LAST_STAT %chin% c("S","999"), LAST_UPB, 0), na.rm=TRUE),
  			"Repurchased UPB"= sum(ifelse(LAST_STAT =="R", LAST_UPB, 0), na.rm=TRUE),
  			"Modified UPB"= sum(ifelse(MOD_FLAG =="Y", LAST_UPB, 0), na.rm=TRUE),
  			"Default UPB"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),LAST_UPB, 0) , na.rm=TRUE),
  			"Net Loss Rate"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),NET_LOSS, 0), na.rm = TRUE)/sum(ORIG_AMT, na.rm = TRUE))
  			, by=list(Vintage=VinYr)], "Vintage")

  # Performance UPB Totals
  # Columns: ORIG_AMT, LAST_STAT, LAST_UPB, MOD_FLAG, DISP_DT, NET_LOSS
  Perf.Stat.Sums2 <- Combined_Data[, list(
  			Vintage= "Total",
  			"Loan Count"= .N,
  			"Total Orig. UPB"= sum(ORIG_AMT, na.rm = TRUE),
  			"Active UPB"= sum(ifelse(LAST_STAT %chin% c("C", "1", "2", "3", "4", "5", "6", "7", "8", "9"), LAST_UPB, 0), na.rm=TRUE),
  			"Prepaid UPB"= sum(ifelse(LAST_STAT =="P", LAST_UPB, 0), na.rm=TRUE),
  			"REO Disposition UPB"= sum(ifelse(LAST_STAT=="F", LAST_UPB, 0), na.rm=TRUE),
  			"Alternative Disposition"= sum(ifelse(LAST_STAT %chin% c("S","999"), LAST_UPB, 0), na.rm=TRUE),
  			"Repurchased UPB"= sum(ifelse(LAST_STAT =="R", LAST_UPB, 0), na.rm=TRUE),
  			"Modified UPB"= sum(ifelse(MOD_FLAG =="Y", LAST_UPB, 0), na.rm=TRUE),
  			"Default UPB"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),LAST_UPB, 0) , na.rm=TRUE),
  			"Net Loss Rate"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),NET_LOSS, 0), na.rm = TRUE)/sum(ORIG_AMT, na.rm = TRUE))]

  # Merge Totals with breakout by Vintage for Full Performance Statistics Table of UPB amounts
  Perf.Stat.Sums <- rbind(Perf.Stat.Sums1, Perf.Stat.Sums2)
  createSheet(Charts, name = "Perf.Stat.Sums")
  writeWorksheet(Charts, Perf.Stat.Sums, sheet = "Perf.Stat.Sums", startRow = 1, startCol = 1)

  rm(Perf.Stat.Sums1, Perf.Stat.Sums2)

  # Historical Net Loss Statistics by Vintage
  # Columns: LAST_STAT, DISP_DT, LAST_UPB, ORIG_AMT, INT_COST, Tot_Liq_Ex, FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST, Total_Cost, NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS, Tot_Procs, NET_LOSS, VinYr
  HistNetLoss1a <- setorder(Combined_Data[,list(
  			"Loan Count"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), 1, 0)),
  			"UPB for Liquiditions"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE),
  			"Default UPB % of Orig. UPB"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm=TRUE)/sum(ORIG_AMT, na.rm = TRUE))*100),  
  			"Interest on Delinquent Loans"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), INT_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Liquidition Exp."= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Liq_Ex, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Foreclosure Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), FCC_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Prop.Pres. Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), PP_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Asset Recovery Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), AR_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Miscellaneous Holding Expenses And Credits"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), IE_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Associated Taxes"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), TAX_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Total_Cost, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),  
  			"Sales Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NS_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Credit Enhancement Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), CE_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Repurchase/Make Whole Proceeds"=sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), RMW_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Other Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), O_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Procs, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Severity"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Net Loss"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE))
  			, by=list(Vintage=VinYr)], "Vintage")

  # Historical Loss Totals
  # Columns: LAST_STAT, DISP_DT, LAST_UPB, ORIG_AMT, INT_COST, Tot_Liq_Ex, FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST, Total_Cost, NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS, Tot_Procs, NET_LOSS
  HistNetLoss1b <- setorder(Combined_Data[,list(
  			"Vintage"= "Total",
  			"Loan Count"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), 1, 0)),  
  			"UPB for Liquiditions"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE),
  			"Default UPB % of Orig. UPB"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm=TRUE)/sum(ORIG_AMT, na.rm = TRUE))*100),  
  			"Interest on Delinquent Loans"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), INT_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Liquidition Exp."= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Liq_Ex, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Foreclosure Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), FCC_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Prop.Pres. Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), PP_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Asset Recovery Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), AR_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Miscellaneous Holding Expenses And Credits"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), IE_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Associated Taxes"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), TAX_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Total_Cost, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),  
  			"Sales Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NS_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Credit Enhancement Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), CE_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Repurchase/Make Whole Proceeds"=sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), RMW_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Other Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), O_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Procs, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Severity"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Net Loss"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE))], "Vintage")

  # Merge Totals with breakout by Vintage for Full Historical Net Loss Table 
  HistNetLosso	<- rbind(HistNetLoss1a,HistNetLoss1b)
  HistNetLoss2	<- as.data.frame(t(as.data.frame(HistNetLosso)))
  colnames(HistNetLoss2) <- unique(HistNetLosso$Vintage)
  HistNetLoss2	<- HistNetLoss2[2:nrow(HistNetLoss2),]

  createSheet(Charts, name = "Orig.Loss.Stat")
  writeWorksheet(Charts, HistNetLoss2, sheet = "Orig.Loss.Stat", startRow = 1, startCol = 1, rownames="Row Names")

  # Historical Net Loss Statistics by Disposition Year
  # Columns: LAST_STAT, DISP_DT, LAST_UPB, ORIG_AMT, INT_COST, Tot_Liq_Ex, FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST, Total_Cost, NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS, Tot_Procs, NET_LOSS, DispYr
  HistNetLoss1c <- setorder(Combined_Data[,list(
  			"Loan Count"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), 1, 0)),
  			"UPB for Liquiditions"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE),
  			"Default UPB % of Orig. UPB"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm=TRUE)/sum(ORIG_AMT, na.rm = TRUE))*100),  
  			"Interest on Delinquent Loans"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), INT_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Liquidition Exp."= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Liq_Ex, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Foreclosure Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), FCC_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Prop.Pres. Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), PP_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Asset Recovery Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), AR_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Miscellaneous Holding Expenses And Credits"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), IE_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Associated Taxes"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), TAX_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Total_Cost, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),  
  			"Sales Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NS_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Credit Enhancement Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), CE_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Repurchase/Make Whole Proceeds"=sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), RMW_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Other Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), O_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Procs, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Severity"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Net Loss"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE))
  			, by=list(Disposition=DispYr)], "Disposition")

  # Historical Loss Totals
  # Columns: LAST_STAT, DISP_DT, LAST_UPB, ORIG_AMT, INT_COST, Tot_Liq_Ex, FCC_COST, PP_COST, AR_COST, IE_COST, TAX_COST, Total_Cost, NS_PROCS, CE_PROCS, RMW_PROCS, O_PROCS, Tot_Procs, NET_LOSS
  HistNetLoss1d <- setorder(Combined_Data[,list(
  			"Disposition"= "Total",
  			"Loan Count"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), 1, 0)),  
  			"UPB for Liquiditions"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE),
  			"Default UPB % of Orig. UPB"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm=TRUE)/sum(ORIG_AMT, na.rm = TRUE))*100),  
  			"Interest on Delinquent Loans"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), INT_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Liquidition Exp."= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Liq_Ex, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Foreclosure Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), FCC_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Prop.Pres. Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), PP_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Asset Recovery Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), AR_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Miscellaneous Holding Expenses And Credits"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), IE_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Associated Taxes"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), TAX_COST, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Costs"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Total_Cost, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),  
  			"Sales Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NS_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Credit Enhancement Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), CE_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Repurchase/Make Whole Proceeds"=sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), RMW_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Other Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), O_PROCS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Proceeds"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), Tot_Procs, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Severity"= sprintf("%.3f%%", (sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE)/sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), LAST_UPB, 0), na.rm = TRUE))*100),
  			"Total Net Loss"= sum(ifelse((LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))), NET_LOSS, 0), na.rm = TRUE))], "Disposition")

  # Merge Totals with breakout by Vintage for Full Historical Net Loss Table 
  HistNetLossd	<- rbind(HistNetLoss1c,HistNetLoss1d)
  HistNetLoss3	<- as.data.frame(t(as.data.frame(HistNetLossd)))
  colnames(HistNetLoss3) <- unique(HistNetLossd$Disposition)
  HistNetLoss3	<- HistNetLoss3[2:nrow(HistNetLoss3),]

  createSheet(Charts, name = "Disp.Loss.Stat")
  writeWorksheet(Charts, HistNetLoss3, sheet = "Disp.Loss.Stat", startRow = 1, startCol = 1, rownames="Row Names")

  # Save the .xlsx document of all tables
  saveWorkbook(Charts)


# Removing full dataset from R Environment
rm(list= ls()[!(ls() %in% c('Combined_Data'))])

####################################################################
# End of Part 2; Summary Statistics Step
####################################################################
