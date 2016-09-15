# This program is written in R programming language version ‘3.2.3’ 64-bit installed on a Linux server. "R is a free software environment for statistical computing and graphics" with 
# no guarantees. R compiles and runs on a wide variety of UNIX platforms, Windows and MacOS." To download a free copy of R visit "http://www.r-project.org/".
# In addition to base R, the following R packages were used in this analysis:
# package "data.table" version 1.9.6
# package "reshape2" version 1.4.1
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
# Summary statistics will be outputted as separate tabs in the xlsx file. The file will be saved as "Summary_File_102.xlsx" in the "Results" folder. 
# If the folder does not already exist, then it will be created. The "fannie_mae_code - Performance Data.r" code must be run before running this code. 
# This code requires the Performance Data to be in the RData folder. The Performance data will be combined into a single data table and saved in the "Results"
# folder as "FNMA_Performance_Data_102.RData"
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
used_cols 	<- c( "CSCORE_MN", "DISP_DT", "DTI", "LAST_STAT", "LAST_UPB", "NET_LOSS", "NUM_BO", 
		      "OCC_STAT", "OCLTV", "OLTV", "ORIG_AMT", "ORIG_DTE", "ORIG_RT", "PURPOSE", "VinYr" )
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
Combined_Data		<- rbindlist( Combined_Data_All, fill=TRUE )
rm( Combined_Data_All )
gc()

# Save a Copy to disk
save( Combined_Data, file=file.path( Result_location, "FNMA_Performance_Data_102.RData" ) )


# Remove all objects created besides the final data set.
rm(list= ls()[!(ls() %in% c('Combined_Data', 'Result_location'))])



####################################################################
# Start of Part 2; Summary Statistics Step
####################################################################


  # Create the output file
  Charts	<- loadWorkbook( file.path( Result_location, "Summary_File_102.xlsx" ), create = TRUE)

  # Calculate Spread at Origination (SATO)
  # Columns: ORIG_RT, ORIG_AMT, ORIG_DTE
  Vint.SATO1 	<- addmargins(xtabs(ORIG_RT*ORIG_AMT~ORIG_DTE, data=Combined_Data))
  Vint.SATO2 	<- addmargins(xtabs(ORIG_AMT~ORIG_DTE, data=Combined_Data))
  Vint.SATO	<- as.data.frame(Vint.SATO1/Vint.SATO2)
  colnames(Vint.SATO) <- c("ORIG_DTE","Avg.NoteRt")

  Combined_Data	<- as.data.table(merge(Combined_Data, Vint.SATO, by="ORIG_DTE"))
  Combined_Data[, SATO:= ORIG_RT - Avg.NoteRt ]

  # Create buckets for continuous attributes, Risk Flag, and group number of borrowers
  # Columns: NUM_BO, DTI, OCC_STAT, PURPOSE, OCLTV, OLTV, CSCORE_MN, ORIG_AMT
  Combined_Data[,c("RskFctrs", "OcltvBkt", "OltvBkt", "FicoBkt", "DtiBkt", "OrigAmtBkt","NumBoBkt", "SATOBkt")
              :=list((ifelse(NUM_BO=="1", 1, 0)+(ifelse(is.na(DTI), 1, ifelse(DTI>45, 1, 0)))+ifelse(OCC_STAT=="I", 1, 0)+ifelse(PURPOSE=="C", 1, 0)),
                     as.character(cut(OCLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),                                                                 
                     as.character(cut(OLTV, breaks = c(-Inf, 0, 60, 65, 70, 75, 80, 85, 90, 97, Inf),labels = c('NA', '(0-60]', '(60-65]', '(65-70]', '(70-75]', '(75-80]', '(80-85]', '(85-90]', '(90-97]', '(97+)'), right = TRUE, ordered = TRUE)),                                                                 
                     as.character(cut(CSCORE_MN, breaks = c(-Inf, 0, 620, 660, 700, 740, 780, Inf), labels = c('NA','[0-620)', '[620-660)', '[660-700)', '[700-740)', '[740-780)', '[780+)'), right = FALSE, ordered = TRUE)),
                     as.character(cut(DTI, breaks = c(-Inf, 0, 20, 30, 40, 45, Inf), labels = c('NA', '[0-20)', '[20-30)', '[30-40)', '[40-45)', '[45+)'), right = FALSE, ordered = TRUE)),
                     as.character(cut(ORIG_AMT, breaks = c(-Inf, 0, 85000, 110000, 125000, 150000, 175000, 200000, 417000, Inf),
                                      labels = c('NA', '[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)'), right = FALSE, ordered = TRUE)),
                     as.character(as.character(ifelse(NUM_BO=="","Missing", ifelse(!(NUM_BO %chin% c("1","2")), "3+", NUM_BO)))),
                     as.character(cut(SATO, breaks = c(-Inf, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, Inf), labels = c('NA', '(-, -2%]', '(-2%,-1.5%)', '[-1.5%,-.5%)', '[-.5%,0)', '[0,.5%)', '[.5%, 1%)', '[1%, 1.5%)', '[1.5%, 2%)', '[2%,+)'), right = FALSE, ordered = TRUE)))]

  # Create 'Missing' buckets for continuous attributes
  Combined_Data[, OcltvBkt:= ifelse( is.na(OcltvBkt), 'MissingOCLTV', OcltvBkt ) ]
  Combined_Data[, OltvBkt:=  ifelse( is.na(OltvBkt),   'MissingOLTV', OltvBkt ) ]
  Combined_Data[, FicoBkt:=  ifelse( is.na(FicoBkt),   'MissingFICO', FicoBkt ) ]
  Combined_Data[, DtiBkt:=   ifelse( is.na(DtiBkt),    'MissingDTI',  DtiBkt ) ]
  Combined_Data[, RskFctrs:= ifelse( is.na(RskFctrs),  0,  RskFctrs ) ]
  
  # For the following calculations we need subsets of the data.
  # Columns: VinYr, LAST_STAT, DISP_DT
  Combined_Data_Default	<- Combined_Data[(LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),]
  Combined_Data_Yrs	<- subset( Combined_Data, VinYr %in% unique( Combined_Data_Default$VinYr ) )

  # Remove the Combined Data from memory
  rm( Combined_Data )
  gc()


  # Create a subset of the dataset for the LTV/FICO tables
  # Columns: VinYr, LAST_STAT, DISP_DT
  Combined_Data_Default_2006 	<- Combined_Data_Default[(VinYr=="2006") & (LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),]
  Combined_Data_Yrs_2006 	<- Combined_Data_Yrs[(VinYr=="2006"),]

  # Create a subset for the 210 Cohort Analysis
  # Columns: VinYr, LAST_STAT, DISP_DT
  Combined_Data_Default_2007 	<- Combined_Data_Default[(VinYr=="2007") & (LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),]
  Combined_Data_Yrs_2007 	<- Combined_Data_Yrs[(VinYr=="2007"),]

  # Create a subset of the dataset for the SATO Tables
  # Columns: VinYr, LAST_STAT, DISP_DT
  Combined_Data_Default_2010 	<- Combined_Data_Default[(VinYr=="2010") & (LAST_STAT %chin% c("F", "S") & !(is.na(DISP_DT))),]
  Combined_Data_Yrs_2010 	<- Combined_Data_Yrs[(VinYr=="2010"),]


  


  # The following section will calculate the default, severity and loss rates accross various dimensions and write the results to an Excel workbook
  # XTab Default Rate by Vintage & Occupancy
  # Columns: LAST_UPB, OCC_STAT, VinYr, ORIG_AMT
  Vint.OCC.Def1		<- addmargins(xtabs(LAST_UPB~OCC_STAT+VinYr, data=Combined_Data_Default))
  Vint.OCC.Def2		<- addmargins(xtabs(ORIG_AMT~OCC_STAT+VinYr, data=Combined_Data_Yrs))
  Vint.OCC.Def		<- as.data.frame(Vint.OCC.Def1/Vint.OCC.Def2)
  Vint.OCC.Def		<- dcast(Vint.OCC.Def,OCC_STAT~VinYr,value.var = "Freq")
  Vint.OCC.Def$OCC_STAT	<- factor(Vint.OCC.Def$OCC_STAT, levels= c('P', 'S', 'I', 'Sum'))
  Vint.OCC.Def		<- with(Vint.OCC.Def, Vint.OCC.Def[order(OCC_STAT),])

  createSheet(Charts, name = "Vint.OCC")
  writeWorksheet(Charts, Vint.OCC.Def, sheet = "Vint.OCC", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & Occupancy
  # Columns: NET_LOSS, OCC_STAT, VinYr, LAST_UPB
  Vint.OCC.Sev1		<- addmargins(xtabs(NET_LOSS~OCC_STAT+VinYr, data=Combined_Data_Default))
  Vint.OCC.Sev2		<- addmargins(xtabs(LAST_UPB~OCC_STAT+VinYr, data=Combined_Data_Default))
  Vint.OCC.Sev		<- as.data.frame(Vint.OCC.Sev1/Vint.OCC.Sev2)
  Vint.OCC.Sev		<- dcast(Vint.OCC.Sev,OCC_STAT~VinYr,value.var = "Freq")
  Vint.OCC.Sev$OCC_STAT	<- factor(Vint.OCC.Sev$OCC_STAT, levels= c('P', 'S', 'I', 'Sum'))
  Vint.OCC.Sev		<- with(Vint.OCC.Sev, Vint.OCC.Sev[order(OCC_STAT),])

  writeWorksheet(Charts, Vint.OCC.Sev, sheet = "Vint.OCC", startRow = 7, startCol = 1)

  # XTab Loss Rate by Vintage & Occupancy
  # Columns: NET_LOSS, OCC_STAT, VinYr, ORIG_AMT
  Vint.OCC.Loss1	<- addmargins(xtabs(NET_LOSS~OCC_STAT+VinYr, data=Combined_Data_Default))
  Vint.OCC.Loss2	<- addmargins(xtabs(ORIG_AMT~OCC_STAT+VinYr, data=Combined_Data_Yrs))
  Vint.OCC.Loss		<- as.data.frame(Vint.OCC.Loss1/Vint.OCC.Loss2)
  Vint.OCC.Loss		<- dcast(Vint.OCC.Loss,OCC_STAT~VinYr,value.var = "Freq")
  Vint.OCC.Loss$OCC_STAT <- factor(Vint.OCC.Loss$OCC_STAT, levels= c('P', 'S', 'I', 'Sum'))
  Vint.OCC.Loss		<- with(Vint.OCC.Loss, Vint.OCC.Loss[order(OCC_STAT),])

  writeWorksheet(Charts, Vint.OCC.Loss, sheet = "Vint.OCC", startRow = 13, startCol = 1)

  # XTab Default Rate by CLTV & Occupancy for 2006 Vintage
  # Columns: LAST_UPB, OCC_STAT, ORIG_AMT
  LTV.OCC.Def1		<- addmargins(xtabs(LAST_UPB~OcltvBkt+OCC_STAT, data=Combined_Data_Default_2006))
  LTV.OCC.Def2		<- addmargins(xtabs(ORIG_AMT~OcltvBkt+OCC_STAT, data=Combined_Data_Yrs_2006))
  LTV.OCC.Def		<- as.data.frame(LTV.OCC.Def1/LTV.OCC.Def2)
  LTV.OCC.Def		<- dcast(LTV.OCC.Def,OCC_STAT~OcltvBkt,value.var = "Freq")
  LTV.OCC.Def$OCC_STAT	<- factor(LTV.OCC.Def$OCC_STAT, levels= c('P', 'S', 'I', 'Sum'))
  LTV.OCC.Def		<- with(LTV.OCC.Def, LTV.OCC.Def[order(OCC_STAT),])

  writeWorksheet(Charts, LTV.OCC.Def, sheet = "Vint.OCC", startRow = 19, startCol = 1)


  # XTab Default Rate by Vintage & Refinance Purpose
  # Columns: LAST_UPB, PURPOSE, VinYr, ORIG_AMT
  Vint.REFI.Def1	<- addmargins(xtabs(LAST_UPB~PURPOSE+VinYr, data=Combined_Data_Default))
  Vint.REFI.Def2	<- addmargins(xtabs(ORIG_AMT~PURPOSE+VinYr, data=Combined_Data_Yrs))
  Vint.REFI.Def		<- as.data.frame(Vint.REFI.Def1/Vint.REFI.Def2)
  Vint.REFI.Def		<- dcast(Vint.REFI.Def,PURPOSE~VinYr,value.var = "Freq")
  Vint.REFI.Def$PURPOSE	<- factor(Vint.REFI.Def$PURPOSE, levels= c('P', 'R', 'C', 'U', 'Sum'))
  Vint.REFI.Def		<- with(Vint.REFI.Def, Vint.REFI.Def[order(PURPOSE),])

  createSheet(Charts, name = "Vint.PURP")
  writeWorksheet(Charts, Vint.REFI.Def, sheet = "Vint.PURP", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & Refinance Purpose
  # Columns: NET_LOSS, PURPOSE, VinYr, LAST_UPB
  Vint.REFI.Sev1	<- addmargins(xtabs(NET_LOSS~PURPOSE+VinYr, data=Combined_Data_Default))
  Vint.REFI.Sev2	<- addmargins(xtabs(LAST_UPB~PURPOSE+VinYr, data=Combined_Data_Default))
  Vint.REFI.Sev		<- as.data.frame(Vint.REFI.Sev1/Vint.REFI.Sev2)
  Vint.REFI.Sev		<- dcast(Vint.REFI.Sev,PURPOSE~VinYr,value.var = "Freq")
  Vint.REFI.Sev$PURPOSE	<- factor(Vint.REFI.Sev$PURPOSE, levels= c('P', 'R', 'C', 'U', 'Sum'))
  Vint.REFI.Sev		<- with(Vint.REFI.Sev, Vint.REFI.Sev[order(PURPOSE),])

  writeWorksheet(Charts, Vint.REFI.Sev, sheet = "Vint.PURP", startRow = 8, startCol = 1)

  # XTab Loss Rate by Vintage & Refinance Purpose
  # Columns: NET_LOSS, PURPOSE, VinYr, ORIG_AMT
  Vint.REFI.Loss1	<- addmargins(xtabs(NET_LOSS~PURPOSE+VinYr, data=Combined_Data_Default))
  Vint.REFI.Loss2	<- addmargins(xtabs(ORIG_AMT~PURPOSE+VinYr, data=Combined_Data_Yrs))
  Vint.REFI.Loss	<- as.data.frame(Vint.REFI.Loss1/Vint.REFI.Loss2)
  Vint.REFI.Loss	<- dcast(Vint.REFI.Loss,PURPOSE~VinYr,value.var = "Freq")
  Vint.REFI.Loss$PURPOSE <- factor(Vint.REFI.Loss$PURPOSE, levels= c('P', 'R', 'C', 'U', 'Sum'))
  Vint.REFI.Loss	<- with(Vint.REFI.Loss, Vint.REFI.Loss[order(PURPOSE),])

  writeWorksheet(Charts, Vint.REFI.Loss, sheet = "Vint.PURP", startRow = 15, startCol = 1)

  # XTab Default Rate by CLTV & Occupancy for 2006 Vintage
  # Columns: LAST_UPB, PURPOSE, ORIG_AMT
  LTV.REFI.Def1		<- addmargins(xtabs(LAST_UPB~OcltvBkt+PURPOSE, data=Combined_Data_Default_2006))
  U 			<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Sum 			<- LTV.REFI.Def1[1:10,4]
  LTV.REFI.Def1		<- as.table(cbind(LTV.REFI.Def1[1:10,1:3], U, Sum))
  LTV.REFI.Def2		<- addmargins(xtabs(ORIG_AMT~OcltvBkt+PURPOSE, data=Combined_Data_Yrs_2006))
  LTV.REFI.Def		<- as.data.frame(LTV.REFI.Def1/LTV.REFI.Def2)
  colnames(LTV.REFI.Def) <- c("OcltvBkt","PURPOSE","Freq")
  LTV.REFI.Def		<- dcast(LTV.REFI.Def,PURPOSE~OcltvBkt,value.var = "Freq")
  LTV.REFI.Def$PURPOSE	<- factor(LTV.REFI.Def$PURPOSE, levels= c('P', 'R', 'C', 'U', 'Sum'))
  LTV.REFI.Def		<- with(LTV.REFI.Def, LTV.REFI.Def[order(PURPOSE),])

  writeWorksheet(Charts, LTV.REFI.Def, sheet = "Vint.PURP", startRow = 22, startCol = 1)


  # XTab Default Rate by Vintage & Number of Borrowers
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.NumBo.Def1	<- addmargins(xtabs(LAST_UPB~NumBoBkt+VinYr, data=Combined_Data_Default))
  Vint.NumBo.Def2	<- addmargins(xtabs(ORIG_AMT~NumBoBkt+VinYr, data=Combined_Data_Yrs))
  Vint.NumBo.Def	<- as.data.frame(Vint.NumBo.Def1/Vint.NumBo.Def2)
  Vint.NumBo.Def	<- dcast(Vint.NumBo.Def,NumBoBkt~VinYr,value.var = "Freq")

  createSheet(Charts, name = "Vint.NumBo")
  writeWorksheet(Charts, Vint.NumBo.Def, sheet = "Vint.NumBo", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & Number of Borrowers
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.NumBo.Sev1	<- addmargins(xtabs(NET_LOSS~NumBoBkt+VinYr, data=Combined_Data_Default))
  Vint.NumBo.Sev2	<- addmargins(xtabs(LAST_UPB~NumBoBkt+VinYr, data=Combined_Data_Default))
  Vint.NumBo.Sev	<- as.data.frame(Vint.NumBo.Sev1/Vint.NumBo.Sev2)
  Vint.NumBo.Sev	<- dcast(Vint.NumBo.Sev,NumBoBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.NumBo.Sev, sheet = "Vint.NumBo", startRow = 8, startCol = 1)

  # XTab Loss Rate by Vintage & Number of Borrowers
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.NumBo.Loss1	<- addmargins(xtabs(NET_LOSS~NumBoBkt+VinYr, data=Combined_Data_Default))
  Vint.NumBo.Loss2	<- addmargins(xtabs(ORIG_AMT~NumBoBkt+VinYr, data=Combined_Data_Yrs))
  Vint.NumBo.Loss	<- as.data.frame(Vint.NumBo.Loss1/Vint.NumBo.Loss2)
  Vint.NumBo.Loss	<- dcast(Vint.NumBo.Loss,NumBoBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.NumBo.Loss, sheet = "Vint.NumBo", startRow = 15, startCol = 1)


  # XTab Default Rate by Vintage & FICO
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.Fico.Def1	<- addmargins(xtabs(LAST_UPB~FicoBkt+VinYr, data=Combined_Data_Default))
  Vint.Fico.Def2	<- addmargins(xtabs(ORIG_AMT~FicoBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Fico.Def		<- as.data.frame(Vint.Fico.Def1/Vint.Fico.Def2)
  Vint.Fico.Def		<- dcast(Vint.Fico.Def,FicoBkt~VinYr,value.var = "Freq")
  Vint.Fico.Def$FicoBkt	<- factor(Vint.Fico.Def$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  Vint.Fico.Def		<- with(Vint.Fico.Def, Vint.Fico.Def[order(FicoBkt),])

  createSheet(Charts, name = "Vint.Fico")
  writeWorksheet(Charts, Vint.Fico.Def, sheet = "Vint.Fico", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & FICO
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.Fico.Sev1	<- addmargins(xtabs(NET_LOSS~FicoBkt+VinYr, data=Combined_Data_Default))
  Vint.Fico.Sev2	<- addmargins(xtabs(LAST_UPB~FicoBkt+VinYr, data=Combined_Data_Default))
  Vint.Fico.Sev		<- as.data.frame(Vint.Fico.Sev1/Vint.Fico.Sev2)
  Vint.Fico.Sev		<- dcast(Vint.Fico.Sev,FicoBkt~VinYr,value.var = "Freq")
  Vint.Fico.Sev$FicoBkt	<- factor(Vint.Fico.Sev$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  Vint.Fico.Sev		<- with(Vint.Fico.Sev, Vint.Fico.Sev[order(FicoBkt),])

  writeWorksheet(Charts, Vint.Fico.Sev, sheet = "Vint.Fico", startRow = 11, startCol = 1)

  # XTab Loss Rate by Vintage & FICO
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.Fico.Loss1	<- addmargins(xtabs(NET_LOSS~FicoBkt+VinYr, data=Combined_Data_Default))
  Vint.Fico.Loss2	<- addmargins(xtabs(ORIG_AMT~FicoBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Fico.Loss	<- as.data.frame(Vint.Fico.Loss1/Vint.Fico.Loss2)
  Vint.Fico.Loss	<- dcast(Vint.Fico.Loss,FicoBkt~VinYr,value.var = "Freq")
  Vint.Fico.Loss$FicoBkt <- factor(Vint.Fico.Loss$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  Vint.Fico.Loss	<- with(Vint.Fico.Loss, Vint.Fico.Loss[order(FicoBkt),])

  writeWorksheet(Charts, Vint.Fico.Loss, sheet = "Vint.Fico", startRow = 21, startCol = 1)


  # XTab Default Rate by Vintage & Original Loan Amount
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.OrigAmt.Def1	<- addmargins(xtabs(LAST_UPB~OrigAmtBkt+VinYr, data=Combined_Data_Default))
  Vint.OrigAmt.Def2	<- addmargins(xtabs(ORIG_AMT~OrigAmtBkt+VinYr, data=Combined_Data_Yrs))
  Vint.OrigAmt.Def	<- as.data.frame(Vint.OrigAmt.Def1/Vint.OrigAmt.Def2)
  Vint.OrigAmt.Def	<- dcast(Vint.OrigAmt.Def,OrigAmtBkt~VinYr,value.var = "Freq")
  Vint.OrigAmt.Def$OrigAmtBkt <- factor(Vint.OrigAmt.Def$OrigAmtBkt, levels= c('[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)', 'Sum'))
  Vint.OrigAmt.Def	<- with(Vint.OrigAmt.Def, Vint.OrigAmt.Def[order(OrigAmtBkt),])

  createSheet(Charts, name = "Vint.OrigAmt")
  writeWorksheet(Charts, Vint.OrigAmt.Def, sheet = "Vint.OrigAmt", startRow = 1, startCol = 1)

  # XTab Severity by Vintage &  Original Loan Amount
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.OrigAmt.Sev1	<- addmargins(xtabs(NET_LOSS~OrigAmtBkt+VinYr, data=Combined_Data_Default))
  Vint.OrigAmt.Sev2	<- addmargins(xtabs(LAST_UPB~OrigAmtBkt+VinYr, data=Combined_Data_Default))
  Vint.OrigAmt.Sev	<- as.data.frame(Vint.OrigAmt.Sev1/Vint.OrigAmt.Sev2)
  Vint.OrigAmt.Sev	<- dcast(Vint.OrigAmt.Sev,OrigAmtBkt~VinYr,value.var = "Freq")
  Vint.OrigAmt.Sev$OrigAmtBkt <- factor(Vint.OrigAmt.Sev$OrigAmtBkt, levels= c('[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)', 'Sum'))
  Vint.OrigAmt.Sev	<- with(Vint.OrigAmt.Sev, Vint.OrigAmt.Sev[order(OrigAmtBkt),])

  writeWorksheet(Charts, Vint.OrigAmt.Sev, sheet = "Vint.OrigAmt", startRow = 12, startCol = 1)

  # XTab Loss Rate by Vintage & Original Loan Amount
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.OrigAmt.Loss1	<- addmargins(xtabs(NET_LOSS~OrigAmtBkt+VinYr, data=Combined_Data_Default))
  Vint.OrigAmt.Loss2	<- addmargins(xtabs(ORIG_AMT~OrigAmtBkt+VinYr, data=Combined_Data_Yrs))
  Vint.OrigAmt.Loss	<- as.data.frame(Vint.OrigAmt.Loss1/Vint.OrigAmt.Loss2)
  Vint.OrigAmt.Loss	<- dcast(Vint.OrigAmt.Loss,OrigAmtBkt~VinYr,value.var = "Freq")
  Vint.OrigAmt.Loss$OrigAmtBkt <- factor(Vint.OrigAmt.Loss$OrigAmtBkt, levels= c('[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)', 'Sum'))
  Vint.OrigAmt.Loss	<- with(Vint.OrigAmt.Loss, Vint.OrigAmt.Loss[order(OrigAmtBkt),])

  writeWorksheet(Charts, Vint.OrigAmt.Loss, sheet = "Vint.OrigAmt", startRow = 23, startCol = 1)

  # XTab Default Rate by FICO & OrigAmt for 2006 Vintage
  # Columns: LAST_UPB, ORIG_AMT
  FICO.OrigAmt.Def1	<- addmargins(xtabs(LAST_UPB~FicoBkt+OrigAmtBkt, data=Combined_Data_Default_2006))
  FICO.OrigAmt.Def2	<- addmargins(xtabs(ORIG_AMT~FicoBkt+OrigAmtBkt, data=Combined_Data_Yrs_2006))
  FICO.OrigAmt.Def	<- as.data.frame(FICO.OrigAmt.Def1/FICO.OrigAmt.Def2)
  colnames(FICO.OrigAmt.Def) 	<- c("FicoBkt","OrigAmtBkt","Freq")
  FICO.OrigAmt.Def$OrigAmtBkt	<- factor(FICO.OrigAmt.Def$OrigAmtBkt, levels= c('[0-85k)', '[85k-110k)', '[110k-125k)', '[125k-1500k)', '[150k-175k)', '[175k-200k)', '[200k-417k)', '[417k+)', 'Sum'))
  FICO.OrigAmt.Def$FicoBkt	<- factor(FICO.OrigAmt.Def$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  FICO.OrigAmt.Def	<- with(FICO.OrigAmt.Def, FICO.OrigAmt.Def[order(FicoBkt, OrigAmtBkt),])
  FICO.OrigAmt.Def	<-dcast(FICO.OrigAmt.Def,OrigAmtBkt~FicoBkt,value.var = "Freq")

  writeWorksheet(Charts, FICO.OrigAmt.Def, sheet = "Vint.OrigAmt", startRow = 34, startCol = 1)


  # XTab Default Rate by Vintage & OCLTV
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.Ocltv.Def1	<- addmargins(xtabs(LAST_UPB~OcltvBkt+VinYr, data=Combined_Data_Default))
  MissingOCLTV		<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Sum 			<- Vint.Ocltv.Def1[10,]
  Vint.Ocltv.Def1	<- as.table(rbind(Vint.Ocltv.Def1[1:9,], MissingOCLTV, Sum))
  Vint.Ocltv.Def2	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Ocltv.Def	<- as.data.frame(Vint.Ocltv.Def1/Vint.Ocltv.Def2)
  colnames(Vint.Ocltv.Def) <- c("OcltvBkt","VinYr","Freq")
  Vint.Ocltv.Def	<- dcast(Vint.Ocltv.Def,OcltvBkt~VinYr,value.var = "Freq")

  createSheet(Charts, name = "Vint.Ocltv")
  writeWorksheet(Charts, Vint.Ocltv.Def, sheet = "Vint.Ocltv", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & OCLTV
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.Ocltv.Sev1	<- addmargins(xtabs(NET_LOSS~OcltvBkt+VinYr, data=Combined_Data_Default))
  MissingOCLTV		<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Sum			<- Vint.Ocltv.Sev1[10,]
  Vint.Ocltv.Sev1	<- as.table(rbind(Vint.Ocltv.Sev1[1:9,], MissingOCLTV, Sum))
  Vint.Ocltv.Sev2	<- addmargins(xtabs(LAST_UPB~OcltvBkt+VinYr, data=Combined_Data_Default))
  Sum			<- Vint.Ocltv.Sev2[10,]
  Vint.Ocltv.Sev2	<- as.table(rbind(Vint.Ocltv.Sev2[1:9,], MissingOCLTV, Sum))
  Vint.Ocltv.Sev	<- as.data.frame(Vint.Ocltv.Sev1/Vint.Ocltv.Sev2)
  colnames(Vint.Ocltv.Sev) <- c("OcltvBkt","VinYr","Freq")
  Vint.Ocltv.Sev	<- dcast(Vint.Ocltv.Sev,OcltvBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Ocltv.Sev, sheet = "Vint.Ocltv", startRow = 14, startCol = 1)

  # XTab Loss Rate by Vintage & OCLTV
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.Ocltv.Loss1	<- addmargins(xtabs(NET_LOSS~OcltvBkt+VinYr, data=Combined_Data_Default))
  MissingOCLTV		<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Sum			<- Vint.Ocltv.Loss1[10,]
  Vint.Ocltv.Loss1	<- as.table(rbind(Vint.Ocltv.Loss1[1:9,], MissingOCLTV, Sum))
  Vint.Ocltv.Loss2	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Ocltv.Loss	<- as.data.frame(Vint.Ocltv.Loss1/Vint.Ocltv.Loss2)
  colnames(Vint.Ocltv.Loss) <- c("OcltvBkt","VinYr","Freq")
  Vint.Ocltv.Loss	<- dcast(Vint.Ocltv.Loss,OcltvBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Ocltv.Loss, sheet = "Vint.Ocltv", startRow = 27, startCol = 1)


  # XTab Default Rate by Vintage & DTI
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.Dti.Def1		<- addmargins(xtabs(LAST_UPB~DtiBkt+VinYr, data=Combined_Data_Default))
  Vint.Dti.Def2		<- addmargins(xtabs(ORIG_AMT~DtiBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Dti.Def		<- as.data.frame(Vint.Dti.Def1/Vint.Dti.Def2)
  Vint.Dti.Def		<- dcast(Vint.Dti.Def,DtiBkt~VinYr,value.var = "Freq")

  createSheet(Charts, name = "Vint.Dti")
  writeWorksheet(Charts, Vint.Dti.Def, sheet = "Vint.Dti", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & DTI
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.Dti.Sev1 	<- addmargins(xtabs(NET_LOSS~DtiBkt+VinYr, data=Combined_Data_Default))
  Vint.Dti.Sev2 	<- addmargins(xtabs(LAST_UPB~DtiBkt+VinYr, data=Combined_Data_Default))
  Vint.Dti.Sev		<- as.data.frame(Vint.Dti.Sev1/Vint.Dti.Sev2)
  Vint.Dti.Sev		<- dcast(Vint.Dti.Sev,DtiBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Dti.Sev, sheet = "Vint.Dti", startRow = 10, startCol = 1)

  # XTab Loss Rate by Vintage & DTI
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.Dti.Loss1 	<- addmargins(xtabs(NET_LOSS~DtiBkt+VinYr, data=Combined_Data_Default))
  Vint.Dti.Loss2 	<- addmargins(xtabs(ORIG_AMT~DtiBkt+VinYr, data=Combined_Data_Yrs))
  Vint.Dti.Loss		<- as.data.frame(Vint.Dti.Loss1/Vint.Dti.Loss2)
  Vint.Dti.Loss		<- dcast(Vint.Dti.Loss,DtiBkt~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Dti.Loss, sheet = "Vint.Dti", startRow = 19, startCol = 1)


  # XTab Default Rate by Vintage & SATO
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.SATO.Def1	<- addmargins(xtabs(LAST_UPB~SATOBkt+VinYr, data=Combined_Data_Default))
  Vint.SATO.Def2	<- addmargins(xtabs(ORIG_AMT~SATOBkt+VinYr, data=Combined_Data_Yrs))
  Vint.SATO.Def		<- as.data.frame(Vint.SATO.Def1/Vint.SATO.Def2)
  Vint.SATO.Def		<- dcast(Vint.SATO.Def,SATOBkt~VinYr,value.var = "Freq")
  Vint.SATO.Def$SATOBkt	<- factor(Vint.SATO.Def$SATOBkt, levels= c('(-, -2%]', '(-2%,-1.5%)', '[-1.5%,-.5%)', '[-.5%,0)', '[0,.5%)', '[.5%, 1%)', '[1%, 1.5%)', '[1.5%, 2%)', '[2%,+)', 'NA', 'Sum'))
  Vint.SATO.Def		<- with(Vint.SATO.Def, Vint.SATO.Def[order(SATOBkt),])

  createSheet(Charts, name = "Vint.SATO")
  writeWorksheet(Charts, Vint.SATO.Def, sheet = "Vint.SATO", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & SATO
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.SATO.Sev1	<- addmargins(xtabs(NET_LOSS~SATOBkt+VinYr, data=Combined_Data_Default))
  Vint.SATO.Sev2	<- addmargins(xtabs(LAST_UPB~SATOBkt+VinYr, data=Combined_Data_Default))
  Vint.SATO.Sev		<- as.data.frame(Vint.SATO.Sev1/Vint.SATO.Sev2)
  Vint.SATO.Sev		<- dcast(Vint.SATO.Sev,SATOBkt~VinYr,value.var = "Freq")
  Vint.SATO.Sev$SATOBkt	<- factor(Vint.SATO.Sev$SATOBkt, levels= c('(-, -2%]', '(-2%,-1.5%)', '[-1.5%,-.5%)', '[-.5%,0)', '[0,.5%)', '[.5%, 1%)', '[1%, 1.5%)', '[1.5%, 2%)', '[2%,+)', 'NA', 'Sum'))
  Vint.SATO.Sev		<- with(Vint.SATO.Sev, Vint.SATO.Sev[order(SATOBkt),])

  writeWorksheet(Charts, Vint.SATO.Sev, sheet = "Vint.SATO", startRow = 14, startCol = 1)

  # XTab Loss Rate by Vintage & SATO
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.SATO.Loss1	<- addmargins(xtabs(NET_LOSS~SATOBkt+VinYr, data=Combined_Data_Default))
  Vint.SATO.Loss2	<- addmargins(xtabs(ORIG_AMT~SATOBkt+VinYr, data=Combined_Data_Yrs))
  Vint.SATO.Loss	<- as.data.frame(Vint.SATO.Loss1/Vint.SATO.Loss2)
  Vint.SATO.Loss	<- dcast(Vint.SATO.Loss,SATOBkt~VinYr,value.var = "Freq")
  Vint.SATO.Loss$SATOBkt <- factor(Vint.SATO.Loss$SATOBkt, levels= c('(-, -2%]', '(-2%,-1.5%)', '[-1.5%,-.5%)', '[-.5%,0)', '[0,.5%)', '[.5%, 1%)', '[1%, 1.5%)', '[1.5%, 2%)', '[2%,+)', 'NA', 'Sum'))
  Vint.SATO.Loss	<- with(Vint.SATO.Loss, Vint.SATO.Loss[order(SATOBkt),])

  writeWorksheet(Charts, Vint.SATO.Loss, sheet = "Vint.SATO", startRow = 27, startCol = 1)

  # Add XTab SATO by LTV & FICO for 2006 Vintage
  # Columns: ORIG_AMT
  LTV.FICO.SATO1.06	<- addmargins(xtabs(SATO*ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Default_2006))
  LTV.FICO.SATO2.06	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Yrs_2006))
  LTV.FICO.WA.SATO.06	<- as.data.frame(LTV.FICO.SATO1.06/LTV.FICO.SATO2.06)
  LTV.FICO.WA.SATO.06	<- dcast(LTV.FICO.WA.SATO.06,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.WA.SATO.06$FicoBkt <- factor(LTV.FICO.WA.SATO.06$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.WA.SATO.06	<- with(LTV.FICO.WA.SATO.06, LTV.FICO.WA.SATO.06[order(FicoBkt),])

  writeWorksheet(Charts, LTV.FICO.WA.SATO.06, sheet = "Vint.SATO", startRow = 40, startCol = 1)

  # Add XTab SATO by LTV & FICO for 2010 Vintage
  # Columns: ORIG_AMT
  LTV.FICO.SATO1.10	<- addmargins(xtabs(SATO*ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Default_2010))
  LTV.FICO.SATO2.10	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Yrs_2010))
  LTV.FICO.WA.SATO.10	<- as.data.frame(LTV.FICO.SATO1.10/LTV.FICO.SATO2.10)
  LTV.FICO.WA.SATO.10	<- dcast(LTV.FICO.WA.SATO.10,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.WA.SATO.10$FicoBkt <- factor(LTV.FICO.WA.SATO.10$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.WA.SATO.10	<- with(LTV.FICO.WA.SATO.10, LTV.FICO.WA.SATO.10[order(FicoBkt),])

  writeWorksheet(Charts, LTV.FICO.WA.SATO.10, sheet = "Vint.SATO", startRow = 50, startCol = 1)

  # Add XTab Note Rate by LTV & FICO for 2010 Vintage
  # Columns: ORIG_RT, ORIG_AMT
  LTV.FICO.ORIGRT1	<- addmargins(xtabs(ORIG_RT*ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Default_2010))
  LTV.FICO.ORIGRT2	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Yrs_2010))
  LTV.FICO.WA.ORIGRT	<- as.data.frame(LTV.FICO.ORIGRT1/LTV.FICO.ORIGRT2)
  LTV.FICO.WA.ORIGRT	<- dcast(LTV.FICO.WA.ORIGRT,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.WA.ORIGRT$FicoBkt <- factor(LTV.FICO.WA.ORIGRT$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.WA.ORIGRT	<- with(LTV.FICO.WA.ORIGRT, LTV.FICO.WA.ORIGRT[order(FicoBkt),])

  writeWorksheet(Charts, LTV.FICO.WA.ORIGRT, sheet = "Vint.SATO", startRow = 60, startCol = 1)

  saveWorkbook(Charts)


  # XTab Default Rate by Vintage & Risk Factors
  # Columns: LAST_UPB, VinYr, ORIG_AMT
  Vint.Rsk.Def1		<- addmargins(xtabs(LAST_UPB~RskFctrs+VinYr, data=Combined_Data_Default))
  Vint.Rsk.Def2		<- addmargins(xtabs(ORIG_AMT~RskFctrs+VinYr, data=Combined_Data_Yrs))
  Vint.Rsk.Def		<- as.data.frame(Vint.Rsk.Def1/Vint.Rsk.Def2)
  Vint.Rsk.Def		<- dcast(Vint.Rsk.Def,RskFctrs~VinYr,value.var = "Freq")

  createSheet(Charts, name = "Vint.Rsk")
  writeWorksheet(Charts, Vint.Rsk.Def, sheet = "Vint.Rsk", startRow = 1, startCol = 1)

  # XTab Severity by Vintage & Risk Factors
  # Columns: NET_LOSS, VinYr, LAST_UPB
  Vint.Rsk.Sev1		<- addmargins(xtabs(NET_LOSS~RskFctrs+VinYr, data=Combined_Data_Default))
  Vint.Rsk.Sev2		<- addmargins(xtabs(LAST_UPB~RskFctrs+VinYr, data=Combined_Data_Default))
  Vint.Rsk.Sev		<- as.data.frame(Vint.Rsk.Sev1/Vint.Rsk.Sev2)
  Vint.Rsk.Sev		<- dcast(Vint.Rsk.Sev,RskFctrs~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Rsk.Sev, sheet = "Vint.Rsk", startRow = 10, startCol = 1)

  # XTab Loss Rate by Vintage & Risk Factors
  # Columns: NET_LOSS, VinYr, ORIG_AMT
  Vint.Rsk.Loss1	<- addmargins(xtabs(NET_LOSS~RskFctrs+VinYr, data=Combined_Data_Default))
  Vint.Rsk.Loss2	<- addmargins(xtabs(ORIG_AMT~RskFctrs+VinYr, data=Combined_Data_Yrs))
  Vint.Rsk.Loss		<- as.data.frame(Vint.Rsk.Loss1/Vint.Rsk.Loss2)
  Vint.Rsk.Loss		<- dcast(Vint.Rsk.Loss,RskFctrs~VinYr,value.var = "Freq")

  writeWorksheet(Charts, Vint.Rsk.Loss, sheet = "Vint.Rsk", startRow = 19, startCol = 1)


  # XTab Default Rate by LTV & FICO for 2006 Vintage
  # Columns: LAST_UPB, ORIG_AMT
  LTV.FICO.Def1		<- addmargins(xtabs(LAST_UPB~OcltvBkt+FicoBkt, data=Combined_Data_Default_2006))
  LTV.FICO.Def2		<- addmargins(xtabs(ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Yrs_2006))
  LTV.FICO.Def		<- as.data.frame(LTV.FICO.Def1/LTV.FICO.Def2)
  LTV.FICO.Def		<- dcast(LTV.FICO.Def,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.Def$FicoBkt	<- factor(LTV.FICO.Def$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.Def		<- with(LTV.FICO.Def, LTV.FICO.Def[order(FicoBkt),])
  createSheet(Charts, name = "LTV.FICO")
  writeWorksheet(Charts, LTV.FICO.Def, sheet = "LTV.FICO", startRow = 1, startCol = 1)

  # XTab Severity by LTV & FICO for 2006 Vintage
  # Columns: NET_LOSS, LAST_UPB
  LTV.FICO.Sev1		<- addmargins(xtabs(NET_LOSS~OcltvBkt+FicoBkt, data=Combined_Data_Default_2006))
  LTV.FICO.Sev2		<- addmargins(xtabs(LAST_UPB~OcltvBkt+FicoBkt, data=Combined_Data_Default_2006))
  LTV.FICO.Sev		<- as.data.frame(LTV.FICO.Sev1/LTV.FICO.Sev2)
  LTV.FICO.Sev		<- dcast(LTV.FICO.Sev,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.Sev$FicoBkt	<- factor(LTV.FICO.Sev$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.Sev		<- with(LTV.FICO.Sev, LTV.FICO.Sev[order(FicoBkt),])

  writeWorksheet(Charts, LTV.FICO.Sev, sheet = "LTV.FICO", startRow = 11, startCol = 1)

  # XTab Loss Rate by LTV & FICO for 2006 Vintage
  # Columns: NET_LOSS, ORIG_AMT
  LTV.FICO.Loss1	<- addmargins(xtabs(NET_LOSS~OcltvBkt+FicoBkt, data=Combined_Data_Default_2006))
  LTV.FICO.Loss2	<- addmargins(xtabs(ORIG_AMT~OcltvBkt+FicoBkt, data=Combined_Data_Yrs_2006))
  LTV.FICO.Loss		<- as.data.frame(LTV.FICO.Loss1/LTV.FICO.Loss2)
  LTV.FICO.Loss		<- dcast(LTV.FICO.Loss,FicoBkt~OcltvBkt,value.var = "Freq")
  LTV.FICO.Loss$FicoBkt	<- factor(LTV.FICO.Loss$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.Loss		<- with(LTV.FICO.Loss, LTV.FICO.Loss[order(FicoBkt),])

  writeWorksheet(Charts, LTV.FICO.Loss, sheet = "LTV.FICO", startRow = 21, startCol = 1)


  # Calculate UPB %s & default rates for the 210 cohorts, for the 2007 vintage
  # Columns: ORIG_AMT, LAST_UPB
  LTV.FICO.Rsk.UPB1	<- addmargins(xtabs(ORIG_AMT~FicoBkt+OcltvBkt+RskFctrs, data=Combined_Data_Yrs_2007))
  LTV.FICO.Rsk.UPB2	<- sum(Combined_Data_Yrs_2007$ORIG_AMT)
  LTV.FICO.Rsk.UPB	<- LTV.FICO.Rsk.UPB1/LTV.FICO.Rsk.UPB2
  LTV.FICO.Rsk.UPB	<- as.data.frame(LTV.FICO.Rsk.UPB)
  colnames(LTV.FICO.Rsk.UPB) 	<- c("FicoBkt","OcltvBkt","RskFctrs","Freq")
  LTV.FICO.Rsk.UPB$FicoBkt	<- factor(LTV.FICO.Rsk.UPB$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.Rsk.UPB	<- with(LTV.FICO.Rsk.UPB, LTV.FICO.Rsk.UPB[order(FicoBkt),])
  LTV.FICO.Rsk.UPB	<- dcast(LTV.FICO.Rsk.UPB,FicoBkt+RskFctrs~OcltvBkt,value.var = "Freq")

  createSheet(Charts, name = "210.Cohorts")
  writeWorksheet(Charts, LTV.FICO.Rsk.UPB, sheet = "210.Cohorts", startRow = 1, startCol = 1)

  LTV.FICO.Rsk.Def1	<- addmargins(xtabs(LAST_UPB~FicoBkt+OcltvBkt+RskFctrs, data=Combined_Data_Default_2007))
  LTV.FICO.Rsk.Def2	<- addmargins(xtabs(ORIG_AMT~FicoBkt+OcltvBkt+RskFctrs, data=Combined_Data_Yrs_2007))
  LTV.FICO.Rsk.Def	<- as.data.frame(LTV.FICO.Rsk.Def1/LTV.FICO.Rsk.Def2)
  colnames(LTV.FICO.Rsk.Def) 	<- c("FicoBkt","OcltvBkt","RskFctrs","Freq")
  LTV.FICO.Rsk.Def$FicoBkt	<- factor(LTV.FICO.Rsk.Def$FicoBkt, levels= c('[780+)', '[740-780)', '[700-740)', '[660-700)', '[620-660)', '[0-620)', 'MissingFICO', 'Sum'))
  LTV.FICO.Rsk.Def	<- with(LTV.FICO.Rsk.Def, LTV.FICO.Rsk.Def[order(FicoBkt),])
  LTV.FICO.Rsk.Def	<- dcast(LTV.FICO.Rsk.Def,FicoBkt+RskFctrs~OcltvBkt,value.var = "Freq")

  writeWorksheet(Charts, LTV.FICO.Rsk.Def, sheet = "210.Cohorts", startRow = 1, startCol = 14)


  # Save the .xlsx document of all tables
  saveWorkbook(Charts)


####################################################################
# End of Part 2; Summary Statistics Step
####################################################################
