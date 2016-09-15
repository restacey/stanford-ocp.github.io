#!/bin/bash 
#
#all commands that start with SBATCH contain commands that are just used by SLURM for scheduling  
#################
#set a job name  
#SBATCH --job-name=FannieData_Model_ECAP
#################  
#a file for job output, you can check job progress
#SBATCH --output=FannieData_Model_ECAP.out
#################
# a file for errors from the job
#SBATCH --error=FannieData_Model_ECAP.err
#################
#time you think you need; default is one hour
#in minutes in this case, hh:mm:ss
#SBATCH --time=16:00:00
#################
#quality of service; think of it as job priority (was normal)
#SBATCH --qos=bigmem
#SBATCH --partition=bigmem
#################
#memory per node; default is 4000 MB per CPU (used 16000 before)
#SBATCH --mem=1300000
#asasSBATCH --mem=1500000
#################
#mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL
#################
#send mail to this address
#SBATCH --mail-user=restacey@stanford.edu

module load java/8u91
module load R/3.3.0
mkdir -p ~/R/x86_64-unknown-linux-gnu-library/3.2

# Run models
#Rscript /scratch/PI/giesecke/Stacey/Fannie_Mae_Loan_Level_Data/RCode/'fannie_mae_code - ECAP Model - Preprocessed 2.r'
Rscript /scratch/PI/giesecke/Stacey/Fannie_Mae_Loan_Level_Data/RCode/'fannie_mae_code - ECAP Model - Preprocessed 2_bigmem2.r'