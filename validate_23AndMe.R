#!/usr/bin/env Rscript
### A command line tool to validate that an input file is in 23andMe raw data format ###
### Heavily inspired by https://github.com/awreynolds/DTC_genomics_convertor/blob/master/DTC_genomics_convertor.r
### Phil Palmer ###
### phil@lifebit.ai ###
### August 2019 ###

# opt<-list()
# opt$file<-"~/Documents/GitHub/common-latest-geno/test/bad_file.txt"
# opt$file<-"~/Documents/GitHub/common-latest-geno/test/8589.23andme.6953"

#welcome text
intro<-"\n"
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"                     23AndMe File Validator\n")
intro<-paste(intro,"                       by Phil Palmer\n")
intro<-paste(intro,"                       v0.1 (August 2019)\n")
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"-------------------------------------------------------------------\n\n")
cat(intro)

#import required packages
library(optparse)
library(data.table)

option_list <- list(
  optparse::make_option(c("-i", "--file"), action="store", default="",
              help="Path to input file.")
)
opt<-optparse::parse_args(OptionParser(option_list=option_list))

# define functions
validate_file <- function(x) {
  tryCatch({
    return (data.table::fread(x))
  },warning=function(w) {
    stop(paste0("Invalid 23andMe file, could not be read & so failed validation\n", w))
  })
}
validate_ncols <- function(df) {
  if (ncol(df) != 4) {
    error_message <- paste0("Number of tab seperated columns is ", 
                            ncol(df), 
                            ", not 4 as expected. Please make sure this is a valid 23AndMe file")
    warning(error_message)
  }
}
validate_header <- function(df) {
  input_header <- colnames(df)
  expected_header <- c("# rsid","chromosome","position","genotype")
  if (!setequal(input_header,expected_header)) {
    input_header_string <- paste( unlist(input_header), collapse='\t')
    expected_header_string <- paste( unlist(expected_header), collapse='\t')
    error_message <- paste0('Header in input file: "', 
                            input_header_string, 
                            '"\nDoes not match expected header of: "', 
                            expected_header_string, 
                            '"')
    stop(error_message)
  }
}
validate_nspns <- function(df) {
  snps <- nrow(input_df)
  if (snps < 100000) {
    error_message <- paste0("It looks like there are only ", snps, " SNPs in your input file")
    warning(error_message)
  }
}
validate_23AndMe <- function(x) {
  # read input file
  input_df <- validate_file(x)
  #hold header of 23andMe file
  recover_header_command<-paste("sed '/^# rsid/q' ",x," | sed '$d' > temp_header.txt",sep = "")
  system(recover_header_command)
  # check number of tab seperated columns
  validate_ncols(input_df)
  # check column header - commented w/ the correct names
  validate_header(input_df)
  # check the number of snps
  validate_nspns
  message("Input 23AndMe file passed validation succesfully.")
}

validate_23AndMe(opt$file)