### DTC Genomics Convertor v0.1 ###
### A command line tool to go between 23andMe, AncestryDNA, and FTDNA raw data formats ###
### Austin Reynolds ###
### awreynolds@utexas.edu ###
### June 2017 ###
#note: make sure your file paths do not have spaces in them. for whatever reason the shell gets confused with spaces when you call from R

# opt<-list()
# opt$file<-"~/Desktop/FTDNA.csv"
# opt$format<-"FTDNA"
# opt$convert_to<-"23andMe"
# opt$out<-"~/test.txt"

#welcome text
intro<-"\n"
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"                     DTC Genomics Convertor\n")
intro<-paste(intro,"                       by Austin Reynolds\n")
intro<-paste(intro,"                       v0.1 (June 2017)\n")
intro<-paste(intro,"-------------------------------------------------------------------\n")
intro<-paste(intro,"-------------------------------------------------------------------\n\n")
cat(intro)

#import required packages
library(optparse)
library(data.table)
library(stringr)

#define options
option_list <- list(
  make_option(c("-i", "--file"), action="store", default="",
              help="Path to input file."),
  make_option(c("-f", "--format"), action="store", default="",
              help="The format of the input file. Accepted options: \"23andMe\",\"AncestryDNA\",\"FTDNA\" "),
  make_option(c("-c", "--convert_to"), action="store", default="",
              help="The format that you want the output file to be. Accepted options: \"23andMe\",\"AncestryDNA\",\"FTDNA\" "),
  make_option(c("-o", "--out"), action="store", default="",
              help="Path to output file")
)
opt<-parse_args(OptionParser(option_list=option_list))

#define functions
convertor_Ancto23<-function(x){
  #hold header of AncestryDNA file
  recover_header_command<-paste("sed '/^rsid/q' ",opt$file," | sed '$d' > temp_header.txt",sep = "")
  system(recover_header_command)
  #convert to 23andMe format
  x$genotype<-paste(x$allele1,x$allele2,sep = "")
  x<-x[,c(1,2,3,6)]
  colnames(x)[1]<-"# rsid"
  write.table(x,"temp_tailer.txt",sep = "\t",col.names = TRUE,row.names = FALSE,quote = FALSE)
  #combine header and tailer
  combine_command<-paste("cat temp_header.txt temp_tailer.txt > ",opt$out,sep = "")
  system(combine_command)
  #cleanup
  cleanup_command<-paste("rm temp_header.txt temp_tailer.txt")
  system(cleanup_command)
}
convertor_FTto23<-function(x){
  #convert to 23andMe format
  colnames(x)<-c("# rsid","chromosome","position","genotype")
  write.table(x,opt$out,sep = "\t",col.names = TRUE,row.names = FALSE,quote = FALSE)
}
convertor_23toAnc<-function(x){
  #hold header of 23andMe file
  recover_header_command<-paste("sed '/^# rsid/q' ",opt$file," | sed '$d' > temp_header.txt",sep = "")
  system(recover_header_command)
  #convert to 23andMe format
  genotypes<-as.data.frame(str_split_fixed(x$genotype,"",2))
  x<-cbind(x[,c(1,2,3)],genotypes)
  colnames(x)<-c("rsid","chromosome","position","allele1","allele2")
  write.table(x,"temp_tailer.txt",sep = "\t",col.names = TRUE,row.names = FALSE,quote = FALSE)
  #combine header and tailer
  combine_command<-paste("cat temp_header.txt temp_tailer.txt > ",opt$out,sep = "")
  system(combine_command)
  #cleanup
  cleanup_command<-paste("rm temp_header.txt temp_tailer.txt")
  system(cleanup_command)
}
convertor_FTtoAnc<-function(x){
  #convert to AncestryDNA format
  genotypes<-as.data.frame(str_split_fixed(x$RESULT,"",2))
  x<-cbind(x[,c(1,2,3)],genotypes)
  colnames(x)<-c("rsid","chromosome","position","allele1","allele2")
  write.table(x,opt$out,sep = "\t",col.names = TRUE,row.names = FALSE,quote = FALSE)
}
convertor_23toFT<-function(x){
  #convert to FTDNA format
  new_colnames<-list("RSID","CHROMOSOME","POSITION","RESULT")
  write.table(new_colnames,"temp_header.txt",sep=",",col.names = FALSE, row.names = FALSE,quote = FALSE)
  x<-data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  write.table(x,"temp_tailer.txt",sep = ",",col.names = FALSE, row.names = FALSE,quote = TRUE)
  #combine header and tailer
  combine_command<-paste("cat temp_header.txt temp_tailer.txt > ",opt$out,sep = "")
  system(combine_command)
  #cleanup
  cleanup_command<-paste("rm temp_header.txt temp_tailer.txt")
  system(cleanup_command)
}
convertor_AnctoFT<-function(x){
  #convert to FTDNA format
  x$genotype<-paste(x$allele1,x$allele2,sep = "")
  x<-x[,c(1,2,3,6)]
  x<-data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  new_colnames<-list("RSID","CHROMOSOME","POSITION","RESULT")
  write.table(new_colnames,"temp_header.txt",sep=",",col.names = FALSE, row.names = FALSE,quote = FALSE)
  write.table(x,"temp_tailer.txt",sep = ",",col.names = FALSE, row.names = FALSE,quote = TRUE)
  #combine header and tailer
  combine_command<-paste("cat temp_header.txt temp_tailer.txt > ",opt$out,sep = "")
  system(combine_command)
  #cleanup
  cleanup_command<-paste("rm temp_header.txt temp_tailer.txt")
  system(cleanup_command)
}

#workflow
if (opt$format=="23andMe"){
  #read in 23andMe data
  input_df<-fread(opt$file)
  if (opt$convert_to=="23andMe"){
    print("Input and output format are the same.")
    quit()
  } else if (opt$convert_to=="AncestryDNA"){
    convertor_23toAnc(input_df)
  } else if (opt$convert_to=="FTDNA"){
    convertor_23toFT(input_df)
  } else{
    print("--convert_to option not recognized. Type '--help' for accepted options.")
    quit()
  }
} else if (opt$format=="AncestryDNA"){
  #read in AncestryDNA data
  input_df<-fread(opt$file)
  if (opt$convert_to=="AncestryDNA"){
    print("Input and output format are the same.")
    quit()
  } else if (opt$convert_to=="23andMe"){
    convertor_Ancto23(input_df)
  } else if (opt$convert_to=="FTDNA"){
    convertor_AnctoFT(input_df)
  } else{
    print("--convert_to option not recognized. Type '--help' for accepted options.")
    quit()
  }
} else if (opt$format=="FTDNA"){
  #read in FTDNA data
  input_df<-fread(opt$file)
  if (opt$convert_to=="FTDNA"){
    print("Input and output format are the same.")
    quit()
  } else if (opt$convert_to=="23andMe"){
    convertor_FTto23(input_df)
  } else if (opt$convert_to=="AncestryDNA"){
    convertor_FTtoAnc(input_df)
  } else{
    print("--convert_to option not recognized. Type '--help' for accepted options.")
    quit()
  }
} else {
  print("--format option not recognized. Type '--help' for accepted options.")
  quit()
}

print("Conversion complete!")
