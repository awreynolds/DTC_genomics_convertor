DTC Genomics Convertor v0.1
=================

A command line tool to go between 23andMe, AncestryDNA, and Family Tree DNA raw data formats.


Required Packages
-----------------

The DTC Genomics Convertor requires the following packages: *optparse*, *data.table*, and *stringr*. Please install those before using.


Example Use
-----------

```
Rscript DTC_genomics_convertor.r --file your_file.txt --format "23andMe" --convert_to "AncestryDNA" --out your_new_file.txt
```


Options
-------

```
--file         Path to your input file.
--format       Format of your input file. Either "23andMe", "AncestryDNA", or "FTDNA".
--convert_to   Format you want to convert your file to. Either "23andMe", "AncestryDNA", or "FTDNA".
--out          Path to your outfile
```


Note
----

If your input file or directory have spaces in them, this script will fail. R and the shell don't play nice when spaces are involved. Just change the name of your file/directory and the script should work fine. 
