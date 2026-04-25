# tarr (development version)

October 17, 2020
The nbs_base.parquet file now defaults to the path paths$communicable/NBS/nbs_base.parquet.   Addtional "base" files have been consolidated to one.
Fixed the issue where the wrong path was being searched.

Added capability when backing up files for consistent naming. A file like "folder/test.parquet" will be backed up as "folder/test_Oct-17-24_10-33-23_backup.parquet"
The new filename takes the original file name, adds a date time stamp and the "_backup" the end using the same extension as the original file.
The default folder for backup files is the same as the original file.

October 16, 2024
* Changed the baseNBSUpdate()function to  use the nbs_process() function.
* arrow::read_parquet() was returning a data frame but keeping the file connection open That resulted in errors when trying to writto the same file 
or changes its name.  Switched to nanoparquest::read_parquet().


Changes October 15, 2024
* Added get_nbs_2_epitrax_fields() function to return the field_names_nbs_epitrax vector.  That vector is used internally
to convert epitrax fields to nbs fields