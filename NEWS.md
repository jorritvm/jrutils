# jrutils v0.9.0
## NOTES
general
- updated roxygen documentation

lib_other
- bugfix in loc (syntax errors)

## NEW FEATURES
lib_list
- new lib file in the jrutils package focused on list operations
- added merge_lists_by_key

lib_io
- added check_csv_region()
- added convert_csv_regional_style()

lib_other
- split loc() into more versatile functions list_code_files() and lines_of_code_per_file()
- kept loc() as as shortcut to both functions

lib_table
- added move_data_columns_right()

lib_vector
- added fractionalise() and rescale()
- added fill_A_NA_with_B()

<br />

# jrutils v0.8.1
## NOTES
lib_other
- bugfix in loc to exclude /renv/ code

<br />

# jrutils v0.8.0
## NOTES
general
- fixed roxygen missing @import in several functions

lib_project
- removed this lib and both deprecated lines of code functions

## NEW FEATURES
lib_io
- added clean_path()
- added init_project_folder()
- added wtc() alias to write_table_to_clipboard()
- added loc() as a new and fully featured lines of code counter

lib_rstudio
- added get_session_type()

lib_other
- removed duplicate convert_datetime_to_timeid()

<br />

# jrutils v0.7.0
## NOTES
- improved time_stamp() function arguments

## NEW FEATURES
- added remove_tail_na() to remove rows that contain NA from the tail of a data frame
- added tsampnum(), a numerical-character-only timestamp
- added numeric_date_to_iso8601 to cnoverts excel numerical dates to R Date objects
- added file_backup() to easily backup a file
- added nearest_pos() and nearest_val() to find closest values in a vector
- added get_season_from_date()

<br />

# jrutils v0.6.0
## NOTES
- improved tik-tok functions output
- now() was renamed to tstamp() to avoid overloading lubridate::now() 

## NEW FEATURES
lib_table
- added list_of_vectors_to_dt() to convert a list of vectors and dataframes to datatable

lib_vector
- added vector_to_csv() and csv_to_vector to convert between csv strings and string vectors

lib_other
- added pt() for progress tracking
- added docvar() for quickly outputing a df structure for use in roxygen documentation

<br />

# jrutils v0.5.0
## NEW FEATURES
lib_table
- added merge_overwrite() to  merge a data.table x and y (left join) and overwrite the values in x with those in y for the matching columns
- added insert_at_element() to inject a vector at a specific location in another vector
- added 'header' argument to two_column_csv_to_list()
- added docs for str_right() and str_left()
- added namespace exports for aliasses tl, tr, br, bl
- added distribute_vector_over_matrix

<br />

# jrutils v0.4.0
## NEW FEATURES
lib_datetime
- added now() to get an iso8601 character1 representation of today's date

lib_excel
- added excel_cell_to_RC() to convert excel cell reference to RxCx format
- added letters_to_numbers() to convert excel column references to numerics
- added wtx() to quickly write data frames to usefull excel

lib_io 
- added two_column_xlsx_to_list() to convert a 2 column excel sheet to a list of key-value pairs

lib_other
- modified tik() and tok() to prepend messages with nicely formatted timestamps

lib_string
- added str_left() and str_right() to mimick excel LEFT() and RIGHT() functions
- added remove_all_spec_char() to remove all non alphanumerical characters except - and _ from a string
- added remove_win_reserved_char() to remove characters from a string that are not allowed in windows file paths

lib_table
- improved topleft for more robust inspection of narrow or small data.frames
- added functions topright(), bottomleft(), bottomright() to the existing topleft() function
- added aliasses tl(), tr(), bl() and br() to these 4 functions


<br />

# jrutils v0.3.0

## NOTES
- Improved function documentation
- reorganised some functions in the lib files

## NEW FEATURES

lib_other
- added get_native_list_separator() function
- added tik() and tok() to benchmark more verbosely
- added load_packages_robustly() to load packages, and install if unavailable

lib_conversion
- added named_list() to create a list where for each element the name of the variable is the key and the content is the variable content
- added to string_gently_to_boolean() which but returns the original string if unsuccessful
- added string_gently_to_numeric() which but returns the original string if unsuccessful

lib_datetime
- added convert_datetime_to_timeid() to convert a standard year datetime to a numerical time ID

lib_io
- added two_column_csv_to_list() to parse a 2 column csv file into a list

lib_table
- wtf() now uses the system's default list separator by default

<br />

# jrutils v0.2.0

## NOTES
- Improved function documentation
- reorganised some functions in the lib files

## NEW FEATURES
lib_io 
- added write_table_to_clipboard() to quickly put a data.frame on the clipboard
- added robust_fread() to be less hindered by US vs EU CSV convention
- added combine_path() to combine parts of a file path into a real filepath

lib_other
- added catn() to cat with a trailing newline

lib_table
- added topleft() to inspect the topleft corner of a data.frame

<br />

# jrutils v0.1.0

## NOTES
- First release of this utility package

## NEW FEATURES

lib_io
- added clip_path() to convert backslash into forward slash on the clipboard
- added wopen() to open a given directory in windows explorer
- added wtf() to write a data.frame to a temporary file

lib_other
- added update_jrutils_package() to self update this package

lib_rstudio
- added print_globalenv_size()
- added print_var_size()
- added clc() to clear the rstudio console screen
- added clear() to clear the global environment
- added clearplots() to remove all plots in rstudio
