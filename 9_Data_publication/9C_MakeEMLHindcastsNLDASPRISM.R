##MakeEMLNLDASPRISM
##Author: Mary Lofton
##Date: 10MAY20

#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

#load packages
library(tidyverse)

#read in final data files to check format
nldas <- read_csv("C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM/NLDAS_solar_radiation_2009-2016.csv")
prism <- read_csv("C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM/PRISM_precipitation_2009-2016.csv")

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

devtools::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)


#Step 1: Create a directory for your dataset
#in this case, our directory is C:\Users\Mary Lofton\Dropbox\Ch5\EDI.hindcasts.NLDAS.PRISM

#Step 2: Move your dataset to the directory

#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#we have two tables and one zip file

#Step 5: Import the core metadata templates

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage, as we have both continuous and
#categorical variables and want to report our geographic location

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_geographic_coverage
?template_taxonomic_coverage

# Import templates for our dataset licensed under CCBY, with 2 tables.
template_core_metadata(path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                       data.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                       data.table = "NLDAS_solar_radiation_2009-2016.csv",
                       write.file = TRUE)

template_table_attributes(path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                          data.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                          data.table = "PRISM_precipitation_2009-2016.csv",
                          write.file = TRUE)

#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
#only have to do this once even if have multiple data tables
template_geographic_coverage(path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                          data.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
                          data.table = "NLDAS_solar_radiation_2009-2016.csv",
                          empty = TRUE,
                          write.file = TRUE)


#for this project we also need taxonomic coverage for G. echinulata
#however, I am using a file created for a different Gloeo pub
#so we are not going to create a new file in this script

#Step 6: Script your workflow
#that's what this is.

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information
#refs to other data pubs associated with the GLEON Bayes Gloeo project are in here

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#good sources for keyword thesauri:
#lter controlled vocabulary
#cuahsi controlled vocabulary

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Typically, the lead PI needs to be listed several times; that person has to be listed separately for his/her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#you will need to edit the attributes files for each data table you are publishing
#grab attribute names and definitions from your metadata Google doc
#for units....
# View and search the standard units dictionary
view_unit_dictionary()

#Step 13: Custom Units
#if units are not in the unit dictionary, you need to fill them out in the
#custom_units.txt file
#in our case, this is lnColoniesPerLiter, wattsPerMeterSquared, and millimetersPerDay

#Step 14: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 15: Categorical variables
#we don't have any - hooray! :-)

#Step 16: Geographic coverage
#fill in the text file with the appropriate bounding box for your site
#we added both a bounding box for Lake Sunapee as well as
#point coordinates for the South Herrick Cove sampling site

#Step 17: Taxonomic coverage
#again, got this file from Bethel so no need to edit here - thx, Bethel! :-)

## Step 18: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using your username and password.

# Select Tools --> Data Package Identifier Reservations and click
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations"
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 19: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  data.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  eml.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  dataset.title = "Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016",
  temporal.coverage = c("2009-05-29", "2016-09-28"), #needs edited!!
  maintenance.description = 'completed', #ongoing or completed if we don't ever plan on updating this
  data.table = c("NLDAS_solar_radiation_2009-2016.csv","PRISM_precipitation_2009-2016.csv"),
  data.table.name = c("NLDAS solar radiation data","PRISM precipitation data"),
  data.table.description = c("NLDAS solar radiation data","PRISM precipitation data"),
  other.entity = "Gechinulata_hindcasts.zip",
  other.entity.name = "Lake Sunapee G. echinulata density hindcast NetCDF files",
  other.entity.description = "Lake Sunapee G. echinulata density hindcast NetCDF files",
  user.id = '', #you need credentials for this part
  user.domain = 'EDI',
  package.id = 'edi.18.5') #will need to change this

## Step 20: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using your username and password

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File",
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder),
# then click Upload. Files will upload and your EML metadata will be checked
# for errors. If there are no errors, your data product is now published!
# If there were errors, click the link to see what they were, then fix errors
# in the xml file.
# Note that each revision results in the xml file increasing one value
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the
# evaluation check again, until you receive a message with no errors.

## Step 21: PUBLISH YOUR DATA! ####
# Reserve a package.id for your error-free data package.
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using your permanent credentials.

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next
# Available Identifier". A new value will appear in the "Current data package
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 19

make_eml(
  path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  data.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  eml.path = "C:/Users/Mary Lofton/Dropbox/Ch5/EDI.hindcasts.NLDAS.PRISM",
  dataset.title = "Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016",
  temporal.coverage = c("2009-05-29", "2016-09-28"), #needs edited!!
  maintenance.description = 'completed', #ongoing or completed if we don't ever plan on updating this
  data.table = c("NLDAS_solar_radiation_2009-2016.csv","PRISM_precipitation_2009-2016.csv"),
  data.table.name = c("NLDAS solar radiation data","PRISM precipitation data"),
  data.table.description = c("NLDAS solar radiation data","PRISM precipitation data"),
  other.entity = "Gechinulata_hindcasts.zip",
  other.entity.name = "Lake Sunapee G. echinulata density hindcast NetCDF files",
  other.entity.description = "Lake Sunapee G. echinulata density hindcast NetCDF files",
  user.id = '', #you need credentials for this part
  user.domain = 'EDI',
  package.id = 'edi.18.5') #will need to change this

# Once your xml file with your PUBLISHED package.id is Done, return to the
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file
# associated with your PUBLISHED package.id. Look through the rendered
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File",
# choose your metadata (.xml) file associated with your PUBLISHED package.id
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder),
# then click Upload. Files will upload and your EML metadata will be checked for
# errors. Since you checked for and fixed errors in the staging environment, this
# should run without errors, and your data product is now published!

# Click the package.id hyperlink to view your final product! HOORAY!
# Have a cookie or a beer, whichever you feel is more appropriate. :-)
