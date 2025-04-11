# CorpHousing
Paper looking at corporate ownership of rental property in St. Louis County

##Project Updates

-April 9, 2025
Updated with new data from Assessor's office and reworked code to load original data form zip files on external drive.

- Sept. 27, 2024
I continued to clean up the codes and write information about the codes in this readme file. I have also learned that using *st_intersects()* results in much faster intersections than the full *st_interstion()* command. The output is a list with each main item being one of the buffer zones and the values being the row numbers for the observations in the parcel data that are within that buffer. Since this is a rather quick process, I can do it over and over again when needed. Where the length might come from is then calculating the averages;however, it seems i should be able to do that in parallel to speed up.

## Scripts

### Owner Filter Parallel.R
This script file attempts to clean the ownership data and determine the type of ownership for the full dataset for the years 2001 to 2020. It does so using parallel processing so is rather quick, even on the desktop. The first data environment that is saved includes each of the yearly ownership files labeled *own_dat20yr*. The second part of the script uses these dataframes to compile full set of all owners in a single dataframe and attempts to clean some typographical errors in zip code, ownership cities, and state codes. There is one part of the code that relies on an external file that was cleaned by hand, but this is commented out. The cleaned file is saved as a .csv file and is loaded each time. Once this is all corrected, the file is saved as the *OWN.RData* file in ./Build/Output

### Census.R
This script uses *tidycensus* package to download the tiger map files and the ACS data for some of the necessary values. Currently does not download all possible census controls. The maps are downloaded separately for the 2000, 2010, and 2020 census tracts. The census tract map files are compiled into one sf object by the name *cen.map* and is saved as *CenMap.RData* in the Build/Output folder. The bottom of the code creates a map of the Percent below 2X the Poverty Level for the three years.

**NOTE:When we did this before, we noticed that not all census tracts showed up in all years when we faceted for the full sample**

### Sales.R
This script cleans the full sample of all properties sold over the time frame 2001 to 2020 and uses the BLS API to convert all prices to real prices based on CPI data. The file also limits the nominal price to greater than $1,000 which results in loosing about 615 observations. The script then creates the log of the adjusted price and saves the data as *Sales.RData* in the Build/Output folder.

### Sale_Owner.R
This script utilizes the *Sales.RData* file and the *OWN.RData* file to create a file showing how each of the parcel ownership changed from a sale. We are assuming at this point that the data is correct in the year prior to the listed sale year and the year after the posted sale year. There is some indication this may not always be the case. There also may be cases where the ownership change without sale, but we are ignoring that at the moment. There is then a set of dummy variables created and a categorical variable based on tenure. The variable ten1 is the categorical variable defined as such:
1. Presale equal to "Not Owner" and Postsale equal to "Not Owner" is a value of 1.
2. Presale equal to "Owner" and Postsale equal to "Not Owner" is a value of 2.
3. Presale equal to "Not Owner" and Postsale equal to "Owner" is a value of 3.
4. Presale equal to "Owner" and Postsale equal to "Owner" is a value of 4.
5. I do code for cases where the pre or post sale is missing as would be the case for new properties, but none exist in the final data.

There are also a set of dummy variables as follows:
- P2C: Private change to corporate
- P2P: Private remains as Private
- C2C: Corporate remains as corporate
- C2P: Corporate changes to Private

This dataframe is then saved as *sal_own.RData* in the Build/Output folder.

### Buffer.R
This script creates 1/4, 1/2, and 3/4 mile buffers around each of the properties in the sold dataframe using the 2021 parcel map from STL based on centroids created by the *sf* package. I am still working on the intersection part to determine which properties are located within these buffers. Each is saved as an .RData file in the Build/Output folder.

### Tract Parcel.R
This script uses the st_intersects to put each of the parcel IDs from the 2021 parcel map into the census tracts from the 2000, 2010, and 2020 census for easier merging and mapping. The dataframe parcel is saved as *tract_parcel.RData* in the Build/Output folder. This supersedes the Maps.R script which is moved to archive.

##Archived Scripts

### Maps.R
This script uses the ownership data and links it with census tract data and sf objects to draw maps for share of ownership to show the progression of corporate ownership in census tracts by year from 2002 to 2020