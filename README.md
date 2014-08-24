# README


## R Script

This project contains only one R script called **`run_analysis.R`**. 
The script works in R with no need for additional packages as long as the HAR Dataset is in the working directory under the name of *UCI HAR Dataset*.



### HAR Dataset

The HAR Dataset folder should be found at [this link](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "HAR Dataset"). In order for **`run_analysis.R`** to work, this folder must be unzipped in R's working directory under the name of *UCI HAR Dataset*.

As it can be read in the **`README.txt`** file of the mentioned data set, the information corresponds to several measurements of accelerometers and gyroscopes in a Samsung Galaxy S smartphone on the waists of 30 subjects performing different activities. For more details, please refer to the mentioned data set's documentation.



### Step 1: Merging Data Sets

The first part of the script extracts all the information from different files in *UCI HAR Dataset* and its subfolders. Then it merges them in order to form a single data set called **`DS1`**. The script uses the **`read.table()`** function to extract the information and the **`cbind()`** and **`rbind()`** functions to merge the data sets.



### Step 2: Extracting Specific Variables

The second part of the script subests **`DS1`** in order to keep only the variables representing the mean and standard deviation for each measurement.

According to the data set's documentation, variables (features) containing *-mean()* and *-std()* in their names correspond to the measurements' mean and standard deviation respectively. 

The script first finds the indexes of such variables by applying the **`grep()`** function and then subsets **`DS1`** by those indexes. Now the **`DS1`** data frame contains only the desired variables.



### Step 3: Using Descriptive Activity Names

The project instructions required to apply descriptive activity names to the **`activity`** variable instead of integers, as included in the original data set. 

Despite the original data set containing a file called **`activity_labels.txt`** with the label code, the renaming of the **`activity`** variable was done manually, since there are only 6 factors. The variable was saved as **`character`**. The assignments described in **`activity_labels.txt`** were kept in this step.



### Step 4: Descriptive Variable Names

For Step 4, *Camel case* was used in order to improve readability, since the names of the variables are too long if they're to be descriptive. Although this goes against the *all lowercase* rule of tidy variable names, the user should consider the benefits of this practice, as suggested by TA David Hood in the Step 4 forum (see [this comment](https://class.coursera.org/getdata-006/forum/thread?thread_id=132#comment-329 "TA David Hood's suggestion")). 

In order to fix the variable names, the **`gsub()`** function was used several times. The description of the variables is documented in **`CodeBook.md`** within this repository and to more detail in the **`features_info.txt`** file in the HAR Dataset.



### Step 5: Tidy Data Set for Averages

The last part of this script creates a second, independent tidy data set containing the average of each variable for each subject and each activity. The data set is stored as **`DS2`**.

The data set is generated by the **`aggregate()`** function.

More detailed information on the transformations executed by this script can be found in the **`CodeBook.md`** file in this repository.