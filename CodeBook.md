# CODE BOOK

This document provides detailed information about the **`tidyDataset.txt`** file included in this repository, as well as for the **`DS1`** and **`DS2`** dataframes generated by the R script **`run_analysis.R`**. An explanation of the **`run_analysis.R`** script can be found within this repository in the **`README.md`** file. 

### Origin of the data

The data used for this project was the [Human Activity Recognition Using Smartphones Dataset](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "Download dataset") [1]. It was obtained through a series of experiments with the purpose of recognizing certain activities through different measurements on smartphones. According to the dataset's documentation:

>The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

>The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

>### For each record it is provided:

> - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
> - Triaxial Angular velocity from the gyroscope.
> - A 561-feature vector with time and frequency domain variables.
> - Its activity label.
> - An identifier of the subject who carried out the experiment.

>### The dataset includes the following files:

> - 'Readme.txt'
> - 'features_info.txt': Shows information about the variables used on the feature vector.
> - 'features.txt': List of all features.
> - 'activity_labels.txt': Links the class labels with their activity name.
> - 'train/X_train.txt': Training set.
> - 'train/y_train.txt': Training labels.
> - 'test/X_test.txt': Test set.
> - 'test/y_test.txt': Test labels.

>The following files are available for the train and test data. Their descriptions are equivalent. 

> - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
> - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
> - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
> - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.

> ### Notes:

> - Features are normalized and bounded within [-1,1].
> - Each feature vector is a row on the text file.

> For more information about this dataset contact: activityrecognition@smartlab.ws

### R script

The R script included in this repository (**`run_analysis.R`**) performs the following operations on the *HAR Dataset*:

#### Reading the information:

- Extraction of the **`features.txt`**, **`subject_train.txt`**, **`X_train.txt`**, **`y_train.txt`**, **`subject_test.txt`**, **`X_test.txt`**, and **`y_test.txt`** files into R objects using the **`read.table()`** function.

#### Merging of the train and the test data, as well as subjects and activities:

- Concatenation of the **`"subject"`** and **`"activity"`** strings to the object containing the feature names from **`features.txt`** using the **`c()`** function (positions 1 and 2 respectively).

- Merging of the train and test data for X (features) using the **`rbind()`** function.

- Merging of the train and test data for y (activities) using the **`rbind()`** function.

- Merging of the train and test data for subject using the **`rbind()`** function.

- Merging of the subject, y and X data (in that order) using the **`cbind()`** function, stored in the **`DS1`** data frame.

- Assignment of the feature names to the newly created data frame using the **`colnames()`** function.

#### Subsetting of the unified data frame:

After this unification, the script identifies only those variables containing the means or standard deviations for each measurement. According to the *HAR Dataset*'s documentation in **`features_info.txt`**:

> The set of variables that were estimated from these signals are: 

> mean(): Mean value
> std(): Standard deviation

So the following operations are then performed:

- Identification of the variables containing *mean()* or *std()* through the **`grep()`** function along with the **`union()`** function (to get both *mean()* and *std()*) and the **`sort()`** function (to preserve the original layout of the variables).

- Subsetting of the **`DS1`** data frame using the previously obtained indexes.

#### Assigning of descriptive activity names to the `activity` variable:

- Transformation of the **`activity`** variable to a factor variable through the **`as.factor()`** function.

- Substitution of the **`activity`** variable levels through the **`levels()`** function as follows:
    - 1 -> "walking"
    - 2 -> "walkingUpstairs"
    - 3 -> "walkingDownstairs"
    - 4 -> "sitting"
    - 5 -> "standing"
    - 6 -> "laying"

- Transformation of the **`activity`** variable to character through the **`as.character()`** function.

#### Labeling of the variables with descriptive names

- Transformation of the original variable names to *Camel case* using the **`colnames()`** and **`gsub()`** functions, changing certain patterns as follows:
    - Supression of "-"
    - "mean()" to "Mean"
    - "std()" to "SD"
    - "tB" to "timeB"
    - "fB" to "freqB"
    - "tG" to "timeG"
    - "Mag" to "Magnitude"

#### Creation of a second data set `DS2`

- Creation of a tidy data set containing the averages of each measurement variable for each subject and each activity. The **`aggregate()`** function was used to create this new data set.

### `tidyDataset.txt`

The **`DS2`** data set created by **`run_analysis.R`** was exported as a text (*.txt) file using the **`write.table()`** function. It contains 180 observations of 68 variables.

#### `subject` variable (1)

Subject ID number. There are 30 subjects in the experiment, each of which performed 6 different activities.

#### `activity` variable (2)

Activities performed by the subjects. There are 6 possible values for this variable: "walking", "walkingUpstairs", "walkingDownstairs", "sitting", "standing", and "laying".

#### Averages of measurement variables (3:68)

The averages of the mean (variable name containing *Mean*) and standard deviation (variable name containing *SD*) of every numeric variable for each subject and each activity. 

The description of what each of these variables represent is in the **`features_info.txt`** file of the *HAR Dataset*. 

## References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012