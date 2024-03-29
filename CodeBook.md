Purpose of run_analysis.R and the output files produced by running said script.
========================================================

the file run_analysis.R takes the data from the UCIHAR Dataset and processes it (details below). The working direcctory should be set to same as the UCI HAR Dataset folder



The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. Check the README.txt* file for further details about this dataset.

run_analysis.R works by reading in the files from test and training sets, loading in variable names and appropriately name the R variables' columns and rows. The Test and Train datasets are merged into a singl matrix of (observations) x (variables), where each observation is one of the 10299 data points captured in the appropriate study[2], and each of the rows is one of the variables measured. The script proceeds to creating a reduced version of the dataset where only "means" of the 3D data are included in the "reduced" version of the dataset. This reduced dataset is write to the path in a txt format to the file xMatrixReduced.txt

run_analysis.R also produces a new dataset which consists in taking the mean variable values for each subject and creating a matrix of (variables) x (subjects) where entry (i,j) is the mean of the ith variable for the jth subject. This varibale is called newData and is writen to the path onto a file newData.txt

*which is contained in the zip file of the downloadable data.
