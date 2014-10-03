mda.lakes
=====
##Model Data Assimilation - Lakes

By: Jordan Read, Luke Winslow, Gretchen Hansen

This code base is part of the modeling effort for Wisconsin Lakes. 
Please reference our [paper on this work](http://dx.doi.org/10.1016/j.ecolmodel.2014.07.029). 

##Functionality

Just musing here...

Want to encapsulate as much of the whole WiLMA running process in one file as possible. A single model run could look like this

````
library(WiLMA) 

#Setup and run model
run_glm_chained(site_id='805400', path='~/run_dir')
#the above function writes all relevant information for that lake to the directory
# this includes grabbing the driver file and generating the NML file.
# then runs the model

#There could be different functions that run the model in different ways
run_glm(site_id='805400', path='~/run_dir')
#this does a non-chained run

####Important: Why not have a separate "setup" function
# That is challenging. We don't know if the user wants to do a chained run
# so we may not setup the ice data (or it is wasted setup because it is not used)
# So the key thing at this stage is having functions that setup the workspace
# as they need them and run the model as they need them, leaving the NCDF files around
# for later analysis.

#### Hmmm, how do the below functions know the lakeID? 
##    standardized metadata file?
#Once the model is run, calculated some habitat metrics
habitat_metrics = calculate_habitat_chained(path='~/run_dir')
habitat_metrics_krose = calculate_habitat_chained_krose(path='~/run_dir')

#Maybe compare to validation data
cal_val = run_compare_validation(run_path='~/run_dir')
plot_cal_val(run_path='~/run_dir')

````

