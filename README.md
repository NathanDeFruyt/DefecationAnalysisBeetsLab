# Defecation Analysis Beets Lab

This script analyses defecation data. This script was written
in R version 4.0.4 "Lost Library Book" with warmth and love for the dearest 
of my colleagues Signe Kenis (signe.kenis@kuleuven.be), but should work in 
other version of R as well. Instructions on installing R for the first 
time can be found here: https://posit.co/downloads/. 

More info on the format your data should be in, can be requested with 
Signe, with Prof. Dr. Isabel Beets (isabel.beets@kuleuven.be), or at my adress
(nathan.defruyt@kuleuven.be). Behavioural data was collected in BORIS
(https://www.boris.unito.it/).

Please refer to the above contacts for any suggestions or questions you would have.

The script provides an easy to use interface for colleagues that do not code
to quickly extract two features of defecation data: 
- cycle length
- expulsion frequency

## Data format
Check the examples folder for an example data set.

The data needs to be stored in a separate folder for each day. Since this is 
a manually scored dataset, experiments ought to be blinded. Name your data files
using the code, and add an explanatory code file.

## Using the script

This is not a fancy script. I tried to make it easily accessible. There are three 
steps: 
1. select whether you want to read data from one folder, or from multiple folders at once (yes/no)
2. point to the folder where to find your data
3. download the data in the data tab of the applet that opens
4. [optional]: you can also check your data immediately using the visualisation and statistics tabs

## [Basics] Running a script in RStudio

in R is relatively simple: 
1. find out how to download the script from here
2. open it in R
3. if you're using a recent version of RStudio, you will be asked whether you want to 
   download packages. Agree with this. Otherwise, install packages in the first section 
   of the script.
5. for some as of yet unidentified reason, it's important to run this line by line:
   put your cursor on the line you want to run and press <ctrl> + <enter>
   the cursor will automatically shift to the next portion.
