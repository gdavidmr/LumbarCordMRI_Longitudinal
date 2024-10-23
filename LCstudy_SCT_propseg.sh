#!/bin/bash

# ==========================================================
# The script performs spinal cord (SC) segmentation using the propseg
# algorithm of SCT. The script is adapted to the folder structure of the baseline study of the longitudinal
# LC data and is used to segment the mean dw images for SC.
#
# Arguments:
#   only one argument, a text file that contains all the subject folders (one folder in a row)
# ==========================================================

# Get the first argument
subjects=$(cat $1)

# Loop across all subject folders
for subject in $subjects
do

echo "Processing: "$subject

key=${subject: -11}
region=cervical

dw=$subject/diff_ep2d_"$region"/mean_b500_rcDWI4d_"$region"_"$key".nii
sct_propseg -i $dw -c dwi -ofolder $subject/diff_ep2d_"$region"

b0=$subject/diff_ep2d_"$region"/mean_b0_rcDWI4d_"$region"_"$key".nii
sct_propseg -i $b0 -c t2 -ofolder $subject/diff_ep2d_"$region"

done
