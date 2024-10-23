#!/bin/bash

# ----------------------------------------
# The script performs spinal cord (SC) and gray matter (GM) segmentation using the deep learning-based
# algorithm of SCT.
#
# Arguments:
#   only one argument, a text file that contains all the subject folders (one folder in a row)
# ----------------------------------------

# Get first argument
subjects=$(cat $1)

# loop over all subject folders
for subject in $subjects
do

echo "Processing: "$subject

# Specify subfolder of structural data
Fstr=t2s_medic_lumbar

# Get medic
t2s=$(ls $subject/$Fstr/resliced_avg*_medic_*.nii)
base=`basename $t2s .nii`

# Segment SC
sct_deepseg_sc -i $t2s \
	       -c t2s \
	       -ofolder $subject/$Fstr/segm_sct

# Segment GM 
sct_deepseg_gm -i $t2s
mv $subject/$Fstr/*gmseg.nii $subject/$Fstr/segm_sct

done


