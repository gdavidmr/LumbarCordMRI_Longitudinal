#!/bin/bash

# =========================== DESCRIPTION ===================================
# The scripts extracts metrics (FA, MD, AD, RD) from normalized DTI dataset.
# Note: DTI data have to be normalized before running this script.
# 		So, first run LCstudy_SCT_normalize_dmri.sh
#
# ===========================================================================

flag=cervical
#flag=lumbar

if [[ $flag = "cervical" ]]
then
	zmin=870
	zmax=930
elif [[ $flag = "lumbar" ]]
then
	zmin=170
	zmax=200
fi

subjects=$(cat $1)

for subject in $subjects
do

key=${subject:-11}
echo $key

FA=$(ls $subject/diff_ep2d_$flag/normalized/FA_*reg.nii)
sct_extract_metric -i $FA \
	-f /home/gdavid/toolboxes/sct_4.0.0/data/PAM50/atlas \
	-method wa \
	-z ${zmin}:${zmax} \
	-discard-neg-val 1 \
	-o $subject/diff_ep2d_$flag/normalized/metric_labels_FA.txt
	
MD=$(ls $subject/diff_ep2d_$flag/normalized/MD_*reg.nii)
sct_extract_metric -i $MD \
	-f /home/gdavid/toolboxes/sct_4.0.0/data/PAM50/atlas \
	-method wa \
	-z ${zmin}:${zmax}\
	-discard-neg-val 1 \
	-o $subject/diff_ep2d_$flag/normalized/metric_labels_MD.txt
	
AD=$(ls $subject/diff_ep2d_$flag/normalized/AD_*reg.nii)
sct_extract_metric -i $AD \
	-f /home/gdavid/toolboxes/sct_4.0.0/data/PAM50/atlas \
	-method wa \
	-z ${zmin}:${zmax} \
	-discard-neg-val 1 \
	-o $subject/diff_ep2d_$flag/normalized/metric_labels_AD.txt
	
RD=$(ls $subject/diff_ep2d_$flag/normalized/RD_*reg.nii)
sct_extract_metric -i $RD \
	-f /home/gdavid/toolboxes/sct_4.0.0/data/PAM50/atlas \
	-method wa \
	-z ${zmin}:${zmax} \
	-discard-neg-val 1 \
	-o $subject/diff_ep2d_$flag/normalized/metric_labels_RD.txt
	
done
