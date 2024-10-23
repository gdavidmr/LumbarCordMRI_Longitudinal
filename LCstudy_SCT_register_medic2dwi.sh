#!/bin/bash

# The script is intended to perform intra-subject registration between the t2*w medic and the t2w (b0) images from the dti dataset.
# Input: text file with subject folders

#flag=cervical
flag=lumbar

subjects=$(cat $1)

for subject in ${subjects[@]}
do

echo $subject
cd $subject

key=${subject: -11}

# Select b0 (target) and medic (source)
b0=$subject/diff_ep2d_$flag/mean_b0_rcDWI4d_"$flag"_"$key".nii
t2s=$subject/t2s_medic_$flag/resliced_avg_*_medic_*.nii

# Select SC mask for b0 and medic
b0_sc=$subject/diff_ep2d_$flag/mean_b0_rcDWI4d_"$flag"_"$key"_seg_sc_corr.nii
t2s_sc=$subject/t2s_medic_$flag/segm_jim/mask_sc.nii

# Co-registration
sct_register_multimodal -i $t2s \
	-d $b0 \
	-iseg $t2s_sc \
	-dseg $b0_sc \
	-ofolder $subject/t2s_medic_$flag/coreg2dwi \
	-param step=1,type=seg,algo=centermass,metric=MeanSquares,slicewise=1,smooth=3:step=2,type=seg,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10:step=3,type=im,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10
	 
mkdir $subject/diff_ep2d_$flag/fit_robust/coreg2medic
mv $subject/t2s_medic_$flag/coreg2dwi/mean_b0_*reg.nii $subject/diff_ep2d_$flag/fit_robust/coreg2medic
mv $subject/t2s_medic_$flag/coreg2dwi/warp_mean_b0_*.nii.gz $subject/diff_ep2d_$flag/fit_robust/coreg2medic

# Apply transformation fields on DTI maps
sct_apply_transfo -i $subject/diff_ep2d_$flag/fit_robust/FA_*.nii -d $t2s -w $subject/diff_ep2d_$flag/fit_robust/coreg2medic/warp_mean_b0_rcDWI4d_"$flag"_"$key"2resliced_avg_*_medic_"$flag"_"$key".nii.gz
sct_apply_transfo -i $subject/diff_ep2d_$flag/fit_robust/MD_*.nii -d $t2s -w $subject/diff_ep2d_$flag/fit_robust/coreg2medic/warp_mean_b0_rcDWI4d_"$flag"_"$key"2resliced_avg_*_medic_"$flag"_"$key".nii.gz
sct_apply_transfo -i $subject/diff_ep2d_$flag/fit_robust/AD_*.nii -d $t2s -w $subject/diff_ep2d_$flag/fit_robust/coreg2medic/warp_mean_b0_rcDWI4d_"$flag"_"$key"2resliced_avg_*_medic_"$flag"_"$key".nii.gz
sct_apply_transfo -i $subject/diff_ep2d_$flag/fit_robust/RD_*.nii -d $t2s -w $subject/diff_ep2d_$flag/fit_robust/coreg2medic/warp_mean_b0_rcDWI4d_"$flag"_"$key"2resliced_avg_*_medic_"$flag"_"$key".nii.gz
mv $subject/*_reg.nii $subject/diff_ep2d_$flag/fit_robust/coreg2medic

done
