#!/bin/bash

# ==================== DESCRIPTION OF THE SCRIPT ==========================

# The script performs normalization of spinal cord dMRI dataset to the PAM50 template
# using Spinal Cord Toolbox (SCT) commands. The script works only if structural data are
# available beside the dMRI data.

# Unlike the typical brain fMRI normalization approach, where the EPI images are first registered to the structural image, which is then
# registered to the MNI template (EPI -> structural, structural - > MNI), here we use another strategy following the guidelines of SCT.
# In short, structural image is first normalized to the PAM50 template, and the obtained transformation matrix is fed into the normalization
# of the dMRI dataset as initialization.

# Note that here we assume that the dMRI and medic images are already segmented.

# Usage of the script:
#  the script requires one input parameter, a text file listing all the subject folders
#  note here that you need to specify only the subject folder, not the subfolders where the dMRI and medic data are located

# ===========================================================================


flag=cervical
#flag=lumbar

subjects=$(cat $1)

for subject in ${subjects[@]}
do

key=${subject: -11}

# ===================== Specifications ===========================
# Adjust the following parameters according to your file system!!!

# select b0 (target) and medic (source)flag=cervical
b0=$subject/diff_ep2d_$flag/mean_b0_rcDWI4d_"$flag"_"$key".nii
t2s=$subject/t2s_medic_$flag/resliced_avg_*_medic_*.nii

# select SC mask for b0 and medic
b0_sc=$subject/diff_ep2d_$flag/mean_b0_rcDWI4d_"$flag"_"$key"_seg_sc_corr.nii
t2s_sc=$subject/t2s_medic_$flag/segm_jim/mask_sc.nii


# ============ Normalizing the T2*-weighted MEDIC images ========================
# first we normalize medic to the t2s PAM50 template
# the normalization itself consists of multiple steps including labeling vertebrae, creating labels,
# and registration to the template 

# label discs -> done manually
if [[ $flag = "cervical" ]]
then
	mkdir $subject/t2s_medic_$flag/normalized
	sct_label_utils -i $t2s -create-viewer 3,4 -o $subject/t2s_medic_$flag/normalized/labels_disc.nii.gz
elif [[ $flag = "lumbar" ]]
then
	mkdir $subject/t2s_medic_$flag/normalized
	sct_label_utils -i $t2s -create-viewer 18,20 -o $subject/t2s_medic_$flag/normalized/labels_disc.nii.gz
fi

# Registration to template
# medic -> PAM50
sct_register_to_template -i $t2s \
	-s $t2s_sc \
	-l $subject/t2s_medic_$flag/normalized/labels_disc.nii.gz \
	-c t2 \
	-ref template \
	-ofolder $subject/t2s_medic_$flag/normalized
	#-param step=1,type=seg,algo=centermass,metric=MeanSquares,slicewise=1,smooth=3:step=2,type=seg,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10:step=3,type=im,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10

	
# =================== Normalizing dMRI =============================
# next, we normalize the dMRI dataset to the template using the previously obtained transformation
# note: here, the source image is the template and the target is the dMRI as suggested by SCT, but the order does not really
# 	matter as SCT performs the registration in both directions and also outputs the inverse transformation

# Get average dw image, which will drive the normalization
# Average dw is recommended because it has an excellent cord contrast.
dwi=$subject/diff_ep2d_$flag/mean_b500_rcDWI4d_"$flag"_"$key".nii
dwi_sc=$subject/diff_ep2d_$flag/mean_b500_rcDWI4d_"$flag"_"$key"_seg_sc_corr.nii

# Register the template to dMRI (backtransformation is output too)
# Note the different coreg parameters for cervical and lumbar data. These parameters are the results of extensive testing and optimization.
if [[ $flag = "cervical" ]]
then
sct_register_multimodal \
	-i $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-d $dwi \
	-iseg $SCT_DIR/data/PAM50/template/PAM50_cord.nii.gz \
	-dseg $dwi_sc \
	-param step=1,type=seg,algo=centermass,metric=MeanSquares,slicewise=1,smooth=3:step=2,type=seg,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10:step=3,type=im,algo=bsplinesyn,metric=MeanSquares,slicewise=1,iter=10 \
	-initwarp $subject/t2s_medic_$flag/normalized/warp_template2anat.nii.gz \
	-initwarpinv $subject/t2s_medic_$flag/normalized/warp_anat2template.nii.gz \
	-ofolder $subject/diff_ep2d_$flag/normalized
elif [[ $flag = "lumbar" ]]
then				
sct_register_multimodal \
	-i $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-d $dwi \
	-iseg $SCT_DIR/data/PAM50/template/PAM50_cord.nii.gz \
	-dseg $dwi_sc \
	-param step=1,type=seg,algo=centermass,metric=MeanSquares,slicewise=1,smooth=3:step=2,type=seg,algo=slicereg,metric=MeanSquares,slicewise=1,smooth=3:step=3,type=seg,algo=affine,metric=MeanSquares,slicewise=1,smooth=3 \
	-initwarp $subject/t2s_medic_$flag/normalized/warp_template2anat.nii.gz \
	-initwarpinv $subject/t2s_medic_$flag/normalized/warp_anat2template.nii.gz \
	-ofolder $subject/diff_ep2d_$flag/normalized
fi
			
# Rename warping fields for clarity
dwi_base=$(basename -- "$dwi")
dwi_base=${dwi_base:0:-4}
mv $subject/diff_ep2d_$flag/normalized/warp_PAM50_t12"$dwi_base".nii.gz $subject/diff_ep2d_$flag/normalized/warp_template2dmri.nii.gz
mv $subject/diff_ep2d_$flag/normalized/warp_"$dwi_base"2PAM50_t1.nii.gz $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz

# Warp template objects to dMRI (backtransformation is output as well)
sct_warp_template -d $dwi \
 -w $subject/diff_ep2d_$flag/normalized/warp_template2dmri.nii.gz \
 -a 1 \
 -s 1 \
 -ofolder $subject/diff_ep2d_$flag/normalized/

# Apply the forward transformation (dmri->template) on all DTI maps
FA=$(ls $subject/diff_ep2d_$flag/fit_robust/FA_*.nii)
base=`basename $FA .nii`
sct_apply_transfo -i $FA \
 -d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
 -w $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz \
 -o $subject/diff_ep2d_$flag/normalized/"$base"_reg.nii

MD=$(ls $subject/diff_ep2d_$flag/fit_robust/MD_*.nii)
base=`basename $MD .nii`
sct_apply_transfo -i $MD \
 -d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
 -w $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz \
 -o $subject/diff_ep2d_$flag/normalized/"$base"_reg.nii

AD=$(ls $subject/diff_ep2d_$flag/fit_robust/AD_*.nii)
base=`basename $AD .nii`
sct_apply_transfo -i $AD \
 -d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
 -w $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz \
 -o $subject/diff_ep2d_$flag/normalized/"$base"_reg.nii

RD=$(ls $subject/diff_ep2d_$flag/fit_robust/RD_*.nii)
base=`basename $RD .nii`
sct_apply_transfo -i $RD \
 -d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
 -w $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz \
 -o $subject/diff_ep2d_$flag/normalized/"$base"_reg.nii

RES=$(ls $subject/diff_ep2d_$flag/fit_robust/rms_*.nii)
base=`basename $RES .nii`
sct_apply_transfo -i $RES \
 -d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
 -w $subject/diff_ep2d_$flag/normalized/warp_dmri2template.nii.gz \
 -o $subject/diff_ep2d_$flag/normalized/"$base"_reg.nii

done
