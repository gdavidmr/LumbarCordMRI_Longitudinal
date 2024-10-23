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


# ===================== specifications ==============================
# Adjust the following parameters according to your file system!!!

subjects=$(cat $1)

for subject in $subjects
do

echo "Processing: " $subject
cd $subject

# specify subfolder of structural data
Fstr=t2s_medic_cervical

# specify subfolder of dMRI data
Fdmri=diff_ep2d_cervical/fit_wols

# ==================== normalizing medic ========================
# first we normalize medic to the t2s PAM50 template
# the normalization itself consists of multiple steps including labeling vertebrae, creating labels,
# and registration to the template 

# get medic
t2s=$(ls $Fstr/resliced_avg*_medic_*.nii)
base=`basename $t2s .nii`

mkdir $Fstr/normalized

# Label vertebrae
sct_label_vertebrae -i $t2s \
	-s $Fstr/segm_sct/"$base"_seg_sc_corr.nii \
	-c t2 \
	-ofolder $Fstr/normalized \
	-initc2

# Create labels
sct_label_utils -i $Fstr/normalized/"$base"_seg_sc_corr_labeled.nii \
	-vert-body 2,3 \
	-o $Fstr/normalized/"$base"_seg_sc_corr_labels.nii

# Register to template
# -i: medic
# -s: SC segmentation of medic
sct_register_to_template -i $t2s \
	-s $Fstr/segm_sct/"$base"_seg_sc_corr.nii \
	-l $Fstr/normalized/"$base"_seg_sc_corr_labels.nii \
	-c t2s \
	-ofolder $Fstr/normalized

# =================== normalizing dMRI =============================
# next, we normalize the dMRI dataset to the template using the previously obtained transformation
# note: here, the source image is the template and the target is the average dw image as suggested by SCT, but the order does not really
# 	matter as SCT performs the registration in both directions and also outputs the inverse transformation

# get average dw image, which will drive the normalization
# average dw is recommended because it has an excellent cord contrast.
dmri=$(ls $Fdmri/meanDWI_rcDW4d_cervical_*_month00.nii)
base=`basename $dmri .nii`

mkdir $Fdmri/normalized

# Register the template to dMRI
# note that we use the normalization of medic as prior
# we use the initwarpinv command because we want to get the inverse trafo (dmri->template) as well
sct_register_multimodal \
	-i $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-d $dmri \
	-iseg $SCT_DIR/data/PAM50/template/PAM50_cord.nii.gz \
	-dseg $Fdmri/"$base"_seg.nii \
 	-param step=1,type=seg,algo=slicereg,smooth=5:step=2,type=seg,algo=bsplinesyn,metric=MeanSquares,smooth=1,iter=3 \
	-initwarp $Fstr/normalized/warp_template2anat.nii.gz \
	-initwarpinv $Fstr/normalized/warp_anat2template.nii.gz \
	-ofolder $Fdmri/normalized/

# Rename warping fields for clarity
mv $Fdmri/normalized/warp_PAM50_t12"$base".nii.gz $Fdmri/normalized/warp_template2dmri.nii.gz
mv $Fdmri/normalized/warp_"$base"2PAM50_t1.nii.gz $Fdmri/normalized/warp_dmri2template.nii.gz

# Warp template objects to dMRI
sct_warp_template -d $dmri \
	-w $Fdmri/normalized/warp_template2dmri.nii.gz \
	-a 1 \
	-s 1 \
	-ofolder $Fdmri/normalized/

# Apply the forward transformation (dmri->template) on all DTI maps
FA=$(ls $Fdmri/FA_*_month00.nii)
base=`basename $FA .nii`
sct_apply_transfo -i $FA \
	-d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-w $Fdmri/normalized/warp_dmri2template.nii.gz \
	-o $Fdmri/normalized/"$base"_reg.nii

MD=$(ls $Fdmri/MD_*_month00.nii)
base=`basename $MD .nii`
sct_apply_transfo -i $MD \
	-d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-w $Fdmri/normalized/warp_dmri2template.nii.gz \
	-o $Fdmri/normalized/"$base"_reg.nii

AD=$(ls $Fdmri/AD_wols_*_month00.nii)
base=`basename $AD .nii`
sct_apply_transfo -i $AD \
	-d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-w $Fdmri/normalized/warp_dmri2template.nii.gz \
	-o $Fdmri/normalized/"$base"_reg.nii

RD=$(ls $Fdmri/RD_*_month00.nii)
base=`basename $RD .nii`
sct_apply_transfo -i $RD \
	-d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-w $Fdmri/normalized/warp_dmri2template.nii.gz \
	-o $Fdmri/normalized/"$base"_reg.nii

RES=$(ls $Fdmri/RES_*_month00.nii)
base=`basename $RES .nii`
sct_apply_transfo -i $RES \
	-d $SCT_DIR/data/PAM50/template/PAM50_t1.nii.gz \
	-w $Fdmri/normalized/warp_dmri2template.nii.gz \
	-o $Fdmri/normalized/"$base"_reg.nii

# =================== Extract values =============================
#sct_extract_metric -i $FOLDER/DW_ep2d_cervical/FA_robust_rcDWI_P01_ep2ddiff_sw.nii \
#	-f $FOLDER/DW_ep2d_cervical/atlas \
#	-l 12,13 \
#	-o $FOLDER/DW_ep2d_cervical/atlas/metric_label.txt

done
