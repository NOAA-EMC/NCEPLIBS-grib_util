#!/bin/sh
set -x

wgrib2_ver=2.0.7
start_dir=$PWD

for dirr in copygb2 degrib2 grbindex tocgrib2 tocgrib \
            cnvgrib copygb  grb2index grib2grib tocgrib2super
do
    echo "starting $dirr"
    cd $start_dir/${dirr}.fd
    ./compile_${dirr}_wcoss.sh |& tee compile_${dirr}_wcoss.log
    rm $dirr
    echo "ending $dirr"
    cd ..
done

# Install wgrib
echo "starting wgrib"	       
cd $start_dir/wgrib.cd
./compile_wgrib_wcoss.sh |& tee compile_wgrib_wcoss.log
rm wgrib
echo "ending wgrib"
cd ..

# # Install wgrib2
# echo "starting wgrib2"
# cd $start_dir/wgrib2_v${wgrib2_ver}
# ./compile_wgrib2_wcoss.sh ${wgrib2_ver} |& tee compile_wgrib2_wcoss.log
# echo "ending wgrib2"
# cd ../..

