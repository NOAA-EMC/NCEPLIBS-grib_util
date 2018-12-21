#!/bin/sh
set -x

wgrib2_ver=v2.0.5
start_dir=$PWD

for dirr in cnvgrib21_gfs copygb2 degrib2    grbindex   tocgrib2 tocgrib \
            cnvgrib       copygb  grb2index  grib2grib  tocgrib2super
do
    cd $start_dir/${dirr}.fd
    echo "starting $dirr"
    ./compile_${dirr}_wcoss.sh &>compile_${dirr}_wcoss.log
    rm $dirr
    echo "ending $dirr"
    cd ..
done

# Install wgrib
cd $start_dir/wgrib.cd
echo "starting wgrib"
./compile_wgrib_wcoss.sh &>compile_wgrib_wcoss.log
rm wgrib
echo "ending wgrib"
cd ..

# Install wgrib2
cd $start_dir/wgrib2_${wgrib2_ver}/sorc
echo "starting wgrib2"
./compile_wgrib2_wcoss.sh &>compile_wgrib2_wcoss.log
echo "ending wgrib2"
cd ../..

