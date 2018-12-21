#!/bin/sh

for test_name in cnvgrib copygb2 wgrib2 
do
    export dir=` pwd `
    export data=$dir/data
    export output_g1=$dir/output_g1
    export output_g2=$dir/output_g2
    export output_dir=$dir/${test_name}_output
    mkdir -p $data $output_g1 $output_g2 $output_dir
    echo " "
    echo -n "Running test ${test_name}..."
    echo " "
    ./test_${test_name}_wcoss.sh &>${output_dir}/${test_name}.log
    err=$?
    echo " "
    echo " done"
    echo " "
    if [ $err -ne 0 ]; then
        >&2 echo "WARNING: ONE OR MORE ERRORS WERE REPORTED!"
    fi
    mv $data $output_g1 $output_g2 $output_dir/ 
    echo " "
    echo "  --> Please view $output_dir/$test_name.log"
done

