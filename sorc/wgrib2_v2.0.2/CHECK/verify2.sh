#!/bin/sh
# Public Domain  8/2015 Wesley Ebisuzaki ..
#
# verify.sh 2nd edition
#
# This is a completely rewritten script to verify wgrib2
#
#  Step 1 .. create RESULTS file
#    ./verify2.sh
#                   output is placed in ./RESULTS
#  Step 2 .. compare results
#    ./verify2.sh <RESULTS
#
#  To test compile on a different machine
#    A) create RESULTS file on a "good" machine
#         The rest take place on the new machine
#    B) compile wgrib2 using the same options used to create RESULTS
#    C) modify ./verify2.sh to point to the test version of wgrib2
#    D) run ./verify2 <RESULTS
#
#  There are 3 types of testing done by this script
#
#  1) thread:  Threading test: one vs 3 threads
#  2) eqv:     equivalence testing: running functionally equivalent commands
#  3) verf:    regression testing:  old vs new versions
#
#     NOTE: regression testing can fail.  Grib tables can change.  The inventory
#     format can change.  Some machine can do arithmetic slightly differently.

# wgrib2 = old version of wgrib2 for regression testing
# wgrib2new = new/test version of wgrib2
wgrib2=wgrib2
wgrib2new=wgrib2new

export mode=''
while [ "$1" != '' ]
do
  if [ "$1"  = 'SAVE' ] ; then
     mode='SAVE'
     shift 1
     continue
  fi
  if [ "$1"  = '-wgrib2' ] ; then
     wgrib2="$2"
     shift 2
     continue
  fi
  if [ "$1"  = '-wgrib2new' ] ; then
     wgrib2new="$2"
     shift 2
     continue
  fi
  echo "run verification on wgrib2"
  echo "$0 [SAVE] [-wgrib2 filename] [-wgrib2new filename] [<RESULTS]"
  echo "if SAVE, create baseline using wgrib2, save results in RESULTS"
  echo "if !SAVE, test wgrib2new, use baseline file RESULTS"
  echo
  echo "option=$1?"
  exit
done
echo "wgrib2=$wgrib2 wgrib2new=$wgrib2new mode=$mode"

version=`$wgrib2new -version | cut -f2 -d' ' | awk '{print $2*100+$1}' FS=/`
echo "version=$version"

files="ds.td.bin gep19.t00z.pgrb2af180 grid221.g2 png.grb2 FR_land_cosmo_de.grib2 NGAC2d.GrbF00"
#files2 - only files that new_grid supports
files2="gep19.t00z.pgrb2af180 grid221.g2 png.grb2 NGAC2d.GrbF00"
#files3 - at least two records
files3="ds.td.bin gep19.t00z.pgrb2af180 NGAC2d.GrbF00"

results="./RESULTS"
if [ "$mode" = 'SAVE' ] ; then
   touch $results
   rm $results
fi

touch dummy

# see if proj4 is installed
export proj4=0
$wgrib2new dummy -proj4 1
[ $? -eq 0 ] && proj4=1

# see if interpolates is installed
export new_grid=0
$wgrib2new dummy -new_grid_winds earth
[ $? -eq 0 ] && new_grid=1

# verf checks to see if no change from old to new version (regression testing)

function verf {
   echo "regression: $1"
   cmd1="`echo $4 | sed "s=WGRIB2=$wgrib2=g"` | cksum"
   cmd2="`echo $4 | sed "s=WGRIB2=$wgrib2new=g"` | cksum"
   if [ "$mode" = 'SAVE' ] ; then
      for f in $3
      do
        echo `eval $cmd1` >> $results
      done
   else
      for f in $3
      do
         n=`eval $cmd2`
         read m
         if [ "$n" != "$m" ] ; then
            echo "fail $1 file=$f cmd=$cmd1"
            [ "$3" = "FATAL" ] && exit 8 
         fi
      done
   fi
}

# eqv checks to see that two statements give the same results
# always FATAL if does not pass

function eqv {
   [ "$mode" = 'SAVE' ] && return 0
   echo "eqv: $1"
   cmd1="`echo $3 | sed "s=WGRIB2=$wgrib2new=g"` | cksum"
   cmd2="`echo $4 | sed "s=WGRIB2=$wgrib2new=g"` | cksum"

   for f in $2
   do
      n=`eval $cmd1`
      m=`eval $cmd2`
      if [ "$n" != "$m" ] ; then
         echo "fail $1 file=$f cmd=$cmd1"
         echo "fail $1 file=$f cmd=$cmd2"
         exit 8 
      fi
   done
}

# thread tests a statement with 1 and 3 cpus
# should get the same results otherwise FATAL error

function thread {
   [ "$mode" = 'SAVE' ] && return 0
   echo "threading: $1"
   cmd1="`echo $3 | sed "s=WGRIB2=$wgrib2new=g"` | cksum"
   for f in $2
   do
      export OMP_NUM_THREADS=1
      n=`eval $cmd1`
      export OMP_NUM_THREADS=3
      m=`eval $cmd1`
      export OMP_NUM_THREADS=1
      if [ "$n" != "$m" ] ; then
         echo "fail $1 file=$f cmd=$cmd1"
         exit 8 
      fi
   done
}

thread "test decode/-bin" "$files" 'WGRIB2 $f -bin - -inv /dev/null'
thread "test decode/-ieee" "$files" 'WGRIB2 $f -ieee - -inv /dev/null'
thread "test encode c1" "$files" 'WGRIB2 $f -set_grib_type c1 -grib_out - -inv /dev/null'
thread "test encode c2" "$files" 'WGRIB2 $f -set_grib_type c2 -grib_out - -inv /dev/null'
thread "test encode c3" "$files" 'WGRIB2 $f -set_grib_type c3 -grib_out - -inv /dev/null'
thread "test encode s" "$files" 'WGRIB2 $f -set_grib_type s -grib_out - -inv /dev/null'
thread "test lat comp gctpc 0" "$files" 'WGRIB2 $f -d 1 -gctpc 0 -rpn rcl_lat -checksum data'
thread "test lon comp gctpc 0" "$files" 'WGRIB2 $f -d 1 -gctpc 0 -rpn rcl_lon -checksum data'
thread "test lat comp gctpc 1" "$files" 'WGRIB2 $f -d 1 -gctpc 1 -rpn rcl_lat -checksum data'
thread "test lon comp gctpc 1" "$files" 'WGRIB2 $f -d 1 -gctpc 1 -rpn rcl_lon -checksum data'
thread "test new_grid" "$files2" 'WGRIB2 $f -new_grid_winds earth -new_grid ncep grid 221 - -inv /dev/null'


if [ "$proj4" -eq 1 ] ; then
   thread "test lat comp proj4 1" "$files" 'WGRIB2 $f -d 1 -proj4 1 -rpn rcl_lat -checksum data'
   thread "test lon comp proj4 1" "$files" 'WGRIB2 $f -d 1 -proj4 1 -rpn rcl_lon -checksum data'
fi

eqv "test s_out"     "$files"  'WGRIB2 $f -s | cut -f3- -d:' 'WGRIB2 $f -s_out junk >/dev/null ; cat junk'
eqv "test match_fs"  "$files"  'WGRIB2 $f -match_fs "UGRD"' 'WGRIB2 $f -match "UGRD"'
eqv "test not_fs"    "$files"  'WGRIB2 $f -not_fs "UGRD"' 'WGRIB2 $f -not "UGRD"'
eqv "test if_fs"     "$files"  'WGRIB2 $f -if_fs "UGRD" -s -fi' 'WGRIB2 $f -if "UGRD" -s -fi'
eqv "test not_if_fs" "$files"  'WGRIB2 $f -not_if_fs "UGRD" -s -fi' 'WGRIB2 $f -not_if "UGRD" -s -fi'
eqv "test -d"        "$files3" 'WGRIB2 -d 2 -s $f' 'WGRIB2 -match "^(2|2.1):" $f'
eqv "test <grib"     "$files"  'WGRIB2 -for 2:4 $f' 'cat $f | WGRIB2 - -for 2:4'

if [ $version -gt 201503 ] ; then
   eqv "test fgrep"     "$files"  'WGRIB2 $f | fgrep "1:" | WGRIB2 -i $f | wc' 'WGRIB2 $f | WGRIB2 -i $f -fgrep "1:" | wc'
   eqv "test fgrep2"    "$files"  'WGRIB2 $f | fgrep ":500 mb:" | fgrep ":HGT:" | WGRIB2 -i $f | wc' \
				  'WGRIB2 $f | WGRIB2 -i $f -fgrep ":500 mb:" -fgrep ":HGT:" | wc'
   eqv "test fgrep_v"   "$files"  'WGRIB2 $f | fgrep -v ":500 mb:" | WGRIB2 -i $f | wc' \
				  'WGRIB2 $f | WGRIB2 -i $f -fgrep_v ":500 mb:" | wc'
   eqv "test egrep"     "$files"  'WGRIB2 $f | egrep ":(1000|500) mb:" | WGRIB2 -i $f | wc' \
				  'WGRIB2 $f | WGRIB2 -i $f -egrep ":(1000|500) mb:" | wc'
fi


verf "test -s"          "WARN"   "$files"   'WGRIB2 -s $f'
verf "test -v"          "WARN"   "$files"   'WGRIB2 -v $f'
verf "test -ieee"       "FATAL"  "$files"   'WGRIB2  $f -ieee - -inv /dev/null'
verf "test -text"       "WARN"   "$files"   'WGRIB2  $f -text - -inv /dev/null'
verf "test -cvs"        "WARN"   "$files"   'WGRIB2  $f -csv - -inv /dev/null'
verf "test -spread"     "WARN"   "$files"   'WGRIB2  $f -spread - -inv /dev/null'
verf "test -grib_out"   "WARN"   "$files"   'WGRIB2  $f -grib_out - -inv /dev/null'
verf "test -match 1"    "FATAL"  "$files"   'WGRIB2 $f -match "(UGRD|VGRD|TMP)"  -varX'
verf "test -if 1"       "FATAL"  "$files"   'WGRIB2 $f -if "(UGRD|VGRD|TMP)" -varX -fi'
verf "test -ijsmall"    "FATAL"  "$files"   'WGRIB2 $f -ijsmall_grib 1:10 2:20 - -inv /dev/null'
verf "test -small_grib" "FATAL"  "$files"   'WGRIB2 $f -small_grib 270:300 20:40 - -inv /dev/null'
verf "test ncep_norm"   "FATAL"  "$files"   'WGRIB2 $f -ncep_norm - -inv /dev/null'
verf "test -stats"      "WARN"   "$files"   'WGRIB2 -stats $f'
verf "test -grib_out j" "FATAL"  "$files"   'WGRIB2 $f -g2clib 0 -set_grib_type j -inv /dev/null -grib_out -'
verf "test -submsg_uv"  "FATAL"  "$files"   'WGRIB2 $f -new_grid_vectors UGRD:VGRD -submsg_uv - -inv /dev/null'
verf "test -wndspd/dir" "FATAL"  "$files"   'WGRIB2 $f -wind_speed - -wind_dir - -inv /dev/null'

if [ "$new_grid" -eq 1 ] ; then
   verf "test -new_grid 1" "FATAL"  "$files2"  'WGRIB2 $f -new_grid_winds earth -new_grid ncep grid 221 - -inv /dev/null'
fi
