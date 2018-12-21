# make for wgrib

wgrib:	wgrib.c
	cc -O3 -m64  -axCORE-AVX2 -o wgrib -DDEF_T62_NCEP_TABLE=opn -DFAST_GRIBTAB -DP_TABLE_FIRST wgrib.c

