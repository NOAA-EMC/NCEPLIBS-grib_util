#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * gmerge    w. ebisuzaki
 * public domain May 2009
 *  8/2012 Manfred Schwarb - added declarations, exit(0)
 *  2/2015 Wesley Ebisuzaki - can write to stdout by filename='-'
 *
 * takes the input of N files or pipes containing grib2 files
 * and merges them into one file.
 *
 */

#define VERSION "gmerge v1.2 2/2015"

unsigned long int uint8(unsigned char *);
int rd_msg(FILE *, FILE *);

#define N 32

int main(int argc, char **argv) {

    FILE *out, *p[N];
    int eofs[N];
    int i, n, done;

    if (argc < 4) {
	fprintf(stderr,"%s bad arg: output (list of grib-inputs)\n",VERSION);
	exit(8);
    }

    /* open output file */
    if (strcmp(argv[1], "-") == 0) {
	out = stdout;
    }
    else {
        if ((out = fopen(argv[1],"wb")) == NULL) {
	    fprintf(stderr,"bad arg: output=%s\n",argv[1]);
	    exit(8);
        }
    }
    n = argc - 2;
    for (i = 0; i < n; i++) {
	p[i] = fopen(argv[i+2], "rb");
	if (p[i] == NULL) {
	    fprintf(stderr,"bad file: %s\n",argv[i+2]);
	    exit(8);
	}
        eofs[i] = 0;
    }

    done = 0;
    while (done != n) {
        done  = 0;
	for (i = 0; i < n; i++) {
	    if (eofs[i] == 1) {
	        done++;
	    }
	    else {
    	       if (rd_msg(p[i], out)) {
		    done++;
		    eofs[i] = 1;
		}
	    }
	}
    }
    exit(0);
}

#define BSIZE 4096

int rd_msg(FILE *in, FILE *out) {
    long unsigned int n;
    int i,j,k;
    unsigned char header[BSIZE];

    i = fread(header, 1, 16, in);
    if (i != 16) return -1;
    if (header[0] != 'G' || header[1] != 'R' || header[2] != 'I' || 
		header[3] != 'B') return -1;

     n = uint8(&(header[8]));

     j = n < BSIZE ? n : BSIZE;
     k = fread(header+16,1,j-16,in);
     if (k != j-16) return -1;

     fwrite(header,1,j,out);
     n -= j;

     while (n) {
         j = n < BSIZE ? n : BSIZE;
         k = fread(header,1,j,in);
         fwrite(header,1,j,out);
         n -= j;
    }
    return 0;
}
