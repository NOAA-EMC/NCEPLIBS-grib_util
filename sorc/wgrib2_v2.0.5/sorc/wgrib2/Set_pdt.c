#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * the set options
 *
 * routines make a generic PDT
 *
 * 9/2008 Public Domain by Wesley Ebisuzaki
 * 10/2013 use update_sec4()
 * 7/2014 added copy more metadata when making new pdt
 *        if copying metadata, check for number of time ranges, make larger if needed
 */

/*
 * HEADER:100:set_pdt:misc:1:makes new pdt, X=(+)PDT_number or X=(+)PDT_number:size of PDT in octets, +=copy metadata
 */


int f_set_pdt(ARG1) {

    int pdt, i, len, copy_metadata, extra_time_range, k;
    unsigned char new_sec4[SET_PDT_SIZE];
    unsigned char *new_sec[9];
    unsigned char *p_old, *p_new;

    if (mode < 0) return 0;

    i = sscanf(arg1,"%d:%d", &pdt, &len);
    if (i == 0) fatal_error("set_pdt: X=PDT_number[:PDT_SIZE]","");

    copy_metadata = arg1[0] == '+';

    /* number of bytes for extra time ranges */
    extra_time_range = 0;
    if (copy_metadata) {
       k = stat_proc_n_time_ranges_index(sec);
       if (k >= 0) {
           extra_time_range = sec[4][k];
	   if (extra_time_range == 255) fatal_error("set_pdt: missing n_time_ranges","");
           extra_time_range = extra_time_range >= 2 ? 12*(extra_time_range - 1) : 0;
       }
    }
       
    if (i == 1) {	// use default PDT size
        len = smallest_pdt_len(pdt);
	if (len < 0) fatal_error_i("set_pdt: unsupported pdt=%d",pdt);
	len += extra_time_range;
    }

    if (len > SET_PDT_SIZE) fatal_error_ii("set_pdt: maximum pdt len is %d wanted %d", SET_PDT_SIZE, len);

    uint_char(len, new_sec4);		    // size of sec[4];
    new_sec4[4] = 4;                        // section number
    uint2_char(0, new_sec4+5);              // no extra coordinates
    uint2_char(pdt, new_sec4+7);            // pdt
    for (i = 9; i < len; i++) new_sec4[i] = 255;

    if (copy_metadata) {
	for (i = 0; i < 9; i++) new_sec[i] = sec[i];
        new_sec[4] = new_sec4;
	new_sec4[9] = sec[4][9];		// parmmeter category 4.1
	new_sec4[10] = sec[4][10];		// parameter number   4.2

	p_old = code_table_4_3_location(sec);
	p_new = code_table_4_3_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = code_table_4_4_location(sec);
	p_new = code_table_4_4_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 5; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_5a_location(sec);
	p_new = code_table_4_5a_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 6; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_5b_location(sec);
	p_new = code_table_4_5b_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 6; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_6_location(sec);
	p_new = code_table_4_6_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = perturbation_number_location(sec);
	p_new = perturbation_number_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = number_of_forecasts_in_the_ensemble_location(sec);
	p_new = number_of_forecasts_in_the_ensemble_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = code_table_4_7_location(sec);
	p_new = code_table_4_7_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	/* probabilty info */
	p_old = code_table_4_9_location(sec);
	p_new = code_table_4_9_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = -2; i < 11; i++) p_new[i] = p_old[i];
	}

	p_old = background_generating_process_identifier_location(sec);
	p_new = background_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = analysis_or_forecast_generating_process_identifier_location(sec);
	p_new = analysis_or_forecast_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = hours_of_observational_data_cutoff_after_reference_time_location(sec);
	p_new = hours_of_observational_data_cutoff_after_reference_time_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    *p_new++ = *p_old++;
	    *p_new = *p_old;
	}

	p_old = observation_generating_process_identifier_location(sec);
	p_new = observation_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	/* time ranges for stat processing */
	p_old = stat_proc_verf_time_location(sec);
	p_new = stat_proc_verf_time_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
            k = (58-34) + extra_time_range;
	    for (i = 0; i < k; i++) p_new[i] = p_old[i];
	}

	/* percentile values */
	p_old = percentile_value_location(sec);
	p_new = percentile_value_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

    }
    update_sec4(sec, new_sec4);
    return 0;
}

