# grbindex

# Introduction

The index file serves as a table of contents for the GRIB1 file,
enabling quick access to the data.

For GRIB2 files, use <a href="../grb2index/index.html">grb2index</a>.

There can be a gap before the first GRIB1 message of at most 32000
bytes and gaps between messages of at most 4000 bytes. 

# Usage

@code
grbindex \<GRIB1 file\> \<index filename\>
@endcode

The first argument is the name of the input GRIB1 file. The second argument is
the name of the output index file.

# Index File Format

Version 1 of the index file format is used with GRIB1 files, and has the
following format:
     
- 81-byte "Steve Lord" header with 'GB1IX1' in columns 42-47
- 81-byte header with number of bytes to skip before index records,
number of bytes in each index record, number of index records, and
GRIB1 file basename written in format ('IX1FORM:',3i10,2x,a40).
     
Each following index record corresponds to a GRIB1 message
and has the format. All integers are in big-endian format
in the file.

-  byte 001-004: bytes to skip in data file before GRIB1 message
-  byte 005-008: bytes to skip in message before pds
-  byte 009-012: bytes to skip in message before gds (0 if no gds)
-  byte 013-016: bytes to skip in message before bms (0 if no bms)
-  byte 017-020: bytes to skip in message before bds
-  byte 021-024: bytes total in the message
-  byte 025-025: GRIB version number
-  byte 026-053: product definition section (pds)
-  byte 054-095: grid definition section (gds) (or nulls)
-  byte 096-101: first part of the bit map section (bms) (or nulls)
-  byte 102-112: first part of the binary data section (bds)
-  byte 113-172: (optional) bytes 41-100 of the pds
-  byte 173-184: (optional) bytes 29-40 of the pds
-  byte 185-320: (optional) bytes 43-178 of the gds

