# grb2index

# Introduction

The index file serves as a table of contents for the GRIB1 file,
enabling quick access to the data.

For GRIB1 files, use <a href="../grbindex/index.html">grbindex</a>.

There can be a gap before the first GRIB message of at most 32000
bytes and gaps between messages of at most 4000 bytes.

# Usage

@code
grb2index \<GRIB2 file\> \<index filename\>
@endcode

The first argument is the name of the input GRIB2 file. The second argument is
the name of the output index file.

# Index File Format

Version 2 of the index file format is used with GRIB2 files, and has
the following format:

The index file has two header records:
- 81-byte "Steve Lord" header with 'GB2IX1' in columns 42-47
- 81-byte header with number of bytes to skip before index
records, total length in bytes of the index records, number of
index records, and GRIB file basename written in format
('IX1FORM:',3i10,2x,a40).

Each following index record corresponds to a GRIB1 message and
contains the following fields. All integers are in big-endian format
in the file.

- byte 001 - 004 length of index record
- byte 005 - 008 bytes to skip in data file before grib message
- byte 009 - 012 bytes to skip in message before lus (local use) set = 0, if no local section.
- byte 013 - 016 bytes to skip in message before gds
- byte 017 - 020 bytes to skip in message before pds
- byte 021 - 024 bytes to skip in message before drs
- byte 025 - 028 bytes to skip in message before bms
- byte 029 - 032 bytes to skip in message before data section
- byte 033 - 040 bytes total in the message
- byte 041 - 041 grib version number (currently 2)
- byte 042 - 042 message discipline
- byte 043 - 044 field number within grib2 message
- byte 045 -  ii identification section (ids)
- byte ii+1-  jj grid definition section (gds)
- byte jj+1-  kk product definition section (pds)
- byte kk+1-  ll the data representation section (drs)
- byte ll+1-ll+6 first 6 bytes of the bit map section (bms)

