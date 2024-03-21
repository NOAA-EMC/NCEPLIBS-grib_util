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

or

@code
grb2index \<idxver\> \<GRIB2 file\> \<index filename\>
@endcode

The first argument is the index format, 1 or 2. The second is the name
of the input GRIB2 file. The third argument is the name of the output
index file.

# Index File Format

The index file has two header records:
1. an 81-byte header with 'GB2IX1' in columns 42-47
2. an 81-byte header with the index version number, the number of
bytes to skip before index records, total length in bytes of the
index records, number of index records, and the GRIB file basename.

Each record in the index table contains the following fields. All
integers are in big-endian format in the file. The only difference
between index version 1 and index version 2 is the size of the
field containing the number of bytes to skip in file before
message. To accomodate files > 2 GB, this must be a 64-bit int.

Index Version 1 | Index Version 2 | Contents
----------------|-----------------|---------
001 - 004 | 001 - 004 | length of index record
005 - 008 | 005 - 012 | bytes to skip in data file before grib message
009 - 012 | 013 - 016 | bytes to skip in message before lus (local use) set = 0, if no local section.
013 - 016 | 017 - 020 | bytes to skip in message before gds
017 - 020 | 021 - 024 | bytes to skip in message before pds
021 - 024 | 025 - 028 | bytes to skip in message before drs
025 - 028 | 029 - 032 | bytes to skip in message before bms
029 - 032 | 033 - 036 | bytes to skip in message before data section
033 - 040 | 037 - 044 | bytes total in the message
041 - 041 | 045 - 045 | grib version number (always 2)
042 - 042 | 046 - 046 | message discipline
043 - 044 | 047 - 048 | field number within grib2 message
045 -  ii | 045 -  ii | identification section (ids)
ii+1-  jj | ii+1-  jj | grid definition section (gds)
jj+1-  kk | jj+1-  kk | product definition section (pds)
kk+1-  ll | kk+1-  ll | the data representation section (drs)
ll+1-ll+6 | ll+1-ll+6 | first 6 bytes of the bit map section (bms)


