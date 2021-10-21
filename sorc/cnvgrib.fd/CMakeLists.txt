# This is the CMake file for the cnvgrib utility of the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src cnv12.f cnv22.f gds2gdt.f makepdsens.f pds2pdtens.f
    putgbexn.f cnv21.f cnvgrib.f gdt2gds.f makepds.f pds2pdt.f
    setbit.f)

set(exe_name cnvgrib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE g2::g2_4
  bacio::${bacio_name} w3nco::w3nco_4 ${JASPER_LIBRARIES} PNG::PNG
  ${ZLIB_LIBRARY})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
