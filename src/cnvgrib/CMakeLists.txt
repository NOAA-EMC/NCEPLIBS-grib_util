# This is the CMake file for the cnvgrib utility of the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

# These are the source files for cnvgrib.
set(fortran_src cnv12.F90 cnv22.F90 gds2gdt.F90 makepdsens.F90
pds2pdtens.F90 putgbexn.F90 cnv21.F90 cnvgrib.F90 gdt2gds.F90
makepds.F90 pds2pdt.F90 setbit.F90)

# Build the executable.
message(STATUS "In cnvgrib using bacio library ${bacio_name}")
message(STATUS "In cnvgrib ${JASPER_LIBRARIES}")
message(STATUS "In cnvgrib ${ZLIB_LIBRARIES}")
set(exe_name cnvgrib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} g2::g2_4
  bacio::${bacio_name} w3emc::w3emc_4 ${JASPER_LIBRARIES} PNG::PNG
  ${ZLIB_LIBRARY})

# Install cnvgrib.
install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
