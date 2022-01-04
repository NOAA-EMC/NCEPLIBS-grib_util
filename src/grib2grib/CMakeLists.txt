# This is the CMake file for the grib2grib utility of the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -i8 -r8 -assume noold_ldout_format -axCORE-AVX2 -fpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-integer-8 -fdefault-real-8 -cpp ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${CMAKE_Fortran_FLAGS}")
endif()

set(fortran_src grib2grib.f hexchar.f)

set(exe_name grib2grib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_8 bacio::${bacio_name})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()
