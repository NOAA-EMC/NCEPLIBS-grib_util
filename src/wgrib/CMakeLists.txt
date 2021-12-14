# This is the CMake file for the wgrib utility of the
# NCEPLIBS-grib_util project.
#
# Mark Potts, Kyle Gerheiser
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -m64")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -m64")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

add_compile_definitions("DEF_T62_NCEP_TABLE=opn" "FAST_GRIBTAB" "P_TABLE_FIRST")

set(c_src wgrib.c)

set(exe_name wgrib)
add_executable(${exe_name} ${c_src})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)

# If doxygen documentation is enabled, build it.
if(ENABLE_DOCS)
  add_subdirectory(docs)  
endif()














