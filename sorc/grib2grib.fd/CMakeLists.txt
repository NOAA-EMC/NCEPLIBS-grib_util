if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -i8 -r8 -assume noold_ldout_format -axCORE-AVX2 -fpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fdefault-integer-8 -fdefault-real-8 -cpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src grib2grib.f hexchar.f)

set(exe_name grib2grib)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} w3nco::w3nco_8 bacio::bacio_8)

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
