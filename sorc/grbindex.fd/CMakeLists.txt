if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -assume noold_ldout_format -convert big_endian -axCORE-AVX2 -fpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -cpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src grbindex.f)

set(exe_name grbindex)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} w3nco_4 bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
