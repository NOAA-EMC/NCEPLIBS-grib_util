if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -convert big_endian -axCORE-AVX2 -fpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "-g -fconvert=big-endian -cpp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src grb2index.f)

set(exe_name grb2index)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} g2::g2_4 w3nco::w3nco_4 bacio::bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
