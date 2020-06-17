if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -axCORE-AVX2")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS "-g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set(fortran_src tocgrib2.f)

set(exe_name tocgrib2)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  g2_4
  w3nco_4
  bacio_4
  ${PNG_LIBRARIES}
  ${JASPER_LIBRARIES}
  ${ZLIB_LIBRARY})

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
