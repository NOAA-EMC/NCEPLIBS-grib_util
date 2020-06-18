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
target_link_libraries(${exe_name} w3nco::w3nco_4 bacio::bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION bin)
