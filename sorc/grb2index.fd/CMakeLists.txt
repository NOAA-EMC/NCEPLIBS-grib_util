
if(IntelComp)
  set(fortran_flags " -g" "-O3" "-convert big_endian" "-axCORE-AVX2" "-fpp")
elseif(GNUComp)
  set(fortran_flags "-g" "-O3" "-fconvert=big-endian" "-cpp")
else()
  message("unknown compiler!")
  exit()
endif()

set(exe_name grb2index)

file(GLOB fortran_src
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/*.F)

add_executable(${exe_name} ${fortran_src})

target_compile_options(${exe_name} PRIVATE ${fortran_flags})

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_4")
set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(${exe_name} PUBLIC "${module_dir}")

target_link_libraries(${exe_name} g2_4 w3nco_4 bacio_4)

install(TARGETS ${exe_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION include_4)

