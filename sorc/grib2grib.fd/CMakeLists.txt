
if(IntelComp)
  set(fortran_flags "-g" "-O3" "-r8" "-i8" "-assume noold_ldout_format" "-axCORE-AVX2" "-fpp")
elseif(GNUComp)
  set(fortran_flags "-g" "-O3" "-fdefault-real-8" "-fdefault-integer-8" "-cpp")
else()
  message("unknown compiler!")
  exit()
endif()

set(exe_name grib2grib )

file(GLOB fortran_src
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/*.F)

add_executable(${exe_name} ${fortran_src})

target_compile_options(${exe_name} PRIVATE ${fortran_flags})

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_4")
set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(${exe_name} PUBLIC "${module_dir}")

target_link_libraries(${exe_name} w3nco_8 bacio_8)

install(TARGETS ${exe_name}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib) 
install(DIRECTORY ${module_dir} DESTINATION include_8)

