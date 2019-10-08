cmake_minimum_required(VERSION 2.6)
set(EXENAME cnvgrib )
file(GLOB EXE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f ${CMAKE_CURRENT_SOURCE_DIR}/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/*.F)
#file(GLOB EXE_C_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
  if(IntelComp)
    message("setting intel flags")
    set(CMAKE_Fortran_8_FLAGS "-O3 -g " CACHE INTERNAL "" )
  else()
    message("unkown compiler!")
    exit()
  endif()
endif()

#set_source_files_properties( ${EXE_C_SRC} PROPERTIES COMPILE_FLAGS ${CMAKE_C_FLAGS} )
include_directories( ${G2_INC4} )
set_source_files_properties( ${EXE_SRC} PROPERTIES COMPILE_FLAGS ${CMAKE_Fortran_8_FLAGS} )
#add_definitions( ${CMAKE_Fortran_8_FLAGS} )
add_executable(${EXENAME} ${EXE_SRC} ${EXE_C_SRC} )
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_8")
set_target_properties(${EXENAME} PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_8")
target_include_directories(${EXENAME} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include_8")
target_link_libraries(${EXENAME} ${G2_LIB4} ${BACIO_LIB4} ${W3NCO_LIB4} ${JASPER_LIBRARIES} ${PNG_LIBRARIES} ${ZLIB_LIBRARY} )

file(MAKE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

install(TARGETS ${EXENAME}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
install(DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/ DESTINATION include_8 )

