cmake_minimum_required(VERSION 2.6)
set(EXENAME wgrib )
file(GLOB EXE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
if(IntelComp)
    message("setting intel flags")
    set(CMAKE_C_FLAGS "-O3 -m64 -DDEF_T62_NCEP_TABLE=opn -DFAST_GRIBTAB -DP_TABLE_FIRST" CACHE INTERNAL "" )
else()
    message("unkown compiler!")
    exit()
endif()


add_executable(${EXENAME} ${EXE_SRC} ${EXE_C_SRC} )
set_target_properties( ${EXENAME} PROPERTIES COMPILE_FLAGS ${CMAKE_C_FLAGS} )
set(CMAKE_INCLUDE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_4")
set_target_properties(${EXENAME} PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include_4")
target_include_directories(${EXENAME} PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/include_4")
target_link_libraries(${EXENAME} ${W3NCO_LIB4} ${BACIO_LIB4} )

file(MAKE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY})

install(TARGETS ${EXENAME}
        RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
install(DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/ DESTINATION include_4 )

