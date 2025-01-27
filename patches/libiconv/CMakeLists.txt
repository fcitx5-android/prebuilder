cmake_minimum_required(VERSION 3.22.1)
project(libiconv)

set(DLL_VARIABLE "")
set(EILSEQ "")
set(ICONV_CONST "")
set(USE_MBSTATE_T "1")
set(BROKEN_WCHAR_H "0")
set(HAVE_WCHAR_T "1")
configure_file("include/iconv.h.in" "iconv.h" @ONLY)
configure_file("libcharset/include/localcharset.h.in" "localcharset.h" @ONLY)

set(LIBICONV_HEADER
    "${CMAKE_CURRENT_BINARY_DIR}/iconv.h"
)
set(LIBICONV_SOURCE
    lib/iconv.c
    libcharset/lib/localcharset.c
)

add_library(iconv STATIC ${LIBICONV_SOURCE})
target_include_directories(iconv PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}")
install(TARGETS iconv)
install(FILES ${LIBICONV_HEADER} DESTINATION "include")
