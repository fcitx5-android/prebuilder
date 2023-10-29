cmake_minimum_required(VERSION 3.22.1)
project(libiconv)

set(LIBICONV_HEADER
    "include/iconv.h"
)
set(LIBICONV_SOURCE
    lib/iconv.c
    libcharset/lib/localcharset.c
)

add_library(iconv STATIC ${LIBICONV_SOURCE})
target_include_directories(iconv PRIVATE include libcharset/include)
install(TARGETS iconv)
install(FILES ${LIBICONV_HEADER} DESTINATION include)
