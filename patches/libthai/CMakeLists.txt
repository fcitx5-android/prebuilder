cmake_minimum_required(VERSION 3.22.1)
project(libthai)

set(LIBTHAI_HEADER
    "include/thai/thailib.h"
    "include/thai/thcell.h"
    "include/thai/thinp.h"
)
set(LIBTHAI_SOURCE
    src/thcell/thcell.c
    src/thctype/thctype.c
    src/thctype/wtt.c
    src/thinp/thinp.c
)

add_library(thai STATIC ${LIBTHAI_SOURCE})
target_include_directories(thai PRIVATE include)
install(TARGETS thai)
install(FILES ${LIBTHAI_HEADER} DESTINATION include/thai)
