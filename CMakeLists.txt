cmake_minimum_required(VERSION 2.8.3)
project(euslime)

find_package(catkin REQUIRED)
find_package(Git QUIET)

catkin_package()

include_directories()

if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
# Update submodules as needed
    option(GIT_SUBMODULE "Check submodules during build" ON)
    if(GIT_SUBMODULE)
        message(STATUS "Submodule update")
        execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                        RESULT_VARIABLE GIT_SUBMOD_RESULT)
        if(NOT GIT_SUBMOD_RESULT EQUAL "0")
            message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
        endif()
    endif()
endif()

install(FILES
  README.md
# EusLisp files
  slime-toplevel.l
  slime-util.l
# Emacs Lisp files
  euslime-config.el
  slime-euslisp.el
# Destination
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
)

install(FILES
  slime-repl-ansi-color/README.md
  slime-repl-ansi-color/LICENSE
  slime-repl-ansi-color/slime-repl-ansi-color.el
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}/slime-repl-ansi-color
)
