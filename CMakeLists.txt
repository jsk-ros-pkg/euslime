cmake_minimum_required(VERSION 2.8.3)
project(euslime)

find_package(catkin REQUIRED catkin_virtualenv)
find_package(Git QUIET)

catkin_package()

# Configure files
configure_file(
  ${PROJECT_SOURCE_DIR}/euslime.el.in
  ${PROJECT_SOURCE_DIR}/euslime.el
)

# git submodule update
find_package(Git QUIET)
option(GIT_SUBMODULE "Check submodules during build" ON)
if(Git_FOUND AND GIT_SUBMODULE)
  message(STATUS "Submodule update")
  execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE GIT_SUBMOD_RESULT)
  if(NOT GIT_SUBMOD_RESULT EQUAL "0")
    message(FATAL_ERROR "git submodule update failed with ${GIT_SUBMOD_RESULT}")
  endif()
endif()


catkin_generate_virtualenv()

# Install files
install(FILES
  README.md
  requirements.txt
# EusLisp files
  slime-connection.l
  slime-roseus.l
  slime-toplevel.l
  slime-util.l
# Emacs Lisp files
  euslime-config.el
  euslime.el
# Destination
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}
)

install(FILES
  slime-repl-ansi-color/README.md
  slime-repl-ansi-color/LICENSE
  slime-repl-ansi-color/slime-repl-ansi-color.el
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}/slime-repl-ansi-color
)

# Install Python
catkin_python_setup()
catkin_install_python(PROGRAMS
  scripts/euslime
  DESTINATION ${CATKIN_PACKAGE_BIN_DESTINATION}
)
