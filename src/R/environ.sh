#!/usr/bin/env sh
#
# env-vars
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/EnvVar.html
#
# libPaths
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/libPaths

# only VAR=VALUE per line, nothing else!

#R_HOME="${HOME}/R"
R_BROWSER="brave"
# only in .libPaths() if dir exists
R_LIBS_USER="${HOME}/R/library/4.0"
R_HISTFILE="${HOME}/.Rhistory"
