#!/usr/bin/env sh

#------------------------------------------------------------------------------#
# download output from gitignore and append it to content

__CONTENT="\
#------------------------------------------------------------------------------#
# gitignore\
"

__IGNORED_ITEMS="macos,visualstudiocode"
__CONTENT="${__CONTENT}$(curl -L -s https://www.gitignore.io/api/${__IGNORED_ITEMS})\n"



#------------------------------------------------------------------------------#
# append custom

__CONTENT="${__CONTENT}\n\n\n\
#------------------------------------------------------------------------------#
# Custom

/custom/

.vscode/"



#------------------------------------------------------------------------------#
# print to console for piping

echo "${__CONTENT}"
