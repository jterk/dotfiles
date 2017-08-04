#
# There are many .bashrc files. This one is mine.
#

if which pyenv > /dev/null
then
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
fi

export PATH=$HOME/bin:$PATH
