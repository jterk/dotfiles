#
# There are many .bashrc files. This one is mine.
#

# Boxen, if present
if [ -e /opt/boxen/env.sh ]
then
    source /opt/boxen/env.sh
fi

if which pyenv > /dev/null
then
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
fi

export PATH=$HOME/bin:$PATH
