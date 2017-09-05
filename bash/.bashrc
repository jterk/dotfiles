#
# There are many .bashrc files. This one is mine.
#

DBXRC="$HOME/.bashrc.dbx"

if [[ -e "$DBXRC" ]]
then
    source "$DBXRC"
fi

export PATH=$HOME/bin:$PATH
