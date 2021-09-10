#
# There are many .bashrc files. This one is mine.
#

# Work stuff, perhaps
DBXRC="$HOME/.bashrc.dbx"

if [[ -e "$DBXRC" ]]
then
    source "$DBXRC"
fi

export PATH=$HOME/bin:$PATH

# Local Perl on Windows
PERL_LOCAL=/usr/local/share/perl5/site_perl

if [[ -e "$PERL_LOCAL" ]]
then
    export PERL5LIB="$PERL_LOCAL"
fi

# ssh-pageant on Windows
SSH_PAGEANT=/usr/bin/ssh-pageant

if [[ -e $SSH_PAGEANT ]]
then
    eval $(/usr/bin/ssh-pageant -r -a "/tmp/.ssh-pageant-$USERNAME")
fi

# Cargo, perhaps
CARGO_ENV="$HOME/.cargo/env"

if [[ -e $CARGO_ENV ]]
then
  source "$CARGO_ENV"
fi

# vterm in Emacs depends on this
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi
