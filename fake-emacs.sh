# Creating a scratch Emacs
# - See Nic Ferrier's http://www.emacsbites.com/episode/scratch-emacs.

# emacs_exec="emacs -nw"
emacs_exec="open -n -a /Applications/Emacs-24-2-1.app --args"

function fake-emacs {
    [ "$1" != "" ] || { echo "missing arg -- need an installation name" ; return 1 ; }
    emacsd=$1
    shift
    [ -d $emacsd ] || mkdir $emacsd
    HOME="`pwd`/$emacsd" $emacs_exec $*
}
