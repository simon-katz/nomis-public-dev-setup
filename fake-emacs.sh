# Creating a scratch Emacs
# - See Nic Ferrier's http://www.emacsbites.com/episode/scratch-emacs.

case "$OSTYPE" in
  darwin*)  emacs_exec="open -n -a /Applications/Emacs-24-2-1.app --args" ;; 
  msys)     emacs_exec="emacs" ;;
  *)        emacs_exec="emacs -nw" ;;
esac

function fake-emacs {
    [ "$1" != "" ] || { echo "missing arg -- need an installation name" ; return 1 ; }
    emacsd=$1
    shift
    [ -d $emacsd ] || mkdir $emacsd
    HOME="`pwd`/$emacsd" $emacs_exec $*
}
