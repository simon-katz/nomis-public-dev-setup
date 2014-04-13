# nomis-emacs-configuration

Simon Katz's Emacs init.

This repository contains both `.emacs` stuff and `.emacs.d/` stuff.

Why combine `.emacs` stuff and `.emacs.d/` stuff in a single repo?
Although this stuff would download `.emacs.d/` stuff if it didn't exist,
that's a recipe for not knowing what versions of packages you have and
having different versions if you download packages at different times
or on different machines. That's caused me problems.

Contents:

* `make-emacs-installation.sh`

    A script to download packages in to `.../emacs-installation/.emacs.d/`

* `run-emacs-installation.sh`

    A script to run Emacs. (You can run this Emacs config from here without having to set up .emacs and .emacs.d in your home directory.)
    
* `emacs-installation/`

    The Emacs installation.

* `emacs-installation/.emacs`

    An init file that is automatically loaded by Emacs. This loads `dot-emacs.el`.

* `emacs-installation/.emacs.d/`

    Emacs's .emacs.d directory. Amongst other things, this contains packages downloaded by Emacs.

* `emacs-installation/dot-emacs.el`

    The top-level Emacs tailoring file. This loads other stuff.

* `emacs-installation/emacs-init-files`

    A directory containing files to tailor the Emacs installation.

To use:

* Browse, see what's here, copy stuff and modify it.

or:

* Really use it:

    * Either:
  
        * Run `run-emacs-installation.sh`
        
        or 
        
        * Install this as your real Emacs configuration:
            * Create a symbolic link from `~/.emacs.d` to `.../emacs-installation/.emacs.d/`.
        
            and


            * Create a symbolic link from `~/.emacs`   to `.../emacs-installation/.emacs`.
