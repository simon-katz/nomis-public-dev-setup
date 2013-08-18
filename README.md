# nomis-dot-emacs

Simon Katz's Emacs init.

This repository contains both `.emacs` stuff and `.emacs.d/` stuff.

Why combine `.emacs` stuff and `.emacs.d/` stuff? Although the
`.emacs` stuff would download `.emacs.d/` stuff if it didn't exist,
that's a recipe for not knowing what versions of packages you have and
having different versions if you download packages at different times
or on different machines. That's caused me problems.

Contents:

*   `.../dot-emacs-d-stuff/`

     My .emacs.d stuff.

*   `.../dot-emacs-stuff/`

    My `.emacs` stuff.

    * `.../dot-emacs-stuff/dot-emacs.el`

        Essentially my `.emacs`. This loads other stuff.

    * `.../dot-emacs-stuff/for-anyone/`

        A directory containing things that are probably good for anyone to use.

    * `.../dot-emacs-stuff/for-me/`

        A directory containing things that may not be good for
          anyone other than me to use, either because they are crap or
          because they may change significantly or because I haven't
          decided yet that they're ok. Of course, you can copy
          anything you want or use what's here as a source of ideas.

To use:
* Browse, see what's here, copy stuff and modify it.

or:

* Really use it:
    * Create a symbolic link from `~/.emacs.d` to `.../dot-emacs-d-stuff/`.
    * Create a symbolic link from `~/.emacs` to
      `.../dot-emacs-stuff/dot-emacs.el`. (Or maybe load
      `.../dot-emacs-stuff/dot-emacs.el` from your `~/.emacs`.)
