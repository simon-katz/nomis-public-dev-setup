;;; -*- Emacs-Lisp -*-


;;; JSK re-working / highlighting / whatever of the documentation for
;;; windows.el.

;;; jsk-begin
;;;   Note that I've changed all `C-c C-w' to `C-c C-q' to match the
;;;   configuration change I've made.  (I made the configuration change
;;;   because of a conflict with Slime.)
;;;
;;;   I've also removed references to pre/post Emacs 19, and included
;;;   only the Emacs 19 or later stuff.
;;; jsk-end



;;; Window manager for GNU Emacs.
;;; $Id: windows.el,v 2.48 2010/05/23 12:33:40 yuuji Exp $
;;; (c) 1993-2010 by HIROSE Yuuji [yuuji@gentei.org]
;;; Last modified Sun May 23 21:27:31 2010 on firestorm

;;;[What is Windows?]
;;;
;;;    Windows.el  enables  you  to:
;;;     - have multiple favorite window configurations at the same
;;;       time
;;;     - switch between them
;;;     - save all window configurations and some global or
;;;       buffer-local variables into a file
;;;     - restore the saved info.
;;;
;;;    This package provides the `named(numbered) frame'  that can be
;;;    selected directly with their name.   With revive.el, all frames
;;;    displaying buffers, window configurations and size can be saved
;;;    and restored.

;;;[Installation]
;;;
;;;    Put windows.el into the directory load-path indicates, and put
;;;    the following expressions into your ~/.emacs.
;;;
;;;       (require 'windows)
;;;       (win:startup-with-window)
;;;       (define-key ctl-x-map "C" 'see-you-again)

;;;[Key Bindings]
;;;
;;;    The default prefix key stroke for Windows is `C-c C-w'.  If it
;;;    causes  you some  troubles, see  the  section  `Customizations'.
;;;
;;;    jsk-begin
;;;      I've changed this to `C-c C-q'.
;;;    jsk-end
;;;
;;;    Here are the default key bindings.
;;;
;;;        C-c C-q 1    (Q)  Switch to window 1
;;;        C-c C-q 2    (Q)  Switch to window 2
;;;        C-c C-q 3    (Q)  Switch to window 3
;;;        C-c C-q 4    (Q)  Switch to window 4
;;;        C-c C-q 5    (Q)  Switch to window 5
;;;        C-c C-q 6    (Q)  Switch to window 6
;;;        C-c C-q 7    (Q)  Switch to window 7
;;;        C-c C-q 8    (Q)  Switch to window 8
;;;        C-c C-q 9    (Q)  Switch to window 9
;;;        C-c C-q 0         Select a non- windows.el frame
;;;        C-c C-q SPC  (Q)  Switch to window previously shown
;;;        C-c C-q C-n       Switch to next window
;;;        C-c C-q C-p       Switch to previous window
;;;        C-c C-q !    (Q)  Delete current window
;;;        C-c C-q C-w       Window operation menu
;;;        C-c C-q C-r       Resume menu
;;;        C-c C-q C-l       Local resume menu
;;;        C-c C-q C-s       Switch task
;;;        C-c C-q =    (Q)  Show window list
;;;
;;;    For the commands marked (Q), the C-q may be omitted.
;;;    To disable these  quick key strokes, set the variable
;;;    win:quick-selection to `nil' in your ~/.emacs.

;;;[Description]
;;;(for Emacs19 or later)
;;;
;;;    Windows.el manages   maximum  10 (or   more if you  configure)
;;;    frames.  The first frame is `window#1'.  If you want to create a
;;;    new frame as  `window#2', type `C-c C-q 2'.  You can  create or
;;;    switch  numbered frames as  many  as you  want.  Unlike standard
;;;    Emacs's frame selection (`C-x  5 o'), windows.el enables you  to
;;;    go to the desired frame directly.
;;;
;;;    For detailed  frame  management, read the  section `Other
;;;    functions'.  In that section, the term `window' is used to refer
;;;    the frame.

;;;[Description]
;;;(for Emacs18 or emacs -nw)
;;;
;;;    jsk-begin
;;;      It's very unclear whether the following is for Emacs 19+.
;;;    jsk-end
;;;
;;;    If you have wrote `win:startup-with-window' in your  .emacs,
;;;    the initial window configuration is memorized in the buffer 1.
;;;    Otherwise you have to memorize it by typing `C-c C-q 1'.
;;;    Anyway, say you are editing some program in buffer 1.
;;;
;;;    After a  while, a mail has arrived.   You may wish to assign a
;;;    mail mode configuration  to the  buffer 2.   Type `C-c C-q 2' to
;;;    create the buffer 2,  and  you  will  see  the  `window creation
;;;    menu':
;;;
;;;  C)reate D)uplicate P)reserve F)indfile B)uff X)M-x k)KeyFunc&exit N)o:
;;;
;;;    in the  minibuffer.   Since  you don't need  the current  window
;;;    configuration  (programming  configuration) to read  mails, type
;;;    `c', stands for Create, to create the new  window configuration.
;;;    Then, after invoke the mail reader, current window configuration
;;;    will turn to mail oriented one. (Of course you can directly call
;;;    e-mail mode by typing `x' at the window creation menu.) When you
;;;    finish  to  read  your  mail,  you  can  return  to  programming
;;;    configuration with `C-c C-q 1'.
;;;
;;;    Let's read the NetNews.  M-x gnus...  O.K.!  You've read.  You
;;;    may have typed `q' to quit gnus until yesterday.   But you don't
;;;    have to do it  from  today.   You  can go  back  to  programming
;;;    immediately by typing  `C-c C-q 1'!  Oops, you  have not created
;;;    the  new  configuration buffer  yet.  So the  window  you see is
;;;    still the window 1.  If you switch the window to another, you'll
;;;    lose the  configuration in the buffer 1.  In this case, type `p'
;;;    (stands for  Preserve) at the window creation  menu after typing
;;;    `C-c C-q  3' (see Note #1).   Windows.el doesn't  update the
;;;    buffer 1 and saves  the  current window configuration of the
;;;    NetNews into the buffer 3.
;;;    N.B. You can't preserve  the buffer contents unless you 
;;;    have saved it into the resume file described below.
;;;
;;;    Then, type `C-c C-q 2' to read the mail, type `C-c C-q 3' when
;;;    you are  tired with programming.   `C-c C-q  SPC' is very
;;;    convenient when you want to exchange two configurations.
;;;
;;;    If you forget what windows are  allocated to the buffers, type
;;;    `C-c C-q =' to display  the configuration buffer names and their
;;;    corresponding  editing buffer names in    the menu buffer.   The
;;;    entry preceded by `*' is the selected  buffer and the entry with
;;;    `+'  is the buffer previously  selected(that is, the buffer `C-c
;;;    C-w SPC' will select).  In this buffer, you can move cursor with
;;;    `n' or `p', and select that window  with SPC.  Type `?' for more
;;;    information.
;;;
;;;    (Note #1)
;;;    At a point of this time, while window configuration buffer holds
;;;    the  programming environment,  the screen of GNUS  is displayed.
;;;    And,  notice  that `p'(Preserve)  at  the  window creation  menu
;;;    doesn't  work  when the  Windows uses `frame' as window unit  on
;;;    Emacs 19.
;;;
;;;[Other functions]
;;;
;;;    Typing `C-c C-q C-w' displays the menu as follows.
;;;
;;;    N)ext P)rev R)ecent D)elete K)ill S)ave2buf L)oad-from-buf A)save-As
;;;
;;;    In this menu, `n', `p' is for switching window to the next or
;;;    previous, `r' is for recovering the window recently seen before
;;;    the window switching operation (see Note #2), `d' is for
;;;    deleting current window. `k' is same as `d' except closing all
;;;    visible file(s). And type `s' to save the current window
;;;    configuration into the corresponding buffer, `a' to save the
;;;    current window configuration into specified buffer. Note that
;;;    `s' and `l' is unavailable on frame environment.
;;;
;;;    (Note #2)
;;;    When using frame on Emacs 19, it is impossible  to store a frame
;;;    recently seen, so `r'  (win-recent-window)  selects a frame that
;;;    is not  allocated to Windows instead, if any (equivalent to `C-c
;;;    C-w 0').
;;;
;;;    When you  create  the  new window, you see the window creation
;;;    menu which  ask you  how to  handle  the  current  configuration
;;;    buffer  and  what  configuration the  new  window must  be.  The
;;;    entries of the prompt stand for the followings.
;;;
;;;        Create        After saving the current window
;;;                configuration into the current buffer,
;;;                create a newly allocated window.
;;;        Duplicate    After saving the current window
;;;                configuration into the current buffer,
;;;                create the new window with the same
;;;                configuration as the current one.
;;;        Preserve    Without updating the current buffer,
;;;                use the current window configuration
;;;                as the new window.  This function is not
;;;                available on Emacs 19 with frame.
;;;        Findfile    Find-file on new window.
;;;        Buff        Switch to buffer on new window.
;;;        M-x        Call a command on new window.
;;;        No        Cease window creation.
;;;
;;;[Resume]
;;;

;;; JSK: THIS IS THE PLACE -- 1 of N

;;;    Windows.el  can  resume  your  environment  with  a  help   of
;;;    `revive.el'.  You can see the menu by typing `C-c C-q C-r'.
;;;
;;;         A)save-all R)estore-all S)ave-this L)oad-this N)Load# ~)read-~
;;;
;;;    Type `a' to save all window configurations into a file, and type
;;;    `r' to restore configurations saved  in that  file.  `s' to save
;;;    the  current window configuration, `l'  to  load current  window
;;;    from file, `n' to load a window specified by a  number.  And use
;;;    `~' when you type `a' instead of `r' by mistake.  And it is much
;;;    more convenient  to kill emacs with `C-x  C'.   If you  want  to
;;;    resume that  context, call  `resume-windows' (or C-c C-q  C-r r)
;;;    just after starting Emacs.
;;;
;;;[Local resume]
;;;
;;;    If you want to have  many sets of window  configurations, type
;;;    `C-c C-q  C-l'   to  specify  the  directory   where   another
;;;    configuration file is to resid,  and operate in the same way  as
;;;    you  do in resume menu described  above.  You can change sets of
;;;    configurations directory by directory.
;;;
;;;    By  the  way,  most  of  tasks  are being  done  in a  certain
;;;    directory.   If you have a couple of or more jobs to do at once,
;;;    the  function  `win-switch-task' is very useful  to  switch your
;;;    tasks.  It  saves the current  set of window configurations into
;;;    current configuration file, flushes buffers, and reads  the next
;;;    set of window  configurations for  the  next task  from  another
;;;    configuration file.
;;;
;;;[Customizations]
;;;
;;;    To change  the prefix key stroke  to  `C-c w' for example,
;;;    put the following expressions into your ~/.emacs.
;;;
;;;        (setq win:switch-prefix "\C-cw")
;;;        (define-key global-map win:switch-prefix nil)
;;;        (define-key global-map "\C-cw1" 'win-switch-to-window)
;;;
;;;    And you can also change the key stroke of window selection to
;;;    `a' to `z' other than `1' to `9'.
;;;
;;;        (setq win:switch-prefix "\C-cw")
;;;        (define-key global-map win:switch-prefix nil)
;;;        (define-key global-map "\C-cwb" 'win-switch-to-window)
;;;        (setq win:base-key ?`)        ;; '`' is before 'a'
;;;        (setq win:max-configs 27)    ;; '`' to 'z' are 27 chars.
;;;        (setq win:quick-selection nil)    ;; Not assign `C-c LETTER'
;;;
;;;    Notice that '`' is the previous character  of 'a' in ASCII code,
;;;    and  that C-c w  ` is bound  to swap  the configuration  in  the
;;;    buffer '`' and the current buffer.
;;;
;;;    If you don't use `frame' even on Emacs 19 with X Window.  Set
;;;    win:use-frame to nil in ~/.emacs.
;;;
;;;    If you hate raising of  frames at win-save-all-configurations,
;;;    set win:no-raise-at-save to t.
;;;
;;;[For frame users]
;;;


;;; JSK: THIS IS THE PLACE -- 2 of N


;;;    When you start to use windows.el,  you may create  a new frame
;;;    with  old  operation  `C-x  5 f'  or  so.  Frames  created  with
;;;    standard frame operation are not marked in the windows ring.  To
;;;### incorporate sucn an  orphan frame into  windows  ring, type `C-c
;;;### C-w C-w' and select `A)save-as' from the menu.   And if you want
;;;    to switch to  orphan frames, type  `C-c C-q 0',  which  switches
;;;    frame to isolated frames and rotate them.
;;;
;;;    By default, windows.el name a frame title (bar) as follows.
;;;    
;;;        Emacs[1]:*scartch*
;;;    
;;;    This causes frame  title  always representing the frame  number.
;;;    So you can switch   to any of  them with  key operation  of your
;;;    window manager, `fvwm' for example:
;;;    --- .fvwmrc ---
;;;    key 1 A C Warp "" Emacs[1]
;;;    key 2 A C Warp "" Emacs[2]
;;;      :
;;;    key 9 A C Warp "" Emacs[9]
;;;    
;;;    If you use fvwm2
;;;    AddToFunc DeiconifyFocusAndWarp "I" Iconify -1
;;;    +                "I" FocusAndWarp $0 $1
;;;    
;;;    Key 1 A C    Next [Emacs?1?:*] DeiconifyFocusAndWarp
;;;     :
;;;    Key 9 A C    Next [Emacs?9?:*] DeiconifyFocusAndWarp
;;;    #(fvwm2 can't escape [ and ], so cause matches with them by ?s.)
;;;
;;;    Thus C-1, C-2, ..., C-9 directly select window frame `Emacs[1]'
;;;    through `Emacs[9]'.
;;;    In this  hook, two  variables  `frame'  and `index', which  have
;;;    frame object, frame number respectively, are available.
