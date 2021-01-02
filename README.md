# consult.el - Consulting completing-read

[![](https://melpa.org/packages/consult-badge.svg)](https://melpa.org/#/consult)

This package provides various commands based on the Emacs completion function
`completing-read`, in particular a more advanced buffer switching command
`consult-buffer` which allows to select from buffers, recent files and more.
Furthermore various search commands are provided, like `consult-grep` and
`consult-line`, which resembles [Swiper](https://github.com/abo-abo/swiper#swiper).

The commands are compatible with completion systems based on the standard Emacs
`completing-read` API, notably the default completion system,
[Icomplete](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html),
[Selectrum](https://github.com/raxod502/selectrum) and
[Embark's live-occur](https://github.com/oantolin/embark/).
If Icomplete is used, it is recommended to install
[Icomplete-vertical](https://github.com/oantolin/icomplete-vertical). The
completion system specifics in this package are kept to a minimum.

There are the [Marginalia](https://github.com/minad/marginalia/) and
[Embark](https://github.com/oantolin/embark/) packages, which can be combined
with Consult. The `marginalia-mode` enriches the completion display with
annotations. The annotations show for example documentation strings or file
information. Embark provides local actions (like a context menu), which can be
executed while selecting a candidate in the minibuffer. For example, when
selecting a file, an action to delete the file is offered.

*Note*: If you use [Ivy](https://github.com/abo-abo/swiper#ivy) or
[Helm](https://github.com/emacs-helm/helm), you probably don't need Consult,
since both packages already bring their own rich set of additional commands.

## Screenshots

consult-mark

![consult-mark](https://github.com/minad/consult/blob/main/images/consult-mark.png?raw=true)

consult-line

![consult-line](https://github.com/minad/consult/blob/main/images/consult-line.png?raw=true)

consult-outline

![consult-outline](https://github.com/minad/consult/blob/main/images/consult-outline.png?raw=true)

marginalia-mode

![marginalia-mode](https://github.com/minad/marginalia/blob/main/marginalia-mode.png?raw=true)

## Available commands

Most provided commands follow the naming scheme `consult-<thing>`.

### Virtual Buffers

  * `consult-buffer` (`-other-window`, `-other-frame`): Enhanced version of
     `switch-to-buffer` with support for virtual buffers. Supports live preview
     and recursive editing while previewing. The command supports narrowing. You
     can type `f SPC` in order to narrow to recent files. Ephemeral buffers can
     be shown by pressing `SPC` - it works the same way as `switch-buffer`.
     Supported narrowing keys:
     * b Buffers
     * f Files
     * m Bookmarks
     * p Project (only available if `consult-project-root-function` is configured)
     * v Views (only available if `consult-view-*-function`s are configured)
  * `consult-bookmark`: Select or create bookmark. You might prefer the more
    powerful `consult-buffer` instead, which includes bookmarks.
  * `consult-recent-file` (`-other-window`, `-other-frame`): Select a recent
     files. You might prefer the more powerful `consult-buffer` instead, which
     includes recent files.

### Editing

  * `consult-register`: Select from list of registers.
  * `consult-yank`, `consult-yank-pop`: Enhanced version of `yank` and
    `yank-pop` which allows selecting from the kill-ring. Live preview is
    supported when selecting from the kill-ring.
  * `consult-kmacro`: Select macro from the macro ring and execute it.

### Navigation

  * `consult-goto-line`: Jump to line number enhanced with live preview. This is
    a drop-in replacement for `goto-line`.
  * `consult-mark`: Jump to a marker in the `mark-ring`. Supports live preview
    and recursive editing.
  * `consult-global-mark`: Jump to a marker in the `global-mark-ring`. Supports
    live preview and recursive editing.
  * `consult-outline`: Jump to a heading of the outline. Supports live preview
    and recursive editing.
  * `consult-imenu`: Jump to imenu item. Supports live preview, recursive
    editing and narrowing.

### Search

  * `consult-line`: Enter search string and select from matching lines. Supports
    live preview and recursive editing. The symbol at point and the recent
    isearch string are added to the "future history" and can be accessed by
    pressing `M-n`.
  * `consult-multi-occur`: Replacement for `multi-occur` which uses
    `completing-read-multiple`.
  * `consult-grep`, `consult-ripgrep`, `consult-git-grep`: Search for regular
    expression in current directory. Grep is invoked asynchronously, while you
    enter the search term. You are required to enter at least
    `consult-async-min-input` characters in order for the search to get started.
    The input string is split into two parts, if the first character is a
    punctuation character, like `#`. For example `"#grep-regexp#filter-string`,
    is split at the second `#`. The string `"grep-regexp"` is passed to Grep,
    the `"filter-string"` is passed to the *fast* Emacs filtering to further
    narrow down the list of matches. This is particularily useful if you are
    using an advanced completion style like orderless. `consult-grep` supports
    preview. If `consult-project-root-function` is configured and the function
    returns non-nil, `consult-grep` searches the current project directory.
    Otherwise the `default-directory` is searched.  For example, if you use
    [projectile](https://github.com/bbatsov/projectile/), set
    `consult-project-root-function` to `projectile-project-root`. If
    `consult-grep` is invoked with prefix argument, you can specify the
    directory manually, i.e., `C-u M-g r`.
  * `consult-find`, `consult-fdfind`, `consult-locate`: Find file by matching
    the path against a regexp. Like `consult-grep` either the project root or
    the current directory is used as root directory for the search. The input
    string is treated similarly to `consult-grep`, where the first part is
    passed to find, and the second part is used for Emacs filtering. Note that
    `find` uses wildcards, e.g. enter `*consult*`, to find all files containing
    the string "consult". In contrast, fdfind uses regular expressions.

### Compilation errors

  * `consult-error`: Jump to an error. Supports live preview
    and recursive editing. In contrast to `consult-flycheck` it shows the line
    and is more general since it is not tied to a specific backend.
  * `consult-flycheck`: Jump to flycheck error. Supports live preview and
    recursive editing. The command supports narrowing. Press `e SPC`, `w SPC`,
    `i SPC` to only show errors, warnings and infos respectively. This command
    requires to install the additional `consult-flycheck.el` package since the
    main `consult.el` package only depends on Emacs core components.
  * `consult-flymake`: Jump to Flymake diagnostic, like `consult-flycheck`.

### Histories

  * `consult-complex-command`: Select a command from the `command-history`. This
    command is a `completing-read` version of `repeat-complex-command` and can
    also be considered a replacement for the `command-history` command from
    chistory.el.
  * `consult-history`: Insert a string from the current buffer history.
    This command can be invoked from the minibuffer. In that case the history
    stored in the minibuffer-history-variable is used.

### Minor and Major modes

  * `consult-minor-mode-menu`: Enable/disable minor mode. Supports narrowing to
     on/off/local/global modes by pressing `i/o/l/g SPC` respectively.
  * `consult-mode-command`: Run a command from the currently active minor or
     major modes. Supports narrowing to local-minor/global-minor/major mode via
     the keys `l/g/m`.

### Miscellaneous

  * `consult-apropos`: Replacement for `apropos` with completion.
  * `consult-file-externally`: Select a file and open it externally, e.g. using
    `xdg-open` on Linux.
  * `consult-completion-in-region`: Function which can be used as
     `completion-in-region-function`. This way, the minibuffer completion UI
     will be used for `completion-at-point`. This function is particularily
     useful in combination with Icomplete-vertical, since Icomplete does not
     provide its own `completion-in-region-function`. In contrast, Selectrum
     already comes with its own function.
  * `consult-theme`: Select a theme and disable all currently enabled themes.
     Supports live preview of the theme while scrolling through the candidates.

## Live previews

Some Consult commands support live previews. For example when you scroll through
the items of `consult-line`, the buffer will scroll to the corresponding
position. It is possible to jump back and forth between the minibuffer and the
buffer to perform recursive editing while the search is ongoing. Previews must
be explicitly enabled via `consult-preview-mode`. Furthermore for each command,
a customizable variable is offered to selectively enable/disable preview.

## Narrowing to subsets

Consult has special support to narrow to candidate subsets. When you use the
`consult-buffer` command, you can press `b SPC` and the list of candidates will
be restricted such that only buffers are shown. If you press `DEL` afterwards,
the full candidate list will be shown again. Furthermore a narrowing/widening
key can be configured which can be pressed to achieve the same effect, see the
configuration variables `consult-narrow-key` and `consult-widen-key`.

## Asynchronous candidates and filtering

Consult has support for asynchronous generation of candidate lists. This feature
is used for example by `consult-grep`, where the list of matches is generated
dynamically while the user is typing a grep regular expression. Furthermore the
found matches can then be narrowed using the installed Emacs completion-style,
which can be very powerful if you are using for example the `orderless`
completion style. This is possible since part of the input string is treated as
input to grep and part of the input is used for filtering. The input string is
split at a punctuation character, using a similar syntax as Perl regular
expressions.

Examples:

* `#defun`: Search for "defun" using grep.
* `#defun#consult`: Search for "defun" using grep, filter with the word
  "consult".
* `/defun/consult`: It is also possible to use other punctuation characters.
* `#to#`: Force searching for "to" using grep, since the grep pattern must be
  longer than `consult-async-min-input` characters by default.

## Package configuration

It is recommended to manage package configurations with `use-package`. The
Consult package only provides commands and does not add any keybindings. In
order to use the enhanced commands, you must configure the keybindings yourself.

Note that there are three packages as of now: `consult.el`,
`consult-selectrum.el` and `consult-flycheck.el`. Consult has been split such
that the main package `consult.el` only depends on Emacs core components.

~~~ elisp
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)     ;; "M-s o" is a good alternative.
         ("M-g l" . consult-line)        ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)        ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark) ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)    ;; or consult-grep, consult-ripgrep
         ("M-g f" . consult-find)        ;; or consult-fdfind, consult-locate
         ("M-g i" . consult-imenu)
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

;; Enable Consult-Selectrum integration.
;; This package should be installed if Selectrum is used.
(use-package consult-selectrum
  :after selectrum
  :demand t)

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))
~~~

### Configuration settings

| Variable                      | Default          | Description                                             |
|-------------------------------|------------------|---------------------------------------------------------|
| consult-after-jump-hook       | '(recenter)      | Functions to call after jumping to a location           |
| consult-async-default-split   | "#"              | Separator character used for splitting #async#filter    |
| consult-async-input-throttle  | 0.5              | Input throttle for asynchronous commands                |
| consult-async-input-debounce  | 0.25             | Input debounce for asynchronous commands                |
| consult-async-min-input       | 3                | Minimum numbers of letters needed for async process     |
| consult-async-refresh-delay   | 0.25             | Refresh delay for asynchronous commands                 |
| consult-goto-line-numbers     | t                | Show line numbers for `consult-goto-line`               |
| consult-imenu-narrow          | …                | Mode-specific narrowing keys for `consult-imenu`        |
| consult-imenu-toplevel        | …                | Mode-specific toplevel names used by `consult-imenu`    |
| consult-line-numbers-widen    | t                | Show absolute line numbers when narrowing is active.    |
| consult-line-point-placement  | 'match-beginning | Placement of the point used by `consult-line`           |
| consult-mode-command-filter   | "-mode$\\|--"    | Filter regexp for `consult-mode-command`                |
| consult-mode-histories        | …                | Mode-specific history variables                         |
| consult-narrow-key            | nil              | Narrowing prefix key during completion                  |
| consult-widen-key             | nil              | Widening key during completion                          |
| consult-preview-buffer        | t                | Enable buffer preview during selection                  |
| consult-preview-error         | t                | Enable error preview during selection                   |
| consult-preview-flycheck      | t                | Enable flycheck error preview during selection          |
| consult-preview-flymake       | t                | Enable flymake diagnostic preview during selection      |
| consult-preview-global-mark   | t                | Enable global mark preview during selection             |
| consult-preview-grep          | t                | Enable grep preview during selection                    |
| consult-preview-line          | t                | Enable line preview during selection                    |
| consult-preview-mark          | t                | Enable mark preview during selection                    |
| consult-preview-outline       | t                | Enable outline preview during selection                 |
| consult-preview-theme         | t                | Enable theme preview during selection                   |
| consult-preview-yank          | t                | Enable yank preview during selection                    |
| consult-preview-max-count     | 10               | Maximum number of files to keep open during preview     |
| consult-preview-max-size      | 102400           | Size limit for previewed files                          |
| consult-project-root-function | nil              | Function which returns current project root             |
| consult-themes                | nil              | List of themes to be presented for selection            |
| consult-view-list-function    | nil              | Function which returns a list of view names as strings  |
| consult-view-open-function    | nil              | Function to open a view by name                         |

## Related packages

It is recommended to install the following package combination:

* consult: This package
* consult-flycheck: Provides the consult-flycheck command
* consult-selectrum: Provides integration with Selectrum
* selectrum or icomplete-vertical: Vertical completion systems
* marginalia: Annotations for the completion candidates
* embark: Action commands, which can act on the completion candidates
* orderless: Completion style, Flexible candidate filtering
* prescient: Frecency-based candidate sorting, also offers filtering

Note that all packages are independent and can potentially be exchanged with
alternative components, since there exist no hard dependencies. Furthermore it
is possible to get started with only Selectrum and Consult and add more
components later to the mix.

## Acknowledgements

You probably guessed from the name that this package took inspiration
from [Counsel](https://github.com/abo-abo/swiper#counsel) by Oleh Krehel.
Some of the commands found in this package originated in the
[Selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).

Code contributions:
* [Omar Antolín Camarena](https://github.com/oantolin/)
* [Sergey Kostyaev](https://github.com/s-kostyaev/)
* [okamsn](https://github.com/okamsn/)
* [Clemens Radermacher](https://github.com/clemera/)
* [Tom Fitzhenry](https://github.com/tomfitzhenry/)
* [jakanakaevangeli](https://github.com/jakanakaevangeli)
* [inigoserna](https://github.com/inigoserna/)

Advice and useful discussions:
* [Clemens Radermacher](https://github.com/clemera/)
* [Omar Antolín Camarena](https://github.com/oantolin/)
* [Protesilaos Stavrou](https://gitlab.com/protesilaos/)
* [Steve Purcell](https://github.com/purcell/)
* [Adam Porter](https://github.com/alphapapa/)
* [Manuel Uberti](https://github.com/manuel-uberti/)
* [Tom Fitzhenry](https://github.com/tomfitzhenry/)
* [Howard Melman](https://github.com/hmelman/)
