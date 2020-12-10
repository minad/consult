# consult.el - Consulting completing-read

[![](https://melpa.org/packages/consult-badge.svg)](https://melpa.org/#/consult)

This package provides various commands based on the Emacs completion function
`completing-read`, in particular a more advanced buffer switching command
`consult-buffer` and a [Swiper](https://github.com/abo-abo/swiper#swiper)-like
search command `consult-line`.

The commands are compatible with completion-systems based on the standard Emacs
API, e.g., the Emacs builtin
[Icomplete](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html)
and [Selectrum](https://github.com/raxod502/selectrum). If Icomplete is used, it
is recommended to install
[Icomplete-vertical](https://github.com/oantolin/icomplete-vertical). The goal
is to keep the completion-system specifics in this package to a minimum.

Note that there is the [Marginalia](https://github.com/minad/marginalia/)
package, which can be combined with Consult. The `marginalia-mode` enriches the
completion display with annotations. The annotations show for example
documentation strings or file information. Marginalia has been extracted from
[Embark](https://github.com/oantolin/embark/) and Consult since both packages
provided minibuffer annotations. Embark provides local actions (like a context
menu), which can be executed while selecting a candidate in the minibuffer. For
example, when selecting a file, an action to delete the file is offered.

If you use [Ivy](https://github.com/abo-abo/swiper#ivy) or
[Helm](https://github.com/emacs-helm/helm), you probably don't need Consult,
since both packages already bring their own rich set of additional commands.

## Screenshots

consult-mark

![consult-mark](https://github.com/minad/consult/blob/master/images/consult-mark.png?raw=true)

consult-line

![consult-line](https://github.com/minad/consult/blob/master/images/consult-line.png?raw=true)

consult-outline

![consult-outline](https://github.com/minad/consult/blob/master/images/consult-outline.png?raw=true)

marginalia-mode

![marginalia-mode](https://github.com/minad/marginalia/blob/main/marginalia-mode.png?raw=true)

## Available commands

Most provided commands follow the naming scheme `consult-<thing>`.

### Virtual Buffers

  * `consult-buffer` (`-other-window`, `-other-frame`): Enhanced version of
     `switch-to-buffer` with support for virtual buffers. Supports live preview
     and recursive editing while previewing. If Selectrum is used
     `consult-buffer` supports prefixes for narrowing. You can type `b SPC`, `f
     SPC`, `m SPC` and `v SPC` in order to narrow to buffers, files, bookmarks
     and views respectively. Unfortunately this is (not yet?) supported by the
     generic `completing-read` implementation.
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

### Help and Discoverability

  * `consult-apropos`: Replacement for `apropos` with completion.

Note that there is the [Marginalia](https://github.com/minad/marginalia/)
package, which provides `marginalia-mode`. Enabling this mode annotates
completions with richer information (e.g. `M-x`, `describe-face`,
`describe-symbol`, `helpful-function`, …).

### Histories

  * `consult-command-history`: Select a command from the `command-history`.
  * `consult-history`: Insert a string from the current buffer history.
    This command can be invoked from the minibuffer. In that case the history
    stored in the minibuffer-history-variable is used.

### Navigation and Search

  * `consult-line` (`-symbol-at-point`, `-from-isearch`): Select from matching
    lines. Supports live preview and recursive editing. There are
    two variants, which search for the symbol at point and for the most
    recent isearch string respectively.
  * `consult-mark`: Jump to a marker in the `mark-ring`. Supports live preview
    and recursive editing.
  * `consult-outline`: Jump to a heading of the outline. Supports live preview
    and recursive editing.
  * `consult-flycheck`: Jump to flycheck error. Supports live preview
    and recursive editing. Flycheck users should install the separate
    `consult-flycheck` package to obtain this functionality.
  * `consult-imenu`: Jump to imenu item. Supports live preview
    and recursive editing.

### Miscellaneous

  * `consult-file-externally`: Select a file and open it externally, e.g. using `xdg-open` on Linux.
  * `consult-multi-occur`: Replacement for `multi-occur` which uses `completing-read-multiple`.
  * `consult-completion-in-region`: Function which can be used as `completion-in-region-function`.
     This way, the minibuffer completion UI will be used for `completion-at-point`.
     This function is particularily useful in combination with Icomplete-vertical,
     since Icomplete does not provide its own `completion-in-region-function`.
     In contrast, Selectrum already comes with its own function.
  * `consult-minor-mode-menu`: Enable/disable minor mode.
  * `consult-theme`: Select a theme and disable all currently enabled themes.
    Supports live preview of the theme while scrolling through the candidates.

## Live previews

Some of the commands support live previews. For example when you scroll through
the items of `consult-line`, the buffer will jump to the corresponding position.
It is possible to jump back and forth between the minibuffer and the buffer to
perform recursive editing while a search is ongoing. In case you do not like
live previews or find them distracting, for each of the commands supporting
preview, there is a customizable variable which allows disabling the preview. In
order for live previews to work you must enable `consult-preview-mode`.

## Package configuration

It is recommended to manage package configurations with `use-package`. The
Consult package only provides commands and does not add any keybindings. In
order to use the enhanced commands, you must configure the keybindings yourself.

~~~ elisp
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (("C-c h" . consult-history)
         ("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ("M-g i" . consult-imenu)   ;; "M-s i" is a good alternative
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         :map flycheck-command-map
         ("!" . consult-flycheck))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

;; Optionally enable richer annotations using the Marginalia package
(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
~~~

### Configuration settings

| Variable                   | Def | Description                                             |
|----------------------------|-----|---------------------------------------------------------|
| consult-line-numbers-widen | t   | Show absolute line numbers when narrowing is active.    |
| consult-mode-histories     | …   | Mode-specific history variables                         |
| consult-preview-buffer     | t   | Enable buffer preview during selection                  |
| consult-preview-line       | t   | Enable line preview during selection                    |
| consult-preview-mark       | t   | Enable mark preview during selection                    |
| consult-preview-outline    | t   | Enable outline preview during selection                 |
| consult-preview-theme      | t   | Enable theme preview during selection                   |
| consult-preview-yank       | t   | Enable yank preview during selection                    |
| consult-recenter           | t   | Recenter after jumping to a location                    |
| consult-themes             | nil | List of themes to be presented for selection            |

## Related packages

It is recommended to install the following package combination:

* selectrum or icomplete-vertical: Vertical completion systems
* consult: This package
* marginalia: Annotations for the completion candidates
* embark: Action commands, which can act on the completion candidates
* orderless or prescient: Completion style, filters the candidates, Prescient also offers sorting.

Note that all packages are independent and can potentially be exchanged with
alternative components, since there exist no hard dependencies. Furthermore it
is possible to get started with only Selectrum and Consult and add more
components later to the mix.

## Acknowledgements

You probably guessed from the name that this package took inspiration
from [Counsel](https://github.com/abo-abo/swiper#counsel) by Oleh Krehel.
Note that we are not yet on the same level as Counsel in terms of covered functionality.

Some of the commands found in this package originated in the
[Selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).

Advice and useful discussions:
* [Clemens Radermacher](https://github.com/clemera/)
* [Omar Antolín Camarena](https://github.com/oantolin/)
* [Protesilaos Stavrou](https://gitlab.com/protesilaos/)

Code contributions:
* [Omar Antolín Camarena](https://github.com/oantolin/)
* [Sergey Kostyaev](https://github.com/s-kostyaev/)
* [okamsn](https://github.com/okamsn/)
