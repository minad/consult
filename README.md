# consult.el - Consulting completing-read

This package provides various commands based on the Emacs completion function `completing-read`.
The commands are compatible with completion-systems based on the standard Emacs API,
e.g., [Icomplete](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html) and
[Selectrum](https://github.com/raxod502/selectrum).
Note that if you use [Ivy](https://github.com/abo-abo/swiper#ivy)
or [Helm](https://github.com/emacs-helm/helm), you don't need consult,
since both libraries already bring their own rich set of additional commands.

The goal is to keep the completion-system specifics in this library to a minimum.
As of now, the `consult-buffer` command is only supported if Selectrum is used.
We will probably provide a variant of `consult-buffer` which is a bit less rich,
but works purely with `completing-read`. Furthermore live previews are only implemented
for Selectrum as of now, but Icomplete support is planned.

## Available commands

Most provided commands follow the naming scheme `consult-thing`. This is the list of currently supported commands:

* Buffers/Files/Bookmarks
  * `consult-bookmark`: Select or create bookmark
  * `consult-buffer` (`-other-window`, `-other-frame`): Enhanced version of `switch-to-buffer` with support for virtual buffers. Supports live preview.
  * `consult-recent-file` (`-other-window`, `-other-frame`): Select a recent files (you might prefer the more powerful `consult-buffer` instead)
* Editing
  * `consult-register`: Select from list of registers
  * `consult-yank`, `consult-yank-pop`: Enhanced version of `yank` and `yank-pop` which allows selecting from the kill-ring.
* Help
  * `consult-apropos`: Replacement for `apropos` with completion
  * `consult-face`: Describe face with preview during selection
* Histories
  * `consult-command-history`: Select a command from the `command-history`
  * `consult-minibuffer-history`: Insert a string from the `minibuffer-history`
* Jumping/Search
  * `consult-line`: Jump to a line matching the selected text. Supports live preview.
  * `consult-mark`: Jump to a marker in the `mark-ring`. Supports live preview.
  * `consult-outline`: Jump to a heading of the outline. Supports live preview.
  * `consult-multi-occur`: Replacement for `multi-occur`
* Miscellaneous
  * `consult-minor-mode`: Enable/disable minor mode
  * `consult-theme`: Select a theme and disable all currently enabled themes. Supports live preview.

## Live previews

Some of the commands support live previews. For example when you scroll through the items of `consult-line`,
the buffer will jump to the corresponding position. It is possible to jump back and forth between
the minibuffer and the buffer to perform recursive editing while a search is ongoing.

## Screenshots

consult-mark

![consult-mark](https://github.com/minad/consult/blob/master/images/consult-mark.gif?raw=true|height)

consult-line

![consult-line](https://github.com/minad/consult/blob/master/images/consult-line.gif?raw=true)

consult-outline

![consult-outline](https://github.com/minad/consult/blob/master/images/consult-outline.gif?raw=true)

consult-face

![consult-face](https://github.com/minad/consult/blob/master/images/consult-face.gif?raw=true)

## Usage

The consult library only provides commands and does not add any keybindings. In order to
use the enhanced commands, you must configure the keybindings yourself.

~~~ elisp
;; Example configuration
(use-package consult
  ;; Replace bindings
  :bind (("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))
~~~

## Acknowledgments

You probably guessed from the name that this package is inspired by and partially derived from
[Counsel](https://github.com/abo-abo/swiper#counsel) (Author Oleh Krehel, Copyright Free Software Foundation, Inc.).
Note that we are far from counsel in terms of covered functionality.
Furthermore some of the commands found in this package were taken from the
[Selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).
