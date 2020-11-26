# consult.el - Consultation using completing-read

This package provides various commands based on the Emacs completion function `completing-read`.
The functions should be compatible with any completion-system based on the standard Emacs API,
e.g., icomplete and selectrum. At least the goal is to keep the completion-system specifics to a minimum.
Commands which can only be implemented using selectrum-specific extensions, will be packaged up separately
before release.

## Available commands

Most provided commands follow the naming scheme `consult-thing`. This is the list of currently supported commands:

* `consult-multi-occur`: Replacement for `multi-occur`
* `consult-recent-file` (`-other-window`, `-other-frame`): Select a recent files (you might prefer the more powerful `consult-buffer` instead)
* `consult-mark`: Jump to a marker in the `mark-ring`
* `consult-line`: Jump to a line matching the selected text
* `consult-buffer` (`-other-window`, `-other-frame`): Enhanced version of `switch-to-buffer` with support for virtual buffers
* `consult-yank`, `consult-yank-pop`: Enhanced version of `yank` and `yank-pop` which allows selecting from the kill-ring.
* `consult-register`: Select from list of registers
* `consult-theme`: Select a theme and disable all currently enabled themes
* `consult-bookmark`: Select or create bookmark
* `consult-apropos`: Replacement for `apropos` with completion
* `consult-command-history`: Select a command from the `command-history`
* `consult-minibuffer-history`: Insert a string from the `minibuffer-history`

## Usage

The consult library will only provide commands and never add keybindings. In order to
use the enhanced functions, you must configure the keybindings yourself.

~~~ elisp
;; Example configuration
(use-package consult
  :demand t
  ;; Replace bindings
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-g m" . consult-mark)
         ("M-g l" . consult-line)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))
  :config
  ;; Replace functions
  (fset 'multi-occur #'consult-multi-occur))
~~~

## Acknowledgments

You can probably guess from the name that this package is inspired by [counsel](https://github.com/abo-abo/swiper/blob/master/counsel.el).
Note that we are far from counsel in terms of covered functionality.
Many of the commands found in this package are derived from the
[selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).
