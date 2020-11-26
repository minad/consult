# consult.el - Consult things using completing-read

This package provides various commands based on the Emacs completion function `completing-read`.
The functions should be compatible with any completion-system based on the standard Emacs API,
e.g., icomplete and selectrum. At least the goal is to keep the completion-system specifics to a minimum.
Commands which can only be implemented using selectrum-specific extensions, will be packaged up separately
before release.

## Available commands

Most commands provided by consult follow the naming scheme `consult-thing`.
For now these commands are provided:

* `consult-multi-occur`: Replacement for `multi-occur`
* `consult-recent-file`: Select a recent files (you might prefer the more powerful `consult-buffer` instead)
* `consult-mark`: Jump to a marker in the `mark-ring`
* `consult-line`: Jump to a line matching the selected text
* `consult-buffer`: Enhanced version of `switch-to-buffer` with support for virtual buffers
* `consult-yank`, `consult-yank-pop`: Enhanced version of `yank` and `yank-pop` which allows selecting from the kill-ring.
* `consult-register`: Select from list of registers
* `consult-theme`: Select a theme and disable all currently enabled themes

The commands are based on the versions found in the [selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).

## Usage

The consult library will only provide commands and never add keybindings. In order to
use the enhanced functions, you must configure the keybindings yourself.

~~~ elisp
;; Example configuration
(use-package consult
  :demand t
  ;; Replace bindings
  :bind (("C-x b" . consult-buffer)
         ("M-g m" . consult-mark)
         ("M-g l" . consult-line)
         ("C-x r x" . consult-register)
         ("M-y" . consult-yank-pop))
  :config
  ;; Replace functions
  (fset 'multi-occur #'consult-multi-occur))
~~~
