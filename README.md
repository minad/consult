# consult.el - Various interactive functions using completing-read

This library provides various utility functions based on the Emacs completion function `completing-read`.
The functions should be compatible with any completion-system based on the standard Emacs API,
e.g., icomplete and selectrum. At least the goal is to keep the completion-system specifics to a minimum.
Functions which can only be implemented using selectrum-specific extensions, will be packaged up separately
before release.

For now these functions are provided:

* `consult-multi-occur`: Replacement for `multi-occur`
* `consult-find-recent-file`: Select a recent files
* `consult-marks`: Jump to a marker in the `mark-ring`
* `consult-switch-buffer`: Enhanced version of `switch-to-buffer` with support for virtual buffers
* `consult-yank`, `consult-yank-pop`: Enhanced version of `yank` and `yank-pop` which allows selecting from the kill-ring.
* `consult-register`: Select from list of registers
* `consult-theme`: Select a theme and disable all currently enabled themes

The commands are based on the versions found in the [selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands).
