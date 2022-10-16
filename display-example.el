(setq display-buffer-alist
      `(("\\`\\*no-window\\*\\'"
         (display-buffer-no-window))
        ("\\`\\*at-bottom\\*\\'"
         (display-buffer-at-bottom))
        ("\\`\\*in-side-window\\*\\'"
         (display-buffer-in-side-window))
        ("\\`\\*pop-up-window\\*\\'"
         (display-buffer-pop-up-window))))
