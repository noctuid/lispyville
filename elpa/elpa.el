(setq package-user-dir
      (expand-file-name (format ".cask/%s.%s/elpa"
                                emacs-major-version
                                emacs-minor-version)))
(package-initialize)
(add-to-list 'load-path default-directory)
