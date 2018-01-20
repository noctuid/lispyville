(setq package-user-dir
      (expand-file-name (format ".cask/%s/elpa"
                                (concat emacs-version
                                        (when (boundp 'emacs-build-number)
                                          (format ".%s" emacs-build-number))))))
(package-initialize)
(add-to-list 'load-path default-directory)
