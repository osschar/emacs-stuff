Example fragments from `.emacs`:
```
(autoload 'find-cfiles "~/emacs/find-cfiles.el")
(add-hook 'find-file-not-found-hooks 'find-cfiles)

(defun root-indent () (interactive)
       (setq c-basic-offset 3)
       (setq js-indent-level 3)
       (setq-default indent-tabs-mode nil)
)
(defun auto-root-indent () (interactive)
       (add-hook 'c++-mode-hook 'root-indent)
       (add-hook 'javascript-mode-hook 'root-indent)
)

(defun setup-for-root() (interactive)
       (auto-root-indent)
       (setq find-cfiles-hext "hxx")
       (setq find-cfiles-cext "cxx")
       (setq find-cfiles-hdirpreflist '("" "../inc/" "../inc/ROOT/"))
       (setq find-cfiles-cdirpreflist '("" "../src/" "../../src/"))
)

(defun setup-for-xrootd() (interactive)
       (auto-root-indent)
       (setq find-cfiles-hext "hh")
       (setq find-cfiles-cext "cc")
)
```


Example fragments from `.bashrc`:
```
alias emacs-for-root='emacs -f setup-for-root'
alias emacs-for-xrootd='emacs -f setup-for-xrootd'
```
