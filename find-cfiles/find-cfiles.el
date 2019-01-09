;;; find-cfile.el --- find file hook for handling c files and their headers;	$Id: find-cfiles.el,v 1.6 2002/05/26 12:15:04 jona Exp jona $	


;; Copyright (C) 1997-2002 Jan Jona Javorsek
;; Lincensed under GPL (Gnu Public Licence)

;; Author: Jan Jona Javorsek <jan.javorsek@guest.arnes.si>
;; Created: 1997
;; Modified: May 2002
;; Version: 1.6
;; Keywords: convenience

;;; Commentary:
;; Consider finding corresponding .c and .h files when the user selects
;; a nonexisting dot-terminated file.
;; 
;; If a couple of files with default extensions is found it is open.
;; If not, the user is promted to see if a new pair is to be created,
;; using either default or user supplied extensions.
;; 
;; If the user does not specify an extension, find-cfiles first uses
;; defaults in find-cfiles-[ch]filename and then a list of possible
;; extensions in find-cfiles-[ch]filenamelist to and open existing
;; files or buffers. (This shoul probably modified to use actual
;; extension pairs and create missing members of pairs on the fly?)
;; 
;; If no files are found, new files are created with user-supplied
;; extension or the deraults from find-cfiles-[ch]filename.
;; 
;; The new files are populated using prototype skeleton files, usually
;; named .SKEL.extension, located with a bottom-up recursive search
;; from the file's directory. The word FNAME (or the contents of
;; find-cfiles-proto-regexp) in skeleton files is replaced with the
;; name of the current file, facilitating writing a lot of files
;; with smiliral includes/headers/structures.

;;; BUGS AND PLANS: 
;; 
;; This will effectively disable automatic retrival of dot terminated
;; files from version-controlled archives if there is no readable
;; copy of the file. User interaction can circumvent this, but is
;; misleading.
;; 
;; It will also create a *new* frame even if the buffers existed
;; before when frames are being used.
;; 
;; 
;; Various ways of limiting the skelfile search have not been
;; implemented since most users are happy to include a single file
;; always.
;; 

;;; SETTING UP
;; 
;; To set find-cfiles up, put this in .emacs:
;; (setq find-cfiles-other-frame nil) ;; OR what you want
;; (autoload 'find-cfiles "PATH/IF/NEEDED//find-cfiles.el")
;; (autoload 'populate-cfile "PATH/IF/NEEDED//find-cfiles.el")
;; (add-hook 'find-file-not-found-hooks 'find-cfiles)

;;; Code:

;;;###autoload
(defun find-cfiles ()
  "Find file and consider finding corresponding .c and .h files.
Used a hook in find-file-not-found-hooks."
  (if (not (string= 
	    (substring (buffer-file-name) 
		       (- (length (buffer-file-name)) 1)) 
	    ".")) 
      (eval nil) ; loose if not dot-terminated
;;;
;;; User interaction (beggining of else part)
    (setq find-cfiles--hfilename find-cfiles-hfilename)
    (setq find-cfiles--cfilename find-cfiles-cfilename)
    (setq find-cfiles--user-answer (read-from-minibuffer "No such file: find C files (n or c-header/h-header, empty for default)?"))
    (setq find-cfiles--proto-hfilename nil)
    (setq find-cfiles--proto-cfilename nil)
    (if (let ((case-fold-search nil))
	  (and (> (length find-cfiles--user-answer) 0) 
	       (string= (substring find-cfiles--user-answer 0 1) "n")))
	(eval nil)
      (if (let ((case-fold-search nil))
	    (string= find-cfiles--user-answer ""))
	  (setq find-cfiles--hfilename find-cfiles-hfilename)
	  (setq find-cfiles--cfilename find-cfiles-cfilename)
	  (setq find-cfiles--proto-hfilename nil)
	  (setq find-cfiles--proto-cfilename nil)
;;;	(setq find-cfiles--cfilename find-cfiles--user-answer)
	  ;; put all after / in hfile, the rest in cfile, using defualts for empty
	  (if (string-match "/" find-cfiles--user-answer)
	      (progn
		(setq find-cfiles--cfilename 
		      (substring find-cfiles--user-answer 0 (string-match "/" find-cfiles--user-answer)))
		(setq find-cfiles--hfilename 
		      (substring find-cfiles--user-answer (match-end 0) (length find-cfiles--user-answer)))
		(if (< (length find-cfiles--cfilename) 1)
		    (setq find-cfiles--cfilename  find-cfiles-cfilename))
		(if (< (length find-cfiles--hfilename) 1)
		    (setq find-cfiles--hfilename  find-cfiles-hfilename)))
;;;     else part
	  (setq find-cfiles--cfilename find-cfiles--user-answer)
	  (setq find-cfiles--hfilename find-cfiles-hfilename)))
;;;   Set up windows/frames
      (if (eval find-cfiles-other-frame)
	  (select-frame (make-frame find-cfiles-other-frame-parameters))
	(delete-other-windows))
;;;   Set up files (shuld split this in FUNCTIONS and use with args!!!!!)
      (let ((current-name (buffer-file-name)) 
	    (dead-buffer (current-buffer)))
	(if (not (or (file-readable-p (concat current-name find-cfiles--hfilename)) 
		     (get-file-buffer (concat current-name find-cfiles--hfilename))
		     (setq find-cfiles--proto-hfilename (find-cfiles--check-list-for-existing-files find-cfiles-hfilenamelist))
))
	    (progn 
	      (find-file (concat current-name find-cfiles--hfilename))
	      (find-cfiles-fillproto find-cfiles-proto-filename current-name find-cfiles--hfilename))
	  (find-file (concat current-name (or find-cfiles--proto-hfilename find-cfiles--hfilename))))
	(if (not (or (file-readable-p (concat current-name find-cfiles--cfilename)) 
		     (get-file-buffer (concat current-name find-cfiles--cfilename))
		     (setq find-cfiles--proto-cfilename (find-cfiles--check-list-for-existing-files find-cfiles-cfilenamelist))
))
	    (progn 
	      (set-buffer 
	       (find-file-other-window (concat current-name find-cfiles--cfilename)))
	      (find-cfiles-fillproto find-cfiles-proto-filename current-name find-cfiles--cfilename))
	(set-buffer 
	 (find-file-other-window (concat current-name (or find-cfiles--proto-cfilename find-cfiles--cfilename)))))
	(or (kill-buffer dead-buffer) 
	    (message "Warning: cfiles scratch buffer not killed!"))
	(eval t)))))


;;;;;;;;;;;;;;;;;;;;;;;

(defun find-cfiles--split-path ()
    (string-match "\\`\\([^/]*/\\)*\\([^/]\\)*\\'" current-file)
    (setq dir-elements  (match-data))
    (setq path-list 
	  (append (list 
		   (substring current-file
;;;;			      (nth 2 dir-elements) (nth 3 dir-elements)))
			      (nth 0 dir-elements) (nth 3 dir-elements)))
		  path-list))
    (setq current-file  
	  (substring current-file
		     (nth 0 dir-elements) (nth 2 dir-elements)))
    (if (> (length current-file) 2)
	  (find-cfiles--split-path)
;;;(message "left: %s" current-file)
      ))

(defun find-cfiles--file-name-directory-aslist (current-file)
  "Return the list of parent directories of file 
as absolute fully-qualified paths."
  (let ((path-list nil) (dir-elements nil))
    (find-cfiles--split-path)
    (eval 'path-list)))

(defun find-cfiles--find-skelfile (dirname)
  (message "LOOKING FOR: %s" dirname)
  (if (file-readable-p dirname)
      (concat dirname)
    (eval nil)
))

(defun find-cfiles--include-skelfile (dirname)
  (if (and dirname (< find-cfiles-include-skelfiles-sofar 
	find-cfiles-include-skelfile-number))
	   (progn (setq find-cfiles-include-skelfiles-sofar  (+ find-cfiles-include-skelfiles-sofar 1))
		  (message "Processing skeilfle: %s" find-cfiles-include-skelfiles-sofar)
	     (beginning-of-buffer)
	(insert-file-contents dirname)
	(perform-replace find-cfiles-proto-regexp truename nil nil nil)
	(message "Cfiles autoincluded file \"%s\" in buffer \"%s\"." 
		 dirname (buffer-name))
	(eval 'dirname))))

(defun find-cfiles-fillproto (filename truename extension)
  (setq truename (substring (file-name-nondirectory truename)  0 -1))
  (setq find-cfiles-include-skelfiles-sofar 0) ;our buffer local counter
  (let ((filename (concat filename "." extension))
	(truename truename)
	(extension extension)
	(filelist nil))

    (setq filelist 
	  (mapcar '(lambda (file) 
		     (find-cfiles--find-skelfile (concat file filename)))
		  (find-cfiles--file-name-directory-aslist
		   (buffer-file-name))))

    (setq filelist (nreverse filelist)) ; bottoms up!
    (save-excursion 
      (mapcar 'find-cfiles--include-skelfile filelist)
      (perform-replace find-cfiles-proto-regexp truename nil nil nil))))

; For extension lists
(defun find-cfiles--check-list-for-existing-files (extension-list)
(car (delq 'nil (mapcar 'find-cfiles--return-existing-filename extension-list))))

(defun find-cfiles--return-existing-filename (extension)
(if (or (file-readable-p (concat current-name extension)) 
	(get-file-buffer (concat current-name extension)))
    extension
  nil))

;;;; Our marvellous user variables for you to admire and trash up
  
(defvar find-cfiles-cfilename "cxx"
  "*The extension for C files used in finding C files and ther headers
when the user specifies a non-existing dot terminated file.")

(defvar find-cfiles-hfilename "h"
  "*The extension for header files used in finding C files and ther headers
when the user specifies a non-existing dot terminated file.")

(defvar find-cfiles-proto-filename ".SKEL"
  "*Used to find a skeleton file in home and current dir. Skeleton file
is copied into the newly generated file with the same extension.")

(defvar find-cfiles-cfilenamelist '("cxx" "C" "cpp" "cc")
  "*The C file extensions find-cfile uses to check for existing C files. First match used.")

(defvar find-cfiles-hfilenamelist '("h" "H" "hpp" "hh")
  "*The C file extensions find-cfile uses to check for existing header files. First match used.")


(defvar find-cfiles-proto-regexp "FNAME"
"*Occurances of this regexp in proto file are replaced after inserting.")

(defvar find-cfiles-other-frame t
"*Opens new buffers in a new frame. Semi-obvious, eh?")

(defvar find-cfiles-other-frame-parameters ()
"*Parameters passed to the frame opened for cfile buffers. See make-frame.")

(defvar find-cfiles-include-skelfile-rootdirs "\\(/home\\)\\|\\(/u\\)\\|\\(/usr\\)\\|\\(/[^:]+:\\)"
  "*Regular expression matching root directories for the search.
(The first dir from bellow that is NOT searched. The limit.)
NOT IMPLEMENTED"
)

(defvar find-cfiles-include-skelfile-number 1
  "*Defines how manly skel files are to be included (from current dir up).
Nil stands for all files, 0 for none. NIL IS AN ERROR NOW - NOT FULLY IMPLEMENTED"
)

(setq find-cfiles-include-skelfiles-sofar 0)
;(make-variable-buffer-local 'find-cfiles-include-skelfiles-sofar)

(defvar find-cfiles-include-skelfile-recursion nil
  "*Defines how manly directories are searched for skel files.
(From current dir up). This preceedes find-cfiles-include-skelfile-number.
Nil stands for unlimited recursion, 0 for none.  NOT IMPLEMENTED"
)

(provide 'find-cfiles)
;;; find-cfiles.el ends here