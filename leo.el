;;; leo.el --- Query leo dictionary

;; Copyright (C) 2013 Mario Sandler

;: Author: Mario Sandler <mario.sandler@unsicher-im-netz.de>
;; Keywords: dictionary
:; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package helps accessing the leo.org online-dictionaries from
;; inside Emacs.  The Response parser is currently very primitive. It
;; shows only the first so-called 'section' of the response. Usually
;; thats enough (for my usecase).  The response is simply formatted
;; using (table-capture).

;;; Code:

(defvar leo--result-buffer-name "*LEO*")

(defgroup leo nil
  "Leo dictionary."
  :group 'comm)

(defcustom leo-translation
  "ende"
  "Default translation."
  :type '(string)
  :group 'leo)

(defun leo--makeurl (translation word)
  "Return URL for a leo translation"
  (let ((leo-baseurl "http://dict.leo.org/dictQuery/m-vocab/{translation}/query.xml"))
    (concat (replace-regexp-in-string "{translation}" translation leo-baseurl)
            "?lang=de" "&search=" (url-hexify-string word))))

(defun leo--buffer-toxml (buffer)
  "Convert the HTTP response in BUFFER to XML parse tree."
  (let ((xml nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq xml (xml-parse-region (point) (point-max)))
      (kill-buffer buffer))
    xml))

(defun leo-query-sync (translation word)
  "Returns the parsed xml response of a leo translation"
  (let ((leo-url (leo--makeurl translation word))
        (url-request-method "GET"))
    (message "Leo query %s: %s" translation word)
    (leo--buffer-toxml
     (url-retrieve-synchronously leo-url))))

(defun leo-handle-xml (xml)
  "extracts the first <section> from the response"
  (let* ((sectionlist (xml-get-children (car xml) 'sectionlist))
         (first-section (xml-get-children (car sectionlist) 'section)))
    (car first-section)))

(defun leo-section-entry (entry)
  (let* ((side (xml-get-children entry 'side))
         (side-1 (xml-get-children (first side) 'repr))
         (side-2 (xml-get-children (second side) 'repr)))
    (list (leo-xml-tostring side-1) (leo-xml-tostring side-2))))

(defun leo-xml-tostring (l)
  "Convert a xml parse tree fragment in L to string"
  (cond ((stringp l) l)
        ((listp l) (apply 'concat (remove-if-not 'eval (mapcar 'leo-xml-tostring l))))
        ((numberp l) (number-to-string l))
        (t "")))

(defun leo--show-translation (translation word)
  "Lookup the current word at cursor position"
  (let* ((transl (mapcar 'leo-section-entry (xml-get-children
                                             (leo-handle-xml (leo-query-sync "ende" word))
                                             'entry)))
         (rows (list-length (car transl)))
         (leo-window nil))
    (if (= rows 2)
        ;; if valid response:
        (with-current-buffer (get-buffer-create leo--result-buffer-name)
          ;; clear buffer
          (delete-region (point-min) (point-max))
          ;; show buffer in a new or existing window
          (when (not (get-buffer-window (current-buffer) t)) ;; buffer not visible 'in a window'
            (display-buffer leo--result-buffer-name))
          (let ((leo-frame (window-frame (get-buffer-window (current-buffer) t)))
                (start (point)))
            (loop for (side-1 side-2) in transl
                  count (insert (concat (decode-coding-string side-1 'utf-8) "#"
                                        (decode-coding-string side-2 'utf-8) "\012")))
            ;; convert to pretty table
            (table-capture start (point) "#" "\n" 'left  (/ (- (frame-width leo-frame) 3) 2))
            (delete-matching-lines "^+")
            ;; remove first line
            (beginning-of-buffer)
            (forward-line 1)
            (delete-region (point-min) (point))))
      ;; else: query failed:
      (error "Search failed for: %s" word))))

(defun leo-current-word-default ()
  (interactive)
  (leo--show-translation leo-translation (current-word)))

(global-set-key (kbd "M-#") 'leo-current-word-default)

(provide 'leo)
 
;;; leo.el ends here
