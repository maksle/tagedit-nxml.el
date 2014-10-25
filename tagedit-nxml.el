;;; tagedit-nxml.el --- make tagedit compatible with nxml-mode

;; Copyright (C) 2014 Maksim Grinman <maxchgr@gmail.com>

;; Author: Maksim Grinman <maxchgr@gmail.com>
;; Package-Requires: ((tagedit "1.4.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Commentary: 

;; This makes tagedit compatible with nxml-mode. nxml-mode is the
;; defaut xml mode and would benefit from tagedit features, but it is not
;; derived from sgml-mode.

;; Thanks to Magnar Sveen for the awesome tagedit package.

;; To use it:
;; 
;; (add-hook 'nxml-mode-hook
;;   (lambda () 
;;     (tagedit-mode)
;;     (require 'tagedit-nxml)
;;     (enable-tagedit-nxml))


(defun te-nxml/forward-list (&optional n)
    (cl-flet 
        ((start-or-end-tag? () 
                            (nxml-token-before) 
                            (memq xmltok-type '(start-tag end-tag empty-element))))
      (let ((n (or n 1)))
        (if (< n 0)
            (backward-list (abs n))
          (while (> n 0)
            (xmltok-forward)
            (while (not (start-or-end-tag?))
              (xmltok-forward))
            (setq n (- n 1)))))))

(defun te-nxml/backward-list (&optional n)
    (let ((n (or n 1)))
      (if (< n 0)
          (te-nxml/forward-list (abs n))
        (while (> n 0)
          (te-nxml/backward-sexp)
          (unless (looking-at-p "<")
            (te-nxml/backward-sexp))
          (setq n (- n 1))))))

(defun te-nxml/backward-sexp (&optional n)
  "Allow user to have `nxml-sexp-element-flag' be non-nil if they
 prefer but still have the backward-sexp behavior that tagedit
 expects for tagedit functions."
  (let ((nxml-sexp-element-flag nil))
    (backward-sexp n)))

(defun te-nxml/forward-sexp (&optional n)
  "Allow user to have `nxml-sexp-element-flag' be non-nil if they
 prefer but still have the forward-sexp behavior that tagedit
 expects for tagedit functions."
  (let ((nxml-sexp-element-flag nil))
    (forward-sexp n)))

(defun te-nxml/point-inside-string? ()
  "For some reason the syntax-ppss trick to check if inside a
string doesn't work in nxml mode. 

This returns the xmltok attribute if we are in an attribute string of the tag
containing point. Otherwise it returns nil."
  (save-excursion 
    (forward-char 1)
    (nxml-token-before)
    (goto-char xmltok-start) 
    (xmltok-forward))
  (let ((attr (find-if (lambda (att) 
                           (and (<= (xmltok-attribute-value-start att) (point))
                                (>= (xmltok-attribute-value-end att) (point)))) 
                         xmltok-attributes)))
    attr))

(defun te-nxml/current-tag ()
  "Return current tag alist that `te/current-tag-fn' expects."
  (save-excursion
    (te-nxml/get-context)
    (save-excursion
      (forward-char 1)
      (nxml-token-before))
    (let ((name (if (memq xmltok-type '(cdata-section))
                    "CDATA"
                  (xmltok-start-tag-qname)))
          (self-closing (if (eq xmltok-type 'empty-element) :t :f))
          (beg xmltok-start)
          (end (progn
                 (te-nxml/skip-tag-forward)
                 (point))))
      `((:name . ,name)
        (:self-closing . ,self-closing)
        (:beg . ,beg)
        (:end . ,end)))))

(defun te-nxml/skip-tag-forward ()
  "Equivalent in behavior to sgml-skip-tag-forward, but it also
handles comments as tags which is nice because it
allows us to slurp/barf/etc them too without messing up the
structure."
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 1))
    (nxml-token-before))
  (cond ((memq xmltok-type '(empty-element))
         (goto-char xmltok-start)
         (nxml-forward-element))
        ((memq xmltok-type '(start-tag))
         (goto-char xmltok-start)
         ;; to get editable regions to work which make the nxml momentarily invalid
         ;; or when trying to edit an unclosed start tag
         (let ((start xmltok-start))
           (condition-case nil
               (nxml-forward-element)
             (error (goto-char start) (te-nxml/forward-list)))))
        ((memq xmltok-type '(end-tag cdata-section comment processing-instruction))
         (goto-char xmltok-start)
         (xmltok-forward))
        ;; edge case when in cdata section
        ((and (memq xmltok-type '(not-well-formed))
              (looking-at-p "]]>"))
         (forward-char 3))
        ((memq xmltok-type '(data space))
         ;; (xmltok-forward)
         ;; (te-nxml/skip-tag-forward)
         (te-nxml/forward-list)
         )))

(defun te-nxml/get-context ()
  "Unlike te-sgml/get-context it doesn't return anything, but it
does go to context start location like sgml-get-context does
which is all we need. Some tagedit functions would work fine with
sgml-get-context but not all."
  (save-excursion
    (ignore-errors (forward-char 1))
    (nxml-token-before))
  (cond ((memq xmltok-type '(start-tag comment cdata-section processing-instruction empty-element))
         (goto-char xmltok-start))
        ((memq xmltok-type '(end-tag data space))
         (nxml-backward-up-element))
        ((memq xmltok-type '(not-well-formed))
         ;; edge case, sort of a hack but should always work because either the
         ;; xml is invalid which violates all assumptions or we're in a
         ;; cdata-section
         (sgml-get-context)
         (te-nxml/get-context))))


(defun enable-tagedit-nxml ()
  ;; Set our nxml-verions of the functions 
  (set (make-local-variable 'te/skip-tag-forward-fn) 'te-nxml/skip-tag-forward)
  (set (make-local-variable 'te/current-tag-fn) 'te-nxml/current-tag)
  (set (make-local-variable 'te/forward-list-fn) 'te-nxml/forward-list)
  (set (make-local-variable 'te/backward-list-fn) 'te-nxml/backward-list)
  (set (make-local-variable 'te/forward-sexp-fn) 'te-nxml/forward-sexp)
  (set (make-local-variable 'te/backward-sexp-fn) 'te-nxml/backward-sexp)
  (set (make-local-variable 'te/point-inside-string-fn) 'te-nxml/point-inside-string?))


(provide 'tagedit-nxml)
