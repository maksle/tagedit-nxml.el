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

;; Caveats:

;; Experimental features don't work currently but some of them are html-mode
;; specific. There's no way currently to make
;; `tagedit-disable-experimental-features' buffer-local that I know of so we
;; have to (try) to disable them manually. See `enable-tagedit-xml'.
;; 
;; Also, forward-list and backward-list had to be advised.

;; To use it:
;; 
;; (add-hook 'nxml-mode-hook
;;   (lambda () 
;;     (tagedit-mode)
;;     (require 'tagedit-nxml)
;;     (enable-tagedit-nxml))


(defadvice forward-list (around nxml-version (&optional n) activate)
  "`forward-list' and `backward-list' list don't do anything
useful in nxml mode and don't behave the same way as in sgml
mode. It would be nice if this was overridable in tagedit-mode so
that we don't have to use advice."
  (if (not (equal mode-name "nXML"))
      ad-do-it
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
            (setq n (- n 1))))))))

(defadvice backward-list (around nxml-version (&optional n) activate)
  "`forward-list' and `backward-list' list don't do anything
useful in nxml mode and don't behave the same way as in sgml
mode. It would be nice if this was overridable in tagedit-mode so
that we don't have to use advice."
  (if (not (equal mode-name "nXML"))
      ad-do-it
    (let ((n (or n 1)))
      (if (< n 0)
          (forward-list (abs n))
        (while (> n 0)
          (backward-sexp)
          (unless (looking-at-p "<")
            (backward-sexp))
          (setq n (- n 1)))))))

(defadvice te/select-attribute (around nxml-version () activate)
  "For some reason the syntax-ppss trick to check if inside a
string doesn't work in nxml mode, but we have a real xml parser."
  (if (not (equal mode-name "nXML"))
      ad-do-it
    (search-forward "\"")
    (nxml-token-before)
    (save-excursion (goto-char xmltok-start) (xmltok-forward))
    (let ((attr (find-if (lambda (att) 
                           (and (< (xmltok-attribute-name-start att) (point))
                                (>= (xmltok-attribute-value-end att) (- (point) 2)))) 
                         xmltok-attributes)))
      (goto-char (+ 1 (xmltok-attribute-value-end attr)))
      (set-mark (point))
      (goto-char (xmltok-attribute-name-start attr)))))

(defun te-nxml/current-tag ()
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
  "Equivalent in behavior to sgml-skip-tag-forward, but unlike
the sgml version it also handles comments correctly as tags which
is nice because it allows us to slurp/barf/etc them too without
messing up the structure."
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 1))
    (nxml-token-before))
  (cond ((memq xmltok-type '(empty-element))
         (goto-char xmltok-start)
         (nxml-forward-element))
        ((memq xmltok-type '(start-tag))
         (goto-char xmltok-start)
         ;; hack to get editable regions to work
         (ignore-errors (nxml-forward-element)))
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
         (forward-list)
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
  ;; Turn on tagedit mode
  ;; (tagedit-mode)

  ;; Set our nxml-verions of the functions that are overridable without
  ;; defadvice
  (set (make-local-variable 'te/skip-tag-forward-fn)
       'te-nxml/skip-tag-forward)
  (set (make-local-variable 'te/current-tag-fn)
       'te-nxml/current-tag)
  
  ;; this is the default value, but is required so that backward-sexp does what
  ;; tagedit expects.
  (setq nxml-sexp-element-flag nil) 

  ;; There's no way currently to make `tagedit-disable-experimental-features'
  ;; buffer-local that I know of so we have to disable them manually.
  (set (make-local-variable 'tagedit-experimental-features-on?) nil)
  (te/turn-off-tag-editing)
  (define-key tagedit-mode-map (kbd "<") nil) ; how do I make this not affect html-mode?
  (define-key tagedit-mode-map (kbd ">") nil)) ; how do I make this not affect html-mode?


(provide 'tagedit-nxml)
