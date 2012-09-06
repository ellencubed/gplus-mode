;;; gplus-mode.el --- Major mode for Google Plus

;; Copyright (C) 2012  Ellen Taylor

;; Author: Ellen Taylor <ellen+cubed(AT SIGN)gmail(DOT)com>
;; Keywords: googleplus

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; gplus-mode.el is a major mode for emacs that allows reading
;; activity streams as well as searching profiles.

;;; Code:

(require 'json)
(require 'url)

(defvar url-http-end-of-headers)
(defvar gplus-api-key nil)
(defvar gplus-my-profile-id nil)
(defvar gplus-profile-id nil)
(defvar gplus-root "https://www.googleapis.com/plus/v1")

(defvar gplus-data nil)

(defvar gplus-data-profile nil)
(defvar gplus-data-search nil)
(defvar gplus-data-activities nil)

(defvar gplus-mode-hook nil)
(defvar gplus-last-feed nil)
(defvar gplus-mode-map nil)
(defvar gplus-last-query nil)
(defvar gplus-last-type nil)

(defvar gplus-temp-icon nil)

(defun gplus-arg-list (&rest rest-var)
  "Takes cons cells for arguments to attach to the url"
  (concat
   "?"
   (mapconcat 
	(lambda (arg)
	  (concat
	   (url-hexify-string (car arg))
	   "="
	   (url-hexify-string (cdr arg))))
	rest-var
	"&")))

(defun gplus-people-query (q)
  "Construct profile query url for query Q"
  (concat
   gplus-root
   "/people"
   (gplus-arg-list (cons "key" gplus-api-key) (cons "query" q))))

(defun gplus-goto-profile-at-point ()
  "Load profile for the the url at point"
  (interactive)
  (if (and 
	   (equal (buffer-name (current-buffer)) (gplus-buffer-name 'query))
	   (thing-at-point-looking-at thing-at-point-url-regexp))
	  (progn
		(setq gplus-profile-id (car (last (split-string (thing-at-point 'url) "/"))))
		(gplus-feed-refresh (gplus-activities-list gplus-profile-id "public") 'activity)
		(gplus-display-posts))))

(defun gplus-activities-list (id col)
  "Contruct activities url for the profile id ID from the collection COL" 
  (concat 
   gplus-root
   "/people/"
   id
   "/activities/"
   col
   (gplus-arg-list (cons "key" gplus-api-key))))

(defun gplus-search-profiles (q)
  "Search for profiles match the term Q"
  (interactive "sSearch for profile: ")
  (gplus-feed-refresh (gplus-people-query q) 'query)
  (setq gplus-last-query q)
  (gplus-display-search-results)
  (gplus-goto-buffer 'query))

(defun gplus-display-search-results ()
  "Display search results in query buffer"
  (let ((value)(inhibit-read-only t)(results))
	(when (not (get-buffer (gplus-buffer-name 'query)))
	  (gplus-new-buffer 'query))
	(with-current-buffer (get-buffer-create (gplus-buffer-name 'query))
	  (erase-buffer)
	  (setq results (append (plist-get gplus-data-search :items) nil))
	  (if (not (equal nil results))
		(progn 
		  (insert (format "Results for search: %s\n\n" gplus-last-query))
		  (dolist (element (append (plist-get gplus-data-search :items) nil) value)
			(gplus-render-result element)))
		(insert (format "No search results for: %s" gplus-last-query)))
	  (goto-char (point-min)))))

(defun gplus-render-result (r)
  "Render a single search result R"
  (let ((name (plist-get r :displayName))
		(url (plist-get r :url)))

  (setq gplus-temp-icon (create-image (url-file-local-copy (plist-get (plist-get r :image) :url))))

  (if (not (equal gplus-temp-icon nil))
	  (progn
		(insert-image gplus-temp-icon)
		(insert (format "\n"))))
  (insert (propertize
		   (format "%s " name)
		   'font-lock-face '(:foreground "red")))
  (insert (propertize
		   (format "[%s]\n" url)
		   'font-lock-face 'bold-italic))))

(defun gplus-feed-refresh (url type)
  "Load data from feed URL into data set for TYPE"
  (let ((json-object-type 'plist))
	(with-current-buffer (url-retrieve-synchronously url)
	  (goto-char url-http-end-of-headers)
	  (setq gplus-data (json-read))
	  (kill-buffer (current-buffer)))
	(setq gplus-last-feed url))
  (setq gplus-last-type type)
  (cond
   ((equal type 'activity)
	(setq gplus-data-activities gplus-data))
   ((equal type 'query)
	(setq gplus-data-search gplus-data))))

(defun gplus-refresh-last ()
  "Refresh last loaded data"
  (interactive)
  (gplus-feed-refresh gplus-last-feed gplus-last-type)
  (cond
   ((equal gplus-last-type 'activity)
	(gplus-display-posts))
   ((equal gplus-last-type 'query)
	(gplus-display-search-results)
	(gplus-goto-buffer 'query))))

(defun gplus ()
  "Initiate gplus-mode"
  (interactive)
  (gplus-feed-refresh (gplus-activities-list gplus-my-profile-id "public") 'activity)
  (gplus-display-posts)
  (switch-to-buffer (gplus-buffer-name 'activity)))

(defun gplus-new-buffer (name)
  "Create new buffer named NAME"
  (with-current-buffer (get-buffer-create (gplus-buffer-name name))
	(gplus-mode)
	(setq buffer-read-only t)))

(defun gplus-buffer-name (type)
  "Create buffer name string for TYPE buffer"
  (format "*Google Plus - %S*" type))

(defun gplus-display-posts ()
  "Display posts from last loaded activity stream"
  (let ((value)(inhibit-read-only t))
	(when (not (get-buffer (gplus-buffer-name 'activity)))
	  (gplus-new-buffer 'activity))
	(with-current-buffer (get-buffer (gplus-buffer-name 'activity))
	  (erase-buffer)
	  (gplus-load-icon)
	  (dolist (element (append (plist-get gplus-data-activities :items) nil) value)
		(gplus-render-post element))
	  (html2text)
	  (fill-region (point-min) (point-max))
	  (goto-char (point-min))))
  (gplus-goto-buffer 'activity))

(defun gplus-render-post (element)
  "Render single post ELEMENT"
  (let ((name (plist-get (plist-get element :actor) :displayName))
		(content (plist-get (plist-get element :object) :content)))
	(insert (propertize 
			 (format "%s\n\n" name) 
			 'font-lock-face '(:foreground "yellow")))
	(if gplus-temp-icon
		(progn
		  (insert-image gplus-temp-icon)
		  (insert (format "\n\n"))))
	  (insert (propertize 
			 (format "%s\n\n" content)
			 'font-lock-face 'bold-italic))))

(defun gplus-goto-activities ()
  "Switch to activities buffer"
  (interactive)
  (switch-to-buffer (gplus-buffer-name 'activities)))

(defun gplus-goto-query ()
  "Switch to query buffer"
  (interactive)
  (switch-to-buffer (gplus-buffer-name 'query)))

(defun gplus-goto-buffer (type)
  "Switch to TYPE gplus buffer"
  (let ((bname (gplus-buffer-name type)))
	(if (get-buffer bname)
		(switch-to-buffer bname))))

(defun gplus-load-icon ()
  "Load icon for user"
  (interactive)
  (if (equal gplus-last-type 'activity)
	  (progn
		(setq gplus-temp-icon (create-image
							   (url-file-local-copy
								 (plist-get 
								  (plist-get 
								   (plist-get 
									(car 
									 (append (plist-get gplus-data-activities :items) nil)) :actor) :image) :url)))))
	(setq gplus-temp-icon nil)))

(when (not gplus-mode-map)
  (setq gplus-mode-map (make-sparse-keymap))
  (define-key gplus-mode-map (kbd "C-c a") 'gplus-goto-activities)
  (define-key gplus-mode-map (kbd "C-c q") 'gplus-goto-query)
  (define-key gplus-mode-map (kbd "C-c r") 'gplus-refresh-last)
  (define-key gplus-mode-map (kbd "C-c s") 'gplus-search-profiles)
  (define-key gplus-mode-map (kbd "RET") 'gplus-goto-profile-at-point))
  

(defun gplus-mode ()
  "Major mode for interacting with Google Plus"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'plus-mode)
  (setq mode-name "GooglePlus")
  (use-local-map gplus-mode-map)
  (run-hooks 'plus-mode-hook))

(provide 'gplus-mode)

;;; gplus-mode.el ends here
