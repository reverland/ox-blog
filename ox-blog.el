;;; ox-blog.el ---   -*- lexical-binding: t; -*-
;; Copyright (C) 2020-  Reverland

;; Author: Reverland <sa@linuxer.me>
;;
;; URL: http://github.com/reverland/ox-blog
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3") (org "0.9"))
;; Keywords: blog, org

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
;;
;;; Commentary:
;;
;; A library to use org mode export/publish to generate blog
;;
;; Copy And Modified From https://vicarie.in/posts/blogging-with-org.html
;; More info in the project site https://github.com/reverland/ox-blog
;;
;;; Code:

(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)
(require 'org-element)
(require 's)

;; customization

(defgroup ox-blog nil
  "Blog with org mode."
  :prefix "ox-blog-"
  :group 'applications
  )

(defcustom ox-blog-website-title "Reverland's Playground"
  "Format or displaying publish dates."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-date-format "%h %d, %Y"
  "Format or displaying publish dates."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-base-directory
  (expand-file-name "~/org/posts")
  "Base content direcotry which have posts."
  :type 'string
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :group 'ox-blog)

(defcustom ox-blog-publishing-directory
  (expand-file-name "~/org/blog")
  "Publish destination direcotry."
  :type 'string
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :group 'ox-blog)

(defcustom ox-blog-sitemap-filename
  "archive.org"
  "Sitemap filename which will list all posts."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-sitemap-title
  "Blog Posts"
  "Sitemap title."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-link-home
  "https://reverland.org"
  "Blog link home."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-copy-right (format
   "copyleft@2012-%s"
   (nth 5 (parse-time-string (current-time-string))))
  "Blog copyright string."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-ga nil
  "Blog google analytic id."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-disqus nil
  "Blog disqus name."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-footer
  (format "<div class=\"ui center aligned segment\">
    <a href=\"%s\">
	  <p> Built with
	    <svg id=\"i-heart\" viewBox=\"0 0 32 32\">
    	  <path d=\"M4 16 C1 12 2 6 7 4 12 2 15 6 16 8 17 6 21 2 26 4 31 6 31 12 28 16 25 20 16 28 16 28 16 28 7 20 4 16 Z\"/>
	    </svg> in
        <img id=\"i-emacs\" src=\"https://www.gnu.org/software/emacs/images/emacs.png\"/>
        <span id=\"view-source-link\"> %s </span>
	  </p>
    </a>
  </div>
"
          ox-blog-link-home
          ox-blog-copy-right
)
  "Blog Website page footer."
  :type 'string
  :group 'ox-blog
  )

(defcustom ox-blog-head
  (concat
  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css\" crossorigin=\"anonymous\">
  <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/blog.css\"/>
  <link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"/static/favicon.ico\">"
  org-html-style-default)
  "Blog website page head, something like css import link."
  :type 'string
  :group 'ox-blog)

(defcustom ox-blog-js
  (concat
  "<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js\"
<script type=\"text/javascript\" src=\"https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js\"> </script>
<script type=\"text/javascript\" src=\"/static/blog.js\"> </script>
"
  org-html-scripts
  )
  "Blog Website page lazyload js."
  :type 'string
  :group 'ox-blog
  )

;; functions
(defun ox-blog-prepare (project-plist)
  "With help from `https://github.com/howardabrams/dot-files'.
Touch `index.org' to rebuilt it.
Argument `PROJECT-PLIST' contains information about the current project."
  (let* ((base-directory (plist-get project-plist :base-directory))
         (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (save-buffer 0))
    (kill-buffer buffer)))

(defun ox-blog-preamble (plist)
  "Pre-amble for whole blog with PLIST."
  (when (s-starts-with-p "20" (file-name-nondirectory (plist-get plist :input-file)))
    (plist-put plist
               :publish-date (format "Published on %s"
                                 (org-export-get-date plist
                                                      ox-blog-date-format))))
  ;; Return a simple banner with navigation links
  ;; (let (ox-blog-plist (org-export--get-inbuffer-options 'ox-blog))
  ;;   )
  "<div id=\"nav\" class=\"ui large secondary teal pointing menu\">
    <a class=\"toc item\">
      <i class=\"sidebar icon\" onclick=\"toggleNav()\"></i>
    </a>
    <a class=\"item\" href=\"/\"> Home </a>
    <a class=\"item\" href=\"/archive.html\"> Posts </a>
    <a class=\"item\" href=\"/archive.xml\"> RSS </a>
    <a class=\"item\" href=\"/index.html\"> About Me </a>
    <a class=\"item\" href=\"/links.html\"> Links </a>
  </div>")

(defun ox-blog-postamble (plist)
  "Post-amble for whole blog with PLIST."
  (concat
   ox-blog-footer
   ox-blog-js
   (format
    "<script>document.addEventListener(\"DOMContentLoaded\", function (e) {
  title = document.getElementsByTagName(\"title\")[0];
  // This is it: \"\" Can you see it?
  if (title.text.length == 1) {
    title.innerText = \"%s\";
  };
});
</script>"
    ox-blog-website-title)
   (when ox-blog-ga
     (format-spec
      "<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=%a\"\></script>
      <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());

      gtag('config', '%a');
      </script>
"
      (format-spec-make ?a ox-blog-ga)
     )
   )

   ;; Add Disqus if it's a post
   (when (and ox-blog-disqus (s-contains-p "posts/" (plist-get plist :input-file)))
     (format "<!-- Disqua JS -->
<div id=\"disqus_thread\" class=\"ui segment\"></div>
  <script type=\"text/javascript\">
   var disqus_shortname = '%s';
   (function() {
     var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
     dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
     (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
   })();
  </script>
  <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
" ox-blog-disqus))
   )
)

(defun ox-blog-publish-find-subtitle (file project)
  "Find the subtitle of FILE in PROJECT."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :subtitle nil t)
        (let* ((parsed-subtitle (org-publish-find-property file :subtitle project 'ox-blog))
               (subtitle
                (if parsed-subtitle
                    ;; Remove property so that the return value is
                    ;; cache-able (i.e., it can be `read' back).
                    (org-no-properties
                     (org-element-interpret-data parsed-subtitle))
                  (file-name-nondirectory (file-name-sans-extension file)))))
          (org-publish-cache-set-file-property file :subtitle subtitle)))))

(defun ox-blog-publish-find-filetags (file project)
  "Find the filetags of FILE in PROJECT."
  (let ((file (org-publish--expand-file-name file project)))
    (or (org-publish-cache-get-file-property file :filetags nil t)
        (let* ((parsed-tags (org-publish-find-property file :filetags project))
               (tags (pcase (type-of parsed-tags)
                       ('string (list parsed-tags))
                       ('symbol (list parsed-tags))
                       ('cons parsed-tags)
                       )))
          (org-publish-cache-set-file-property file :filetags tags)))))

(defun ox-blog-sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with-p "20" entry)
    (format (concat
             "@@html:<div class=\"content\">"
             "<div class=\"header\">@@ [[file:%s][%s]] @@html:</div>"
             "<div class=\"meta\"><span>@@ %s @@html:</span><i class=\"heartbeat icon\"></i></div>"
             "<div class=\"description\"><span> %s </span></div>"
             "<div class=\"extra content\">%s</div>"
             "</div>@@")
            entry
            (org-publish-find-title entry project)
            (format-time-string "%h %d, %Y"
                                (org-publish-find-date entry project))
            (ox-blog-publish-find-subtitle entry project)
            ;; (ox-blog-publish-find-filetags entry project)
            (let ((tags (ox-blog-publish-find-filetags entry project))
                  (result ""))
              (dolist (tag tags result)
                (setq
                 result
                 (concat
                  result
                  "<a class=\"ui tag label\">"
                  tag
                  "</a>"
                  )
                 )
                )
              result
              )
            )))

(defun ox-blog-sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `ox-blog-sitemap-format-entry'."
  (concat "#+TITLE: " title "\n\n"
          "\n#+begin_archive\n"
          "@@html:<div class=\"ui cards\">@@"
          (mapconcat (lambda (li)
                       (format "@@html:<div class=\"ui card\">@@ %s @@html:</div>@@" (car li)))
                     (seq-filter #'car (cdr list))
                     "\n")
          "@@html:</div>@@"
          "\n#+end_archive\n"))

(defun ox-blog-publish-to-html (plist filename pub-dir)
  "Same as `org-html-publish-to-html' but modifies html before finishing.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory."
  (let ((file-path (org-publish-org-to 'ox-blog filename ".html" plist pub-dir)))
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-min))
      (search-forward "<body>")
      (insert (concat "\n<div class=\"ui container\">\n "
                      ))
      (goto-char (point-max))
      (search-backward "</body>")
      (insert "\n</div>\n")
      (save-buffer 0)
      (kill-buffer))
    file-path))

(defun ox-blog-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   ;; (subtitle (plist-get info :subtitle))
	   (publish-date (plist-get info :publish-date))
	   ;; (category (plist-get info :category))
	   ;; (tags (plist-get info :filetags))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header class=\"title ui ribbon label\">\n<h1>%s</h1>\n%s\n</header>\n"
	    "<h1 class=\"title\">%s</h1>%s\n")
	  (org-export-data title info)
    (if publish-date
        (format
         "<div class=\"small\"><i class=\"history icon\"></i>%s</div>"
         publish-date
         )
      )
	  ;; (if subtitle
	  ;;     (format
	  ;;      (if html5-fancy
		;;    "<p class=\"subtitle\">%s</p>\n"
		;;  (concat "\n" (org-html-close-tag "br" nil info) "\n"
		;; 	 "<span class=\"subtitle\">%s</span>\n"))
	  ;;      (org-export-data subtitle info))
	  ;;   "")
    ;; (if category
    ;;     (format "<div class=\"category container\"><span class=\"badge badge-primary\">%s</span></div>" category)
    ;;   "")
    ;; (if tags
    ;;     (format "<div class=\"tags container\">%s</div>"
    ;;             (let ((result ""))
    ;;               (dolist (tag tags result)
    ;;                 (setq result (concat
    ;;                               (format "<span class=\"badge badge-info\">%s</span> " tag) result)))
    ;;               )
    ;;             )
    ;;   "")
    )
   )))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(org-export-define-derived-backend 'ox-blog 'html
  :options-alist '((:category "CATEGORY" nil nil space)
                   (:tags "TAGS" nil nil split))
  :translate-alist '((template . ox-blog-html-template))
  )


(defun ox-blog-get-post-file ()
  "Create an org file in ~/org/posts/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org"
                              name) ox-blog-base-directory)))

;;;###autoload
(defun ox-publish-blog (force)
  "Publish blog whth or without FORCE.
By default, org page only publish that has changed. When prefix means force publish all"
  (interactive "P")
  (org-publish-project "blog" force nil))

;; reasonable default setq
(setq org-export-use-babel nil)
(setq org-confirm-babel-evaluate nil)
(setq org-publish-project-alist
      `(("blog-content"
         :base-directory ,ox-blog-base-directory
         :exclude ".*~"
         :base-extension "org"

         :publishing-directory ,ox-blog-publishing-directory

         :recursive t
         ;;:preparation-function ox-blog-prepare
         :publishing-function ox-blog-publish-to-html

         :with-toc nil
         :with-title org-export-with-title
         :with-date org-export-with-date
         :section-numbers nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-headline-class "ui dividing header"
         :htmlized-source t
         :html-head-extra ,ox-blog-head
         :html-preamble ox-blog-preamble
         :html-postamble ox-blog-postamble

         :auto-sitemap t
         :sitemap-filename ,ox-blog-sitemap-filename
         :sitemap-title ,ox-blog-sitemap-title

         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry ox-blog-sitemap-format-entry
         :sitemap-function ox-blog-sitemap-function
         )
        ("blog-static"
         :base-directory ,ox-blog-base-directory
         :base-extension "jpg\\|png\\|css\\|js\\|ico\\|gif\\|pdf\\|ogg"
         :recursive t
         :publishing-directory ,ox-blog-publishing-directory
         :publishing-function org-publish-attachment
         )
        ("blog-rss"
         :base-directory ,ox-blog-base-directory
         ;; :base-extension "org"

         :html-link-home ,ox-blog-link-home
         :html-link-use-abs-url t

         :rss-extension "xml"

         :publishing-directory ,ox-blog-publishing-directory
         :publishing-function (org-rss-publish-to-rss)

         :exclude ".*"
         :include (,ox-blog-sitemap-filename)
         :section-numbers nil
         :with-toc nil
	       :title ,ox-blog-website-title
         )
	      ("blog"
	       :components
	       ("blog-content" "blog-static" "blog-rss"))))

(provide 'ox-blog)

;;; ox-blog.el ends here
