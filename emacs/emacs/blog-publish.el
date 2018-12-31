;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar org-publish-project-alist)

(defvar jterk/blog-head-extra)
(defvar jterk/blog-postamble)
(defvar jterk/blog-postamble-links)

(setq jterk/blog-head-extra
      (concat
       "<style type='text/css'>"
       "body{font-family:sans-serif;}"
       "h1.title{background:rgb(245,245,245);}"
       "#content,#postamble{width:50em;margin: 0 auto;}"
       "#postamble{text-align:center; background:rgb(245,245,245); padding:0.25em 0;}"
       ".footer{margin:0;}"
       "h1.footer{font-size: 1em;}"
       "ul.footer{list-style: none;}"
       "ul.footer li{display: inline; margin: auto 0.25em;}"
       "</style>"))

(setq jterk/blog-postamble-links
      '(("github" "https://www.github.com/jterk")
        ("linkedin" "https://www.linkedin.com/profile/view?id=14846510")
        ("stack overflow" "https://stackoverflow.com/users/12582/jason-terk")
        ("twitter" "https://twitter.com/goterkyourself")))

(defun jterk/gen-postamble-links (links)
  "Generate links from LINKS."
  (let (result)
    (dolist (link links result)
      (setq result (concat result
                           "<li><a href='"
                           (cadr link)
                           "'>"
                           (car link)
                           "</a></li>")))))

(setq jterk/blog-postamble
      (concat
       "<h1 class='footer'>Elsewhere</h1>"
       "<ul class='footer'>"
       (jterk/gen-postamble-links jterk/blog-postamble-links)
       "</ul>"
       ))

(setq org-publish-project-alist
      `(("blog-pages"
         :base-directory "~/Projects/blog/pages"
         :publishing-directory "~/tmp/blog"
         :publishing-function org-html-publish-to-html

         :with-toc nil
         :section-numbers nil

         :html-head-extra ,jterk/blog-head-extra
         :html-preamble nil
         :html-postamble ,jterk/blog-postamble
         )
        ("blog-entries"
         :base-directory "~/Projects/blog/blog"
         :publishing-directory "~/tmp/blog"
         :publishing-function org-html-publish-to-html
         :recursive t

         :with-date nil
         :with-toc nil
         :section-numbers nil

         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-sort-folders ignore
         :sitemap-style list
         :sitemap-title "Go Terk Yourself"

         :html-head-extra ,jterk/blog-head-extra
         :html-preamble nil
         :html-postamble ,jterk/blog-postamble
         )))

(provide 'blog-publish)
;;; blog-publish.el ends here
