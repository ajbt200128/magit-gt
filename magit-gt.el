;;; magit-gt.el --- A Graphite extension for Magit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Austin Theriault
;;
;; Author: Austin Theriault <austin@cutedogs.org>
;; Maintainer: Austin Theriault <austin@cutedogs.org>
;; Created: March 26, 2025
;; Modified: March 26, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/r2cuser/magit-gt
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A Graphite extension for Magit.
;;
;;; Code:

(require 'magit)

;; Customization
(defgroup magit-gt nil
  "A Graphite extension for Magit."
  :prefix "magit-gt-"
  :group 'magit-extensions)

(defcustom magit-gt-executable "gt"
  "The Graphite executable used by `magit-gt'."
  :type 'string
  :group 'magit-gt)

(defcustom magit-gt-global-arguments '("--no-interactive")
  "Additional arguments to pass to `gt'."
  :type '(repeat string)
  :group 'magit-gt)


;; Noninteractive functions
;;
(defun magit-gt-process-git-arguments (args)
  "Prepare ARGS for a function that invokes Graphite.

* Flatten ARGS, removing nil arguments.
* Prepend `magit-gt-git-global-arguments' to ARGS.
* On w32 systems, encode to `w32-ansi-code-page'."
  (setq args (append magit-gt-global-arguments (flatten-tree args)))
  (if (and (eq system-type 'windows-nt) (boundp 'w32-ansi-code-page))
      ;; On w32, the process arguments *must* be encoded in the
      ;; current code-page (see #3250).
      (mapcar (lambda (arg)
                (encode-coding-string
                 arg (intern (format "cp%d" w32-ansi-code-page))))
              args)
    args))

(defun magit-gt-start-gt (input &rest args)
  "Start a new `gt' process with INPUT and ARGS."
  (run-hooks 'magit-pre-start-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-start-process magit-gt-executable input
           (magit-gt-process-git-arguments args))))

(defun magit-gt-run-gt-async (&rest args)
  "Run `gt' asynchronously with ARGS."
  (magit-msg "Running %s %s" magit-gt-executable
             (let ((m (string-join (flatten-tree args) " ")))
               (remove-list-of-text-properties 0 (length m) '(face) m)
               m))
  (magit-gt-start-gt nil args))

(defun magit-gt-run-gt-with-editor (&rest args)
  "Export GIT_EDITOR and start Graphite with ARGS."
  (magit--record-separated-gitdir)
  (magit-with-editor (magit-gt-run-gt-async args)))

(defun magit-gt-arguments (&rest filters)
  "Return a list of arguments for `magit-gt' with FILTERS."
  (seq-filter (##and (member % filters) %)
              (transient-args 'magit-gt)))

;; Classes
(defclass magit-gt--suffix (transient-suffix)
  ())

(cl-defmethod transient-format-description ((obj magit-gt--suffix))
  (let ((value (delq nil (mapcar #'transient-infix-value transient--suffixes))))
    (replace-regexp-in-string
     "\\[--[^]]+\\]"
     (lambda (match)
       (format (propertize "[%s]" 'face 'transient-inactive-argument)
               (mapconcat (lambda (arg)
                            (propertize arg 'face
                                        (if (member arg value)
                                            'transient-argument
                                          'transient-inactive-argument)))
                          (save-match-data
                            (split-string (substring match 1 -1) "|"))
                          (propertize "|" 'face 'transient-inactive-argument))))
     (cl-call-next-method obj))))

;; Interactive functions
;;;###autoload (autoload 'magit-gt "magit-gt" nil t)
(transient-define-prefix magit-gt ()
  "Invoke Graphite"
  ["Arguments"
   ("-n" "Disable hooks" "--no-verify")
   ("-c" "Create a new commit instead of ammending the current commit" ("-c" "--commit"))
   ("-a" "Stage all changes before committing" ("-a" "--all"))
   ("-d" "Create all new PRs in draft mode" ("-d" "--draft"))
   ("-p" "Publish all PRs being submitted" ("-p" "--publish"))
   ("-U" "Only push branches and update PRs for branches that already have PRs open" ("-u" "--update-only"))
   ("-m" "Mark all PRs being submitted as merge when ready" ("-m" "--merge-when-ready"))
   ("-r" "Rerequest reviews on all PRs being submitted" ("-r" "--rerequest-reviews"))
   ("-v" "Open the PRs in the browser after submitting" ("-v" "--view"))
   ("-s" "Submit descendants of the current branch in addition to its ancestors." ("-s" "--stack"))
   ("-u" "Stage all updates to tracked files before committing" ("-u" "--update"))
   ("-e" "Edit commit message. Default when -c is passed" ("-e" "--edit"))
   ("-i" "Insert this branch between the current branch and its child."("-i" "--insert"))
   ("-f" "Don't prompt for confirmation before overwriting or deleting a branch" ("-f" "--force"))
   ("-n" "Don't restack any branches" "--no-restack")]
  ["Actions"
   ("c" magit-gt-create)
   ("s" magit-gt-submit)
   ("m" magit-gt-modify)
   ("f" magit-gt-sync)
   ("u" magit-gt-up)
   ("d" magit-gt-down)])


;;;###autoload (autoload 'magit-gt-modify "magit-gt" nil t)
(transient-define-suffix magit-gt-modify (args)
  "Modify the current branch by amending its commit or creating a new commit. Automatically restacks descendants.
   If you have any unstaged changes, you will be asked whether you'd like to stage them."
  :class 'magit-gt--suffix
  :description "Update         gt modify [--no-verify] [--commit] [--all]
                       [--update] [--edit]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify" "--commit" "--all" "--edit")))
  (cond ((member "--commit" args)
         (magit-gt-run-gt-with-editor "modify" args))
        (t (magit-gt-run-gt-async "modify" args))))

;;;###autoload (autoload 'magit-gt-submit "magit-gt" nil t)
(transient-define-suffix magit-gt-submit (args)
  "Idempotently force push all branches from trunk to the current branch to GitHub, creating or updating distinct
   pull requests for each. Validates that branches are properly restacked before submitting, and fails if there
   are conflicts."
  :class 'magit-gt--suffix
  :description "Submit         gt submit [--no-verify] [--draft] [--publish]
                       [--update-only] [--merge-when-ready] [--rerequest-reviews]
                       [--view] [--stack]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify" "--draft" "--publish" "--update-only"
                        "--merge-when-ready" "--rerequest-reviews" "--view"
                        "--stack")))
  (magit-gt-run-gt-async "submit" args))

;;;###autoload (autoload 'magit-gt-create "magit-gt" nil t)
(transient-define-suffix magit-gt-create (args)
  "Create a new branch stacked on top of the current branch and commit staged changes."
  :class 'magit-gt--suffix
  :description "Create         gt create [--no-verify] [--all] [--update]
                       [--insert]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify" "--all" "--update" "--insert")))
  ;;TODO magit-commit-assert type vibe
  (let ((default-directory (magit-toplevel)))
    (magit-gt-run-gt-with-editor "create" (append '("-m") args ))))

;;;###autoload (autoload 'magit-gt-sync "magit-gt" nil t)
(transient-define-suffix magit-gt-sync (args)
  "Sync all branches with remote, prompting to delete any branches for PRs that have been merged or closed."
  :class 'magit-gt--suffix
  :description "Sync           gt sync [--no-verify] [--no-restack]
                          [--force] [--all]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify" "--no-restack" "--force" "--all")))
  (magit-gt-run-gt-async "sync" args))

;;;###autoload (autoload 'magit-gt-up "magit-gt" nil t)
(transient-define-suffix magit-gt-up (args)
  "Switch to the child of the current branch"
  :class 'magit-gt--suffix
  :description "Up             gt up [--no-verify]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify")))
  (magit-gt-run-gt-async "up" args))

;;;###autoload (autoload 'magit-gt-down "magit-gt" nil t)
(transient-define-suffix magit-gt-down (args)
  "Switch to the parent of the current branch"
  :class 'magit-gt--suffix
  :description "Down           gt down [--no-verify]"
  (interactive
   (list
    (magit-gt-arguments "--no-verify")))
  (magit-gt-run-gt-async "down" args))


(provide 'magit-gt)
;;; magit-gt.el ends here
