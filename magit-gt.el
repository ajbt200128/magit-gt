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

(defun magit-gt-process-gt-arguments (args)
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

(defun magit-gt-process-gt (destination &rest args)
  "Call Gt synchronously in a separate process, returning its exit code.
DESTINATION specifies how to handle the output, like for
`call-process', except that file handlers are supported.
Enable Cygwin's \"noglob\" option during the call and
ensure unix eol conversion."
  (apply #'magit-process-file
         magit-gt-executable
         nil destination nil
         (magit-gt-process-gt-arguments args)))

(defun magit-gt-call-gt (&rest args)
  "Call Gt synchronously in a separate process.

Function `magit-gt-executable' specifies the Git executable and
option `magit-gt-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'."
  (run-hooks 'magit-pre-call-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-call-process
           magit-gt-executable
           (magit-gt-process-gt-arguments args))))

(defun magit-gt-run-gt (&rest args)
  "Call Gt synchronously in a separate process, and refresh.

Function `magit-gt-executable' specifies the Git executable and
option `magit-gt-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

After Gt returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'."
  (let ((magit--refresh-cache (list (cons 0 0))))
    (prog1 (magit-gt-call-gt args)
      (magit-refresh))))

(defun magit-gt-start-gt (input &rest args)
  "Start a new `gt' process with INPUT and ARGS."
  (run-hooks 'magit-pre-start-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-start-process magit-gt-executable input
           (magit-gt-process-gt-arguments args))))

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




(defun magit-gt-insert (&rest args)
  "Execute graphite with ARGS, insert stdout at point and return exit code.
If `magit-git-debug' in non-nil and the exit code is non-zero, then
insert the run command and stderr into the process buffer."
  (apply #'magit-gt--insert nil args))

(defun magit-gt--insert (return-error &rest args)
  (setq args (flatten-tree args))
  (if (or return-error magit-git-debug)
      (let (log)
        (unwind-protect
            (let (exit errmsg)
              (setq log (make-temp-file "magit-stderr"))
              (delete-file log)
              (setq exit (magit-gt-process-gt (list t log) args))
              (when (or (> exit 0) (eq magit-git-debug 'all))
                (when (file-exists-p log)
                  (with-temp-buffer
                    (insert-file-contents log)
                    (goto-char (point-max))
                    (setq errmsg
                          (cond
                           ((eq return-error 'full)
                            (buffer-string))
                           ((functionp magit-git-debug)
                            (funcall magit-git-debug (buffer-string)))
                           ((magit--locate-error-message)))))
                  (when magit-git-debug
                    (let ((magit-git-debug nil))
                      (with-current-buffer (magit-process-buffer t)
                        (magit-process-finish-section
                         (magit-process-insert-section
                          default-directory magit-gt-executable
                          (magit-gt-process-gt-arguments args)
                          exit log 'magit-section-secondary-heading)
                         exit)))))
                (cond ((not magit-git-debug))
                      (errmsg (message "%s" errmsg))
                      ((zerop exit))
                      ((message "Git returned with exit-code %s" exit))))
              (or errmsg exit))
          (ignore-errors (delete-file log))))
    (magit-gt-process-gt (list t nil) args)))

(defun magit-gt-json (&rest args)
  "Execute Gt with ARGS, returning its output as parsed json.

If Gt exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (magit--with-temp-process-buffer
    (apply #'magit-gt-insert args)
    (json-parse-string (buffer-string) :object-type 'alist)))

(defun magit-gt-string (&rest args)
  "Execute Gt with ARGS, returning the first line of its output.
If there is no output, return nil.  If the output begins with a
newline, return an empty string."
  (setq args (flatten-tree args))
  (magit--with-refresh-cache (cons default-directory args)
    (magit--with-temp-process-buffer
      (apply #'magit-gt-insert args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun magit-gt--trunk ()
  "Return the trunk branch."
  (magit-gt-string "trunk"))

(defun magit-gt--get-branches ()
  "Return a list of branches tracked by Graphite"
  (let* ((branches-alist (magit-gt-json "state")))
    (mapcar 'car branches-alist)))

(defun magit-gt--modify (&optional args)
  "Call gt modify with ARGS"
  (cond ((member "--commit" args)
         (magit-gt-run-gt-with-editor "modify" args))
        (t (magit-gt-run-gt-async "modify" args))))

(defun magit-gt--create (&optional args)
  "Call gt create with ARGS"
  ;;TODO magit-commit-assert type vibe
  (magit-with-toplevel
    (magit-gt-run-gt-with-editor "create" (append '("-m") args ))))

(defun magit-gt--submit (&optional args)
  "Call gt submit with ARGS"
  (magit-gt-run-gt-async "submit" args))

(defun magit-gt--sentinel (fun &optional args)
  "Call FUN with ARGS asynchronously and block until prev magit process is done."
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (if (> (process-exit-status process) 0)
           (magit-process-sentinel process event)
         (process-put process 'inhibit-refresh t)
         (magit-process-sentinel process event)
         (apply fun args))))))

(defun magit-gt--submit-sentinel (&optional args)
  "Call magit-gt--submit with ARGS async and wait until done."
  (magit-gt--sentinel 'magit-gt--submit args))

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
;; Macros
(defmacro magit-gt-args (args)
  `(defun ,(intern (format "magit-gt-%s-arguments" args)) (&rest filters)
     ,(format "Return a list of arguments for `magit-gt %s' with FILTERS." args)
     (seq-filter (##and (member % filters) %)
                 (transient-args (intern (format "magit-gt-%s" ,args))))))
;; Interactive functions
;;;###autoload (autoload 'magit-gt "magit-gt-core-local" nil t)
(transient-define-prefix magit-gt-core-local ()
  "Invoke Graphite's core local workflows"
  ["Arguments"
   ("-N" "Disable hooks" "--no-verify")
   ("-c" "Create a new commit instead of ammending the current commit" ("-c" "--commit"))
   ("-a" "Stage all changes before committing" ("-a" "--all"))
   ("-u" "Stage all updates to tracked files before committing" ("-u" "--update"))
   ("-e" "Edit commit message. Default when -c is passed" ("-e" "--edit"))
   ("-i" "Insert this branch between the current branch and its child"("-i" "--insert"))]
  ["Actions"
   ("c" magit-gt-create)
   ("m" magit-gt-modify)])

(magit-gt-args "core-local")

;;;###autoload (autoload 'magit-gt-modify "magit-gt" nil t)
(transient-define-suffix magit-gt-modify (args)
  "Modify the current branch by amending its commit or creating a new commit. Automatically restacks descendants.
If you have any unstaged changes, you will be asked whether you'd like to stage them."
  :class 'magit-gt--suffix
  :description "Update         gt modify [--no-verify] [--commit] [--all]
                        [--update] [--edit]"
  (interactive
   (list
    (magit-gt-core-local-arguments "--no-verify" "--commit" "--all" "--edit")))
  (magit-gt--modify args))

;;;###autoload (autoload 'magit-gt-create "magit-gt" nil t)
(transient-define-suffix magit-gt-create (args)
  "Create a new branch stacked on top of the current branch and commit staged changes."
  :class 'magit-gt--suffix
  :description "Create         gt create [--no-verify] [--all] [--update]
                        [--insert]"
  (interactive
   (list
    (magit-gt-core-local-arguments "--no-verify" "--all" "--update" "--insert")))
  (magit-gt--create args))

;;;autoload (autoload 'magit-gt "magit-gt-core-remote" nil t)
(transient-define-prefix magit-gt-core-remote ()
  "Invoke Graphite's core remote workflows"
  ["Arguments"
   ("-N" "Disable hooks" "--no-verify")
   ("-a" "Sync branches across all configured trunks" ("-a" "--all"))
   ("-d" "Create all new PRs in draft mode" ("-d" "--draft"))
   ("-p" "Publish all PRs being submitted" ("-p" "--publish"))
   ("-u" "Only push branches and update PRs for branches that already have PRs open" ("-u" "--update-only"))
   ("-m" "Mark all PRs being submitted as merge when ready" ("-m" "--merge-when-ready"))
   ("-r" "Rerequest reviews on all PRs being submitted" ("-r" "--rerequest-reviews"))
   ("-v" "Open the PRs in the browser after submitting" ("-v" "--view"))
   ("-s" "Submit descendants of the current branch in addition to its ancestors" ("-s" "--stack"))
   ("-f" "Don't prompt for confirmation before overwriting or deleting a branch" ("-f" "--force"))
   ("-n" "Don't restack any branches" "--no-restack")]
  ["Actions"
   ("s" magit-gt-submit)
   ("f" magit-gt-sync)])

(magit-gt-args "core-remote")

;;;autoload (autoload 'magit-gt-submit "magit-gt" nil t)
(transient-define-suffix magit-gt-submit (args)
  "Idempotently force push all branches from trunk to the current branch to GitHub, creating or updating distinct
pull requests for each. Validates that branches are properly restacked before submitting, and fails if there
are conflicts."
  :class 'magit-gt--suffix
  :description "Submit         gt submit [--no-verify] [--draft|--publish]
                        [--update-only] [--merge-when-ready] [--rerequest-reviews]
                        [--view] [--stack]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify" "--draft" "--publish" "--update-only"
                                    "--merge-when-ready" "--rerequest-reviews" "--view"
                                    "--stack")))
  (magit-gt--submit args))


;;;###autoload (autoload 'magit-gt-sync "magit-gt" nil t)
(transient-define-suffix magit-gt-sync (args)
  "Sync all branches with remote"
  :class 'magit-gt--suffix
  :description "Sync           gt sync [--no-verify] [--no-restack]
                        [--force] [--all]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify" "--no-restack" "--force" "--all")))
  (magit-gt-run-gt-async "sync" args))

;;;autoload (autoload 'magit-gt "magit-gt-navigation" nil t)
(transient-define-prefix magit-gt-navigation ()
  "Invoke Graphite's navigation workflows"
  ["Arguments"
   ("-N" "Disable hooks" "--no-verify")
   ;;TODO -n number of steps
   ]

  ["Actions"
   ("u" magit-gt-up)
   ("d" magit-gt-down)
   ("c" magit-gt-checkout)
   ("t" magit-gt-top)
   ("b" magit-gt-bottom)])


;;;###autoload (autoload 'magit-gt-up "magit-gt" nil t)
(transient-define-suffix magit-gt-up (args)
  "Switch to the child of the current branch"
  :class 'magit-gt--suffix
  :description "Up             gt up [--no-verify]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify")))
  (magit-gt-run-gt "up" args))

;;;###autoload (autoload 'magit-gt-down "magit-gt" nil t)
(transient-define-suffix magit-gt-down (args)
  "Switch to the parent of the current branch"
  :class 'magit-gt--suffix
  :description "Down           gt down [--no-verify]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify")))
  (magit-gt-run-gt "down" args))

;;;autoload (autoload 'magit-gt-checkout "magit-gt" nil t)
(transient-define-suffix magit-gt-checkout (branch)
  "Switch to the specified branch"
  :class 'magit-gt--suffix
  :description "Checkout       gt checkout"
  (interactive
   (list
    ;; TODO -t -s -a
    (magit-completing-read "Checkout tracked branch" (magit-gt--get-branches) nil t)))
  (magit--checkout branch)
  (magit-refresh))

;;;###autoload (autoload 'magit-gt-top "magit-gt" nil t)
(transient-define-suffix magit-gt-top (args)
  "Switch to the top of the current branch's stack"
  :class 'magit-gt--suffix
  :description "Top            gt top [--no-verify]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify")))
  (magit-gt-run-gt "top" args))

;;;###autoload (autoload 'magit-gt-top "magit-gt" nil t)
(transient-define-suffix magit-gt-bottom (args)
  "Switch to the bottom of the current branch's stack"
  :class 'magit-gt--suffix
  :description "Bottom         gt bottom [--no-verify]"
  (interactive
   (list
    (magit-gt-core-remote-arguments "--no-verify")))
  (magit-gt-run-gt "bottom" args))

;;;autoload (autoload 'magit-gt "magit-gt-stack-management" nil t)
(transient-define-prefix magit-gt-stack-management ()
  "Invoke Graphite's stack management workflows"
  ["Arguments"
   ("-N" "Disable hooks" "--no-verify")
   ("-f" "Apply the hunks to the commits immediately" ("-f" "--force"))
   ("-a" "Stage all unstaged changes before continuing" ("-a" "--all"))
   ("-k" "Keeps the name of the current branch instead of using the name of its parent" ("-k" "--keep"))
   ("-d" "Only restack this branch and its ancestors" "--downstack")
   ("-u" "Only restack this branch and its descendants" "--upstack")
   ("-o" "Only restack this branch" "--only")
   ]
  ["Actions"
   ("a" magit-gt-absorb)
   ("c" magit-gt-continue)
   ("f" magit-gt-fold)
   ;; TODO gt move
   ("r" magit-gt-reorder)
   ("R" magit-gt-restack)])

(magit-gt-args "stack-management")

;;;###autoload (autoload 'magit-gt-absorb "magit-gt" nil t)
(transient-define-suffix magit-gt-absorb (args)
  "Amend staged changes to the relevant commits in the current stack."
  :class 'magit-gt--suffix
  :description "Absorb         gt absorb [--no-verify] [--force] [--all]"
  (interactive
   (list
    (magit-gt-stack-management-arguments "--no-verify" "--force" "--all")))
  (magit-gt-run-gt "absorb" args))

;;;###autoload (autoload 'magit-gt-continue "magit-gt" nil t)
(transient-define-suffix magit-gt-continue (args)
  "Continues the most recent Graphite command halted by a rebase conflict."
  :class 'magit-gt--suffix
  :description "Continue       gt continue [--no-verify] [--all]"
  (interactive
   (list
    (magit-gt-stack-management-arguments "--no-verify" "--all")))
  (magit-gt-run-gt "continue" args))

;;;###autoload (autoload 'magit-gt-fold "magit-gt" nil t)
(transient-define-suffix magit-gt-fold (args)
  "Fold a branch's changes into its parent, update dependencies of descendants of the new combined branch, and
restack."
  :class 'magit-gt--suffix
  :description "Fold           gt fold [--no-verify] [--keep]"
  (interactive
   (list
    (magit-gt-stack-management-arguments "--no-verify" "--keep")))
  (magit-gt-run-gt "fold" args))

;;;###autoload (autoload 'magit-gt-reorder "magit-gt" nil t)
(transient-define-suffix magit-gt-reorder (args)
  "Reorder branches between trunk and the current branch, restacking all of their descendants."
  :class 'magit-gt--suffix
  :description "Reorder        gt reorder [--no-verify]"
  (interactive
   (list
    (magit-gt-stack-management-arguments "--no-verify")))
  (magit-gt-run-gt-with-editor "reorder" args))

;;;###autoload (autoload 'magit-gt-restack "magit-gt" nil t)
(transient-define-suffix magit-gt-restack (args)
  "Ensure each branch in the current stack has its parent in its Git commit history, rebasing if necessary."
  :class 'magit-gt--suffix
  :description "Restack        gt restack [--no-verify] [--downstack]
                        [--upstack] [--only]"
  (interactive
   (list
    (magit-gt-stack-management-arguments "--no-verify" "--downstack" "--upstack" "--only")))
  ;; TODO? interactive rebase a la magit
  (magit-gt-run-gt "restack" args))

;;;autoload (autoload 'magit-gt "magit-gt-branch-management" nil t)
(transient-define-prefix magit-gt-branch-management ()
  "Invoke Graphite's branch management workflows"
  ["Arguments"
   ("-N" "Disable hooks" "--no-verify")
   ("-f" "Get: Overwrite all fetched branches with remote source of truth
    Delete: Delete the branch even if it is not merged or closed." ("-f" "--force"))
   ("-a" "Stage all unstaged changes before absorbing. This will not include untracked files" ("-a" "--all"))
   ("-d" "When syncing a branch that already exists locally, don't sync upstack branches" ("-d" "--downstack"))
   ("-n" "Don't restack any branches in the stack" "--no-restack")
   ("-n" "Don't modify the existing commit messsage" ("-n" "--no-edit"))
   ]
  ["Actions"
   ("a" magit-gt-abort)
   ("d" magit-gt-delete)
   ("g" magit-gt-get)
   ("p" magit-gt-pop)
   ("r" magit-gt-rename)
   ;; TODO gt split
   ("s" magit-gt-squash)
   ("t" magit-gt-track)
   ("U" magit-gt-undo)
   ;; TODO? gt unlink
   ("u" magit-gt-untrack)])

(magit-gt-args "branch-management")

;;;###autoload (autoload 'magit-gt-abort "magit-gt" nil t)
(transient-define-suffix magit-gt-abort (args)
  "Abort the current Graphite command."
  :class 'magit-gt--suffix
  :description "Abort          gt abort [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")))
  (magit-gt-run-gt "abort" (append '("-f") args)))

;;;###autoload (autoload 'magit-gt-delete "magit-gt" nil t)
(transient-define-suffix magit-gt-delete (args branch)
  "Delete the current branch, even if it is not merged or closed."
  :class 'magit-gt--suffix
  :description "Delete         gt delete [--no-verify] [--force]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify" "--force")
    (magit-completing-read "Delete branch" (magit-gt--get-branches) nil t)))
  (magit-gt-run-gt-async "delete" (append args (list branch))))

;;;###autoload (autoload 'magit-gt-get "magit-gt" nil t)
(transient-define-suffix magit-gt-get (args branch)
  "Get the latest changes from the remote source of truth for the current branch."
  :class 'magit-gt--suffix
  :description "Get            gt get [--no-verify] [--force] [--downstack]
                        [--no-restack]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify" "--force" "--downstack" "--no-restack")
    (magit-completing-read "Get branch" (magit-gt--get-branches) nil t nil nil (magit-get-current-branch))))
  (magit-gt-run-gt "get" args))

;;;###autoload (autoload 'magit-gt-pop "magit-gt" nil t)
(transient-define-suffix magit-gt-pop (args)
  "Delete the current branch but retain the state of files in the working tree."
  :class 'magit-gt--suffix
  :description "Pop            gt pop [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")))
  (magit-gt-run-gt "pop" args))

;;;###autoload (autoload 'magit-gt-rename "magit-gt" nil t)
(transient-define-suffix magit-gt-rename (args new-branch)
  "Rename the current branch to NEW-BRANCH."
  :class 'magit-gt--suffix
  :description "Rename         gt rename [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")
    (magit-completing-read "Rename branch" (magit-gt--get-branches) nil t)))
  (magit-gt-run-gt "rename" (append args (list new-branch))))

;;;###autoload (autoload 'magit-gt-squash "magit-gt" nil t)
(transient-define-suffix magit-gt-squash (args)
  "Squash all commits in the current branch into a single commit and restack upstack branches."
  :class 'magit-gt--suffix
  :description "Squash         gt squash [--no-verify] [--no-edit]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify" "--no-edit")))
  (cond ((member "--no-edit" args)
         (magit-gt-run-gt "squash" args))
        (t (magit-gt-run-gt-with-editor "squash" (append '("-m") args)))))

;;;###autoload (autoload 'magit-gt-track "magit-gt" nil t)
(transient-define-suffix magit-gt-track (args branch)
  "Track the current branch on the remote source of truth."
  :class 'magit-gt--suffix
  :description "Track          gt track [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")
    (magit-completing-read "Track branch" (magit-gt--get-branches) nil t nil nil (magit-get-current-branch))))
  ;;TODO --parent
  (magit-gt-run-gt "track" (append '("-f") args (list branch))))


;;;###autoload (autoload 'magit-gt-undo "magit-gt" nil t)
(transient-define-suffix magit-gt-undo (args)
  "Undo the most recent Graphite command."
  :class 'magit-gt--suffix
  :description "Undo           gt undo [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")))
  (magit-gt-run-gt "undo" (append '("-f") args)))

;;;###autoload (autoload 'magit-gt-untrack "magit-gt" nil t)
(transient-define-suffix magit-gt-untrack (args branch)
  "Stop tracking the current branch on the remote source of truth."
  :class 'magit-gt--suffix
  :description "Untrack        gt untrack [--no-verify]"
  (interactive
   (list
    (magit-gt-branch-management-arguments "--no-verify")
    (magit-completing-read "Untrack branch" (magit-gt--get-branches) nil t)))
  (magit-gt-run-gt-async "untrack" (append '("-f") args (list branch))))

;;;autoload (autoload 'magit-gt "magit-gt-web" nil t)
(transient-define-prefix magit-gt-web ()
  "Invoke Graphite's web workflows"
  ["Arguments"
   ("-s" "Open the stack page" ("-s" "--stack"))]
  ["Actions"
   ("d" magit-gt-dash)
   ("m" magit-gt-merge)
   ("p" magit-gt-pr)])

(magit-gt-args "web")

;;;###autoload (autoload 'magit-gt-dash "magit-gt" nil t)
(transient-define-suffix magit-gt-dash (args)
  "Open the Graphite dashboard in the browser."
  :class 'magit-gt--suffix
  :description "Dash           gt dash"
  (interactive
   (list
    (magit-gt-web-arguments)))
  (magit-gt-run-gt-async "dash" args))

;;;###autoload (autoload 'magit-gt-merge "magit-gt" nil t)
(transient-define-suffix magit-gt-merge (args)
  "Merge the pull requests associated with all branches from trunk to the current branch via Graphite."
  :class 'magit-gt--suffix
  :description "Merge          gt merge"
  (interactive
   (list
    (magit-gt-web-arguments)))
  (magit-gt-run-gt-async "merge" args))

;;;###autoload (autoload 'magit-gt-pr "magit-gt" nil t)
(transient-define-suffix magit-gt-pr (args branch)
  "Open the Graphite pull request page in the browser."
  :class 'magit-gt--suffix
  :description "PR             gt pr [--stack]"
  (interactive
   (list
    (magit-gt-web-arguments "--stack")
    (magit-completing-read "Pull request branch" (magit-gt--get-branches) nil t nil nil (magit-get-current-branch))))
  (magit-gt-run-gt-async "pr" (append args (list branch))))

;;;###autoload (autoload 'magit-gt "magit-gt" nil t)
(transient-define-prefix magit-gt ()
  "Invoke Graphite workflows"
  ["Workflows"
   ("l" "Core local workflows" magit-gt-core-local)
   ("r" "Core remote workflows" magit-gt-core-remote)
   ("n" "Navigation" magit-gt-navigation)
   ("s" "Stack management" magit-gt-stack-management)
   ("b" "Branch management" magit-gt-branch-management)
   ("w" "Web views" magit-gt-web)]
  ["Shortcuts"
   ("m" "Modify and submit" magit-gt-modify-and-submit)
   ("c" "Create and submit" magit-gt-create-and-submit)
   ])

;;;###autoload (autoload 'magit-gt "magit-gt-modify-and-submit" nil t)
(defun magit-gt-modify-and-submit ()
  "Modify the current branch and submit it."
  (interactive)
  (magit-gt--modify)
  (magit-gt--submit-sentinel))

;;;###autoload (autoload 'magit-gt "magit-gt-create-and-submit" nil t)
(defun magit-gt-create-and-submit ()
  "Create a new branch and submit it."
  (interactive)
  (magit-gt--create)
  (magit-gt--submit-sentinel))

(provide 'magit-gt)
;;; magit-gt.el ends here
