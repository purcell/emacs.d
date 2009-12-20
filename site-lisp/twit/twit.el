;;; Twit.el --- interface with twitter.com
(defvar twit-version-number "0.3.9")
;; Copyright (c) 2007 Theron Tlax
;;           (c) 2008-2009 Jonathan Arkell
;; Time-stamp: <2007-03-19 18:33:17 thorne>
;; Author: thorne <thorne@timbral.net>, jonnay <jonnay@jonnay.net>
;; Created: 2007.3.16
;; Keywords: comm
;; Favorite Poet: E. E. Cummings

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;;
;; This is the beginnings of a library for interfacing with
;; twitter.com from Emacs.  It is also (more importantly) some
;; interactive functions that use that library.  It's a hack, of
;; course; RMS i am not.  Maybe one of you real programmers would
;; like to clean it up?
;;    -- Theron Tlax
;;
;; Okay! ;)
;;    -- JonathanArkell
;;
;; Twit.el is rapidly maturing library.  Currently it handles your
;; friends timeline, direct messagse (sending and receiving) and
;; even searches.
;;
;; You can use the function `twit-post' to send a tweet.  Heck
;; I'd appreciate knowing who uses twit.el so go ahead and tweet
;; something to that effect!
;;
;; You can use the function `twit-follow-recent-tweets' to have
;; twit.el constantly check updates, and write them in a buffer.
;; alternatly, you can just use `twit-show-recent-tweets'
;; to just show you the last 15 tweets.  In this timeline view
;; type ? to show you a list of commands you can use.
;;
;; For searches, you an use `twit-search' to run a general search.
;; The search will remember the terms you used, so you can use
;; tab completion when promped.  Also, you can customize the
;; variable `twit-completing-searches' to store your common
;; searches for later.  The command `twit-search-at-to-me' to do
;; a @<username> search.  The search functions were just recently
;; added, and their output will be improved soon.
;;
;; For direct messages you can use `twit-show-direct-tweets' to
;; get your direct messages, and `twit-direct' to send them.
;;
;; If you want, you can display images on the timeline as well,
;; customize the variable `twit-show-user-images', and set it to
;; true.  When you do that, you'll also get tab completion on
;; the twit direct function.
;;
;; Both `twit-show-direct-tweets' and `twit-show-recent-tweets'
;; can accept numeric prefix commands.  With a numeric prefix,
;; they will skip to that page.  So:
;;      C-u 2 M-x twit-show-recent-tweets
;; will take you to the next page of recent tweets (older ones)
;;
;; This uses Twitter's XML-based api, not the JSON one because i
;; would like to avoid making the user install third-party libraries
;; to use it.
;;
;; Even though twit.el uses Basic Authentication, it also uses HTTPS
;; to connect to twitter (for all auth based services anyway).  So
;; security problems are dealt with.
;;

;;; Notes:
;; `twit-user' gets my vote for variable name of the year.  Ditto
;; `twit-minor-mode' for mode names.
;; `twit.el' is pronounced twit-el.  When said fast, it should
;;   sound like "tiddle"

;;; Hacking:
;; Feel free to hack on this if you like, and post it back to
;; the emacswiki.  Just be sure to increment the version number
;; and write a change to the change log.
;;
;; Mutli-user support is still very experimental right now.
;; use the macro `with-twit-auth' to temporarily switch auth.
;; This needs better support, coming soon.
;;
;; From versions 0.1.0 onwards, versions are incremented like so:
;; <major>.<minor>.<bugfix/feature>
;;
;; Major versions are only incremented when a release is considered
;; truely stable (i.e. no memory leaks) and doesn't have any bugs.
;;
;; Minor version increments happen when there are significant changes
;; to the file (like changing twit-post-function to twit-post-status)
;;
;; bugfix/feature releases are incremented when new features are added
;; or bugs are fixed, that have little impact.

;;; Testing:
;; Best way to test it in default mode:
;;   emacs --no-site-file --no-init-file
;; In scratch buffer:
;; (load "~/my-elisp/twit.el")
;; (twit-show-recent-tweets)
;; (twit-follow-recent-tweets)

;;; History:
;; Originally by theron tlax <thorne@timbral.net> 2007-3-16
;; * 0.0.1 -- Initial release.  Posting only. (TT)
;; * 0.0.2 -- Near-total rewrite; better documentation; use standard
;;            Emacs xml and url packages; minor mode; a little
;;            abstraction; some stubs for the reading functions. (TT)
;; * 0.0.3 -- Doc and other minor changes. (TT)
;; * 0.0.4 -- (released as 0.0.3 -- Added twit-show-recent-tweets
;;             by Jonathan Arkell)
;; * 0.0.5 -- Add source parameter to posts (TT)
;; * 0.0.6 -- Re-working twit-show-recent-tweets to show more info
;;            (and to get it working for me) -- by H Durer
;; * 0.0.7 -- Keymaps in the buffers for twit-show-recent-tweets and
;;            twit-list-followers; encode the post argument so that it
;;            is a valid post request (TT)
;; * 0.0.8 -- faces/overlays to make the *Twit-recent* buffer look
;;            prettier and more readable (at least for me) -- by H Durer
;; * 0.0.9 -- follow-recent-tweets function created so automagickally
;;            follow tweets every 5 mins.  Also removed twit-mode
;;            on twit-show-recent-tweets.  (it was setting twit-mode
;;            globally, and interfering with planner)  (JA)
;; * 0.0.10 - There is a hook that is run when a new tweet is seen.
;;            This can be used to interface with the todochiku package
;;            to send a notification that there is a new tweet.
;;            (or anything else for that matter)
;;            Twit-user and Twit-pass are now customizeable variables
;;            that work.  Finally, no more constant re-entry of your
;;            username and password. (JA)
;; * 0.0.11 - Updated to set a customization for the
;;            follow-recent-tweets idle timer.  This is so that you
;;            wont get throttled when twitter changes their throttle
;;            time (JA)
;; * 0.0.12 - Changed syncronous url call to an ascynronous one that
;;            doesn't suck, and polls properly.
;;            You can finally stop following recent tweets. (Rev 22) (JA)
;; * 0.0.13 - Fixed twit-debug to be a customizeable variable.  Duh.
;;            Image handling is on the way. (done, just buggy)
;;            Better face definitions, now customizeable.
;;            Zebra-tabling of the recent tweets is half there.
;;            Retrieval of the rate-limiting is working. (Rev 23) (JA)
;; * 0.0.14 - Finished zebra-table for recent tweets. (uses overlays)
;;            Fix for a really crazy bug in carbon emacs. (thanks gr3p3)
;;            Tweaked default fonts to not suck, added more faces.
;;            If showing recent tweets fails, the buffer is no longer
;;            set to blank. (JA)
;; * 0.0.15 - Fixed the automatic rate-limiting on 400 messages from
;;            twitter to work.
;;            Updated rate limiting to use the new format
;;            More messages are handed off to todochiku
;;            Most rate limiting (except the initial follow) is done
;;            Asyncronously.
;;            Verified that twit-list-followers works.
;;            URLs now are fontified and hot.
;;            Maybe (maybe?) fixed the bug where following tweets
;;            kills the mark, and dumps the point at the bottom. (JA)
;; * 0.0.16 - Fixed most compilation warnings. (PeterJones)
;; * 0.0.17 - Fixed a bug where a users username/password pair is
;;            not properly updated through customization.  It's not
;;            100%, but it should be much better now. (JA)
;;            twit-show-recent-tweets doesn't change focus. (thanks Ben Atkin)
;; * 0.0.18 - Fixed a bug where xml entities were not converted while
;;            tweet messages (JonathanCreekmore)
;; * 0.0.19 - Fixed a bug where the previous tweets in the *Twit-recent* buffer
;;            were saved on the undo list every time new tweets came in. (JonathanCreekmore)
;; * 0.0.20 - Added support for "Reply to" in the twit-post function. (JonathanCreekmore)
;; * 0.0.21 - The rate-limit timer is cancelled as well as the main
;;            timer.
;;            Regexp filtering of tweets.  (JonathanArkell)
;; * 0.0.22 - Fixed infinite loop problem if you cannot retrieve the rate-limit.
;;            Could be the cause of the memory leak.
;;          - Added elisp function to return following users as list.
;;          - fixed regex problem (JA)
;; * 0.0.23 - made sure that url-retrive is wrapped around a let to shadow
;;            url-request-method with GET.
;;          - Optional filtering of @ message to people you don't know.
;;          - Added Images!  Whute!  Make sure you check customization options. (JA)
;; * 0.0.24 - Updated image handling to only download and install when images are
;;            turned on.
;;          - Enabled confirmation of posting tweets
;;          - Enabled direct messages. (JA)
;; * 0.0.25 - Updated image handling again so that the local copy of image are used
;;            when available.
;;          - Now you can view direct tweets!  Whute! (JA)
;; * 0.0.26 - You can hit "d" in a list of tweets to direct-message that user.
;;          - Documentation improved, especially interactive and customs
;;          - Documentation merged.
;;          - source code is made more tagging friendly (for free-tagging.el)
;;          - zebra table face definition sucks less.  Hopefully this will
;;            look better for those of you on dark-background screens.
;;          - older tweets and direct messages are viewable with prefix
;;            arguments.  (JA)
;; * 0.1.0  - Finally considered ready for 0.1 primetime! ;)
;;          - Super improved error handling when you try to show recent
;;            tweets.  This should help with peoples install problems.
;;          - HTTPS instead of HTTP.   Let me know if there are any
;;            problems with this, and if I need to have the protocol a
;;            settable option.
;;          - Searches started.  Right now its just the bare minimum of
;;            features.  More and better searches to come.
;;          - Properly Makred my own, and Therons changes in the changelog.
;;          - Added macro for displaying twitter buffers
;;          - commentary, installing, and other documentation improved. (JA)
;; * 0.1.1  - Fixed image bug with filename collisions. (thanks @learnemacs)
;;          - Fixed bug with twit-follow-recent-tweets (thanks @busytoby) (JA)
;; * 0.2.0  - Added the ability to follow people.
;;          - changed underlying code.  twit-post-function is now a general
;;            function for POSTing api messages to twitter.  twit-post-status
;;            is the specific function to post a status message.
;;          - Display of favorite tweets
;;          - Raw posting of favorite tweets
;;          - Post favorite tweets with *, remove favorites with -
;;          - infrastructure to handle reply-to's a lot better (coming soon)
;;          - hitting 's' when the point is on a #tag or @name will search
;;            that entity.
;;          - @names have basic keymaps, like a)dd p)ost to and d)irect msg
;;          - Regex for determining what is and isn't a hashtag should be
;;            better.
;; - 0.2.1  - Fixed a bug that put the list of tweets on the kill ring.
;;          - removed twit-grab-author-of-tweet, (its superceeded by
;;            (twit-get-text-property 'twit-user)
;;          - added with-twit-auth macro, so that you can use more then
;;            one twitter account.  Better support forthcoming)
;;          - fixed potential auth bug with https
;;          - added function to visit url (current url, users twit url)
;;          - fixed regex for @tweets and #tags.
;;          - tweaked get-friends
;;          - made twit-filter-at-tweets use get-friends cache, so you
;;            don't need images on.
;;          - Added ascii logo to title bar
;;          - fixed a bug in image retrieval that would try to retrieve
;;            the image through a POST method instead of GET. (JA)
;; - 0.2.2  - fixed botched refactoring of twit-at-friends-cache  (JA)
;; - 0.2.3  - Multi-account handling improved, you can switch accounts
;;            with `twit-switch-account'
;;          - Added the page number you're on to the title bar
;;          - fixed paging of `twit-show-direct-tweets' (JA)
;; - 0.3.0  - Renamed with-twitter-/foo/ to with-twit-/foo/ for better
;;            consistancy, and less chances to collide with twitter.el.
;;          - fixed bug caused by refactoring with twit-direct
;;          - updated url regex.
;;          - Added timezone patch from remvee (this patch somehow
;;            got lost somewhere but is FINALLY applied)
;;          - added multi-account functions:
;;             - `twit-direct-with-account'
;;             - `twit-post-with-account'
;;             - `twit-show-direct-tweets-with-account'
;;             - `twit-show-at-tweets-with-account'
;;          - ran through checkdoc (JA)
;; - 0.3.1  - Added proper title bars and zebra tables to friends
;;            and followers listings. (JA)
;; - 0.3.2  - Applied patch to show you post length on minibuffer
;;            (Can't find email to properly credit this patch. :P
;;             credit coming soon)
;;          - Applied ieure's patches:
;;            - better (more emacs consistent) keymaps
;;            - reply-to handling, properly set reply Id.
;;            - quiet down url-retrieval.  (speeds things up)
;;            - friends list has twit-user set.
;;            - renamed twit-mode to twit-minor-mode
;;          - removed extraneous keymaps (JA)
;; - 0.3.3  - Auto-install function added.  When you are over text
;;            that matches the name of an elisp file, you can press
;;            "i" to install that file from emacs wiki, provided you
;;            have auto-install. (JA)
;; - 0.3.4  - Added retweet function, bound to "c" (think "copy"). Point
;;            must be over the text of the tweet being RT'd. As a side
;;            effect, the text of a tweet now carries the property
;;            'twit-message .
;;          - Added twit-post-loud-reply, which automatically uses ".@"
;;            instead of "@". Bound to "R".
;;          - small fixes to tweet navigation
;;          - Added URL compression function and twit-post-url, bound
;;            to "u". Prompts for URL first, then the rest of the text.
;;            Currently uses is.gd. (@laciermaths)
;; - 0.3.5  - Fix keymap.  Searches are now "S" (JA)
;;          - forward and back fixed (whym)
;;          - Tightened up search results a little, added author (JA)
;;          - Better error checking on twit-parse-xml. (JA)
;;          - Fixed twit-id to work with favoriting again. (JA)
;;          - added twanalist urls, and API (JA)
;;          - Added api for friendship graphing functions (JA)
;;          - twit-show-followers now accepts numeric prefix arg (JA)
;;          - Changed name of twit-list-followers to twit-show-followers
;;            twit-list-followers is an alias, but its use is deprecated.(JA)
;;          - made sure twit.el will survive the twitpocalypse. (JA)
;;          - Started work on buffer based posting. (unfinished) (JA)
;;          - direct messages now use character counting function. (JA)
;;          - twitter diarrhea filtering function. (JA)
;;          - started work on i18n support.  (unfinished) (JA)
;; - 0.3.6  - Added twit-open-link, and bound it to "o". (BC)
;; - 0.3.7  - Applied patch from http://dme.org/emacs/twit.el.diff
;;            and small fixes by peccu.
;;          - Added '"' to 'white' of twit-favorite-face.
;;          - Changed format twit-write-tweet.(peccu)
;; - 0.3.8  - Exchange TAB mainly for indent to whitespaces.
;;          - remove spaces end of line.
;;          - fix some comments.
;;          - Add `twit-show-favorites-tweets' for favorites.(peccu)
;; - 0.3.9  - Add feature for `twit-visit-link'
;;            timestamp of tweet, goto that tweet page.
;;            in reply to USER of tweet, goto that reply tweet page.
;;            source of tweet, goto sources page.(peccu)

;;; TODO:
;; - remember style buffer posting.

;;; Bugs:
;; * too much Unilinguality.  Multi-lingual messages would be ideal.
;; * Arg.  the mark gets hosed by twit.el.  This needs to be fixed soon.
;;   (found by wilane)
;; * notifications are bunk and need to be re-jiggered, and fit in with
;;   at searches and direct messagse
;; * refresh is broken on some pages, and needs to be handled better.
;; * titlebar shows up at the bottom once in awhile.

;; Please report bugs to the twit emacs wiki page at:
;;   http://www.emacswiki.org/cgi-bin/wiki/TwIt

;;; Roadmap (todo):
;; v1.0 release
;; - optionally authenticate via open-auth instead of http auth.
;; - fix the mark getting hosed.
;; - make follow smarter, and give the latest tweet id as an arg
;; - make follow general, so direct messages and at messages are
;;   updated.
;; - finish exposing the rest of the twitter api (public timeline, etc.)
;; Post 1.0
;; - make the user images float right.  (thanks busytoby)
;; - Integrate the other twit.el that is out there.  Looks like it might
;;   have some serious sexxxy to it.

;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl))

;;* custom helper auth
(defun twit-set-auth (user pass)
   "Set the http url authentication string from USER and PASS."
   (let ((old-http-storage
          (assoc "twitter.com:80" (symbol-value url-basic-auth-storage)))
         (old-https-storage
          (assoc "twitter.com:443" (symbol-value url-basic-auth-storage)))
         (auth-pair
          (cons "Twitter API"
                (base64-encode-string (format "%s:%s" user pass)))))
     (when old-http-storage
       (set url-basic-auth-storage
            (delete old-http-storage (symbol-value url-basic-auth-storage))))
     (when old-https-storage
       (set url-basic-auth-storage
            (delete old-https-storage (symbol-value url-basic-auth-storage))))
     (set url-basic-auth-storage
          (cons (list "twitter.com:443" auth-pair)
                (cons (list "twitter.com:80" auth-pair)
                      (symbol-value url-basic-auth-storage))))))

;;* custom helper auth
(defun twit-set-user-pass (sym val)
  "Set the username/password pair after a customization.

Called with SYM and VAL by customize.  SYM is generally not used.

Note that this function uses a really cheap hack.
Basically the problem is that we need to run this whenever the `twit-user'
and `twit-pass' variables are customized and loaded.  The problem is, this
funciton is executed by cutomzie on Emacs initialization, during the
setting of `twit-user', but before the binding to `twit-pass', throwing an
error.

We get around this by using `condition-case' and handling the void-varible
error."
  (set-default sym val)
  (condition-case nil
      (let ((twit-pass-var 'twit-pass))
        (when (and (not (string= (symbol-value twit-pass-var) ""))
                   (not (string= twit-user "")))
          (twit-set-auth twit-user (symbol-value twit-pass-var))))
    (void-variable nil)))

;; Use this when testing the basic storage engine.
;; Set up for easy space for your C-x C-e execution pleasure.
(when nil
      (set url-basic-auth-storage nil)
      )

;;* custom
(defgroup twit nil
  "twit.el is an emacs package for interfacing with Twitter (http://www.twitter.com),
a microblogging service. The twit.el package provides you with the ability to post
status updates and driect messages, as well as recieve them.  Additionally you can
opt to \"follow\", and the timeline will be updated automagickally.

twit.el also makes use of the Todochiku package, which you can install from here:
http://www.emacswiki.org/emacs/todochiku.el"
  :version twit-version-number
  :group 'twit)

(defcustom twit-user
  ""
  "Your twitter username.
If this is set, attempt to automagickally log into twitter."
  :group 'twit
  :type 'string
  :set 'twit-set-user-pass)

(defcustom twit-pass
  ""
  "Your twitter password.
If this is set, attempt to automagickally log into twitter."
  :group 'twit
  :type 'string
  :set 'twit-set-user-pass)

(defcustom twit-new-tweet-hook
  '()
  "Functions to execute when there is a new tweet.
If you have Todochiku, add \"twit-todochiku\" here, and you will be
notified when a new tweet appears."
  :type 'hook
  :group 'twit)

(defcustom twit-follow-idle-interval
  90
  "How long in time to wait before checking for new tweets.
Right now it will check every 90 seconds, Which will generate a maximum
of 40 requests, leaving you another 30 per hour to play with.

The variable name is a bit of a misnomer, because it is not actually
based on idle time (anymore)."
  :type 'integer
  :group 'twit)

(defvar twit-shadow-follow-idle-interval
  twit-follow-idle-interval
  "Shadow definition of `twit-follow-idle-interval' that we can modify
on the fly.")

(defcustom twit-protocol "http"
  "Which protocol to use for twitter.

If you use http, the requests will be much faster, and there will be
a lot less messages at the bottom of the screen.   However, your
password will be sent plaintext.

If you use https, it is noisier, and slower, but (obviously) secure.

Note, if you change this, you will need to restart emacs for it to
take effect."
  :type '(choice (const :tag "http" "http")
                 (const :tag "https" "https"))

  :group 'twit)

(defcustom twit-language 'en
  "Language 語

Language that twit.el is in.  This is an experimental feature, that
I want to soon move into an international language pack feature for
all elisp packages."
  :type '(choice (const :tag "English" 'en)
                 (const :tag "日本語" 'ja))
  :group 'twit)

(defcustom twit-show-user-images nil
   "Show user images beside each users tweet."
   :type 'boolean
   :group 'twit)

(defcustom twit-user-image-dir
  (concat (car image-load-path) "twitter")
  "Directory where twitter user images are to be stored.

This directory need not be created."
  :type 'string
  :group 'twit)

(defcustom twit-posting-function 'twit-query-for-post
  "Function to get posting input from the user.

This applies to status posts, replies, and direct messages.

NOTE: this is not ready for primetime!  Please leave at `twit-query-for-post'
"
  :type 'function
  :group 'twit)

(defcustom twit-debug
  nil
  "Whether or not to run twit.el in debug mode."
  :group 'twit
  :type 'boolean)

(defcustom twit-debug-mem
  nil
  "Turn on memory debugging."
  :group 'twit
  :type 'boolean)

(defcustom twit-filter-tweets-regex ""
  "Filter all tweets with this regex.

This is useful if you do not want to see a particular style of tweet.
For isntance, if hash-tagging pisses you off, you could set this to \"#\" and
no hash-tagging messages would get to you."
  :type 'regexp
  :group 'twit)

(defcustom twit-filter-at-tweets nil
   "Filter any tweets that have @user of a user you don't know.

If enabled, every tweet will be scanned for any @msgs.  If they contain one,
and the username is not a user that you are following (or you) then it will be
ignored."
   :type 'boolean
   :group 'twit)

(defcustom twit-filter-diarrhea 0
  "After x number of tweets from the same user, ignore tweets by them.

0 means let all tweets through.

Some twitter users have the annoying habit of going on twitter bombs,
where they output 3 or more tweets.  Generally, I find that these
diarrhea sessions are very content free, and worth ignoring.

A good number here is 3."
  :type 'integer
  :group 'twit)

(defcustom twit-completing-searches nil
  "A list of searches you perform over and over again.

This is so that you can use the completion interface for when you want to run a
search."
  :type '(repeat string)
  :group 'twit)

;;* custom multi-account auth
(defcustom twit-multi-accounts 'nil
   "A list of username/password pairs for multi-account usage.

Twit.el can also handle multi-accounts, which is particularly useful for people
 (like me) who have more than one twitter account.

Each item in the list is a pair, with the car being the account name,
and cdr the password.

IF YOU USE THIS FEATURE, BE SURE TO SET YOUR MAIN ACCOUNT IN HERE
AS WELL.  Otherwise your primary login credentials may get wacked."
   :type '(repeat (cons string string))
   :group 'twit)

;;* face
(defface twit-message-face
    '((default
       :family "helv"
       :height 1.1))
  "The font face to use for a twitter message."
  :group 'twit)

(defface twit-author-face
    '((t
       (:height 0.8
        :weight bold
        :family "mono")))
  "The font face to use for the authors name"
  :group 'twit)

(defface twit-info-face
    '((t (:height 0.8 :slant italic)))
  "Face for displaying where, how and when someone tweeted."
  :group 'twit)

(defface twit-title-face
    '((((background light))
       (:background "PowderBlue"
        :underline "DeepSkyBlue"
        :box (:line-width 2 :color "PowderBlue" :style 0)))
      (((background dark))
       (:background "PowderBlue"
        :underline "DeepSkyBlue"
        :box (:line-width 2 :color "PowderBlue" :style 0)))
      (t (:underline "white")))
  "Title Area of the recent tweets buffer."
  :group 'twit)

(defface twit-logo-face
    '((((class color))
       (:family "mono"
        :weight bold
        :height 1.5
        :box (:line-width 2 :color "PowderBlue" :style 0)
        :background "Yellow3"
        :foreground "Yellow1"
        :underline "DeepSkyBlue"))
      (t (:inverse)))
  "(^)o<"
  :group 'twit)

(defface twit-hash-at-face
    '((((class color) (background light))
       (:foreground "GoldenRod3"))
      (((class color) (background dark))
       (:foreground "GoldenRod"))
      (t (:underline "white")))
  "Face to show @msgs in"
  :group 'twit)

(defface twit-zebra-1-face
    '((((class color) (background light))
       (:foreground "black" :background "gray89"
        :box (:line-width 2 :color "gray89" :style 0)))
      (((class color) (background dark))
       (:foreground "white" :background "black"
        :box (:line-width 2 :color "black" :style 0)))
      (t (:inverse)))
  "Color one of zebra-striping of recent tweets and followers list."
  :group 'twit)

(defface twit-zebra-2-face
    '((((class color) (background light))
       (:foreground "black" :background "AliceBlue"
        :box (:line-width 2 :color "AliceBlue" :style 0)))
      (((class color) (background dark))
       (:foreground "white" :background "grey4"
        :box (:line-width 2 :color "grey4" :style 0))))
  "Color two of zebra-striping of recent tweets and followers list."
  :group 'twit)

(defface twit-error-face
    '((((class color))
       (:family "mono"
        :background "FireBrick" :foreground "Black"))
      (t (:inverse)))
  "Color of twit.el errors."
  :group 'twit)

(defface twit-too-long-face
    '((((supports :strike-through t)) :strike-through t )
      (t :inherit 'font-lock-warning-face))

  "Face for highlighting a twit that's too long to post"
  :group 'twit)

(defface twit-url-face
    '((default
       :weight bold))
  "Face for showing hyperlinks"
  :group 'twit)

(defface twit-favorite-face
    '((((class color))
       (:family "mono"
        :weight bold
        :height 2.0
        :foreground "gold1"))
      (t (:underline "white")))
  "Face for displaying the favorite"
  :group 'twit)

(defface twit-fail-whale-face
    '((((class color))
       (:family "mono"
        :weight bold
        :height 4.0
        :box (:line-width 10 :color "SteelBlue3" :style 0)
        :background "SteelBlue3" :foreground "SteelBlue4"))
      (t (:inverse)))
  "(_x___}<"
  :group 'twit)

;;* var keymap
(defvar twit-status-mode-map (make-sparse-keymap)
  "Keymap for status messages and direct messages.")

(defvar twit-followers-mode-map (make-sparse-keymap)
  "Keymap for showing followers and friends.")

;;* var keymap
(defvar twit-key-list
  '(("s" . twit-show-recent-tweets)
    ("f" . twit-show-followers)
    ("@" . twit-show-at-tweets)

    ("w" . twit-post)
    ("t" . twit-post-to)
    ("r" . twit-post-reply)
    ("u" . twit-post-url)
    ("R" . twit-post-loud-reply)
    ("c" . twit-post-retweet)
    ("d" . twit-direct)

    ("n" . twit-next-tweet)
    ("p" . twit-previous-tweet)

    ("*" . twit-add-favorite)
    ("-" . twit-remove-favorite)

    ("a" . twit-add-friend)
    ("k" . twit-remove-friend)

    ("S" . twit-search)

    ("v" . twit-visit-link)
    ("o" . twit-open-link)
    ("A" . twit-analyse-user)
    ("G" . twit-analyse-graph-user)
    ("i" . twit-install-elisp)

    ("q" . bury-buffer)

    ("h" . twit-mode-help)
    ("?" . twit-mode-help)))

(define-key twit-status-mode-map "g" 'twit-show-recent-tweets)
(define-key twit-followers-mode-map "g" 'twit-show-followers)

;;* var keymap
(dolist (info twit-key-list)
  (define-key twit-status-mode-map (car info) (cdr info))
  (define-key twit-followers-mode-map (car info) (cdr info)))

;;* interactive keymap
(defun twit-mode-help ()
  "Show help messages during command `twit-mode'."
  (interactive)
  (message "Help: %s" (append twit-key-list '(("g" . "Reload Current Page")))))

;; *var
(defvar twit-timer
  nil
  "Timer object that handles polling the followers.")

(defvar twit-rate-limit-timer
  nil
  "Timer object to poll the rate-limiting.")

(defvar twit-first-time-through nil)

(defvar twit-window nil
  "Window object to get window-width.")

;;* const url
(defconst twit-base-search-url "http://search.twitter.com")
(defconst twit-base-url (concat twit-protocol "://twitter.com"))
(defconst twit-secure-base-url (concat twit-protocol "://twitter.com"))
;; statuses
(defconst twit-update-url
  (concat twit-base-url "/statuses/update.xml"))
(defconst twit-puplic-timeline-file
  (concat twit-base-url "/statuses/public_timeline.xml?page=%s"))
(defconst twit-friend-timeline-file
  (concat twit-base-url "/statuses/friends_timeline.xml?page=%s"))
(defconst twit-followers-list-url
  (concat twit-base-url "/statuses/followers.xml?page=%s"))
(defconst twit-friend-list-url
  (concat twit-base-url "/statuses/friends.xml"))
(defconst twit-mentions-url
  (concat twit-base-url "/statuses/mentions.xml?page=%s"))
;; rate limit
(defconst twit-rate-limit-file
  (concat twit-base-url "/account/rate_limit_status.xml"))
;; direct messages
(defconst twit-direct-msg-send-url
  (concat twit-base-url "/direct_messages/new.xml"))
(defconst twit-direct-msg-get-url
  (concat twit-base-url "/direct_messages.xml?page=%s"))
;; friends
(defconst twit-add-friend-url
  (concat twit-base-url "/friendships/create/%s.xml"))
(defconst twit-remove-friend-url
  (concat twit-base-url "/friendships/destroy/%s.xml"))
;; favorites
(defconst twit-favorites-url
  (concat twit-base-url "/favorites.xml?page=%s"))
(defconst twit-add-favorite-url
  (concat twit-base-url "/favorites/create/%s.xml"))
(defconst twit-remove-favorite-url
  (concat twit-base-url "/favorites/destroy/%s.xml"))
;; search
(defconst twit-search-url
  (concat twit-base-search-url "/search.atom?q=%s"))

;;* graph const url
(defconst twit-graph-friends-url
  (concat twit-base-url "/friends/ids.xml?user_id=%s"))
(defconst twit-graph-followers-url
  (concat twit-base-url "/followers/ids.xml?user_id=%s"))

;;*const url analyse
(defconst twit-analyse-user-url "http://twanalyst.com/%s"
  "Url for twanalyst.")
(defconst twit-analyse-graph-user-url "http://twanalyst.com/%s/track"
  "Url for twanlyst graph.")
(defconst twit-analyse-suggest-user-url "http://twanalyst.com/%s/suggest"
  "Url for twanlyst suggest")

;;* const
(defconst twit-max-tweet 140 "Maximum length of a tweet.")

;;* const msg language
(defconst twit-post-success-msg
  "Post sent!")
(defconst twit-direct-success-msg
  "Direct Message sent!")

(defconst twit-post-failed-msg "Your posting has failed.")

(defconst twit-too-long-msg
  (format "Post not sent because length exceeds %d characters"
          twit-max-tweet))

(defconst twit-add-friend-success-msg
  "Friend successfully added!")
(defconst twit-add-friend-fail-msg
  "Friend addition failed.")

(defconst twit-remove-friend-success-msg
  "Friend successfully removed!")
(defconst twit-remove-friend-fail-msg
  "Friend removal failed.")

(defconst twit-add-favorite-success-msg
  "Tweet set to favorite")
(defconst twit-add-favorite-fail-msg
  "Setting tweet to favorite failed.")

(defconst twit-remove-favorite-success-msg
  "Tweet removed as favorite.")
(defconst twit-remove-favorite-fail-msg
  "Tweet favorite removal failed.")


;;* const
(defconst twit-standard-rate-limit 100
  "The standard twitter rate limit.")

(defconst twit-rate-limit-offset 5
  "Number of seconds to add to a throttled rate limit for insurance.")

(defconst twit-rate-limit-interval (* 2 60 60)
  "Every 2 Hours check for rate limiting.")

;;* const regex
(defconst twit-filter-at-tweets-retweet-regex "\\bRT[ :]*@"
  "Retweets are tweets that do contain at messages that might be actually interesting.")

(defconst twit-at-regex "@\\([a-zA-Z0-9_]+\\)"
  "Regular expression to parse @messages.")

(defconst twit-hash-at-regex "\\([#@][a-zA-Z0-9_.]+\\)"
  "Regular expression form for matching hashtags (#) and directions (@).")

(defconst twit-url-regex "\\(http://[a-zA-Z0-9.]+\.[a-zA-Z0-9%#;~/.=+&$,?@-]+\\)"
   "Regular expression for urls.")

(defconst twit-emacs-lisp-regex "\\([a-zA-Z0-9-.]+\\)\\.el"
  "Regex for Emacs Lisp files.")

;; regex testing:
;; @_miaux #twit.el #foo @foo @foo-bar


;;* const
(defconst twit-request-headers
  `(("X-Twitter-Client" . "twit.el")
    ("X-Twitter-Client-Version" . ,twit-version-number)
    ("X-Twitter-Client-URL" . "http://www.emacswiki.org/cgi-bin/emacs/twit.el"))
  "Headers sent by every twit.el request.")

;;* const
(defconst twit-time-string "%a %b %e %T %Y"
  "The format of twitter time.")

;;* macro
(defmacro with-twit-buffer (buffer-name &rest forms)
  "Create a twitter buffer with name BUFFER-NAME, and execute FORMS.

The value returned is the current buffer."
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (buffer-disable-undo)
     (toggle-read-only 0)
     (delete-region (point-min) (point-max))
     ,@forms
     (set-buffer-modified-p nil)
     (toggle-read-only 1)
     (use-local-map twit-status-mode-map)
     (goto-char (point-min))
     (current-buffer)))

;;* macro auth
(defmacro with-twit-auth (user pass &rest forms)
  "Set twiter authorization of USER and PASS, and execute FORMS.

This forms the very basic support for multi-user twittering.
See the very end of this file for an example."
  `(let ((,url-basic-auth-storage
          (list (list "twitter.com:80"
                      (cons "Twitter API"
                            (base64-encode-string
                             (format "%s:%s" ,user ,pass))))
                (list "twitter.com:443"
                      (cons "Twitter API"
                            (base64-encode-string
                             (format "%s:%s" ,user ,pass))))))
         (twit-user ,user)
         (twit-pass ,pass)
         (twit-get-friends-cache nil))
     ,@forms))

;;* macro auth multi-account
(defmacro with-twit-account (account &rest forms)
  "Set twitter account to ACCOUNT Execute FORMS.

The account is identified by ACCOUNT in the variable
`twit-multi-accounts'."
  `(let ((multi-account (assoc-string account twit-multi-accounts)))
     (with-twit-auth (car multi-account) (cdr multi-account) ,@forms)))

;;* helper todochiku
(defun twit-alert (msg &optional title)
  "Send some kind of alert MSG to the user, with the title TITLE.

If todochiku is available, use that.  Instead, just message the user."
  (when (null title) (setq title "twit.el"))
  (when (featurep 'todochiku)
    (todochiku-message title msg (todochiku-icon 'social)))
  (message "%s: %s" title msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose library to wrap twitter.com's api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* xmlparse
(defun twit-parse-xml (url method)
  "Retrieve file at URL with METHOD and parse with `xml-parse-fragment'.
Emacs' url package will prompt for authentication info if required.

Note that this parses the entire HTTP request as an xml fragment
and not the response."
  (let ((result nil)
        (url-request-method method)
        (url-show-status nil))
    (save-window-excursion
      (set-buffer (url-retrieve-synchronously url))
      (let ((first-header-line (buffer-substring (goto-char (point-min))
                                                 (search-forward "\n"))))
        (when (twit-header-error-p (twit-parse-header first-header-line))
              (twit-display-error (list first-header-line))
              (error "HTTP error on twit-parse-xml: %s" (twit-get-header-error (twit-parse-header first-header-line)))))
      (goto-char (point-min))
      (setq result (xml-parse-fragment))
      (kill-buffer (current-buffer)))
    result))


;;* xmlparse header
(defun twit-parse-header (header-frag)
  "Parse the HEADER-FRAG, and come back with some status codes.

This returns the HTTP status (for now) as a list of three elements.
 (HTTP/Version code Description)

The header fragment should be the first text node from the parsed
xml.

The header fragment is actually quite important, it will tell us
if we have run into some kind of condition where we cannot
display tweets or other information.  This will ease the fresh-install
pain where uesrs can't see why they have blank timelines."
  "Header format:  (or part we care about)" "HTTP/1.0 <status> <status text>\n"


  (string-match "HTTP/\\([0-9]+\\.[0-9]+\\) \\([1-5][0-9][0-9]\\) \\(.*\\)$"
                header-frag)
  (if (match-string 3 header-frag)
      (list (match-string 1 header-frag)
            (match-string 2 header-frag)
            (match-string 3 header-frag))
      (error "Malformed Header sent to twit-parse-header.   Header: %s"
             header-frag)))

;;* header helper
(defun twit-header-error-p (header)
   "Let us know if the HEADER is an error or not.  Null headers are errors."
   (and (not (null header))
        (<= 400 (string-to-number (cadr header)))))

;;* header display
(defun twit-get-header-error (header)
   "Given a parsed HEADER from `twit-parse-header', return human readable error."
   (if (null header)
       "Null header, probably an error with twit.el."
       (case (string-to-number (cadr header))
         ((200) "Everything is A OK!")
         ((304) "Nothing Changed.")
         ((400) "Bad Request. (probably rate limited)")
         ((401) "Not Authorized.  You need to log in, or your login/password is incorrect.\nIf this is the first time you have tried to use a twitter command\nenter your password and try again.")
         ((403) "You are FORBIDDEN")
         ((404) "Not Found.  404'ed!")
         ((406) "Something is bropken with twit.el's search!")
         ((500) "Something is horribly broken with twit.el, or even Twitter!")
         ((502) "Twitter is down. FAIL WHALE!")
         ((503) "Rate limited on search."))))

;;* header test
(when nil
  (twit-parse-header "HTTP/1.1 401 Unauthorized\nThis is a header\n")
  (twit-header-error-p (twit-parse-header "HTTP/1.1 401 Unauthorized\nThis is a header\n"))
  (twit-get-header-error (twit-parse-header "HTTP/1.1 401 Unauthorized\nThis is a header\n"))
  )

;;* header display
(defun twit-display-error (xml)
  "Given an XML fragment that contain an error, display it to the user."
  (let ((header (twit-parse-header (car xml))))
    (when (twit-header-error-p header)
       (twit-insert-with-overlay-attributes
         "(_x___}<"
         '((face "twit-fail-whale-face")))
       (twit-insert-with-overlay-attributes
          (concat "         HTTP ERROR!  "
                  "(" (cadr header) ") "
                  (caddr header) "\n\n"
                  "  "(twit-get-header-error header) "\n\n"
                  "  The response from twitter was: "
                  (format "%s" (xml-first-childs-value (cadr xml) 'error))
                  "\n\n")
          '((face "twit-error-face"))))))

;;* xmlparse var
(defvar twit-async-buffer 'nil
  "Buffer that stores the temporary XML result for twit.el.")

;;* xmlparse async
(defun twit-parse-xml-async (url callback)
  "Retrieve the resource at URL, and when retrieved call CALLBACK.

This is the asyncronous version of `twit-parse-xml'.  Once that function is
refactored, and its named changed, so should this one."
  (let ((url-request-method "GET")
        (url-show-status nil))
    (setq twit-async-buffer
          (url-retrieve url 'twit-parse-xml-async-retrieve
                        (list url callback)))))

;;* rate-limit var
(defvar twit-rate-limit-halt-flag 'nil
  "Non-nil means we're in the middle of a rate limit op.

This is a flag to  make sure we don't try and get the rate limit, if
there is an error with retrieving the rate limiter.  it is meant to
be used dynamically (i.e. inside of a (let (())) statement.  )")

;;* xmlparse async
(defun twit-parse-xml-async-retrieve (status url callback)
  "Called by `twit-parse-xml-async'.

STATUS is the status from HTTP, URL and CALLBACK were the args from `twit-parse-xml-async'."
  (if (null status)   ; no news is good news.
      (let ((result nil))
        (if (bufferp twit-async-buffer)
            (save-excursion
              (set-buffer twit-async-buffer)
              (goto-char (point-min))
              (setq result (xml-parse-fragment))
              (kill-buffer (current-buffer))))
        (funcall callback status url result))
      (progn
        (twit-alert (format "Cannot retrieve twit URL.  Status is: %S" status))
        (when (equal status '(:error (error http 400)))
          (if (null twit-rate-limit-halt-flag)
              (twit-get-and-set-async-rate-limit)
              (error "URL retrieval error when trying to get rate limiter.  twit-rate-limit-halt-flag is true!"))))))

;;* post handler
(defun twit-handle-post (err success-msg error-msg)
  "General method to handle a twit posting.
This will give us a Guarantee that our posting atually did work.
Argument ERR Whether or not there was an error.  Set by `url-retrieve'.
Argument SUCCESS-MSG Message when action was successful.
Argument ERROR-MSG Message when action failed."
  (cond
    ((null err) (twit-alert success-msg))
    (t (twit-alert error-msg)
       (message "Post HTTP error was: %s\n(_x____}<\n" err)
       (if twit-debug (message "%s" (buffer-string)))))
  (kill-buffer (current-buffer)))

;;* post status
(defun twit-post-status (url post &optional reply-id)
  "Post some kind of tweet status message at URL with the content POST.

REPLY-ID, if set, sets the in_reply_to_status_id parameter."
  (let* ((url-request-method "POST")
         (url-request-data (concat "source=twit.el&status=" (url-hexify-string post)))
         (url-request-data (concat url-request-data
                                   (if reply-id
                                       (format "&in_reply_to_status_id=%s" reply-id)
                                       "")))
         (url-request-extra-headers twit-request-headers)
         (url-show-status nil))
    (if twit-debug (twit-alert url-request-data))
    (url-retrieve url 'twit-handle-post
                  (list twit-post-success-msg twit-post-failed-msg))))

;;* post direct
(defun twit-direct-message (user msg)
  "Post a direct message to USER with MSG."
  (let ((url-request-method "POST")
        (url-request-data (concat "source=twit.el"
                                  "&user=" (url-hexify-string user)
                                  "&text=" (url-hexify-string msg)))
        (url-request-headers twit-request-headers)
        (url-show-status nil))
    (if twit-debug (twit-alert url-request-data))
    (url-retrieve twit-direct-msg-send-url 'twit-handle-post
                  (list twit-direct-success-msg twit-post-failed-msg))))

;; post helper
(defun twit-post-function (url data success-msg fail-msg)
  "Send a generalized post request to URL with DATA, and give user feedback.

User feedback is achieved with SUCCESS-MSG and FAIL-MSG.  usually these are
constants."
  (let ((url-request-method "POST")
        (url-request-data
         (concat data (if (string-equal "" data)
                          "?" "&")
                 "source=twit.el"))
        (url-request-headers twit-request-headers)
        (url-show-status nil))
    (if twit-debug (twit-alert (format "Posting to %s with %s" url data)))
    (url-retrieve url 'twit-handle-post (list success-msg fail-msg))))

;;* rate
(defun twit-parse-rate-limit (xml)
  "Parse the rate limit, and return the hourly limit.
XML should be the twitter ratelimit sxml and should not have any HTTP header
information in its car."
  (let ((limit (assoc 'hourly-limit xml)))
    (if twit-debug (message "Parsed limit %s from xml %s" limit xml))
    (if limit
        (string-to-number (caddr limit)))))

;;* rate interactive
(defun twit-get-rate-limit ()
  "Return the rate limit as a number from the xml."
  (interactive)
  (let ((limit-xml (twit-parse-xml twit-rate-limit-file "GET")))
    (twit-parse-rate-limit (cadr limit-xml))))

;; rate async
(defun twit-get-and-set-async-rate-limit ()
  "Check rate limiting asyncronously, and automagickally set it."
  (interactive)
  (twit-parse-xml-async twit-rate-limit-file 'twit-get-and-set-async-rate-limit-callback))

;; rate async
(defun twit-get-and-set-async-rate-limit-callback (status url result)
  "Callback for `twit-get-and-set-async-rate-limit'.

STATUS, URL and RESULT are all set by `twit-get-and-set-async-rate-limit'."
  (if (null status)
      (if twit-debug (message "Rate Limit XML is %S" result))
      (twit-verify-and-set-rate-limit (twit-parse-rate-limit (cadr result)))
      (twit-alert (format "Cannot retrieve rate limit URL %S! Status: %S"
                          url status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;* read post helper
(defun twit--query-for-post-update (&optional beg end length invert)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin))
         overlay)

    ;; remove old overlays
    (mapcar #'(lambda (overlay)
                (when (overlay-get overlay 'twit--query-for-post)
                  (delete-overlay overlay)))
            (overlays-in (point-min) (point-max)))

    ;; if necessary, add a new one
    (when (> field-length twit-max-tweet)
      (setq overlay (make-overlay (+ field-begin twit-max-tweet) field-end))
      (overlay-put overlay 'face 'twit-too-long-face)
      (overlay-put overlay 'twit--query-for-post t))

    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "(\\(  0\\)/[0-9]+ characters)" nil t)
        (setq overlay (make-overlay (match-beginning 1)
                                    (match-end 1)))

        (overlay-put overlay 'twit--query-for-post t)
        (overlay-put overlay 'display (format "%3d" field-length))

        (let ((face
               `(:foreground
                 ,(if (<= field-length twit-max-tweet) "green" "red"))))

          (when invert
            (setq face (append '(:inverse-video t) face)))

          (overlay-put overlay 'face face))))))

;;* read post helper
(defun twit--query-for-post-exit-minibuffer ()
  (interactive)
  (let* ((field-begin (field-beginning (point-max)))
         (field-end (field-end (point-max)))
         (field-length (- field-end field-begin)))

    (if (<= field-length twit-max-tweet)
        (exit-minibuffer)
        (beep)
        (twit--query-for-post-update nil nil nil t)
        (sit-for 1)
        (twit--query-for-post-update nil nil nil))))

;;* read post helper
(defun twit--query-for-post-minibuffer-setup ()
  "Prepare the minibuffer for a twit entry.
Limit main field length to `twit-max-tweet' characters"

  (twit--query-for-post-update)
  (local-set-key [remap exit-minibuffer]
  #'twit--query-for-post-exit-minibuffer)
  (add-hook 'after-change-functions
  #'twit--query-for-post-update t t))

;;* read post
(defun twit-query-for-post (prompt-heading initial-input)
  "Query for a Twitter.com post text in the minibuffer.

PROMPT-HEADING is the prompt, and has \" (140 char max): \" appended to it.
INITIAL-INPUT is what it is."
  (let ((minibuffer-setup-hook
         (cons #'twit--query-for-post-minibuffer-setup minibuffer-setup-hook)))
    (read-string (concat prompt-heading
                         (format " (  0/%3d characters): "
                                 twit-max-tweet))
                 initial-input)))

;;* post query-buffer mode
(define-derived-mode twit-post-mode text-mode
  "Major mode for posting tweets."
  (define-key 'twit-post-mode-map [(Control c) (Control c)] 'twit-post-))

;;* post query-buffer
(defun twit-buffer-up-for-post (prompt-heading initial-input)
  "Query for a post inside of a regular buffer instead of the minibuffer."
  (pop-to-buffer (get-buffer-create "*twit-post*"))
  (twit-post-mode)
  (insert twit-popup-buffer-message))

;;* post query-buffer
(defun twit-interactive-post ()
  "The second half of posting, Performs a message length check, and posts.

This is the second half of the function `twit-buffer-up-for-post'

Any of the argument we need (url, post, parent-id) are handled via
text properties in the current buffer."
  (interactive)
  (let ((post (buffer-string))
        (url (twit-get-text-property 'twt-url-destination))
        (parent-id (twit-get-text-property 'twit-id))))
  (if (> (length post) twit-max-tweet)
      (error twit-too-long-msg)
      (twit-post-status twit-update-url post)))

;;* last-tweet
(defvar twit-last-tweet '()
  "The last tweet that was posted.
This is a bit of an ugly hack to store the last tweet that was shown through
`twit-write-recent-tweets'.

It is in the format of (timestamp user-id message)")

(setq twit-last-tweet '())

;;* xmlparse
(when (not (functionp 'xml-first-child))
  (defun xml-first-child (node attr)
    "Return the first child of some sxml"
    (car (xml-get-children node attr))))

;;* xmlparse
(when (not (functionp 'xml-first-childs-value))
  (defun xml-first-childs-value (node addr)
    "Return the value of the first child of some sxml. "
    (car (xml-node-children (xml-first-child node addr)))))

;;; xml parsing is a little hacky and needs work.
;;* tweets write last-tweet
(defun twit-write-recent-tweets (xml-data)
  "Function that writes the recent tweets to the buffer.

XML-DATA is the sxml (with http header)."
  (if (twit-header-error-p (twit-parse-header (car xml-data)))
      (twit-display-error xml-data)
      (let* ((first-tweet (xml-first-child (cadr xml-data) 'status))
             (most-recent-tweet (list (xml-first-childs-value first-tweet 'created_at)
                                      (or (xml-first-childs-value (xml-first-child first-tweet 'user) 'screen_name) "??")
                                      (xml-substitute-special (xml-first-childs-value first-tweet 'text))))
             (times-through 1))
        (dolist (status-node (xml-get-children (cadr xml-data) 'status))
                (let ((message (xml-substitute-special (xml-first-childs-value status-node 'text)))
                      (author (xml-first-childs-value (or (xml-first-child status-node 'user)
                                                          (xml-first-child status-node 'sender))
                                                      'name)))
                  (when (not (twit--filter-tweet message author))
                        (twit-write-tweet status-node nil times-through)
                        (setq times-through (+ 1 times-through)))))

        (when (not (equal most-recent-tweet twit-last-tweet))
          (setq twit-last-tweet most-recent-tweet)
          (run-hooks 'twit-new-tweet-hook))))

  ;; go back to top so we see the latest messages
  (goto-address)
  (goto-char (point-min))

  ;; this needs more TLC
  (if twit-debug-mem (message (garbage-collect))))

;;*var filter
(defvar twit-last-author '(nil .0)
  "Author information about the last tweet.

This is a cons, in the format of (author-name . times-tweeted).
This is used by `twit-filter-diarrhea'.")

;;* tweet write helper filter
;; This is a prime refactoring candidate.  It's logic is abysmally complex.
;; Sorry. (JA)
(defun twit--filter-tweet (message author)
  "Return t if the user wants MESSAGE filtered, nil if not."
  (when (> twit-filter-diarrhea 0)
        (if (string-equal author (car twit-last-author))
            (setcdr twit-last-author (+ 1 (cdr twit-last-author)))
            (setq twit-last-author (cons author 1))))
  (not (and  (or (string-equal "" twit-filter-tweets-regex)
                 (null twit-filter-tweets-regex)
                 (not (string-match twit-filter-tweets-regex message)))
             (or (not twit-filter-at-tweets)
                 (not (string-match "@" message))
                 (string-match twit-filter-at-tweets-retweet-regex message)
                 (and twit-filter-at-tweets
                      (twit-at-message-was-from-friend message)))
             (or (= 0 twit-filter-diarrhea)
                 (>= twit-filter-diarrhea
                     (cdr twit-last-author))))))

;;* tweet direct write image
(defun twit-write-tweet (tweet &optional filter-tweets times-through)
  "Insert a tweet into the current buffer.
TWEET should be an xml parsed node, which could be a message or a status node.
FILTER-TWEETS is an optional boolean to disregard filtering.
TIMES-THROUGH is an integer representing the number of times a tweet has been
  displayed, for zebra-tabling."
  (let* ((tweet-id (xml-first-childs-value tweet 'id))
         (user-info (or (xml-first-child tweet 'user) (xml-first-child tweet 'sender)))
         (user-id (or (xml-first-childs-value user-info 'screen_name) "??"))
         (user-name (xml-first-childs-value user-info 'name))
         (location (xml-first-childs-value user-info 'location))
         (user-img (if twit-show-user-images
                       (twit-get-user-image (xml-first-childs-value user-info 'profile_image_url) user-id)
                       nil))

         (timestamp (format-time-string twit-time-string (date-to-time (xml-first-childs-value tweet 'created_at))))
         (message (xml-substitute-special (xml-first-childs-value tweet 'text)))
         (src-info (xml-first-childs-value tweet 'source))
         (src-url (xml-first-childs-value tweet 'source))
         (favorite (xml-first-childs-value tweet 'favorited))
         (reply-to-status (xml-first-childs-value tweet 'in_reply_to_status_id))
         (reply-to-user (xml-first-childs-value tweet 'in_reply_to_screen_name))

         (overlay-start 0)
         (overlay-end 0))

    (when (and src-info (string-match (concat "<a h" "ref=\"\\(.*\\)\">\\(.*\\)<" "/a>") ; the string-match is a bit weird, as emacswiki.org won't accept pages with the href in it per se
                                      src-info))
          (setq src-info (match-string 2 src-info)))
    (when (and src-url (string-match (concat "<a h" "ref=\"\\([^\"]*\\)\".*>\\(.*\\)<" "/a>")
                                      src-url))
          (setq src-url (match-string 1 src-url)))

    (setq overlay-start (point))

    (when (and twit-show-user-images user-img)
          (insert " ")
          (insert-image user-img)
          (insert " "))

    (if (string-equal "true" favorite)
        (twit-insert-with-overlay-attributes "*" '((face . "twit-favorite-face")))
        (insert " "))

    (twit-insert-with-overlay-attributes (concat user-id
                                                 (if user-name
                                                     (concat " (" user-name ")")
                                                     ""))
                                         `((face . "twit-author-face")))
    (insert "\n\t")
    (let* ((message
            (with-temp-buffer
              (let ((fill-column (- (window-width twit-window) 2)))
                (insert "\t")
                (insert message)
                (fill-region (point-min) (point-max))
                (buffer-substring 2 (point-max))))))
      (twit-insert-with-overlay-attributes (twit-keymap-and-fontify-message message)
                                           '((face . "twit-message-face"))
                                           " "))

    (insert "\n")
    (when (or timestamp location src-info)
          (twit-insert-with-overlay-attributes
           (concat "                          "
                   (when timestamp
                     (setq timestamp (propertize timestamp
                                                    'twit-status tweet-id)))
                   (when location (concat " @ " location))
                   (when src-info
                     (setq src-info (propertize (concat " (" src-info ")")
                                                'twit-src src-url)))
                   (when reply-to-user
                     (setq reply-to-user (propertize (concat " in reply to " reply-to-user)
                                                    'twit-reply-status reply-to-status
                                                    'twit-reply-user reply-to-user)))
                   "\n")
           '((face . "twit-info-face")) "" 'right))
    (setq overlay-end (point))
    (let ((o (make-overlay overlay-start overlay-end)))
      (overlay-put o
                   'face (if (= 0 (% times-through 2))
                             "twit-zebra-1-face"
                             "twit-zebra-2-face"))
      (overlay-put o 'twit-id tweet-id)
      (overlay-put o 'twit-user user-id))))

;;* write helper
(defun twit-write-title (title &rest args)
  "Helper function to write out a title bar for a twit buffer.

TITLE is the title to display, and it is formatted with ARGS."
  (twit-insert-with-overlay-attributes
   (concat (propertize "(^)o<" 'face 'twit-logo-face)
           " "
           (apply 'format title args))
   '((face . "twit-title-face"))))

;;* write helper
(defun twit-keymap-and-fontify-message (message)
  "Scan through MESSAGE, and fontify and keymap all #foo and @foo."
  (let ((original-txt (substring message 0))) ;; Just to be sure we're using a copy
    (when (string-match twit-hash-at-regex message) ;; usernames
      (setq message (replace-regexp-in-string
                     twit-hash-at-regex
                     (lambda (str)
                       (let ((type (substring str 0 1))
                             (thing (substring str 1)))
                         (setq str (propertize str
                                               'face 'twit-hash-at-face
                                               'pointer 'hand))
                         (when (string-equal "@" type)
                           (setq str (propertize str 'twit-user thing)))
                         (propertize str 'twit-search (concat type thing))))
                     message)))

    (when (string-match twit-url-regex message) ;; URLs
      (setq message (replace-regexp-in-string
                     twit-url-regex
                     (lambda (str)
                       (let ((map (make-sparse-keymap)))
                         (define-key map [enter] 'twit-visit-link)
                         (define-key map [(control) (enter)] 'twit-visit-link)
                         (define-key map [mouse-1] 'twit-visit-link)
                         (define-key map [mouse 2] 'twit-visit-link)
                         (define-key map [mouse-3] 'twit-visit-link)
                         (propertize str
                                     'face 'twit-url-face
                                     'pointer 'hand
                                     'twit-url str
                                     'keymap map)))
                     message)))
    (when (string-match twit-emacs-lisp-regex message) ;; .el's
      (setq message (replace-regexp-in-string
                     twit-emacs-lisp-regex
                     (lambda (str)
                       (propertize str
                                   'face 'twit-url-face
                                   'elisp str))
                     message)))

    ;; message content (plaintext)
    (propertize message 'twit-message original-txt)))


;;* search write
(defun twit-write-search (atom-data)
  "This function writes atom-based search data ATOM-DATA."
  (buffer-disable-undo)
  (erase-buffer)
  (twit-display-error atom-data)
  (twit-write-title "Search (%s): %s \n"
                    (xml-first-childs-value (cadr atom-data) 'title)
                    (format-time-string "%c"))
  (dolist (entry-node (xml-get-children (cadr atom-data) 'entry))
          (let* ((message (twit-keymap-and-fontify-message (xml-first-childs-value entry-node 'title)))
                 (user (xml-first-childs-value (xml-first-child entry-node 'author) 'name))
                 (user-img (xml-get-attribute (xml-first-child entry-node 'link) 'href)))
            (insert (format "%30s: %s\n" (propertize user 'twit-user user) message)))))


;;* helper write
(defun twit-insert-with-overlay-attributes (text attributes &optional prefix justify)
"Helper function to insert text into a buffer with an overlay.

Inserts TEXT into buffer, add an overlay and apply ATTRIBUTES to the overlay."
  (let ((start (point))
        ;(fill-column (window-width))     ; Having a hard time making ieure's patch look nice
        ;(fill-prefix (or prefix fill-prefix))           ; ditto ieure
        )
    (insert text)
    ;(insert (concat fill-prefix  text))                 ; ditto ieure
    (let ((overlay (make-overlay start (point))))
      (dolist (spec attributes)
        (overlay-put overlay (car spec) (cdr spec))))
    ;(fill-region start (point) justify)                 ; ditto ieure
    ))

;;* image var
(defvar twit-user-image-list 'nil
  "List containing all user images.")
(setq twit-user-image-list 'nil)

;;* image
(defun twit-get-user-image (url user-id)
  "Retrieve the user image from the list, or from the URL.
USER-ID must be provided."
  (let ((img (assoc url twit-user-image-list)))
    (if (and img (not (bufferp (cdr img))))
        (cdr (assoc url twit-user-image-list))
        (if (file-exists-p (concat twit-user-image-dir
                                   "/" user-id "-"
                                   (file-name-nondirectory url)))
            (let ((img (create-image
                        (concat twit-user-image-dir ;; What's an ana for? lol
                                "/" user-id "-"
                                (file-name-nondirectory url)))))
              (add-to-list 'twit-user-image-list (cons url img))
              img)
            (let* ((url-request-method "GET")
                   (url-show-status nil)
                   (url-buffer (url-retrieve url 'twit-write-user-image
                                             (list url user-id))))
              (if url-buffer
                  (progn
                    (add-to-list 'twit-user-image-list (cons url url-buffer))
                    (if twit-debug (message "Added image. List is %s" twit-user-image-list)))
                  (twit-alert (format "Warning, couldn't load %s " url)))
              nil)))))

;;* image
(defun twit-write-user-image (status url user-id)
  "Called by `twit-get-user-image', to write the image to disk.

STATUS, URL and USER-ID are all set by `url-retrieve'."
  (let ((image-file-name
         (concat twit-user-image-dir
                 "/" user-id "-"
                 (file-name-nondirectory url))))
    (when (not (file-directory-p twit-user-image-dir))
      (make-directory twit-user-image-dir))
    (setq buffer-file-coding-system 'no-conversion)
    (setq buffer-file-name image-file-name)
    (goto-char (point-min))
    (delete-region (point-min) (search-forward "\C-j\C-j"))
    (save-buffer)
    (delete (cons url (current-buffer)) twit-user-image-list)
    (kill-buffer (current-buffer))
    (add-to-list 'twit-user-image-list (cons url (create-image image-file-name)))))

;;;
;; Recent tweets timer funciton and callback
;;* recent timer
(defun twit-follow-recent-tweets-timer-function ()
  "Timer function for recent tweets, called via a timer."
  (twit-parse-xml-async (format twit-friend-timeline-file 1)
                        'twit-follow-recent-tweets-async-callback))

;;* recent async timer
(defun twit-follow-recent-tweets-async-callback (status url xml)
  "Callback function, called by `twit-follow-recent-tweets-timer-function'.

STATUS, URL and XML are all set by `twit-follow-recent-tweets-timer-function'."
  (when (not status)
    (save-window-excursion
      (save-excursion
        (with-twit-buffer "*Twit-recent*"
          (twit-write-title "Following Recent tweets: %s\n"
                            (format-time-string "%c"))
          (twit-write-recent-tweets xml))))))

;;* rate var
(defvar twit-last-rate-limit
  twit-standard-rate-limit
  "What is the previous rate limit?")

;;* rate
(defun twit-verify-and-set-rate-limit (limit)
  "Check if limiting is in effect, and if so, set the timer.

LIMIT should be an integer."
  (let ((limit-reset nil)
        (twit-rate-limit-halt-flag 't))
    (if twit-debug (message  "Rate limit is %s, doing ratelimit magic." limit))
    (when (and limit
               (not (= limit 0))
               (not (= twit-last-rate-limit limit)))
      (cond ((< limit twit-standard-rate-limit)
             (progn
               (setq twit-shadow-follow-idle-interval
                     (+ (/ (* 60 60) limit)
                        twit-rate-limit-offset))
               (setq limit-reset 't)
               (twit-alert
                (format "Twitter is under a rate limit.  Timer set to %s seconds."
                        twit-shadow-follow-idle-interval))))
            ((= limit twit-standard-rate-limit)
             (progn
               (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
               (setq limit-reset 't)
               (twit-alert
                (format "Rate limiting relaxed.  Timer set to normal timeout (%s seconds)"
                        twit-shadow-follow-idle-interval))))
            (t ; exceptional case...
             (progn
               (setq twit-shadow-follow-idle-interval twit-follow-idle-interval)
               (setq limit-reset 't)
               (twit-alert
                (format "The twitter rate has exceeded its expected maximum.  This is weird."))))))
    (when (and limit-reset (timerp twit-timer))
      (progn
        (if twit-debug (message "Cancelling and restarting timer."))
        (cancel-timer 'twit-timer)
        (twit-follow-recent-tweets)))
    (setq twit-last-rate-limit limit)))

;;*friends var
(defvar twit-get-friends-cache nil
  "A cache of the friends from `twit-get-friends'.  For completing reads.")

;;* friends
(defun twit-get-friends (&optional cached)
  "Get a list of friends.  If CACHED is non-nil, use the cached version.

This also sets the variable get-friends-cache."
  (when (or (null twit-get-friends-cache)
            (null cached))
    (setq twit-get-friends-cache
          (save-window-excursion
            (save-excursion
              (loop for screen_name in
                   (loop for user in
                        (xml-get-children
                         (cadr (twit-parse-xml twit-friend-list-url "GET"))
                         'user)
                      collect (eighth user))
                 collect (third screen_name))))))
  twit-get-friends-cache)

;;* friends at-filter
(defun twit-at-message-was-from-friend (tweet)
  "Tell us if the text in the TWEET contains an @ that we care about."
  (let ((found-match nil)
        (twit-at-friends
         (mapcar (lambda (s) (concat "@" s))
                 (cons twit-user (twit-get-friends t)))))
    (while  (and (not found-match)
                 (string-match twit-at-regex tweet))
      (setq found-match (member (match-string 0 tweet) twit-at-friends))
      (setq tweet (substring tweet (+ 1 (string-match twit-at-regex tweet)))))
    found-match))

(when nil
  (twit-at-message-was-from-friend "@jonnay is the bomb")
  (twit-at-message-was-from-friend "@sunnay is the bomb"))


;;* todochiku
;;; funciton to integrade with growl.el or todochiku.el
(defun twit-todochiku ()
  "Helper function for use by the todochiku package."
  (todochiku-message "twit.el"
                     (format "From %s:\n%s"
                             (cadr twit-last-tweet)
                             (caddr twit-last-tweet))
                     (todochiku-icon 'social)))

;;* property helper
(defun twit-get-text-property (propname)
  "Return a property named PROPNAME or nil if not available.

This is the reverse of `get-char-property', it checks text properties first."
  (or (get-text-property (point) propname)
      (get-char-property (point) propname)))


;;* read show
(defun twit-check-page-prefix (page)
   "For use with an interactive function.  Turn a PAGE prefix arg into an integer."
   (if (or (null page) (<= page 1)) 1 page))


;;* read account
(defun twit-read-account ()
  "Does a completing read, and return an account."
   (completing-read "Account: "
                    (mapcar 'car twit-multi-accounts)
                    nil
                    t))

;;* read friends
(defun twit-read-friend (prompt &optional req)
  "Does a completing read to find a friend.

PROMPT is the prompt to the user, and should not contain a \": \" at the end.
If there us a twit-user property under the point, that will be added
at the end in parens.

REQ is an optional requirement.  If its true, then the friend must exist in
the friends cache."
  (let* ((cur-author (twit-get-text-property 'twit-user)))
    (completing-read prompt (twit-get-friends t) nil req nil nil cur-author)))


;;* interactive direct
;;;###autoload
(defun twit-direct (user msg)
  "Send USER a direct message MSG.

If you are currently positioned over a tweet, then it will fill in the author
of that tweet as the default recipient.

This will attempt to do a completing read based on the people you
are following, if you have images turned on."
  (interactive
   (list (twit-read-friend "Direct Message To: " t)
         (twit-query-for-post "Message: " "")))
  (if (> (length msg) twit-max-tweet)
      (error twit-too-long-msg)
      (twit-direct-message user msg)))

;;* interactive direct multi-account
(defun twit-direct-with-account (account)
  "Send a user a direct tweet with `twit-direct' under a different ACCOUNT."
  (interactive (list (twit-read-account)))
  (with-twit-account account
    (twit-direct (twit-read-friend "Direct Message To: " t)
                 (read-string "Message: "))))

;;* post interactive tweet
;;;###autoload
(defun twit-post (prefix)
  "Send a post to twitter.com.
Prompt the first time for password and username \(unless
`twit-user' and/or `twit-pass' is set\) and for the text of the
post; thereafter just for post text.  Posts must be <= `twit-max-tweet' chars
long.

A PREFIX argument will prompt you for your post in reply to a
specific author that the cursor is nearest to.  This behavior may
become deprecated."
  (interactive "P")
  (let* ((reply-to (when prefix
                     (twit-get-text-property 'twit-user)))
         (post (twit-query-for-post
                (if reply-to
                    (concat "Reply to " reply-to)
                    "Post")
                (when reply-to
                  (concat "@" reply-to " ")))))
    (if (> (length post) twit-max-tweet)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post))))

;;* post interactive tweet multi-account
(defun twit-post-with-account (account post)
  "Like `twit-post' but under a different ACCOUNT POST a tweet."
  (interactive (list (twit-read-account)
                     (twit-query-for-post "Post: " "")))
  (with-twit-account account
    (if (> (length post) twit-max-tweet)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post))))

;;* post interactive keymap
(defun twit-post-to ()
  "Posts to a particular user.  Mostly used by keymaps."
  (interactive)
  (let ((post (twit-query-for-post "Reply To: " (concat "@" (twit-get-text-property 'twit-user) " "))))
    (if (> (length post) twit-max-tweet)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post))))

;;* post reply interactive
(defun twit-post-reply ()
  "Reply to a status on twitter.com."
  (interactive)
  (let* ((reply-to (twit-get-text-property 'twit-user))
         (parent-id (twit-get-text-property 'twit-id))
         (post (twit-query-for-post (concat "Reply to " reply-to)
                                    (concat "@" reply-to " "))))
    (if (> (length post) 140)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post parent-id))))

;;* post "loud" reply interactive
(defun twit-post-loud-reply ()
  "Reply to a status on twitter.com.

When you write @foo, only those followers of yours who are also
following @foo will see the reply. To get around this, if you
want all of your followers to see the reply anyways begin the
tweet with \".@\" or some other filler character."
  (interactive)
  (let* ((reply-to (twit-get-text-property 'twit-user))
         (parent-id (twit-get-text-property 'twit-id))
         (post (twit-query-for-post (concat "Loud reply to " reply-to)
                                    (concat ".@" reply-to " "))))
    (if (> (length post) 140)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post parent-id))))

;;* post retweet
;;  Begins a post with "RT @foo: waka waka waka || "
(defun twit-post-retweet ()
  "Retweet someone else's post."
  (interactive)
  (let* ((reply-to (twit-get-text-property 'twit-user))
         (parent-id (twit-get-text-property 'twit-id))
         (retweet-text (twit-get-text-property 'twit-message))
         (post (twit-query-for-post
                (concat "Retweeting " reply-to)
                (concat "RT @" reply-to ": " retweet-text " || "))))
    (if (> (length post) 140)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post parent-id))))

;;* post url
;;  Prompts for a URL, then compresses it and starts a tweet with the shortened URL in the body
(defun twit-post-url ()
  "Compress a URL, then start posting a tweet with the result."
  (interactive)
  (let* ((url (compress-url (read-string "URL: ")))
         (post (twit-query-for-post "Post" url)))
    (if (> (length post) twit-max-tweet)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post))))

;; Compresses given URL
;; Examples from other services:
;; TinyURL: http://tinyurl.com/api-create.php?url=
;; Tr.im (requires some kind of account or something?): http://api.tr.im/api/trim_simple?url=
(defun compress-url (url)
  "Compress a URL using is.gd."
  (let* ((returned-request
           (car (twit-parse-xml
                 (concat "http://is.gd/api.php?longurl=" url) "GET")))
          (url-idx (string-match "http://" returned-request 160))
          (compressed-url
           (if url-idx
               (substring returned-request url-idx -1)
               "is.gd is down :(")))
       compressed-url))


;;* post interactive tweet
;;;###autoload
(defun twit-post-region (start end)
  "Send text in the region as a post to twitter.com.

START and END should be the region you want to post.

Uses `twit-post-status' to do the dirty work and to obtain
needed user and password information.  Posts must be <= `twit-max-tweet' chars
long."
  (interactive "r")
  (let ((post (buffer-substring start end)))
    (if (> (length post) twit-max-tweet)
    (error twit-too-long-msg)
    (twit-post-status twit-update-url post))))

;;* post interactive
;;;###autoload
(defun twit-post-buffer ()
  "Post the entire contents of the current buffer to twitter.com.
Uses `twit-post-status' to do the dirty work and to obtain
needed user and password information.  Posts must be <= `twit-max-tweet' chars
long."
  (interactive)
  (let ((post (buffer-substring (point-min) (point-max))))
    (if (> (length post) twit-max-tweet)
        (error twit-too-long-msg)
        (twit-post-status twit-update-url post))))

;;* friend post add
(defun twit-add-friend (user)
  "USER name of person you want to follow."
  (interactive (list (twit-read-friend "Add Friend: ")))
  (twit-post-function (format twit-add-friend-url user)
                      (concat "follow=true&screen_name="
                              (url-hexify-string user))
                      twit-add-friend-success-msg
                      twit-add-friend-fail-msg))

;;* friend post remove
(defun twit-remove-friend (user)
  "USER name of person you want to follow."
  (interactive (list (twit-read-friend "Remove Friend: " t)))
  (twit-post-function (format twit-remove-friend-url user)
                      (concat "screen_name=" (url-hexify-string user))
                      twit-remove-friend-success-msg
                      twit-remove-friend-fail-msg))

;;* your favorite tweets show interactive
;;;###autoload
(defun twit-show-favorites-tweets (page)
  "Display a list of your favorite tweets .

With a numeric prefix argument, it will skip to that PAGE like `twit-show-recent-tweets'."
  (interactive "P")
  (setq page (twit-check-page-prefix page))
  (pop-to-buffer
   (with-twit-buffer (concat "*Twit-favorites-" twit-user "*")
     (twit-write-title "Favorites of  %s (Page %s) %s\n"
                       twit-user page (format-time-string "%c"))
     (let ((times-through 0))
       (dolist
           (status-node (xml-get-children
                         (cadr (twit-parse-xml
                                (format twit-favorites-url page) "GET"))
                         'status))
         (twit-write-tweet status-node t times-through)
         (setq times-through (+ 1 times-through)))))))

;;* favorite add post
(defun twit-add-favorite (post)
  "Add a POST as a favorite."
  (interactive (list (read-from-minibuffer
                      (format "Tweet to Favorite (%s): "
                              (twit-get-text-property 'twit-id))
                      nil
                      nil
                      nil
                      nil
                      (twit-get-text-property 'twit-id))))
  (twit-post-function
   (format twit-add-favorite-url
           (if (not (string-equal "" post))
               post (twit-get-text-property 'twit-id)))
   ""
   twit-add-favorite-success-msg
   twit-add-favorite-fail-msg))

;;* favorite remove post
(defun twit-remove-favorite (post)
  "Remove a POST from favorites."
  (interactive (list (read-from-minibuffer
                      (format "Remove Tweet From Favorites (%s): "
                              (twit-get-text-property 'twit-id))
                      nil
                      nil
                      nil
                      nil
                      (twit-get-text-property 'twit-id))))
  (twit-post-function
   (format twit-remove-favorite-url
           (if (not (string-equal "" post))
               post (twit-get-text-property 'twit-id)))
   ""
   twit-remove-favorite-success-msg
   twit-remove-favorite-fail-msg))

;;* show followers interactive
;; minor modes might be a cause of the memoryleak, see about removing them
;; this is too delicate, depending on the order of the xml elements, and not
;; the actual xml elements.
;;;###autoload
(defun twit-show-followers (&optional page)
  "Display a list of all your twitter.com followers' names."
  (interactive "P")
  (setq page (twit-check-page-prefix page))
  (pop-to-buffer
   (with-twit-buffer "*Twit-followers*"
     (twit-write-title "Followers\n")
     (loop for name in
           (loop for name in
                 (loop for user in
                       (xml-get-children
                        (cadr (twit-parse-xml (format twit-followers-list-url page) "GET")) 'user)
                       collect (eighth user))
                 collect (third name))
           for i upfrom 1
           do (insert (propertize (format "%s\n" name)
                                  'face
                                  (if (= 0 (% i 2))
                                      "twit-zebra-1-face"
                                      "twit-zebra-2-face")
                                  'twit-user name))))))

(defalias 'twit-list-followers 'twit-show-followers
  "The name twit-list-followers is deprecated.  Use `twit-show-followers' instead.")

;;* show friends interactive
;;;###autoload
(defun twit-show-friends ()
   "Display a list of all your friends."
   (interactive)
   (pop-to-buffer
    (with-twit-buffer "*Twit-friends*"
      (let ((friends (twit-get-friends)))
        (twit-write-title (format "Friends (%s):\n" (length friends)))
        (loop for i upfrom 1
           for f in friends do
             (insert (propertize (concat f "\n")
                                 'twit-user f
                                 'face (if (= 0 (% i 2))
                                           "twit-zebra-1-face"
                                           "twit-zebra-2-face"))))))))

;;* twit follow timer interactive
;;;###autoload
(defun twit-follow-recent-tweets ()
  "Set up a timer, and show you the most recent tweets approx every 90 seconds.

You can change the time between each check by customizing
`twit-follow-idle-interval'."
  (interactive)
  (twit-show-recent-tweets nil)
  (setq twit-window (get-buffer-window (buffer-name)))
  (twit-verify-and-set-rate-limit (twit-get-rate-limit))
  (setq twit-rate-limit-timer
        (run-with-timer twit-rate-limit-interval
                        twit-rate-limit-interval
                        'twit-get-and-set-async-rate-limit))
  (setq twit-timer
        (run-with-timer twit-shadow-follow-idle-interval
                        twit-shadow-follow-idle-interval
                        'twit-follow-recent-tweets-timer-function)))

;;* twit follow timer interactive
(defun twit-stop-following-tweets ()
  "When you want to stop following tweets, you can use this function to turn off the timer."
  (interactive)
  (if (featurep 'todochiku)
      (todochiku-message "Twit.el" "Twit.el Stopped Following Tweets"
                         (todochiku-icon 'social)))
  (cancel-timer twit-timer)
  (cancel-timer twit-rate-limit-timer))

;;* tweet show  interactive
;;;###autoload
(defun twit-show-recent-tweets (&optional page)
  "Display a list of the most recent tweets from people you're following.

You can use a prefix argument (C-u <number>) to skip between pages.  If you are
following tweets by using `twit-follow-recent-tweets', these might get
overwritten. (let me know if that behavior bugs you, and I will make
follow-tweets write into a different buffer.)

Currently there is a bug where the first time you show tweets, it might come up
empty.  This is being worked on.

With a numeric prefix arg PAGE, it will skip to that page of tweets.

Patch version from Ben Atkin."
  (interactive "P")
  (setq page (twit-check-page-prefix page))
  (pop-to-buffer
   (with-twit-buffer "*Twit-recent*"
     (twit-write-title "Recent Tweets (Page %s) [%s]\n"
                       page (format-time-string "%c"))
     (twit-write-recent-tweets
      (twit-parse-xml (format twit-friend-timeline-file page) "GET")))))


;;* interactive nav
(defun twit-next-tweet (&optional arg)
  "Move forward to the next tweet.

With argument ARG, move to the ARGth next tweet."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (next-single-char-property-change (point) 'twit-id nil
                                                       (point-max))))
        (number-sequence 1 (or arg 1))))

;;* interactive nav
(defun twit-previous-tweet (&optional arg)
  "Move backward to the previous tweet.

With argument ARG, move to the ARGth previous tweet."
  (interactive "p")
  (mapc (lambda (n)
          (goto-char (previous-single-char-property-change (point) 'twit-id nil
                                                           (point-min))))
        (number-sequence 1 (or arg 1))))

;;* search helper var
(defvar twit-this-sessions-searches 'nil
  "A variable to store any searches that the user has already searched for this session.")

;;* interactive search
(defun twit-search (term)
  "Run a twitter search for TERM.
Note that this is currently \"in beta\". It will get better."
  (interactive
   (list (completing-read "Search Term: "
                          (append twit-completing-searches twit-this-sessions-searches)
                          nil
                          nil
                          (twit-get-text-property 'twit-search))))
  (when (not (or (member term twit-completing-searches)
                 (member term twit-this-sessions-searches)))
    (setq twit-this-sessions-searches (cons term twit-this-sessions-searches)))
  (pop-to-buffer
   (with-twit-buffer (concat "*Twit-Search-" (url-hexify-string term) "*")
     (twit-write-search (twit-parse-xml (format twit-search-url
                                                (url-hexify-string term))
                                        "GET")))))

;;* direct show interactive
;; minor modes might be a cause of the memoryleak, see about removing them
;;;###autoload
(defun twit-show-direct-tweets (page)
  "Display a list of the most recent direct tweets.

With a numeric prefix argument, it will skip to that PAGE like `twit-show-recent-tweets'."
  (interactive "P")
  (setq page (twit-check-page-prefix page))
  (pop-to-buffer
   (with-twit-buffer "*Twit-direct*"
     (twit-write-title "Direct Messages (page %s) [%s]\n"
                       page (format-time-string "%c"))
     (let ((times-through 0))
       (dolist
           (msg-node (xml-get-children
                      (cadr (twit-parse-xml
                             (format twit-direct-msg-get-url page) "GET"))
                      'direct_message))
         (twit-write-tweet msg-node nil times-through)
         (setq times-through (+ 1 times-through)))))))

;;* direct show interactive multi-account
(defun twit-show-direct-tweets-with-account (account page)
  "`twit-show-direct-tweets' with a different account."
  (interactive (list (twit-read-account)
                     current-prefix-arg))
  (with-twit-account account (twit-show-direct-tweets page)))

;;* at-you show interactive
;;;###autoload
(defun twit-show-at-tweets (page)
  "Display a list of tweets that were @ you.

With a numeric prefix argument, it will skip to that PAGE like `twit-show-recent-tweets'."
  (interactive "P")
  (setq page (twit-check-page-prefix page))
  (pop-to-buffer
   (with-twit-buffer (concat "*Twit-at-" twit-user "*")
     (twit-write-title "Twit @%s (Page %s) %s\n"
                       twit-user page (format-time-string "%c"))
     (let ((times-through 0))
       (dolist
           (status-node (xml-get-children
                         (cadr (twit-parse-xml
                                (format twit-mentions-url page) "GET"))
                         'status))
         (twit-write-tweet status-node t times-through)
         (setq times-through (+ 1 times-through)))))))

;;* at-you show interactive multi-account
(defun twit-show-at-tweets-with-account (account page)
  "`twit-show-at-tweets-with-account' with a different account."
  (interactive (list (twit-read-account)
                     current-prefix-arg))
  (with-twit-account account (twit-show-at-tweets page)))

(defalias 'twit-search-at-to-me 'twit-show-at-tweets
  "Aliased to `twit-show-at-tweets', does the same thing with a better interface.")

(defun twit-visit-link ()
  "Vist a link under the point.

If the point is under a hyperlink, goto that link.
If the point is under timestamp of tweet, goto that tweet page.
If the point is under in reply to USER of tweet, goto that reply tweet page.
If the point is under source, goto source page.
If the point is under an @user, go to that users page.
Otherwise goto the authors page."
  (interactive)
  (browse-url (or (twit-get-text-property 'twit-url)
                  (when (twit-get-text-property 'twit-status) ;show tweet page for tweet
                        (concat "http://twitter.com/"
                                (twit-get-text-property 'twit-user)
                                "/status/"
                                (twit-get-text-property 'twit-status)))
                  (when (twit-get-text-property 'twit-reply-user) ;show tweet page for reply
                        (concat "http://twitter.com/"
                                (twit-get-text-property 'twit-reply-user)
                                "/status/"
                                (twit-get-text-property 'twit-reply-status)))
                  (when (twit-get-text-property 'twit-src) ;show source
                    (twit-get-text-property 'twit-src))
                  (when (twit-get-text-property 'twit-user)
                        (concat "http://twitter.com/"
                                (twit-get-text-property 'twit-user))))))

(defun twit-open-link ()
  "Visit (open) the first URL in current tweet.

Check if the tweet under the point contains a URL, and visit the
URL if there is one.  The point does not have to be pointing to
the URL itself."
  (interactive)
  (let* ((end (next-single-char-property-change (point) 'twit-id))
         (start (save-excursion
                  (goto-char end)
                  (previous-single-char-property-change (point) 'twit-id))))
    (save-excursion
      (goto-char start)
      (if (search-forward-regexp twit-url-regex end t)
          (progn
            (forward-char -1)
            (twit-visit-link))
        (message "No URL found in this tweet!")))))

;;* analyse interactive
(defun twit-analyse-user ()
  "Analyse the user under the point with Twanalyst."
  (interactive)
  (browse-url (format twit-analyse-user-url (if (twit-get-text-property 'twit-user)
                                                (twit-get-text-property 'twit-user)
                                                twit-user))))

;;* analyse interactive
(defun twit-analyse-graph-user ()
  "Graph tweets and followers over time with Twanalyst."
  (interactive)
  (browse-url (format twit-analyse-graph-user-url (if (twit-get-text-property 'twit-user)
                                                      (twit-get-text-property 'twit-user)
                                                      twit-user))))

;;* analyse interactive
(defun twit-analyse-suggest-user ()
  "Show other users that you should follow with Twanalyst."
  (interactive)
  (browse-url (format twit-analyse-suggest-user-url (if (twit-get-text-property 'twit-user)
                                                        (twit-get-text-property 'twit-user)
                                                        twit-user))))

;;* install interactive
(defun twit-install-elisp ()
  "Install the elisp library at point.

This function assumes that the elisp library is at Emacs Wiki, and that you
have auto-install installed."
  (interactive)
  (if (featurep 'auto-install)
      (let ((file  (twit-get-text-property 'elisp)))
        (if file
            (auto-install-from-emacswiki file)
            (error "Not on an elisp library, cannot auto-install.")))
      (error "Cannot auto-install elisp library, because auto-install is not installed.")))

;;* multi-account interactive
(defun twit-switch-account (account)
  "Switch twitter account to ACCOUNT.

ACCOUNT should be the account name that was set up in `twit-multi-accounts'."
  (interactive (list (twit-read-account)))
  (let ((acc (assoc-string account twit-multi-accounts)))
    (twit-set-auth (car acc) (cdr acc))))

;;* mode
;;;###autoload
(define-minor-mode twit-minor-mode
    "Toggle twit-minor-mode, a minor mode that binds some keys for posting.
Globally binds some keys to Twit's interactive functions.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

\\{twit-minor-mode-map}" nil
" Twit"
'(("\C-c\C-tp" . twit-post)
  ("\C-c\C-tr" . twit-post-region)
  ("\C-c\C-tb" . twit-post-buffer)
  ("\C-c\C-tf" . twit-show-followers)
  ("\C-c\C-ts" . twit-show-recent-tweets))
  :global t
  :group 'twit
  :version twit-version-number)
(provide 'twit)


;;* testing auth
(when nil

      (with-twit-auth "twit_el" twit-pass
        (message "auth: %s" (symbol-value url-basic-auth-storage))
        (twit-post-status twit-update-url "Testing again.  Hopefully authentication works this time."))

      (with-twit-auth "jonnay" twit-pass
        (message "auth: %s" (symbol-value url-basic-auth-storage))
        (twit-show-recent-tweets))

      )

;;* testing
(when nil

  (shell-command "emacs --no-site-file --no-init-file &" nil nil)
  ;; copy/paste to new emacs instance.
  (load "~/my-elisp/twit.el")
  (twit-show-recent-tweets)
  (twit-follow-recent-tweets)
  )

;;* posting
(when nil
      (let* ((summary " - 0.3.3  - Auto-install function added.  When you are over text
            that matches the name of an elisp file, you can press
            \"i\" to install that file from emacs wiki, provided you
            have auto-install.      ")
         (short-summary "Auto-install magick. Press i on an emacs library in a tweet to install it.")
         (twit-post (format "New #twit.el: http://www.emacswiki.org/emacs-en/twit.el . V %s %s" twit-version-number short-summary)))
    (yaoddmuse-post-library "twit.el" "EmacsWiki" "twit.el" summary t)
    (twit-post-status twit-update-url twit-post)
    (with-twit-auth "twit_el" twit-pass (twit-post-status twit-update-url twit-post))
    (twit-show-recent-tweets))
  )

;; Local Variables:
;; tab-width: 4
;; end:

;;; twit.el ends here
