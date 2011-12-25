;; artbollocks-mode.el - A minor mode to guide art writers.
;; Copyright (c) 2011 Rob Myers <rob@robmyers.org>
;;
;; Based on fic-mode.el
;; Copyright (C) 2010, Trey Jackson <bigfaceworm(at)gmail(dot)com>
;;
;; Non-artbollocks words from: http://matt.might.net/articles/
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage
;;
;; To use, save artbollocks-mode.el to a directory in your load-path.
;;
;; (require 'artbollocks-mode)
;; (add-hook 'text-mode-hook 'turn-on-artbollocks-mode)
;; (add-hook 'org-mode-hook 'turn-on-artbollocks-mode)
;;
;; or
;;
;; M-x artbollocks-mode
;;
;; NOTE: If you manually turn on artbollocks-mode,
;; you you might need to force re-fontification initially:
;;
;;   M-x font-lock-fontify-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable features individually

(defcustom lexical-illusions t
  "Whether to check for lexical illusions"
  :type '(boolean)
  :group 'artbollocks-mode)

(defcustom passive-voice t
  "Whether to check for passive voice"
  :type '(boolean)
  :group 'artbollocks-mode)

(defcustom weasel-words t
  "Whether to check for weasel words"
  :type '(boolean)
  :group 'artbollocks-mode)

(defcustom artbollocks t
  "Whether to check for artbollocks"
  :type '(boolean)
  :group 'artbollocks-mode)

;; Lexical illusion face

(defcustom lexical-illusions-foreground-color "black"
  "Lexical illusions face foreground colour"
  :group 'artbollocks-mode)

(defcustom lexical-illusions-background-color "magenta"
  "Lexical illusions face background color"
  :group 'artbollocks-mode)

(defcustom font-lock-lexical-illusions-face 'font-lock-lexical-illusions-face
  "The face for lexical illusions in artbollocks mode"
  :group 'artbollocks-mode)

(make-face 'font-lock-lexical-illusions-face)
(modify-face 'font-lock-lexical-illusions-face
	     lexical-illusions-foreground-color
             lexical-illusions-background-color
	     nil t nil t nil nil)

;; Passive voice face

(defcustom passive-voice-foreground-color "Gray"
  "Passive voice face foreground colour"
  :group 'artbollocks-mode)

(defcustom passive-voice-background-color "White"
  "Passive voice face background color"
  :group 'artbollocks-mode)

(defcustom font-lock-passive-voice-face 'font-lock-passive-voice-face
  "The face for passive voice words in artbollocks mode"
  :group 'artbollocks-mode)

(make-face 'font-lock-passive-voice-face)
(modify-face 'font-lock-passive-voice-face passive-voice-foreground-color
             passive-voice-background-color nil t nil t nil nil)

;; Weasel words face

(defcustom weasel-words-foreground-color "Brown"
  "Weasel words face foreground colour"
  :group 'artbollocks-mode)

(defcustom weasel-words-background-color "White"
  "Weasel words face background color"
  :group 'artbollocks-mode)

(defcustom font-lock-weasel-words-face 'font-lock-weasel-words-face
  "The face for weasel-words words in artbollocks mode"
  :group 'artbollocks-mode)

(make-face 'font-lock-weasel-words-face)
(modify-face 'font-lock-weasel-words-face weasel-words-foreground-color
             weasel-words-background-color nil t nil t nil nil)

;; Artbollocks face

(defcustom artbollocks-foreground-color "Purple"
  "Font foreground colour"
  :group 'artbollocks-mode)

(defcustom artbollocks-background-color "White"
  "Font background color"
  :group 'artbollocks-mode)

(defcustom font-lock-artbollocks-face 'font-lock-artbollocks-face
  "The face for artbollocks words in artbollocks mode"
  :group 'artbollocks-mode)

(make-face 'font-lock-artbollocks-face)
(modify-face 'font-lock-artbollocks-face artbollocks-foreground-color
             artbollocks-background-color nil t nil t nil nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical illusions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lexical-illusions-regex "\\(\\w+\\)[ \t\r\n]+\\(\\1\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Passive voice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst passive-voice-regex "\\b\\(am\\|are\\|were\\|being\\|is\\|been\\|was\\|be\\)\\s-+\\(\\w+ed\\|awoken\\|been\\|born\\|beat\\|become\\|begun\\|bent\\|beset\\|bet\\|bid\\|bidden\\|bound\\|bitten\\|bled\\|blown\\|broken\\|bred\\|brought\\|broadcast\\|built\\|burnt\\|burst\\|bought\\|cast\\|caught\\|chosen\\|clung\\|come\\|cost\\|crept\\|cut\\|dealt\\|dug\\|dived\\|done\\|drawn\\|dreamt\\|driven\\|drunk\\|eaten\\|fallen\\|fed\\|felt\\|fought\\|found\\|fit\\|fled\\|flung\\|flown\\|forbidden\\|forgotten\\|foregone\\|forgiven\\|forsaken\\|frozen\\|gotten\\|given\\|gone\\|ground\\|grown\\|hung\\|heard\\|hidden\\|hit\\|held\\|hurt\\|kept\\|knelt\\|knit\\|known\\|laid\\|led\\|leapt\\|learnt\\|left\\|lent\\|let\\|lain\\|lighted\\|lost\\|made\\|meant\\|met\\|misspelt\\|mistaken\\|mown\\|overcome\\|overdone\\|overtaken\\|overthrown\\|paid\\|pled\\|proven\\|put\\|quit\\|read\\|rid\\|ridden\\|rung\\|risen\\|run\\|sawn\\|said\\|seen\\|sought\\|sold\\|sent\\|set\\|sewn\\|shaken\\|shaven\\|shorn\\|shed\\|shone\\|shod\\|shot\\|shown\\|shrunk\\|shut\\|sung\\|sunk\\|sat\\|slept\\|slain\\|slid\\|slung\\|slit\\|smitten\\|sown\\|spoken\\|sped\\|spent\\|spilt\\|spun\\|spit\\|split\\|spread\\|sprung\\|stood\\|stolen\\|stuck\\|stung\\|stunk\\|stridden\\|struck\\|strung\\|striven\\|sworn\\|swept\\|swollen\\|swum\\|swung\\|taken\\|taught\\|torn\\|told\\|thought\\|thrived\\|thrown\\|thrust\\|trodden\\|understood\\|upheld\\|upset\\|woken\\|worn\\|woven\\|wed\\|wept\\|wound\\|won\\|withheld\\|withstood\\|wrung\\|written\\)\\b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weasel words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst weasel-words-regex "\\b\\(many\\|various\\|very\\|fairly\\|several\\|extremely\\|exceedingly\\|quite\\|remarkably\\|few\\|surprisingly\\|mostly\\|largely\\|huge\\|tiny\\|\\(\\(are\\|is\\) a number\\)\\|excellent\\|interestingly\\|significantly\\|substantially\\|clearly\\|vast\\|relatively\\|completely\\)\\b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artbollocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst artbollocks-regex "\\b(a priori\\|ad hoc\\|affirmation\\|affirm\\|affirms\\|aporia\\|aporetic\\|appropriates\\|appropriation\\|archetypal\\|archetypical\\|archetype\\|archetypes\\|autonomous\\|autonomy\\|baudrillardian\\|baudrillarian\\|commodification\\|committed\\|commitment\\|commonalities\\|contemporaneity\\|context\\|contexts\\|contextual\\|contextualise\\|contextualises\\|contextualisation\\|contextialize\\|contextializes\\|contextualization\\|convention\\|conventional\\|conventions\\|coterminous\\|critique\\|cunning\\|cunningly\\|death of the author\\|debunk\\|debunked\\|debunking\\|debunks\\|deconstruct\\|deconstruction\\|deconstructs\\|deleuzian\\|desire\\|desires\\|discourse\\|discursive\\|disrupt\\|disrupts\\|engage\\|engagement\\|engages\\|episteme\\|epistemic\\|ergo\\|fetish\\|fetishes\\|fetishise\\|fetishised\\|fetishize\\|fetishized\\|gaze\\|gender\\|gendered\\|historicise\\|historicisation\\|historicize\\|historicization\\|hegemonic\\|hegemony\\|identity\\|identity politics\\|intensifies\\|intensify\\|intensifying\\|interrogate\\|interrogates\\|interrogation\\|intertextual\\|intertextuality\\|irony\\|ironic\\|ironical\\|ironically\\|ironisation\\|ironization\\|ironises\\|ironizes\\|jouissance\\|juxtapose\\|juxtaposes\\|juxtaposition\\|lacanian\\|lack\\|loci\\|locus\\|locuses\\|matrix\\|mocking\\|mockingly\\|modalities\\|modality\\|myth\\|mythologies\\|mythology\\|myths\\|narrative\\|narrativisation\\|narrativization\\|narrativity\\|nexus\\|nodal\\|node\\|normative\\|normativity\\|notion\\|notions\\|objective\\|objectivity\\|objectivities\\|objet petit a\\|ontology\\|ontological\\|operate\\|operates\\|otherness\\|othering\\|paradigm\\|paradigmatic\\|paradigms\\|parody\\|parodic\\|parodies\\|physicality\\|plenitude\\|poetics\\|popular notions\\|position\\|post hoc\\|postmodernism\\|postmodernist\\|postmodernity\\|postmodern\\|practice\\|practise\\|praxis\\|problematic\\|problematics\\|proposition\\|qua\\|reading\\|readings\\|reification\\|relation\\|relational\\|relationality\\|relations\\|representation\\|representations\\|rhizomatic\\|rhizome\\|situate\\|situated\\|situates\\|stereotype\\|stereotypes\\|strategy\\|strategies\\|subjective\\|subjectivity\\|subjectivities\\|subvert\\|subversion\\|subverts\\|text\\|textual\\|textuality\\|thinker\\|thinkers\\|trajectory\\|transgress\\|transgresses\\|transgression\\|transgressive\\|unfolding\\|undermine\\|undermining\\|undermines\\|work\\|works\\|wry\\|wryly\\|zizekian\\|zižekian)\\b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-for-keyword (regex limit)
  "Match the provided regex in the buffer"
  (let ((match-data-to-set nil)
	found)
    (save-match-data
      (while (and (null match-data-to-set)
		  (re-search-forward regex limit t))
	    (setq match-data-to-set (match-data))))
    (when match-data-to-set
      (set-match-data match-data-to-set)
      (goto-char (match-end 0)) 
      t)))

(defun lexical-illusions-search-for-keyword (limit)
  (search-for-keyword lexical-illusions-regex limit))

(defun passive-voice-search-for-keyword (limit)
  (search-for-keyword passive-voice-regex limit))

(defun weasel-words-search-for-keyword (limit)
  (search-for-keyword weasel-words-regex limit))

(defun artbollocks-search-for-keyword (limit)
  (search-for-keyword artbollocks-regex limit))

(defconst lexicalkwlist '((lexical-illusions-search-for-keyword 
			   (2 'font-lock-lexical-illusions-face t))))

(defconst passivekwlist '((passive-voice-search-for-keyword 
			      (0 'font-lock-passive-voice-face t))))

(defconst weaselkwlist '((weasel-words-search-for-keyword 
			     (0 'font-lock-weasel-words-face t))))

(defconst artbollockskwlist '((artbollocks-search-for-keyword 
				  (0 'font-lock-artbollocks-face t))))

(defun add-artbollocks-keywords ()
  (when lexical-illusions
    (font-lock-add-keywords nil lexicalkwlist))
  (when passive-voice
    (font-lock-add-keywords nil passivekwlist))
  (when weasel-words
    (font-lock-add-keywords nil weaselkwlist))
  (when artbollocks
    (font-lock-add-keywords nil artbollockskwlist)))

(defun remove-artbollocks-keywords ()
  (font-lock-remove-keywords nil lexicalkwlist)
  (font-lock-remove-keywords nil passivekwlist)
  (font-lock-remove-keywords nil weaselkwlist)
  (font-lock-remove-keywords nil artbollockskwlist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text metrics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-letters ()
  (how-many "\\w" (point-min) (point-max)))

(defun count-syllables ()
  ;; Naively count vowel runs as syllable markers
  (how-many "[aeiouy]+" (point-min) (point-max)))

(defun count-words ()
  (how-many "\\w+" (point-min) (point-max)))

(defun count-sentences()
  ;; Avoid 8.8 but count this... as a sentence break
  (how-many "\\w[!?.]" (point-min) (point-max)))

;; FIXME: Avoid divide by zero where document is empty or small

(defun automated-readability-index ()
  (let ((words (count-words)))
    (- (+ (* 4.71 (/ (count-letters) words))
	  (* 0.5 (/ words (count-sentences))))
       21.43)))

(defun flesch-reading-ease ()
  (let ((words (count-words)))
    (- 206.834
       (* 1.015 (/ words (count-sentences)))
       (* 84.6 (/ (count-syllables) words)))))

(defun flesch-kinkaid-grade-level ()
  (let ((words (count-words)))
    (- (+ (* 11.8 (/ (count-syllables) words))
	  (* 0.39 (/ words (count-sentences))))
       15.59)))

(defun word-count ()
  "count the number of words in the buffer"
  (interactive)
  (message "Word count: %s" (count-words)))

(defun sentence-count ()
  "count the number of sentences in the buffer"
  (interactive)
  (message "Sentence count: %s" (count-sentences)))

(defun readability-index ()
  "determine the automated readability index of the buffer"
  (interactive)
  (message "Readability index: %s" (automated-readability-index)))

(defun reading-ease ()
  "determine the Flesch reading ease of the buffer"
  (interactive)
  (message "Reading ease: %s" (flesch-reading-ease)))

(defun grade-level ()
  "determine the Flesch-Kinkaid grade level of the buffer"
  (interactive)
  (message "Grade level: %s" (flesch-kinkaid-grade-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst artbollocks-mode-keymap (make-keymap))
(define-key artbollocks-mode-keymap (kbd "C-c [") 'word-count)
(define-key artbollocks-mode-keymap (kbd "C-c ]") 'sentence-count)
(define-key artbollocks-mode-keymap (kbd "C-c \\") 'readability-index)
(define-key artbollocks-mode-keymap (kbd "C-c /") 'reading-ease)
(define-key artbollocks-mode-keymap (kbd "C-c =") 'grade-level)

;;;###autoload
(define-minor-mode artbollocks-mode "highlight passive voice, weasel words and artbollocks in text, provide useful text metrics"
  :lighter " AB"
  :keymap artbollocks-mode-keymap
  :group 'artbollocks-mode
  (if artbollocks-mode
      (add-artbollocks-keywords)
    (remove-artbollocks-keywords)))

(defun turn-on-artbollocks-mode ()
  "turn artbollocks-mode on"
  (interactive)
  (artbollocks-mode 1))

(provide 'artbollocks-mode)

;; TODO
;; Toggle adding word/sentence count to status bar
;; Pluralization
;; Incorporate diction commands if available (and advise on installation if not)
;; Split general writing back out
