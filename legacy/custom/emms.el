;;; emms.el --- Emacs Multimedia System setup

(require 'emms)

;; Setup default values (players...)
(require 'emms-default)

;; Setup
(emms-setup 'level "~/audio/music")

(setq emms-source-list '((emms-source-directory-tree "~/audio/music")))

;; Show the current track each time EMMS
;; starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")

;; Play next song when emms is stoppde interactivily
(add-hook 'emms-player-finished-hook 'emms-next-noerror)

;; When asked for emms-play-directory,
;; always start from this one
(setq emms-source-file-default-directory "~/audio/music/")

;; Sets alsaplayer
(define-emms-simple-player alsa
  (regexp-opt '(".ogg" ".OGG" "mp3" "mpg3" ".FLAC" ".flac" ))
  "alsaplayer")

(setq emms-player-list
      '(emms-player-alsa emms-player-mpg321 emms-player-mplayer))

;; Use ALSA with mpg321
(setq emms-player-mpg321-parameters '("-o" "alsa"))

;;; emms.el ends here
