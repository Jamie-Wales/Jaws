(import portaudio)

(init-audio)
(play-tone 440.0 0.5)  ; Play A4 at half volume
(delay 10)             ; Wait ~1 second
(stop-tone)

;; Or play the scale
(play-scale)
