; Random collage
;
; This script will place randomly distorted portions of the active layer
; on to a new transparent layer.
;
; Based on user-configurable parameters, the script will change the portions
; it copies in various ways, such as rotating, growing, shrinking, and shearing
; them before placing them on to the new layer.
;
; The original layer will be unaffected.
;
; ===========================================================================
;
; LICENSE
;
; Copyright (C) 2020 - Sergey Goldgaber
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; ===========================================================================


(define (choose-random-x-on-layer given-layer)
  (let* ((width (car (gimp-drawable-width given-layer)))
         (random-x (random width)))
    random-x))


(define (choose-random-y-on-layer given-layer)
  (let* ((height (car (gimp-drawable-height given-layer)))
         (random-y (random height)))
    random-y))


(define (choose-random-piece-height absolute-source-piece-limits)
  (let* ((min-source-piece-height
          (cadr (assoc 'min-source-piece-height absolute-source-piece-limits)))
         (max-source-piece-height
          (cadr (assoc 'max-source-piece-height absolute-source-piece-limits)))
         (difference
          (+ 1 (- max-source-piece-height min-source-piece-height)))
         (random-height
          (+ (- min-source-piece-height 1) (random difference))))
    random-height))


(define (choose-random-piece-width absolute-source-piece-limits)
  (let* ((min-source-piece-width
          (cadr (assoc 'min-source-piece-width absolute-source-piece-limits)))
         (max-source-piece-width
          (cadr (assoc 'max-source-piece-width absolute-source-piece-limits)))
         (difference
          (+ 1 (- max-source-piece-width min-source-piece-width)))
         (random-width
          (+ (- min-source-piece-width 1) (random difference))))
    random-width))


(define (copy-random-piece-from-source
         given-image
         source-layer
         absolute-source-piece-limits)
  (gimp-image-set-active-layer given-image source-layer)
  (let* ((random-piece-height
          (choose-random-piece-height absolute-source-piece-limits))
         (random-piece-width
          (choose-random-piece-width absolute-source-piece-limits))
         (random-x-on-source-layer
          (choose-random-x-on-layer
           source-layer))
         (random-y-on-source-layer
          (choose-random-y-on-layer
           source-layer))
         (ignored
          (gimp-image-select-rectangle
           given-image
           CHANNEL-OP-SUBTRACT
           random-x-on-source-layer
           random-y-on-source-layer
           random-piece-width
           random-piece-height))
         (random-piece-from-source
          (car (gimp-edit-named-copy
                source-layer
                "random-collage random piece from source"))))
    random-piece-from-source))


(define (create-collage-layer given-image given-layer)
  ; Clear the selection, so we can make the collage layer transparent later
  (gimp-selection-none given-image)
  (let* ((collage-layer-opacity 100)
         (collage-layer-mode LAYER-MODE-NORMAL)
         ; Create a new collage layer
         (collage-layer (car (gimp-layer-new
                                 given-image
                                 (car (gimp-drawable-width given-layer))
                                 (car (gimp-drawable-height given-layer))
                                 RGB-IMAGE
                                 "Random Collage"
                                 collage-layer-opacity
                                 collage-layer-mode)))
         ; Collage layer parameters (used for layer insertion below)
         (collage-layer-parent    0)  ;  0 = Outside any group
         (collage-layer-position -1)) ; -1 = Above active layer
    (gimp-layer-add-alpha collage-layer)
    ; Make the new collage layer visible
    (gimp-image-insert-layer
     given-image
     collage-layer
     collage-layer-parent
     collage-layer-position)
    (gimp-drawable-edit-clear collage-layer)
    collage-layer))


(define (create-new-source-layer-from-clipboard given-image)
  (let* ((active-drawable (car (gimp-image-get-active-drawable given-image)))
         (floating-selection (car (gimp-edit-paste active-drawable FALSE)))
         (ignored (gimp-floating-sel-to-layer floating-selection))
         (source-layer (car (gimp-image-active-drawable given-image))))
    (gimp-item-set-name source-layer "Random Collage source layer")
    source-layer))


; Convert min and max source piece limits from percentages of source layer
; to absolute values in pixels based on the actual size of the source layer
;
; Note: These values are converted to integers, because we can't work with fractions of a pixel.
(define (get-absolute-source-piece-limits
         given-image
         source-layer
         source-piece-limits-as-percentages)
  (let* ((min-source-piece-height-as-percentage
          (cadr (assoc 'min-source-piece-height-as-percentage source-piece-limits-as-percentages)))
         (min-source-piece-width-as-percentage
          (cadr (assoc 'min-source-piece-width-as-percentage  source-piece-limits-as-percentages)))
         (max-source-piece-height-as-percentage
          (cadr (assoc 'max-source-piece-height-as-percentage source-piece-limits-as-percentages)))
         (max-source-piece-width-as-percentage
          (cadr (assoc 'max-source-piece-width-as-percentage  source-piece-limits-as-percentages)))
         (source-layer-height (car (gimp-drawable-height source-layer)))
         (source-layer-width  (car (gimp-drawable-width  source-layer)))
         (min-source-piece-height
          (trunc (round (* source-layer-height (/ min-source-piece-height-as-percentage 100)))))
         (min-source-piece-width
          (trunc (round (* source-layer-width  (/ min-source-piece-width-as-percentage  100)))))
         (max-source-piece-height
          (trunc (round (* source-layer-height (/ max-source-piece-height-as-percentage 100)))))
         (max-source-piece-width
          (trunc (round (* source-layer-width  (/ max-source-piece-width-as-percentage  100)))))
         (absolute-source-piece-limits `((min-source-piece-height ,min-source-piece-height)
                                         (min-source-piece-width  ,min-source-piece-width)
                                         (max-source-piece-height ,max-source-piece-height)
                                         (max-source-piece-width  ,max-source-piece-width))))
    absolute-source-piece-limits))


; Determine whether to use the active layer or the contents of the clipboard
; as a source
(define (get-source-layer source given-image given-layer)
  (cond
   ((equal? source 0) given-layer)
   ((equal? source 1) (create-new-source-layer-from-clipboard
                       given-image))))


; Pack source-piece-limits-as-percentages in to an alist, for ease of passing around later
(define (get-source-piece-limits-as-percentages
         min-source-piece-height-as-percentage
         min-source-piece-width-as-percentage
         max-source-piece-height-as-percentage
         max-source-piece-width-as-percentage)
  `((min-source-piece-height-as-percentage ,min-source-piece-height-as-percentage)
    (min-source-piece-width-as-percentage  ,min-source-piece-width-as-percentage)
    (max-source-piece-height-as-percentage ,max-source-piece-height-as-percentage)
    (max-source-piece-width-as-percentage  ,max-source-piece-width-as-percentage)))


(define get-type
  (lambda (x)
    (cond
     ((null? x) "null")
     ((char? x) "char")
     ((list? x) "list")
     ((number? x) "number")
     ((pair? x) "pair")
     ((string? x) "string")
     ((symbol? x) "symbol")
     ((vector? x) "vector")
     (#t "unknown type"))))


(define (randomly-place-piece image layer piece)
  (let* ((random-x-on-layer
          (choose-random-x-on-layer layer))
         (random-y-on-layer
          (choose-random-y-on-layer layer))
         (piece-height
          (car (gimp-buffer-get-height piece)))
         (piece-width
          (car (gimp-buffer-get-width piece)))
         (ignored
          (gimp-image-select-rectangle
           image
           CHANNEL-OP-REPLACE
           random-x-on-layer
           random-y-on-layer
           piece-width
           piece-height))
         (paste-name
          (gimp-edit-named-paste
           layer
           piece
           FALSE))
         (active-drawable
          (car
           (gimp-image-get-active-drawable image))))
    (gimp-floating-sel-anchor active-drawable)))


; Place pieces randomly on to the collage layer
;
; This function merely gets the source layer and absolute source-piece-limits
; then hands off to randomly-place-pieces-aux, which does the real work
(define (randomly-place-pieces
         given-image
         given-layer
         collage-layer
         source
         num-pieces
         source-piece-limits-as-percentages)
  (let* ((source-layer
          (get-source-layer source given-image given-layer))
         (absolute-source-piece-limits
          (get-absolute-source-piece-limits
           given-image
           source-layer
           source-piece-limits-as-percentages)))
    (randomly-place-pieces-aux
     given-image
     collage-layer
     source-layer
     num-pieces
     absolute-source-piece-limits)))


; The real work of getting and placing pieces is done here
(define (randomly-place-pieces-aux
         given-image
         collage-layer
         source-layer
         num-pieces
         absolute-source-piece-limits)
  (let loop ((i 0))
    (if (< i num-pieces)
        (let ((random-piece-copied-from-source
               (copy-random-piece-from-source
                given-image
                source-layer
                absolute-source-piece-limits)))
          (randomly-place-piece
           given-image
           collage-layer
           random-piece-copied-from-source)
          (loop (+ i 1))))))


; Main entry point in to the script
; It is registered using script-fu-register and script-fu-menu-register below
(define (script-fu-random-collage
         given-image
         given-layer
         source
         num-pieces
         min-source-piece-height-as-percentage
         min-source-piece-width-as-percentage
         max-source-piece-height-as-percentage
         max-source-piece-width-as-percentage)
  ; Sanity check the heights and widths chosen by the user
  (cond
   ((<= max-source-piece-height-as-percentage min-source-piece-height-as-percentage)
    (gimp-message "Error: Max source piece height is not greater than min source piece height.")
    (quit))
   ((<= max-source-piece-width-as-percentage min-source-piece-width-as-percentage)
    (gimp-message "Error: Max source piece width is not greater than min source piece width.")
    (quit)))
  (gimp-image-undo-group-start given-image)
  (let* ((old-selection (car (gimp-selection-save given-image)))
         ; Create an alist of limits, for convenience in passing around all over the place
         (source-piece-limits-as-percentages
          (get-source-piece-limits-as-percentages
           min-source-piece-height-as-percentage
           min-source-piece-width-as-percentage
           max-source-piece-height-as-percentage
           max-source-piece-width-as-percentage))
         (collage-layer (create-collage-layer given-image given-layer)))
    (randomly-place-pieces
     given-image
     given-layer
     collage-layer
     source
     num-pieces
     source-piece-limits-as-percentages)
    ; Restore old selection
    (gimp-image-select-item given-image CHANNEL-OP-REPLACE old-selection))
  (gimp-image-undo-group-end given-image)
  (gimp-displays-flush))


(script-fu-register "script-fu-random-collage"
                    "Random Collage..."
                    "Place randomly distorted portions of active layer on new transparent layer"
                    "Sergey Goldgaber"
                    "Copyright 2020, Sergey Goldgaber"
                    "Sep 1, 2020"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-OPTION "Source" '("Active layer" "Clipboard")
                    SF-ADJUSTMENT "Number of pieces" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Min source piece height as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Min source piece width as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece height as percentage of source image" '(20 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece width as percentage of source image" '(20 1 100 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-random-collage" "<Image>/Filters/Artistic")
