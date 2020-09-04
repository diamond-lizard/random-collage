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
           CHANNEL-OP-REPLACE
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


(define (randomly-place-piece
         image
         layer
         piece
         rotate
         resize
         min-resize
         max-resize
         shear
         min-shear
         max-shear)
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
           (gimp-image-get-active-drawable image)))
         (center-x
          (trunc (round (/ piece-width 2))))
         (center-y
          (trunc (round (/ piece-height 2))))
         (active-drawable
          (if resize
                                        ; We need to scale up the min and max resize values
                                        ; to integers, beacuse that's what the random function can generate.
                                        ; After the random function we can convert back to floats.
                                        ;
                                        ; NOTE: The scaling is a bit of a hack, as we hardcode the scale amount here
                                        ;       instead of determining it dynamically.
              (let* ((scaled-min-resize (* 1000 min-resize))
                     (scaled-max-resize (* 1000 max-resize))
                     (scaled-difference
                      (+ 1 (- scaled-max-resize scaled-min-resize)))
                     (random-scale-amount
                      (/
                       (+ (- scaled-min-resize 1)
                          (random scaled-difference))
                       1000))
                     (angle 0))
                (car (gimp-item-transform-2d
                      active-drawable
                      center-x
                      center-y
                      random-scale-amount
                      random-scale-amount
                      angle
                      center-x
                      center-y)))
              active-drawable)))
    (if rotate
        (let* ((piece-height
                (car (gimp-drawable-height active-drawable)))
               (piece-width
                (car (gimp-drawable-width active-drawable)))
               (center-x
                (trunc (round (/ piece-width 2))))
               (center-y
                (trunc (round (/ piece-height 2))))
               (max-rotation-angle 359)
               (rotation-angle (random max-rotation-angle)))
          (gimp-item-transform-rotate
           active-drawable
           rotation-angle
           TRUE
           center-x
           center-y)))
    (if shear
        (let* ((shear-type
                (- (random 2) 1))
               (difference
                (+ 1 (- max-shear min-shear)))
               (magnitude
                (+ (- min-shear 1) (random difference))))
          (gimp-item-transform-shear
           active-drawable
           shear-type
           magnitude)))
    (gimp-floating-sel-anchor active-drawable)))


; Place pieces randomly on to the collage layer
;
; This function merely gets the source layer and absolute source-piece-limits
; then hands off to randomly-place-pieces-aux, which does the real work
(define (randomly-place-pieces
         given-image
         given-layer
         collage-layer
         source-layer
         num-pieces
         source-piece-limits-as-percentages
         rotate
         resize
         min-resize
         max-resize
         shear
         min-shear
         max-shear)
  (let* ((absolute-source-piece-limits
          (get-absolute-source-piece-limits
           given-image
           source-layer
           source-piece-limits-as-percentages)))
    (randomly-place-pieces-aux
     given-image
     collage-layer
     source-layer
     num-pieces
     absolute-source-piece-limits
     rotate
     resize
     min-resize
     max-resize
     shear
     min-shear
     max-shear)))


; The real work of getting and placing pieces is done here
(define (randomly-place-pieces-aux
         given-image
         collage-layer
         source-layer
         num-pieces
         absolute-source-piece-limits
         rotate
         resize
         min-resize
         max-resize
         shear
         min-shear
         max-shear)
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
           random-piece-copied-from-source
           rotate
           resize
           min-resize
           max-resize
           shear
           min-shear
           max-shear)
          (loop (+ i 1))))))


; Main entry point in to the script
; It is registered using script-fu-register and script-fu-menu-register below
(define (script-fu-random-collage
         given-image
         given-layer
         num-pieces
         min-source-piece-height-as-percentage
         max-source-piece-height-as-percentage
         min-source-piece-width-as-percentage
         max-source-piece-width-as-percentage
         rotate
         resize
         min-resize
         max-resize
         shear
         min-shear
         max-shear)
  ; Sanity check the heights and widths chosen by the user
  (cond
   ((<= max-source-piece-height-as-percentage min-source-piece-height-as-percentage)
    (gimp-message "Error: Max source piece height is not greater than min source piece height.")
    (quit))
   ((<= max-source-piece-width-as-percentage min-source-piece-width-as-percentage)
    (gimp-message "Error: Max source piece width is not greater than min source piece width.")
    (quit)))
  (cond
   ((<= max-resize min-resize)
    (gimp-message "Error: Max resize is not greater than min resize.")
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
     given-layer
     num-pieces
     source-piece-limits-as-percentages
     rotate
     resize
     min-resize
     max-resize
     shear
     min-shear
     max-shear)
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
                    "RGB, RGBA"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-ADJUSTMENT "Number of pieces" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Min source piece height % (Must be < Max height %)" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece height % (Must be > Min height %)" '(20 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Min source piece width % (Must be < Max width %)" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece width % (Must be > than Min height %)" '(20 1 100 1 10 0 SF-SPINNER)
                    SF-TOGGLE "Rotate?" TRUE
                    SF-TOGGLE "Resize?" TRUE
                    SF-ADJUSTMENT "Min resize (Must be < Max resize)" '(0.1 0.1 2 0.1 0.5 1 SF-SPINNER)
                    SF-ADJUSTMENT "Max resize (Must be > Min resize)" '(2 0.1 2 0.1 0.5 1 SF-SPINNER)
                    SF-TOGGLE "Shear?" TRUE
                    SF-ADJUSTMENT "Min shear (Must be < Max shear)" '(1 1 1000 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max shear (Must be > Min shear)" '(200 1 1000 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-random-collage" "<Image>/Filters/Artistic")
