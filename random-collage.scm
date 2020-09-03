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
    (gimp-drawable-edit-clear collage-layer)))


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
         source-piece-limits-as-percentage)
  (let* ((min-source-piece-height-as-percentage
          (cadr (assoc 'min-source-piece-height-as-percentage source-piece-limits-as-percentage)))
         (min-source-piece-width-as-percentage
          (cadr (assoc 'min-source-piece-width-as-percentage  source-piece-limits-as-percentage)))
         (max-source-piece-height-as-percentage
          (cadr (assoc 'max-source-piece-height-as-percentage source-piece-limits-as-percentage)))
         (max-source-piece-width-as-percentage
          (cadr (assoc 'max-source-piece-width-as-percentage  source-piece-limits-as-percentage)))
         (source-layer-height (car (gimp-drawable-height source-layer)))
         (source-layer-width  (car (gimp-drawable-width  source-layer)))
         (min-source-piece-height
          (trunc (round (* source-layer-height (/ min-source-piece-height-as-percentage 100)))))
         (min-source-piece-width
          (trunc (round (* source-layer-width  (/ min-source-piece-width-as-percentage  100)))))
         (max-source-piece-height
          (trunc (round (* source-layer-height (/ min-source-piece-height-as-percentage 100)))))
         (max-source-piece-width
          (trunc (round (* source-layer-width  (/ min-source-piece-width-as-percentage  100)))))
         (absolute-source-piece-limits `((min-source-piece-height ,min-source-piece-height)
                                         (min-source-piece-width  ,min-source-piece-width)
                                         (max-source-piece-height ,max-source-piece-height)
                                         (max-source-piece-width  ,max-source-piece-width))))
    absolute-source-piece-limits))


; Place pieces randomly on to the collage layer
(define (randomly-place-pieces
         given-image
         given-layer
         collage-layer
         source
         num-pieces
         source-piece-limits-as-percentage)
  (let* ((source-layer
          (cond
           ((equal? source 0) given-layer)
           ((equal? source 1) (create-new-source-layer-from-clipboard
                               given-image))))
         (absolute-source-piece-limits
          (get-absolute-source-piece-limits
           given-image
           source-layer
           source-piece-limits-as-percentage)))))


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
  (gimp-image-undo-group-start given-image)
  (let* ((old-selection (car (gimp-selection-save given-image)))
         ; Create an alist of limits, for convenience in passing around all over the place
         (source-piece-limits-as-percentage
          `((min-source-piece-height-as-percentage ,min-source-piece-height-as-percentage)
            (min-source-piece-width-as-percentage ,min-source-piece-width-as-percentage)
            (max-source-piece-height-as-percentage ,max-source-piece-height-as-percentage)
            (max-source-piece-width-as-percentage ,max-source-piece-width-as-percentage)))
         (collage-layer (create-collage-layer given-image given-layer)))
    (randomly-place-pieces
     given-image
     given-layer
     collage-layer
     source
     num-pieces
     source-piece-limits-as-percentage)
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
                    SF-ADJUSTMENT "Max source piece height as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece width as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-random-collage" "<Image>/Filters/Artistic")
