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


(define (create-collage-layer given-image)
  ; Clear the selection, so we can make the collage later transparent later
  (gimp-selection-none given-image)
  (let* ((selection-lower-right-bounds (get-lower-right-bounds given-image))
         (selection-lower-right-x (car selection-lower-right-bounds))
         (selection-lower-right-y (cadr selection-lower-right-bounds))
         (collage-layer-opacity 100)
         (collage-layer-mode LAYER-MODE-NORMAL)
         ; Create a new collage layer
         (collage-layer (car (gimp-layer-new
                                 given-image
                                 selection-lower-right-x
                                 selection-lower-right-y
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


; Return the lower-right-x and lower-right-y of the given selection
; or of the image, if nothing is selected
(define (get-lower-right-bounds image)
  (let* ((selection-bounds (gimp-selection-bounds image))
         (selection-non-empty (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-y (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-y (head selection-bounds)))
    (list selection-lower-right-x selection-lower-right-y)))


; Get random pieces from the active layer
(define (get-random-pieces
         given-image
         given-layer
         num-pieces
         min-source-piece-size
         max-source-piece-size))


; Return the upper-left-x and upper-left-y of the given selection
; or of the image, if nothing is selected
(define (get-upper-left-bounds image)
  (let* ((selection-bounds (gimp-selection-bounds image))
         (selection-non-empty (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-y (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-y (head selection-bounds)))
    (list selection-upper-left-x selection-upper-left-y)))


; Place pieces randomly on to the collage layer
(define (randomly-place-pieces given-image collage-layer random-pieces))


(define (script-fu-random-collage
         given-image
         given-layer
         num-pieces
         min-source-piece-size
         max-source-piece-size)
  (gimp-image-undo-group-start given-image)
  (let* ((old-selection (car (gimp-selection-save given-image)))
         (random-pieces (get-random-pieces
                         given-image
                         given-layer
                         num-pieces
                         min-source-piece-size
                         max-source-piece-size))
         (collage-layer (create-collage-layer
                            given-image)))
    (randomly-place-pieces given-image collage-layer random-pieces)
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
                    SF-ADJUSTMENT "Number of pices" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Min source piece size as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Max source piece size as percentage of source image" '(10 1 100 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-random-collage" "<Image>/Filters/Artistic")
