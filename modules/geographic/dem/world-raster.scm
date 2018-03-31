;;*-scheme-*
;;; world-raster.scm
;;
;; Copyright (c) 2011, 2012, 2013, 2016, 2018 Matthew Love <matthew.love@colorado.edu>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;--------------------------;;
;; W o r l d . R a s t e r -;;
;;
;; This is the raster of the world at .25/.25 degree increment
;; in WGS84.                ;;
;;--------------------------;;
;;
;;; Code:

(define-module (geographic dem world-raster)
  #:use-module (geographic rasters)
  #:use-module (geographic regions)
  #:export (make-world-raster
	    wld-cells-cnt
	    snarf-wld-tiles
	    region->wld-cells
	    tiled-region))

;; Make a dummy world raster with .25 degree increment.
;; This raster can be queried to find regions for
;; NCEI tiled RASTERs
;; These functions should be moved to dem module.
(define (make-world-raster)
  (let ((grd (make-raster '(-180 180 -90 90) .25 .25 #:dummy #t)))
    grd))

;; Given a region-list and a tile-set (see make-world-raster) output
;; the pixel value of each corner of the region-list.
(define (region->wld-cells region wld-tiles)
  (let* ((ll (list (car region) (caddr region)))
	 (ul (list (cadr region) (caddr region)))
	 (ur (list (cadr region) (cadddr region)))
	 (lr (list (car region) (cadddr region)))
	 (llc (point->pixel ll wld-tiles))
	 (ulc (point->pixel ul wld-tiles))
	 (urc (point->pixel ur wld-tiles))
	 (lrc (point->pixel lr wld-tiles)))
    (list llc ulc urc lrc)))

;; Determine the size of a tiled-region given the cell-values for 
;; the region corners (see region->wld-cells)
(define (wld-cells-cnt wld-cells)
  (list (- (car (cadr wld-cells)) (car (car wld-cells)))
	(- (cadr (caddr wld-cells)) (cadr (car wld-cells)))))

;; Snarf out the geographic regions of the tiles in r-cells list and return a list of the regions.
;; see 'tiled-region.
(define (snarf-x-tiles wld-tiles r-cell cnt regions)
  (if (>= cnt 0)
      (let ((this-cell (list (car r-cell) (+ cnt (cadr r-cell)))))
	(snarf-x-tiles wld-tiles r-cell (1- cnt) (append (list (raster-index->region wld-tiles this-cell)) regions)))
      regions))

(define (snarf-wld-tiles wld-tiles r-cells c-cnt regions)
  (if (>= (car c-cnt) 0)
      (let* ((this-cell (list (+ (car c-cnt) (caar r-cells)) (cadar r-cells)))
	     (x-tiles (snarf-x-tiles wld-tiles this-cell (cadr c-cnt) regions)))
	(snarf-wld-tiles wld-tiles r-cells (list (1- (car c-cnt)) (cadr c-cnt)) x-tiles))
      regions))

;; determine the tiles which contain the region
;; rename this function.
(define (tiled-region region)
  "Tile a region based on the world-raster."
  (if (not (region? region)) (error "invalid region: " region))
  (let* ((wld-tiles (make-world-raster))
	 (r-cells (region->wld-cells region wld-tiles))
	 (c-cnt (wld-cells-cnt r-cells)))
    (if (or (< (car c-cnt) 0) (< (cadr c-cnt) 0))
	(error "invalid region: " region))
    (snarf-wld-tiles wld-tiles r-cells c-cnt '())))

;;; End
