;;; dem-build.scm - generate a shell script to build a digital elevation model.
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
;; Usage: dem-build [ -hv ] [ -X cells ] [ -E increment ] [ -C grid-cmd ] [ -N Name ] -R [ xmin/xmax/ymin/ymax / vector-file ] -I datalist
;;
;; d e m - b u i l d
;;
;; Generate a shell script to build a Digital Elevation Model.
;;
;; GMT and gdal are required for the output shell script to run.
;;
;; The -R switch specifies the region to grid, this can either be a GMT-style region ( -Rxmin/xmax/ymin/ymax ) 
;; or a GMT-formatted vector file with regional polygons. If a vector file is supplied, will generate DEM
;; commands for each region found in the vector file.
;;
;; Use 'dem fuzzy-regions to generate a tile-set vector file.
;;
;; The increment, specified with the -E swich, should be in the style of GMT commands, e.g. "1s" for 1 arc second, etc.
;;
;; Using the -X switch, the region will be expanded by 'cells' cells.
;; e.g. -X 6.5 will extend the region by 6.5 cells
;;
;; The -C switch specifies whether to make the dem with 'surface or 'mbgrid; default is 'surface
;;
;;; Code:

(define-module (geographic scripts dem dem-build)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (geographic ogr-gmt)
  #:use-module (geographic regions)
  #:use-module (geographic rasters)
  #:use-module (geographic spatial)
  #:use-module (geographic dem gmt)
  #:use-module (geographic dem world-raster)
  #:export (dem-build))

(define dem-build-version "0.0.4")

(define %summary "Build a DEM using GMT, etc.")

(define command-synopsis
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (expand (single-char #\X) (value #t))
    (name (single-char #\N) (value #t))
    (cmd (single-char #\C) (value #t))
    (inc (single-char #\E) (value #t))
    (inlist (single-char #\I) (value #t))
    (region (single-char #\R) (value #t))))

(define (display-help)
  (format #t "\
~a
d e m - b u i l d

usage: dem-build [ -hvEICNRX [args] ]
" %summary))

(define (display-version)
  (format #t "\
dem-build (guile-geographic) version ~a
Copyright (c) 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" dem-build-version))

(define (dem-build . args)
  (let* ((options (getopt-long (cons "dem-build" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (expand (option-ref options 'expand "0"))
	  (name (option-ref options 'name (gethostname)))
	  (cmd (option-ref options 'cmd "surface"))
	  (inc (option-ref options 'inc ".3333333s"))
	  (inlist (option-ref options 'inlist #f))
	  (region (option-ref options 'region #f)))
      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else
	(if (and region inlist)
	    (let* ((region-vector (if (file-exists? region) (open-file region "r") #f))
		   (regions (if region-vector 
				(ogr-gmt->scm region-vector) 
				(list (region->polygon (gmt-region->region region))))))
	      (format #t "#!/usr/bin/sh~%")
	      (format #t "## dem-build ~{~a ~}~%~%" args)
	      (dem-grid-regions 
	       regions inlist 
	       #:cmd (string->symbol cmd) 
	       #:name name 
	       #:inc inc 
	       #:expand (string->number expand)))

	    (display-help)))))))

(define main dem-build)

(define* (dem-grid-regions regions datalist-file 
			   #:key 
			   (cmd 'surface) 
			   (inc ".3333333s") 
			   (name (gethostname))
			   (vers "1")
			   (expand 0))
  (if (not (null? regions))
      (let* ((this-region (polygon->region (car regions) '()))
	     (expanded-region (region-expand this-region (gmt-inc->inc inc) #:cells expand))
	     (proc-region (region-expand this-region (gmt-inc->inc inc) #:cells (+ expand 5)))
	     (cgrid-region (region-expand this-region (gmt-inc->inc inc) #:cells (+ expand 20)))
	     (this-name (dem-output-name this-region #:inc inc #:name name #:version vers)))
	(case cmd
	  ((surface)
	   (format #t "## ~a@~a~%" this-name this-region)
	   ;; (format #t "datalist cat -R~a -I~a ~a | " 
	   ;; 	   (region->gmt-region proc-region) inc datalist-file)
	   (format #t "datalist cat -R~a ~a | " 
		   (region->gmt-region proc-region) datalist-file)
	   ;; (format #t "~a | " 
	   ;; 	   (dem-make-gmt-cmd 
	   ;; 	    "gmtselect" 
	   ;; 	    expanded-region 
	   ;; 	    #:verbose #t 
	   ;; 	    #:inc #f))
	   (format #t "~a > ~a.dat~%" 
		   (dem-make-gmt-cmd 
		    "blockmean" 
		    expanded-region 
		    #:inc inc 
		    #:verbose #t) this-name)
	   (format #t "~a ~a.dat~%" 
		   (dem-make-gmt-cmd 
		    "surface" 
		    expanded-region 
		    #:inc inc 
		    #:out-name this-name 
		    #:verbose #t 
		    #:extra "-T.3") this-name)
	   (format #t "#~a ~a.dat~%" 
		   (dem-make-gmt-cmd 
		    "xyz2grd" 
		    expanded-region 
		    #:inc inc 
		    #:out-name (format #f "~a" this-name)
		    #:verbose #t) this-name)
	   (format #t "#gmt grdconvert ~a_d.grd ~a_d.tif=gd:GTiff~%" this-name this-name)
	   (format #t "gmt grdconvert ~a.grd ~a.tif=gd:GTiff~%" this-name this-name)
	   (format #t "gdal_edit.py ~a.tif -a_nodata -9999~%" this-name)
	   (format #t "#gdal_edit.py ~a_d.tif -a_nodata -9999~%" this-name)
	   (format #t "dem hillshade ~a.tif~%" this-name)
	   (format #t "smooth_dem_bathy.py ~a.tif~%" this-name))
	  ((cgrid)
	   (set! this-name (string-append this-name "_nvd2mhw"))
	   (format #t "## ~a@~a~%" this-name this-region)
	   (format #t "gdal_null.py -overwrite -region ~a -cell_size 0.00027777 empty.tif~%" (string-join (map number->string cgrid-region) " "))
	   (format #t "gdal_edit.py -a_nodata -9999 empty.tif~%")
	   (format #t "gdal_translate empty.tif empty.xyz -of XYZ~%")
	   (format #t "dem vdatum -F --ivert navd88 --overt mhw empty.xyz~%")
	   (format #t "~a ./result/empty.xyz | " 
		   (dem-make-gmt-cmd 
		    "blockmean" 
		    expanded-region
		    #:inc inc 
		    #:verbose #t))
	   (format #t "~a~%" 
		   (dem-make-gmt-cmd 
		    "surface" 
		    expanded-region
		    #:inc inc 
		    #:out-name this-name
		    #:verbose #t 
		    #:extra "-T1"))
	   (format #t "gmt grdconvert ~a.grd ~a.tif=gd:GTiff~%" this-name this-name))
	  ((mbgrid)
	   (format #t "## ~a@~a~%" this-name this-region)
	   (format #t "~a~%" 
		   (dem-make-mbgrid-cmd 
		    expanded-region 
		    datalist-file 
		    #:inc inc 
		    #:out-name this-name 
		    #:verbose #t))
	   (format #t "gmt grdconvert ~a.grd ~a.tif=gd:GTiff~%" this-name this-name)))
	  
	(newline)
	(dem-grid-regions 
	 (cdr regions) 
	 datalist-file 
	 #:cmd cmd
	 #:inc inc 
	 #:name name 
	 #:vers vers 
	 #:expand expand))))

;;; End
