#!/usr/bin/env python
### gdal_crop.py
##
## Copyright (c) 2018 Matthew Love <matthew.love@colorado.edu>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy 
## of this software and associated documentation files (the "Software"), to deal 
## in the Software without restriction, including without limitation the rights 
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
## of the Software, and to permit persons to whom the Software is furnished to do so, 
## subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
## INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
## PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
## FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
## ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
##
### Commentary:
##
## crop a gdal grid by the nodata value
##
### Code:

import os
import sys
import numpy as np
from gdalconst import *
from osgeo import osr
from osgeo import gdal

try:
    progress = gdal.TermProgress_nocb
except:
    progress = gdal.TermProgress

_version = "0.0.1"

_license = """
version %s
    """ %(_version)

_usage = """
gdal_crop.py: crop a gdal grid by the nodata value

usage: gdal_crop.py [ file ]

Options:
  file\t\tThe input DEM file-name

  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
gdal_crop.py input.tif

gdal_crop.py v.%s 
""" %(_version)

if __name__ == '__main__':
    
    elev = None
    split_value = 0

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-help' or arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '-version' or arg == '--version':
            print('gdal_crop.py v.%s' %(_version))
            print(_license)
            sys.exit(1)

        elif elev is None:
            elev = arg

        else:
            print(_usage)
            sys.exit(1)

        i = i + 1

    if elev is None:
        print(_usage)
        sys.exit(1)

    if not os.path.exists(elev):
        print("Error: %s is not a valid file" %(elev))
    else:
        progress( 0.0 )
        output_name=elev[:-4]+"_crop.tif"
        elev_g = gdal.Open(elev) #
        
        # get input information
        NDV = elev_g.GetRasterBand(1).GetNoDataValue()
        GeoT = elev_g.GetGeoTransform()
        DataType = elev_g.GetRasterBand(1).DataType
        elev_prj = elev_g.GetProjectionRef()
        elev_array = elev_g.GetRasterBand(1).ReadAsArray() 
        elev_g = None

        elev_array[elev_array == NDV] = np.nan # set NDV to np.nan
        progress(.5)
        nans = np.isnan(elev_array)
        nancols = np.all(nans, axis=0) # True where col is all NAN
        nanrows = np.all(nans, axis=1) # True where row is all NAN
        
        firstcol = nancols.argmin() # the first index where not NAN
        firstrow = nanrows.argmin() # the first index where not NAN
        
        lastcol = len(nancols) - nancols[::-1].argmin() # 8, last index where not NAN
        lastrow = len(nanrows) - nanrows[::-1].argmin() # 10

        out_array = elev_array[firstrow:lastrow,firstcol:lastcol]
        elev_array = None
        out_array[np.isnan(out_array)] = NDV

        out_xorigin = GeoT[0] + (GeoT[1] * firstcol)
        out_yorigin = GeoT[3] + (GeoT[5] * firstrow)
        out_GeoT = [out_xorigin, GeoT[1], 0.0, out_yorigin, 0.0, GeoT[5]]
        outsize = out_array.shape


        #Export Tif
        ods = gdal.GetDriverByName('GTiff').Create( output_name, outsize[1], outsize[0], 1, DataType )
        ods.SetGeoTransform(out_GeoT)
        ods.SetProjection(elev_prj)
        ods.GetRasterBand(1).SetNoDataValue(NDV)
        ods.GetRasterBand(1).WriteArray( out_array )
        progress ( 1 )
### End
