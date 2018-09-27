#!/usr/bin/env python
#
# Description: Check if a grid file contains any null values
#
# 	$Id: has_nulls.py,v 1.3 2010/11/01 20:17:57 mlove Exp $	
#--

#--
import sys
import struct
from osgeo import gdal
#--

#--
def Usage():
    print '''

+---------------+  has_nulls

Usage: has_nulls.py grd_datasource_name

This script will check a GDAL compatible grid file for null-values, 
and will print out the location (as y,x cell numbers) of the first
no-data value found, otherwise, will not print out anything.
'''
#--

def isNaN(num):
    return num != num

#--
#
# Mainline
#
#--
if __name__ == "__main__":

    if len(sys.argv) < 2:
        Usage()
        sys.exit()
    else:
        ingrd = sys.argv[1]

    ds = gdal.Open(ingrd)
    band = ds.GetRasterBand(1)
    ndata = band.GetNoDataValue()

    if isNaN(ndata):
        band.SetNoDataValue(-9999)
        ndata = band.GetNoDataValue()

    for i in range(0, band.YSize):
        scanline = band.ReadRaster(0,i,band.XSize, 1, band.XSize, 1, gdal.GDT_Float32)
        these_values = struct.unpack('f' * band.XSize, scanline)
        tv2 = list(these_values)
        try: 
            tv2.index(ndata)
            print "There is a NoData Value (%s) at y: %s x: %s." %(ndata, i, tv2.index(ndata))
            break
        except:
            tv2=None
    ds = None
#--END
