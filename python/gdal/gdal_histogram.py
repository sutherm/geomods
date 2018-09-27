#!/usr/bin/env python

#  Description:  Return a histogram of the grid data
#

import sys
from osgeo import gdal
import numpy as np

_version = "0.0.1"

def Usage():
    print('Usage: gdal_minmax.py grdfile')
    print('')
    print('Will return xmin xmax ymin ymax zmin zmax')
    print('')
    print('gdal_minmax v.%s' %(grc_version))

def returnMinMax(ingrd):
    # Process the grid file
    ds = gdal.Open(ingrd)
    comp_geot = ds.GetGeoTransform()
    ds_band = ds.GetRasterBand(1)
    ndata = ds_band.GetNoDataValue( )
    cellsize = [float(comp_geot[1]), float(comp_geot[5])]
    xmin = float(comp_geot[3])
    ymax = float(comp_geot[0])
   # xmax = 
    print ndata
    band = ds.GetRasterBand(1)
    tgrid = band.ReadAsArray()
    #tgrid2 = np.delete(tgrid, ndata, 1)
    tgrid2 = np.delete(tgrid, ndata)

    hbin,hist = np.histogram(tgrid2,1000,new=True)
    #print tgrid
    for i,j in enumerate(hbin):
        tmp_val = str(hist[i]) + " " + str(j) + "\n"
        print tmp_val

    #print comp_geot


if __name__ == "__main__":

    if len(sys.argv) <= 1:
        Usage()
        sys.exit()
    else:
        infile = sys.argv[1]

    returnMinMax(infile)

# END
