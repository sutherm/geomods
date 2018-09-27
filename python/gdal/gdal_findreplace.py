#!/usr/bin/env python
#
# Description: Find and replace a value in a gdal-compatible grid file
#
#--

#--
import sys
import os
import struct
import numpy as np
from osgeo import gdal
#--

gfr_version = 0.3

#--
def Usage():
    print('Usage: gdal_findreplace.py [-s_value value] [-t_value value] [-nodata]')
    print('                           [-row value] [-column value]')
    print('                           [-overwrite] src_grid dest_grid')
    print('')
    print('gdal_findreplace v.%s' %(gfr_version))
#--

#--
#
# Mainline
#
#--
if __name__ == "__main__":

    ingrd = None
    outgrd = None
    fdata = None
    rdata = None
    overwrite = False
    verbose = False
    mk_ndata = False
    row = None
    column = None
    
    gdal.AllRegister()
    argv = gdal.GeneralCmdLineProcessor( sys.argv )
    if argv is None:
        sys.exit(0)

    i = 1
    while i < len(argv):
        arg = argv[i]

        if arg == '-s_value':
            fdata = argv[i+1]
            i = i + 1

        elif arg == '-t_value':
            rdata = argv[i+1]
            i = i + 1

        elif arg == '-row':
            row = argv[i+1]
            i = i + 1
            
        elif arg == '-column':
            column = argv[i+1]
            i = i + 1

        elif arg == '-nodata':
            mk_ndata = True

        elif arg == '-overwrite':
            overwrite = True
            
        elif arg == '-verbose':
            verbose = True

        elif arg[0] == '-':
            Usage()

        elif ingrd is None:
            ingrd = arg

        elif outgrd is None:
            outgrd = arg

        else:
            Usage()

        i = i + 1

    if ingrd is None or outgrd is None:
        Usage()
        sys.exit(0)

    if verbose:
        print "%s--->%s" %(fdata,rdata)

    ds = gdal.Open(ingrd)
    band = ds.GetRasterBand(1)
    in_ndata = band.GetNoDataValue()
    if in_ndata is None:
        in_ndata = -9999
    comp_geot = ds.GetGeoTransform()
    outarray = ds.ReadAsArray()

    if fdata == 'all':
        outarray[outarray!=in_ndata]=rdata
    elif np.isnan(float(fdata)):
        outarray[np.isnan(outarray)]=rdata
    else:
        outarray[outarray==float(fdata)]=float(rdata)
    
    # Create the output GDAL Raster

    outfile = outgrd
    (ycount,xcount) = outarray.shape[0],outarray.shape[1]
    
    driver = gdal.GetDriverByName("GTiff")

    if os.path.exists(outfile):
        if not overwrite:
            sys.exit("Error - %s already exists!  Use -O or --overwrite to force an \
            overwrite of the output file." %(outfile))
        else:
            driver.Delete(outfile)
            if verbose:
                print "Warning - Overwriting file %s " %(outfile)
                
    dst_ds = driver.Create(outfile, xcount, ycount, 1, gdal.GDT_Float32)

    if dst_ds is None:
        sys.exit("failed to open output file...%s" %(outfile))
            
    gt = comp_geot #(xextent,comp_geot[1],0,yextent,0,comp_geot[5])
    dst_ds.SetGeoTransform(gt)
    dst_band = dst_ds.GetRasterBand(1)

    if mk_ndata is True:
        dst_band.SetNoDataValue( ndata )
    else:
        dst_band.SetNoDataValue( float(in_ndata) )
        
    # Write Numpy Array to the GDAL Raster Band
    dst_band.WriteArray(outarray)
        
    dst_ds = None
                                                                                                                
    ds = None
#--END
