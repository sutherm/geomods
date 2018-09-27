#!/bin/sh

usage="Usage: clip_gdal.sh [options]
\n\n\
Options:\n\
-I\tInput gdal file\n\
-M\tInput ogr mask file\n\
-O\tOutput clipped gdal file\n\
-i\tInvert the mask\n\
"

#--
# Getopts
#--
while getopts ":I:M:O:i" options; do
    case $options in
	I ) ingrd=$OPTARG;;
	O ) outgrd=$OPTARG;;
	M ) maskpoly=$OPTARG;;
	i ) invert=True;;
	h ) echo -e $usage;;
	\? ) echo -e $usage
	exit 1;;
	* ) echo -e $usage
	    exit 1;;
    esac
done

if [ ! $ingrd ] || [ ! $maskpoly ] ; then
    echo -e $usage
    exit 1
fi

if [ ! $outgrd ] ; then
    outgrd=${ingrd}_clip.tif
fi


##  \ 
##    $1 is the source grid
##    $2 is the mask polygon shapefile
##    $3 is the clipped grid (output)
##    $4 specifies whether or not to invert the mask
##  \

#----- DEFS
msk=$(basename $ingrd)_msk.tif

echo $msk

#----- PROC
## Make a null copy of the input grid, the copy will be the mask grid
gdal_null.py -copy $ingrd $msk -overwrite

## To clip bathy surface, remove the -i switch from gdal_rasterize, 
## add it back in for clipping landforms (i.e. NED)

if [ ! $invert ] ; then
    gdal_rasterize -burn 1 -l $(basename $maskpoly .shp) $maskpoly $msk
else
    gdal_rasterize -i -burn 1 -l $(basename $maskpoly .shp) $maskpoly $msk
fi

## apply the mask to the input grid, and write out the clipped version
gdal_mask.py -mask $msk $ingrd $outgrd

## Uncomment following command to output an xyz file.
#gdal2xyz.py $outgrd | grep -v "\-9999" > $(basename $outgrd .tif).xyz
echo "DONE"
#----- END