#!/bin/sh

usage="Usage: clip_xyz.sh -I input_xyz -M input polygon [-O output_xyz] [-S cellsize] [-D xyz_delimiter] [-i] [-V] [-Q]
\n\n\
Clip an xyz data file according to the given OGR compatible polygon file.  The default will return xyz data points which are inside of the given polygon, to return the data points which are outside the given polygon, use the -i switch to invert the clipping.
\n\
"

#--
# Getopts
#--
while getopts ":I:M:O:iS:D:V" options; do
    case $options in
	I ) inxyz=$OPTARG;;
	O ) outxyz=$OPTARG;;
	M ) maskpoly=$OPTARG;;
	D ) delim=$OPTARG;;
	S ) inc=$OPTARG;;
	i ) invert=True;;
	V ) verbose=True;;
	Q ) quickly=True;;
	h ) echo -e $usage;;
	\? ) echo -e $usage
	exit 1;;
	* ) echo -e $usage
	    exit 1;;
    esac
done

# Check for variables and set them to default 
# values if they aren't set by the user.
if [ ! $inxyz ] || [ ! $maskpoly ] ; then
    echo -e $usage
    exit 1
fi

if [ ! $outxyz ] ; then
    outxyz=${inxyz}_clip.xyz
fi

if [ ! $delim ] ; then
    delim=""
else
    delim="-delimiter ${delim}"
fi

if [ ! $inc ] ; then
    inc=0.00009259259
fi

# DEFS
msk=$(basename $inxyz .xyz)_msk.tif

if [ $verbose ] ; then
    echo $msk
fi
#Generate the null grid
if [ $verbose ] ; then
    gdal_null.py -region $(gmt minmax -C $inxyz | awk '{print $1,$2,$3,$4}') -cell_size $inc -verbose -overwrite $msk
else
    gdal_null.py -region $(gmt minmax -C $inxyz | awk '{print $1,$2,$3,$4}') -cell_size $inc -overwrite $msk
fi

#Burn the polygon onto the null grid - use -i to invert the polygon

if [ $invert ] ; then
    gdal_rasterize -i -burn 1 -l $(basename $maskpoly .shp) $maskpoly $msk
else
    gdal_rasterize -burn 1 -l $(basename $maskpoly .shp) $maskpoly $msk
fi

if [ $quickly ] ; then
    xyz_clip.py $delim -quickly $inxyz $msk $outxyz
else
    xyz_clip.py $delim $inxyz $msk $outxyz
fi

if [ $verbose ] ; then
    echo "Completed"
fi
## END
