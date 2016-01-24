import os
import arcpy
import glob

d = "C:/Users/Emily Burchfield/Box Sync/GIS/Sri Lanka/Vector/Geo Spatial Shapefiles/reproj"

fo = os.listdir(d)
inprj = "C:/Users/Emily Burchfield/Box Sync/GIS/Kandawala Sri Lanka Grid.prj"
outprj = "C:/Users/Emily Burchfield/Box Sync/GIS/WGS 1984 UTM Zone 44N.prj" 


for f in range(len(fo)):
	fi = sorted(glob.glob(d + '/' + fo[f] + '/' + '*.shp'))
	for i in range(len(fi)):
		out = os.path.join(d + '/'+ 'NBRO' + '/' + fo[f] + '/' + fn[1][1:])	
		inf = fi[i].replace('\\','/')
		subprocess.call(['org2org', '-t_srs', 'EPSG:32644', out, inf])
		#system cannot find file specified
		
		
		arcpy.DefineProjection_management(fi[i], inprj)
		fn = fi[i].split(fo[i])
		out = d + '/NBRO' + '/' + fo[f] + fn[1]		
		arcpy.Project_management(fi[i], out, outprj)
		


#command line version
		
		
		import osr
import gdal

#http://geoinformaticstutorial.blogspot.com/2012/10/reprojecting-shapefile-with-gdalogr-and.html

# set file names   
infile = fi[i]
outfile = out
(outfilepath, outfilename) = os.path.split(outfile)              
(outfileshortname, extension) = os.path.splitext(outfilename)   

#python attempt
inprj = osr.SpatialReference()
inprj.ImportFromEPSG(5234)

outprj = osr.SpatialReference()
outprj.ImportFromEPSG(32644)

#coordinate transform
ct = osr.CoordinateTransformation(inprj,outprj)

#open infile
driver = ogr.GetDriverByName('ESRI Shapefile')
indataset = driver.Open(infile, 0)
if indataset is None:
	print 'Could not open file'
	sys.exit(1)
inlayer = indataset.GetLayer()

#create output shapefile
out = d + '/NBRO' + '/' + fo[f] + fn[1]	
if os.path.exists(out):
	driver.DeleteDataSource(outfile)

outdataset = driver.CreateDataSource(outfile)

if outfile is None:   
    print ' Could not create file'   
    sys.exit(1)   
outlayer = outdataset.CreateLayer(outfileshortname, geom_type=ogr.wkbPolygon)   
  
#command line version






