### Adapting the Parcel-Divider toolset by Dahal & Chow 2014
### Adapted by MLiberati during April 2018
### marjorie.liberati@uconn.edu

## Part 1H - Data Prep - Parcel Divider
print "PART 1H - Parcel Divider\n"

# Approximate time = 18-19 hours 

#----------------------------------------------------------#

### Call modules

##import inspyred 
import arcpy
import glob, os, random
from time import time, ctime
import csv

#----------------------------------------------------------#

# script speed
start = time()
print "Start the stopwatch at {0}".format(ctime())

#----------------------------------------------------------#

### Set up arcpy

arcpy.CheckOutExtension("Spatial")
arcpy.env.workspace = r"P:\Projects-Current\Subdivision\Sub_Temp"
arcpy.env.overwriteOutput = True

# clear in_memory before starting
arcpy.Delete_management("in_memory")

#--------------------------------------------------------#

### Important Folders

# Home folder
homeFolder = "P:\Projects-Current\Subdivision"

# Original data files
originalFolder = "Sub_OriginalFiles"

# Derived data - derived from original files
derivedFolder = "Sub_DerivedFiles"

# Temporary or test data 
tempFolder = "Sub_Temp"

# Specify folder path for saving certain files
path = r"{0}\{1}".format(homeFolder,tempFolder)

#----------------------------------------------------------#

# DERIVED FILES

# Info for available parcels for the subdivision analysis
AVAIL_subdiv = r"{0}\{1}\taxmap_AVAILABLE_subdivide.shp".format(homeFolder,derivedFolder)
inFC = r"in_memory\taxmap_AVAILABLE_subdivide"
arcpy.CopyFeatures_management(AVAIL_subdiv, inFC)

### Test area
##AVAIL_subdiv = r"{0}\{1}\TEST_taxmap_AVAILABLE_subdivide.shp".format(homeFolder,tempFolder)
##inFC = r"in_memory\taxmap_AVAILABLE_subdivide"
##arcpy.CopyFeatures_management(AVAIL_subdiv, inFC)

# Lots that were created based on the subdivision analysis
finalFC = r"{0}\{1}\taxmap_AVAILABLE_postsubdivide_1ha.shp".format(homeFolder,derivedFolder)

#------------------------------------------------------#

# TEMPORARY FILES

# Make a backup copy of AVAIL_subdiv that also has an attribute for mainAngle
arcpy.MakeFeatureLayer_management(memory_AVAIL_subdiv, "memory_AVAIL_subdiv_lyr")
inFC = r"{0}\{1}\COPY_taxmap_AVAIL_subdivide.shp".format(homeFolder,tempFolder)
arcpy.CopyFeatures_management(memorey_AVAIL_subdiv, inFC)
arcpy.Delete_management("AVAIL_subdiv_lyr")
field_names = [f.name for f in arcpy.ListFields(inFC)]
if "mainAngle" not in field_names:
    arcpy.AddField_management(inFC,"mainAngle","DOUBLE")

# Polygons that are big enough to be subdivided (Dev_Status = Undiv)
NOobjectName = "{0}\{1}\Parcels_available_forSubdivision.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for polygons at the start of the subdivision rounds -
# polygons get deleted after processing, but partially divided polygons then get readded at the end of the subdivision round
# cursor remains open throughout the subdivision rounds
SELupdate = "{0}\{1}\SelectedParcels_Update.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# File were subdivision lines for each polygon are stored -
# these get used to actually subdivide everything after all the subdivision rounds have been completed
DIVIDE = "{0}\{1}\DividingLines.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file where partial division lines for large polys are stored -
# these lines are also added to DIVIDE, but this file gets wiped after each polygon
NOobjectName = "{0}\{1}\PartialDivision.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for polygon that is about to be divided by PartialDivision.shp 
NOobjectName = "{0}\{1}\Subselect.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for polygons that are created when Subselect.shp is split by PartialDivision.shp
NOobjectName = "{0}\{1}\SplitBYpartials.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for polygons from SplitIntersection.shp which have been processed to remove spurrious polygons
NOobjectName = "{0}\{1}\SplitIntersect.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for partially divided polygons that need to be carried to the next round of subdivision -
# cursor is created and deleted for this file as each large polygon is identified
SELinsert = "{0}\{1}\SelectedParcels_Insert.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Search cursor is created at the end of every subdivision round to transfer the partially divided polygons to SelectedParcels_Update.shp
INSERT = "{0}\{1}\SelectedParcels_Insert.shp".format(homeFolder,tempFolder)

# Insert cursor is created so partially divided polys from INSERT can be subdivided during the next round
NEXTRND = "{0}\{1}\SelectedParcels_Update.shp".format(homeFolder,tempFolder)

# Holding file for lots that are created when DividingLines.shp is dividing a polygon
NOobjectName = "{0}\{1}\tempFC1.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for lots that remain after spurrious polygons have been removed
NOobjectName = "{0}\{1}\tempFC2.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# File for processed lots that are created when DividingLines.shp has been applied to all polygons
NOobjectName = "{0}\{1}\newFC.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Holding file for lots that are associated with a single LOC_ID
NOobjectName = "{0}\{1}\tempFC.shp".format(homeFolder,tempFolder) # saved based on env.workspace

# Reused holding file for lots at a single LOC_ID where slivers have been merged with adjacent lots within the LOC_ID -
# the polygons in tempFC2.shp get appended to finalFC
NOobjectName = "{0}\{1}\tempFC2.shp".format(homeFolder,tempFolder) # saved based on env.workspace

#####################################################################
#####################################################################
#####################################################################

# Specify subdivision parameters

width = int(75)
length = int(75)
##sizeToMerge = 2500 #squared meters (i.e, 0.5+ hectares)
sizeToMerge = 5000 #squared meters (i.e, 1.0+ hectares)

#####################################################################
#####################################################################
#####################################################################

### Define different functions to be used later
### These are copyied directly from "GeneralizedParcelDivider1.py"

import math as math
degToRad = math.pi/180.0
radToDeg = 180.0/math.pi

# Define a function to calculate an angle between two points, i.e.
# the angle of a straight line
def lineAngle (pnt1, pnt2):
    """ with two different points, calculates
    an angle relative to X axis, not azimuth"""
    dY = pnt2[1] - pnt1[1]
    dX = pnt2[0] - pnt1[0]
    if dY == 0 and dX == 0:#i.e. if they are same points
        print "should be distinct points"
    myAngle = math.atan2(dY,dX) * radToDeg
    return myAngle

# Similarly, define a function to get a point given the angle and distance
def getPoint (origXY, angle, distance):
    """Calculates offsets/displacements with light trigometry
    and gets the end point"""
    (offX, offY) = (distance * math.cos(math.radians(angle)),\
                        distance * math.sin(math.radians(angle)))
    endXY = [origXY[0] + offX, origXY[1] + offY]
    return endXY

# Also, create a function to create an arc for cul-de-sac
def createArc(aPnt, radius):
    """Creates arc with an origin and a given radius"""
    origX, origY = aPnt[0], aPnt[1]
    endX, endY = origX + radius, origY
    arcPnts = []
    for i in range(305, -35, -5):
        X = origX + radius * math.cos(degToRad * i)
        Y = origY + radius * math.sin(degToRad *i)
        arcPnt = [X, Y]
        arcPnts.append(arcPnt)
    del arcPnt
    return arcPnts

# Also, create a function to create an arc for cul-de-sac
def createArc1(aPnt, radius):
    """Creates arc with an origin and a given radius"""
    origX, origY = aPnt[0], aPnt[1]
    endX, endY = origX + radius, origY
    arcPnts = []
    for i in range(235, -105, -5):
        X = origX + radius * math.cos(degToRad * i)
        Y = origY + radius * math.sin(degToRad *i)
        arcPnt = [X, Y]
        arcPnts.append(arcPnt)
    del arcPnt
    return arcPnts

# Another function to calculate distance betwn pts or the hypoteneus
def findHypot(pnt1, pnt2):
    Xdiff = pnt2[0] - pnt1[0]
    Ydiff = pnt2[1] - pnt2[1]
    Hypot = math.hypot(Xdiff, Ydiff)
    return Hypot

# Line distance
def lineDist(pnt1, pnt2):
    X = pnt2[0] - pnt1[0]
    XX = X*X
    Y = pnt2[1] - pnt2[1]
    YY = Y*Y
    sum = XX + YY
    dist = math.sqrt(sum)
    return dist

# Another function to calculate the midpoint of a line
def midPoint(P1, P2):
    """finds the mid-point of a line having two distinct points"""
    midPnt = [(P1[0] + P2[0])/2.0, (P1[1] + P2[1])/2.0]
    return midPnt

#####################################################################
################### SUBDIVISION ITERATIONS ##########################
#####################################################################

print "\nCREATE THE SUBDIVISION LINES"

#### This section creates the file "in_memory\DividingLines"
#### which will be used to create lots in the following section

# Count number of polygons within the study area
remaining = arcpy.GetCount_management(inFC)
print "{0} polygons within the study area".format(remaining[0])

# First create file with the 'Undiv' parcels to be divided
whereclause = """ "Dev_Status" = 'Undiv' """
arcpy.MakeFeatureLayer_management(inFC, "inFC_lyr", whereclause)
arcpy.CopyFeatures_management("inFC_lyr", "Parcels_available_forSubdivision.shp")

# Count number of polygons that are eligable for subdivision
remaining = arcpy.GetCount_management("Parcels_available_forSubdivision.shp")
print "{0} polygons eligable for subdivision".format(remaining[0])

# Describe spatial references
desc = arcpy.Describe(inFC)
sr = desc.spatialReference

# Create the file that the convex hulls will be based on
SP_UPDATE = r"in_memory\SelectedParcels_Update"
arcpy.CopyFeatures_management("inFC_lyr", SP_UPDATE)
arcpy.MakeFeatureLayer_management(SP_UPDATE, "SP_UPDATE_lyr")

# Create file that will hold the polygons that need additional round of subdivisions
SP_INSERT = r"in_memory\SelectedParcels_Insert"
arcpy.CreateFeatureclass_management("in_memory", "SelectedParcels_Insert", "POLYGON", "Parcels_available_forSubdivision.shp", "", "", sr)
##SP_INSERT = "SelectedParcels_Insert.shp"
##arcpy.CreateFeatureclass_management(path, "SelectedParcels_Insert.shp", "POLYGON", inFC, "", "", sr)

# Subdivided lots will be added to this file - "DividedParcels"
dividing = r"in_memory\DividingLines"
arcpy.CreateFeatureclass_management("in_memory", "DividingLines", "POLYLINE", inFC, "", "", sr)

# Identify fields for the search cursor
fields = arcpy.ListFields(inFC)
fieldLst = []
for field in fields:
    fieldLst.append(field.name)
fieldLst.append("SHAPE@")

# Keep list of 'Undiv' properties that were too small for subdivision
toosmall = []

# Holding file for convex hull creations
convexhull = r"in_memory\ConvexHull"

round = 1
while round < 10:

    # Count number of polygons that need subdivision this round
    remaining = arcpy.GetCount_management(SP_UPDATE)

    # Check if need to go through this round of subdivision
    if int(remaining[0])==0:
        ##print "Completed subdivision in {0} rounds".format(round-1)
        break
    
    print "START ROUND {0}".format(round)
    print "{0} polygon(s) remaining".format(remaining[0])
    
    # Add angle to parcels that are available for subdivision
    arcpy.CalculatePolygonMainAngle_cartography(SP_UPDATE, "mainAngle")
    
    # Create holding file for partial division lines
    partial = r"in_memory\PartialDivision"
    arcpy.CreateFeatureclass_management("in_memory", "PartialDivision", "POLYLINE", inFC, "", "", sr)
    
    # Create search cursor to work with each minimum convex hull polygon
    SELupdate = arcpy.da.UpdateCursor(SP_UPDATE, fieldLst)
        
    for row in SELupdate:

         # Calculate minimum convex hulls for the remaining parcels 
        whereclause = """ "FID" = {0} """.format(row[fieldLst.index("FID")])
        arcpy.SelectLayerByAttribute_management("SP_UPDATE_lyr", "NEW_SELECTION", whereclause)        
        arcpy.MinimumBoundingGeometry_management("SP_UPDATE_lyr", convexhull, "RECTANGLE_BY_AREA", "NONE")

        #---------------------------------------------------------#
        #---------------------------------------------------------#

        ### PARCELS WITH NEGATIVE OR ZERO MAIN ANGLES
            
        CONVEX = arcpy.da.SearchCursor(convexhull, fieldLst)
        for hull in CONVEX:

            partnum = 0
            pnts = []
            for part in hull[fieldLst.index("SHAPE@")]:
                for pnt in part:
                    if pnt:
                        pnts.append([pnt.X, pnt.Y])
                    else:
                        print("Interior ring")
                partnum += 1
            
            if hull[fieldLst.index("mainAngle")] <= 0: #if main angle is negative

                # get height and width of convex hull
                firstPnt = pnts[2] #always start with furthest right point (pnts[0]), so pnts[2] always has lowest X-axis value 
                x = pnts[1][0] - pnts[2][0] #width of polygon base
                xx = x*x
                y = pnts[1][1] - pnts[2][1] #height difference between points along base (~tilt height)
                yy = y*y
                s = xx + yy
                hypot = math.sqrt(s) #length of baseline
                myAngle = math.atan2(y,x)*radToDeg #baseline tilt angle

                p = pnts[0][0] - pnts[1][0] #width difference between points along side (~tilt width)
                pp = p*p
                q = pnts[0][1] - pnts[1][1] #height of polygon side
                qq = q*q
                t = pp + qq
                hypot2 = math.sqrt(t) #height of polygon
                myAngle2 = math.atan2(q,p)*radToDeg #height tilt angle

                ##print hypot, hypot2, myAngle, myAngle2

                #-----------------------------------------------------#

                # Skip parcel if height and width are too short for division
                if hypot <= length*1.5 and hypot2 <= length*1.5:
                    
                    ##print "Parcel is too small for subdivision"
                    
                    toosmall.append(hull[fieldLst.index("LOC_ID")])

                    del CONVEX
                    continue

                #-----------------------------------------------------#

                # Parcel big enough for vertical but not horizontal division
                elif hypot <= length*1.5 and hypot2 <= length*20:
                    
                    ##print "Parcel being divided vertically but not horizontally"
                    
                    numDiv1 = int(hypot2) / int(width)
                    dist1 = hypot2 / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place point on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle2, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle, hypot)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1
                    del CONVEX
                    continue

                #-----------------------------------------------------#

                # Parcels big enough for vertical and horizontal division
                elif hypot > length*1.5 and hypot <= length*3.2 and hypot2 <= length*10:

                    ##print "Parcel being divided vertically and horizontally"

                    # Divide horizontally first
                    numDiv1 = int(hypot2) / int(width)
                    dist1 = hypot2 / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle2, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle, hypot)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1
        
                    # Then divide vertically 
                    numDiv2 = 2
                    dist1 = hypot / numDiv2
                    p2 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv2):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle, dist1)
                        #create the matching point vertically across the polygon
                        verPnt1 = getPoint(basePnt1, myAngle2, hypot2)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = verPnt1[0], verPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p2
                    del CONVEX
                    continue
                
                #-----------------------------------------------------#

                # Parcel is big enough for vertical and horizontal division,
                # but is also wide enough to need extra handling
                
                else: 

                    ##print "Parcel needs extra round of subdivision"
                    
                    #divide only once Horizontally
                    numDiv1 = 2
                    dist1 = hypot / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle2, hypot2)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #add polyline to shapefile for additional processing
                        HOLD = arcpy.da.InsertCursor(partial,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        HOLD.insertRow(info)
                        del HOLD
                        #select the polygon that needs additional division
                        arcpy.SelectLayerByAttribute_management("SP_UPDATE_lyr", "NEW_SELECTION", """ "FID" = {0} """.format(row[fieldLst.index("FID")]))
                        ##arcpy.CopyFeatures_management("SP_UPDATE_lyr", r"in_memory\Subselect")
                        #split that polygon by the partial division lines
                        arcpy.FeatureToPolygon_management(["SP_UPDATE_lyr", partial], r"in_memory\SplitBYpartials")
                        #reset PartialDivision.shp by removing all lines
                        arcpy.DeleteFeatures_management(partial)
                        # Get rid of spurrious polygons
                        arcpy.Intersect_analysis(["SP_UPDATE_lyr",r"in_memory\SplitBYpartials"], r"in_memory\SplitIntersect","ALL")
                        # Search cursor for resulting file
                        with arcpy.da.SearchCursor(r"in_memory\SplitIntersect", fieldLst) as SPLIT:
                            with arcpy.da.InsertCursor(SP_INSERT, fieldLst) as SELinsert:
                                for split in SPLIT:
                                    SELinsert.insertRow(split)
                        del SPLIT, SELinsert
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1
                    del CONVEX
                    continue
            
            #---------------------------------------------------------#
            #---------------------------------------------------------#

            ### PARCELS WITH POSITIVE MAIN ANGLES
                        
            if hull[fieldLst.index("mainAngle")] > 0: #i.e. the positive angle value

                ##print "Main angle was positive"

                # get height and width of convex hull
                firstPnt = pnts[3] #always start with topright point (pnts[0], so pnts[3] always has lowest X-axis value 
                x = pnts[2][0] - pnts[3][0] #width difference between points along side (~tilt width)
                xx = x*x
                y = pnts[2][1] - pnts[3][1] #height of polygon side
                yy = y*y
                s = xx + yy
                hypot = math.sqrt(s) #height of polygon
                myAngle = math.atan2(y,x)*radToDeg #height tilt angle

                p = pnts[1][0] - pnts[2][0] #width of polygon base
                pp = p*p
                q = pnts[1][1] - pnts[2][1] #height difference between points along base (~tilt height)
                qq = q*q
                t = pp + qq
                hypot2 = math.sqrt(t) #length of baseline
                myAngle2 = math.atan2(q,p)*radToDeg #baseline tilt angle

                ##print hypot, hypot2, myAngle, myAngle2
                
                #-----------------------------------------------------#
                
                # Skip parcel if height (hypot2) and width (hypot) are too short for division
                if hypot2 <= length*1.5 and hypot <= length*1.5:
                    
                    ##print "Parcel is too small for subdivision"
                    
                    toosmall.append(hull[fieldLst.index("LOC_ID")])

                    del CONVEX
                    continue
                
                #-----------------------------------------------------#

                # Parcel big enough for vertical (hypot) but not horizontal (hypot2) division
                elif hypot2 <= length*1.5 and hypot <= length*20:
                    
                    ##print "Parcel big enough for horizonal but not vertical division"
                    
                    numDiv1 = int(hypot) / int(width)
                    dist1 = hypot / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle2, hypot2)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1
                    del CONVEX
                    continue
                    
                #-----------------------------------------------------#
                
                # Parcel big enough for horizontal (hypot2) and vertical (hypot) division, but does not need need blocks
                elif hypot2 > length*1.5 and hypot2 <= length*3.2 and hypot <= length*10:
                    
                    ##print "Parcel can be divided vertically and horizontally (but does not need blocks)"

                    # Start with vertical divisions
                    numDiv1 = int(hypot) / int(width)
                    dist1 = hypot / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle2, hypot2)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1

                    # Then divide horizontally
                    numDiv2 = 2
                    dist2 = hypot2 / numDiv2
                    p2 = dist2
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv2):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist2)
                        basePnt1 = getPoint(firstPnt, myAngle2, dist2)
                        #create the matching point vertically across the polygon
                        verPnt1 = getPoint(basePnt1, myAngle, hypot)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = verPnt1[0], verPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist2 += p2
                    del CONVEX
                    continue
                    
                #-----------------------------------------------------#

                # Parcel big enough for horizontal (hypot2) and vertical (hypot) division,
                # But is wide engouh to need additional processing
                else:
                    
                    ##print "Parcel needs another round of subdivision"
                    
                    # Start by dividing the parcel vertically in two
                    numDiv1 = 2
                    dist1 = hypot / numDiv1
                    p1 = dist1
                    myarray1 = arcpy.Array()
                    for i in range(1, numDiv1):
                        #from the bottom left corner of the polygon, place pont on baseline (myAngle) at certain interval (dist1)
                        basePnt1 = getPoint(firstPnt, myAngle, dist1)
                        #create the matching point vertically across the polygon
                        horPnt1 = getPoint(basePnt1, myAngle2, hypot2)
                        #make coordinates arcpy points
                        X1, Y1 = basePnt1[0], basePnt1[1]
                        X2, Y2 = horPnt1[0], horPnt1[1]
                        begin = arcpy.Point(X1, Y1)
                        end = arcpy.Point(X2, Y2)
                        #add points to an array
                        myarray1.add(begin)
                        myarray1.add(end)
                        #connect a polyline with the array
                        polyline = arcpy.Polyline(myarray1, sr)
                        #add the polyline to the new shapefile
                        DIVIDE = arcpy.da.InsertCursor(dividing,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        DIVIDE.insertRow(info)
                        del DIVIDE
                        #add polyline to shapefile for additional processing
                        HOLD = arcpy.da.InsertCursor(partial,["LOC_ID","SHAPE@"])
                        info = (hull[fieldLst.index("LOC_ID")],polyline)
                        HOLD.insertRow(info)
                        del HOLD
                        #select the polygon that needs additional division
                        arcpy.SelectLayerByAttribute_management("SP_UPDATE_lyr", "NEW_SELECTION", """ "FID" = {0} """.format(row[fieldLst.index("FID")]))
                        arcpy.CopyFeatures_management("SP_UPDATE_lyr", r"in_memory\Subselect")
                        #split that polygon by the partial division lines
                        arcpy.FeatureToPolygon_management([r"in_memory\Subselect", partial], r"in_memory\SplitBYpartials")
                        #reset PartialDivision.shp by removing all lines
                        arcpy.DeleteFeatures_management(partial)
                        # Get rid of spurrious polygons
                        arcpy.Intersect_analysis([r"in_memory\Subselect", r"in_memory\SplitBYpartials"],r"in_memory\SplitIntersect","ALL")
                        # Search cursor for resulting file
                        with arcpy.da.SearchCursor(r"in_memory\SplitIntersect", fieldLst) as SPLIT:
                            with arcpy.da.InsertCursor(SP_INSERT, fieldLst) as SELinsert:
                                for split in SPLIT:
                                    SELinsert.insertRow(split)
                        del SPLIT, SELinsert
                        #reset array for next line that might be added
                        myarray1.removeAll()
                        #advance the distance interval
                        dist1 += p1
                    del CONVEX
                    continue

        # Delete the row from the SELupdate cursor
        SELupdate.deleteRow()
        
    # Delete cursor that was working through SELupdate
    del SELupdate

    # Write insert cursor to the now clear update cursor
    with arcpy.da.SearchCursor(SP_INSERT, fieldLst)as INSERT:
        with arcpy.da.InsertCursor(SP_UPDATE, fieldLst) as NEXTRND:
            for insert in INSERT:
                NEXTRND.insertRow(insert)
    del INSERT, NEXTRND

    # Change any multiparts to single parts
    arcpy.CopyFeatures_management(SP_UPDATE, r"in_memory\SelectedParcels_multipart")
    arcpy.MakeFeatureLayer_management(r"in_memory\SelectedParcels_multipart","SelectedParcels_multipart_lyr")
    arcpy.MultipartToSinglepart_management("SelectedParcels_multipart_lyr",SP_UPDATE)
   
    # Clear all of the features in SelectedParcels_Insert.shp
    arcpy.DeleteFeatures_management(SP_INSERT)
    
    round += 1

divisionlines = r"{0}\{1}\DividingLines.shp".format(homeFolder,tempFolder)
arcpy.CopyFeatures_management(dividing,divisionlines)
    
print "Finished making the division lines"

#------------------------------------------------------#

#### This section will use the file "DividingLines.shp"
#### to create lots that will be saved to ".shp"

print "\nUSE THE DIVISION LINES TO CREATE LOTS"
  
# Make temporary features to hold divisions for each polygon
newFC = r"in_memory\newFC"
arcpy.CreateFeatureclass_management("in_memory", "newFC", "POLYGON", inFC,"","",sr)

# Make feature layer of the division lines and parcels available for subidivision
undivs = "in_memory\Parcels_available_forSubdivision"
arcpy.MakeFeatureLayer_management("Parcels_available_forSubdivision.shp", undivs)
arcpy.MakeFeatureLayer_management(dividing, "dividing_lyr")

#loop through polygons that were available for subdivision 
TRANSFER = arcpy.da.SearchCursor("Parcels_available_forSubdivision.shp", fieldLst)
for row in TRANSFER:
    #create where clause based on the polygon's FID
    whereclause = """ "LOC_ID" = '{0}' """.format(row[fieldLst.index("LOC_ID")])
    #select the polygon by its FID
    arcpy.SelectLayerByAttribute_management(undivs, "NEW_SELECTION", whereclause)
    #select the division lines by their FIDs
    arcpy.SelectLayerByAttribute_management("dividing_lyr", "NEW_SELECTION", whereclause)
    #split the polygon by the associated division lines
    arcpy.FeatureToPolygon_management([undivs, "dividing_lyr"], r"in_memory\tempFC1")
    #get rid of spurrious polygons
    arcpy.Intersect_analysis([undivs, r"in_memory\tempFC1"],r"in_memory\tempFC2","ALL")
    #loop through new polygons in temporary folder
    with arcpy.da.SearchCursor(r"in_memory\tempFC2",fieldLst) as TEMP:
        #creat cursor to add them to the new final file
        with arcpy.da.InsertCursor(newFC, fieldLst) as NEW:
            #add each polygon to the new fil
            for split in TEMP:
                NEW.insertRow(split)
    del TEMP, NEW

newFCshp = r"{0}\{1}\newFC.shp".format(homeFolder,tempFolder)
arcpy.CopyFeatures_management(newFC,newFCshp)

print "Finished creating lots"

#------------------------------------------------------#

## Address polygon slivers that were created during subdivision

print "\nREMOVE SLIVER LOTS"

# Update SHAPE_AREA attribute
arcpy.CalculateField_management("newFC.shp", "SHAPE_AREA", "!shape.area@squaremeters!","PYTHON")

# Get list of LOC_ID for the study area
locIDlst = []
GETids = arcpy.da.SearchCursor(inFC, ["LOC_ID","Dev_Status"])
for row in GETids:
    if row[1] == 'Undiv':
        locIDlst.append([row[0],row[1]])
del GETids

# Make a layer for newFC.shp
arcpy.MakeFeatureLayer_management("newFC.shp", "newFC_lyr")

# Create new file to save the final subdivided lots
arcpy.SelectLayerByAttribute_management("inFC_lyr", "NEW_SELECTION", """ "Dev_Status" <> 'Undiv' """)
finalFC = r"{0}\{1}\taxmap_AVAILABLE_postsubdivide_1ha.shp".format(homeFolder,derivedFolder)
arcpy.CopyFeatures_management("inFC_lyr", finalFC)
arcpy.Delete_management("inFC_lyr")

#loop through LOC_ID to address slivers that resulted from its subdivision 
for ID, status in locIDlst:
    print "Next polyon"
    #select the lots within the subdivided polygon
    arcpy.SelectLayerByAttribute_management("newFC_lyr", "NEW_SELECTION", """ "LOC_ID" = '{0}' """.format(ID))
    #save it to its own file tempFC3
    arcpy.CopyFeatures_management("newFC_lyr", "tempFC3.shp")
    #make a layer for tempFC3
    arcpy.MakeFeatureLayer_management("tempFC3.shp", "tempFC3_lyr")
    #select subdivided lots for that polygon that do not meet the minimum size
    arcpy.SelectLayerByAttribute_management("tempFC3_lyr", "NEW_SELECTION", '"SHAPE_AREA" < {0}'.format(sizeToMerge))
    selected = arcpy.Describe("tempFC3_lyr").FIDSet
    if selected == '':
        continue
    #merge slivers withinin this polygon with a lots that shares its LOC_ID and longest border and save to holding file tempFC4
    arcpy.Eliminate_management(in_features="tempFC3_lyr", out_feature_class="tempFC4.shp", selection="LENGTH")
    #append the lots in the holding file tempFC4 to the final file (taxmap_AVAILABLE_postsubdivide)
    arcpy.Append_management(inputs="tempFC4.shp", target=finalFC, schema_type="TEST")

arcpy.Delete_management("newFC_lyr")
arcpy.Delete_management("tempFC_lyr")

print "Finished removing sliver lots"

#------------------------------------------------------#

#### Delete unneeded files and layers

arcpy.Delete_management("SelectedParcel_Update_lyr")
arcpy.Delete_management("SelectedParcels_multipart_lyr")
arcpy.Delete_management("Parcels_available_forSubdivision_lyr")

print "Deleted unneeded files and layers"

#------------------------------------------------------#

# clear in_memory at end
arcpy.Delete_management("in_memory")

# final script speed
stop = time()
diff = (stop-start)/60
print "Stop the stopwatch at {0}".format(ctime())
print "Final run time = {0} minutes".format(diff)
