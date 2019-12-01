## Part 2A - Genetic Algorithm for Pre-subdivision parcelization in Combo-objective Demo Area
print "PART 3A - Genetic Algorithm for Pre-subdivided parcels in a Combo-objective Demo Area"

## Adapted "inspyred_NSGAII_adaptedthread.py"
## MRL adapted Stabilization_NSGAII_ALLiteration_2017.10.23.py script
## Apply algorithm to each town in MA

########################################################
####################    PREP    ########################
########################################################

# call modules
import inspyred 
import arcpy
import glob, os, random
from time import time, ctime
import csv
import json
import math
import gc

# script speed
start = time()
print "\nStart the stopwatch at {0}".format(ctime())

# setup arcpy
arcpy.CheckOutExtension("Spatial")
arcpy.env.workspace = r"P:\Projects-Current\Subdivision"
arcpy.env.overwriteOutput = 1

# clear in_memory before starting
arcpy.Delete_management("in_memory")

########################################################
################# ASSEMBLE DATA ########################
########################################################

# FOLDERS

# Home folder
homeFolder = "P:\Projects-Current\Subdivision"

# Original data files
originalFolder = "Sub_OriginalFiles"

# Derived data - derived from original files
derivedFolder = "Sub_DerivedFiles"

# Temporary or test data 
tempFolder = "Sub_Temp"

# Demo area folder
demoFolder = "DemoArea_Combo"

# Results data
resultsFolder = "Sub_Results\PRE_COMBO_Originals"
resultsPath = r"{0}\{1}".format(homeFolder,resultsFolder)
if not os.path.isdir(resultsPath):
    os.makedirs(resultsPath)

# Additional results folders
resultsPath = r"{0}\{1}".format(homeFolder,"Sub_Results\PRE_COMBO_forR")
if not os.path.isdir(resultsPath):
    os.makedirs(resultsPath)

#------------------------------------------------------#

# ORIGINAL FILES

#------------------------------------------------------#

# TEMPORARY FILES

#------------------------------------------------------#

# DERIVED FILES

# FINAL taxmap file that only contains properties available for purchase that also have zoning info
AVAIL_zoned = r"{0}\{1}\{2}\demoArea_PRE_combo.shp".format(homeFolder,derivedFolder,demoFolder)
memory_AVAIL_zoned = r"in_memory\demoArea_PRE_combo"
arcpy.CopyFeatures_management(AVAIL_zoned, memory_AVAIL_zoned)

# FINAL protected area file that cooresponds to taxmap property polygons
PERPETUITY = r"{0}\{1}\{2}\demoArea_PERPETUITY_combo.shp".format(homeFolder,derivedFolder,demoFolder)
memory_PERPETUITY = r"in_memory\demoArea_PERPETUITY_comobo"
arcpy.CopyFeatures_management(PERPETUITY, memory_PERPETUITY)

print "\nFinished assembling data"

########################################################
################# ALGORITHM SPECS ######################
########################################################

# populations size
popSize = 2000

# number of generations for the algorithm
maxGens = 50

# mutation rate
mutation = 0.0009

# addition to filename
specs = "P{0}M{1}G{2}".format(popSize,mutation,maxGens)

# today's date - YYYY.MM.DD
date = "2018.07.12"

########################################################
################ PRE-SUBDIVISION GA ####################
########################################################

# Export parcel attributes to nest list so don't have to keep rerunning cursor
 
parcelLst = []

fields = ("TOWN_ID","FID","LOTSIZE_HA","LOT_COST","WghtByArea","RIPAR_HA","COMPACT","PRIORTY_HA")

rowsAVAIL = arcpy.da.SearchCursor(memory_AVAIL_zoned, fields)
for row in rowsAVAIL:

    townID = row[0]
    FID = row[1]
    area = row[2]
    cost = row[3]
    towncharact = row[4]
    riparian = row[5]
    compact = row[6]
    priority = row[7]

    parcelAttributes = [townID,FID,area,cost,towncharact,riparian,compact,priority]

    parcelLst.append(parcelAttributes)
    
# close cursor
del row, rowsAVAIL

parcCnt = len(parcelLst)
print "\nThere are {0} parcels available for selection in the PRE-subdivided landscape".format(parcCnt)

print "\nEXPLANATION OF THE NESTED LIST STRUCTURE"
print "Nest-list for each parcel inclues the following attributes (in order):"
print "0) Town ID,"
print "1) FID,"
print "2) Lot size,"
print "3) Cost,"
print "4) Zone alignment with town character (weighted by area),"
print "5) Riparian habitat,"
print "6) Sum of cost distance raster for proximity to existing PA properties, and"
print "7) Priority habitat (central oak-pine)"

#------------------------------------------------------#

# Export zonal habitat information to nest list so don't have to keep rerunning cursor
# NOTE - excluding Water (Value_1), Urban/Suburban/Built (Value_3), and Agriculture (Value_4)

V6 = 0
V10 = 0
V15 = 0
V16 = 0
V21 = 0
V22 = 0
V23 = 0

values = ["VALUE_6", "VALUE_10", "VALUE_15", "VALUE_16", "VALUE_21", "VALUE_22", "VALUE_23"]
PREhabLst = [V6, V10, V15, V16, V21, V22, V23]
PREhabParcels = []

PREhab = r"{0}\{1}\{2}\zonal_HABinPRE.dbf".format(homeFolder,derivedFolder,demoFolder)
field_zonal = [f.name for f in arcpy.ListFields(PREhab)]

rowsAVAIL = arcpy.da.SearchCursor(PREhab, field_zonal)
for row in rowsAVAIL:

    FID = row[field_zonal.index("VALUE")]
    v1 = row[field_zonal.index("VALUE_1")]
    v3 = row[field_zonal.index("VALUE_3")]
    v4 = row[field_zonal.index("VALUE_4")]
    v6 = row[field_zonal.index("VALUE_6")]
    v10 = row[field_zonal.index("VALUE_10")]
    v15 = row[field_zonal.index("VALUE_15")]
    v16 = row[field_zonal.index("VALUE_16")]
    v21 = row[field_zonal.index("VALUE_21")]
    v22 = row[field_zonal.index("VALUE_22")]
    v23 = row[field_zonal.index("VALUE_23")]   
    PREhabParcels.append([FID,v1,v3,v4,v6,v10,v15,v16,v21,v22,v23])

    for i,value in enumerate(values):

        if value in field_zonal:
            PREhabLst[i] += row[field_zonal.index(value)]
        if value not in field_zonal:
            PREhabLst[i] += float(0)

# close cursor
del row, rowsAVAIL

PRE_area = float(0)
for i in PREhabLst:
    PRE_area += i

print "\nCreated nested list for habitat info for each PRE parcel"

#------------------------------------------------------#

# Export information for habitat within existing PAs
# NOTE - excluding Water (Value_1), Urban/Suburban/Built (Value_3), and Agriculture (Value_4)

V6 = 0
V10 = 0
V15 = 0
V16 = 0
V21 = 0
V22 = 0
V23 = 0

values = ["VALUE_6", "VALUE_10", "VALUE_15", "VALUE_16", "VALUE_21", "VALUE_22", "VALUE_23"]
PAhabLst = [V6, V10, V15, V16, V21, V22, V23]

PAhab = r"{0}\{1}\{2}\zonal_HABinPA.dbf".format(homeFolder,derivedFolder,demoFolder)
field_PAs = [f.name for f in arcpy.ListFields(PAhab)]

rowsAVAIL = arcpy.da.SearchCursor(PAhab, field_PAs)
for row in rowsAVAIL:

        for i,value in enumerate(values):

            if value in field_PAs:
                PAhabLst[i] += row[field_PAs.index(value)]
            if value not in field_PAs:
                PAhabLst[i] += float(0)

# close cursor
del row, rowsAVAIL

PA_area = float(0)
for i in PAhabLst:
    PA_area += i

print "\nCreated nested list for habitat info within existing PAs"

#------------------------------------------------------#

# Establish current percent representation of habitats within watershed
target_percentRep = []
for i,amount in enumerate(PREhabLst):
    percent = (amount + PAhabLst[i]) / (PRE_area + PA_area) * 100
    target_percentRep.append(percent)

print "\nHabitat types within the watershed (order within nested-lists)"
print "V6-Central Oak-Pine, V10-Cliff/Tallus, V15-Central Hardwood Swamps,"
print "V16-NOrth Hardwood/Conifer, V21-North Swamp, V22-Wet/Shrub Meadow, and V23-Emergent Marsh"

print "\nPercent coverage of each habitat within watershed polygons"
for hab in target_percentRep:
    print hab

print "\nPercent coverage of each habitat within current PAs"
for hab in PAhabLst:
    print hab / PA_area * 100

########################################################
############### ALGORITHM OPERATORS ####################
########################################################

print "\n>> DEFINING THE CUSTOM GENERATOR"
def CustomGenerator(random, args):
    nr_inputs = args.get('nr_inputs')
    return_generator = [random.randint(0, 1) for i in range(nr_inputs)]
    return return_generator
print "done"

#------------------------------------------------------#

print "\n>> DEFINING THE CUSTOM EVALUATOR"

PAtable = r"in_memory\neartable_PAonly"
arcpy.GenerateNearTable_analysis(memory_PERPETUITY, memory_PERPETUITY, PAtable, '1000 Meters')

def CustomEvaluator(candidates, args):

    fitness = []

    # calculate max sum of development prob so can caluculate difference
    maxRiparian = float(0)
    maxPriority = float(0)
    for sublist in enumerate(parcelLst):
        maxRiparian += sublist[1][5]
        maxPriority += sublist[1][7]
        
    for c in candidates:
             
        # placeholders for final objective values
        tot_area = float(0)    # Obj 1
        tot_cost = float(0)    # Obj 2
        tot_charact = float(0) # Obj 3
        tot_riparian = float(0)  # Obj 4
        tot_compact = float(0)  # Obj 5
        tot_priority = float(0) # Obj 6
        connectLst = []

        # placeholders for each habitat total
        tot_exclude = float(0)
        tot_V6 = float(0)
        tot_V10 = float(0)
        tot_V15 = float(0)
        tot_V16 = float(0)
        tot_V21 = float(0)
        tot_V22 = float(0)
        tot_V23 = float(0)

        # placeholder for number of parcels within the solution 
        parcelsINsoln = 0 

        # iterate through each parcel to calculate objectives
        for parcelID,parcel in enumerate(c):
            
            # skip parcel if not included in solution
            if parcel == 0:
                continue 

            # calculate objectives for parcels included (= 1) in the soln
            if parcel == 1:

                # add parcel to count for the solution 
                parcelsINsoln += 1 

                # Objective 1 = total area
                tot_area += parcelLst[parcelID][2] #hectares
               
                # Objective 2 = total cost
                tot_cost += parcelLst[parcelID][3] #US dollars

                # Objective 3 = total town character conflict
                tot_charact += parcelLst[parcelID][4] 

                # Objective 4 = minimize not protecting riparian habitat 
                tot_riparian += parcelLst[parcelID][5] #hectares

                # Objective 5 = minimize distance from existing PA properties
                tot_compact += parcelLst[parcelID][6] #cost distance raster for proximity

                # Objective 6 = minimize not protecting priority (i.e., rare) habitat
                tot_priority += parcelLst[parcelID][7] #hectares

                # add parcel ID to list for connectivity metric
                connectLst.append(int(parcelLst[parcelID][1]))

                # Get habitat info from PREhabLst
                #[0]=FID,[1]=water,[2]=developed,[3]=agriculture
                tot_exclude += PREhabParcels[parcelID][1] + PREhabParcels[parcelID][2] + PREhabParcels[parcelID][3]
                tot_V6 += PREhabParcels[parcelID][4]
                tot_V10 += PREhabParcels[parcelID][5]
                tot_V15 += PREhabParcels[parcelID][6]
                tot_V16 += PREhabParcels[parcelID][7]
                tot_V21 += PREhabParcels[parcelID][8]
                tot_V22 += PREhabParcels[parcelID][9]
                tot_V23 += PREhabParcels[parcelID][10]

        # Calculate connectivity metric
        #if no parcels are identified in the solution
        if(len(connectLst)==0):           
            #calculate the distances between only the PA polygons
            tot_connect = float(0)
            rows = arcpy.da.SearchCursor(PAtable,["NEAR_DIST"])
            for row in rows:
                tot_connect = tot_connect + row[0]
            #close cursor
            del row, rows

        #if at least one parcels is identified in the solution
        if(len(connectLst)>0):
            #select parcels included in the solution
            query = "{0} in {1}".format("FID", connectLst)
            query = query.replace("[","(")
            query = query.replace("]",")")
            #write select polygons from solution
            memory_subAVAIL = r"in_memory\memory_subAVAIL"
            arcpy.MakeFeatureLayer_management(memory_AVAIL_zoned, memory_subAVAIL, where_clause=query)        
            #combine selected parcels and surrounding PAs
            output = r"in_memory\parcels_PAandINsoln"
            arcpy.Merge_management([memory_subAVAIL, memory_PERPETUITY], output)
            #calculate the distances between the parcels included in the solution
            table = r"in_memory\neartable_candidateSoln"
            arcpy.GenerateNearTable_analysis(output, output, table, '1000 Meters')        
            #sum the distances between parcels
            tot_connect = float(0)
            rows = arcpy.da.SearchCursor(table,["NEAR_DIST"])
            for row in rows:
                tot_connect = tot_connect + row[0]
            #close cursor
            del row, rows

        # Calculate representativeness for the solution
        totLst = [tot_V6,tot_V10,tot_V15,tot_V16,tot_V21,tot_V22,tot_V23]
        percentLst = []
        for i,tot in enumerate(totLst):
            totwithPA = tot + PAhabLst[i]
            percent = totwithPA / ((tot_area * 10000) - tot_exclude + PA_area) * 100 #convert tot_area from ha to m^2
            percentLst.append(percent)
            
        # Caculate how far from true representativeness
        rep_diff = 0
        for i,percent in enumerate(percentLst):
            diff = abs(target_percentRep[i] - percent)
            rep_diff += diff

        # Minimize the difference between the solution and max outcome of certain objectives
        riparian_diff = maxRiparian - tot_riparian
        priority_diff = maxPriority - tot_priority
        
        # Add objective values to outpt file 
        fitness.append(inspyred.ec.emo.Pareto([tot_cost, tot_charact, riparian_diff, tot_connect, priority_diff, rep_diff], 
                                              maximize = [False, False, False, False, False, False]))
        # Clear up memory space
        junk = gc.collect()

    return fitness

print "done"

#------------------------------------------------------#

print "\n>> DEFINING THE CUSTOM OBSERVER"

def CustomObserver(population, num_generations, num_evaluations, args):
    best = max(population)
    print('Generation {0:6} Completed'.format(num_generations))

    final_arc = ea.archive
    arcsoln=[a.candidate for a in final_arc]
    arcfit= [a.fitness for a in final_arc]

    #write solns and fitnesses to 
    combo = zip(arcsoln, arcfit)

    filename = r"{0}\{1}\{2}gens_PRE_{3}.csv".format(homeFolder, resultsFolder, num_generations, date)
    numParcels = len(arcsoln[0])
    sizeBreak = int(10000)
    numBreaks = math.ceil(float(numParcels)/10000)
    with open(filename, 'w') as output:
        writer = csv.writer(output, lineterminator='\n')
        for i1,i in enumerate(arcsoln):
            combo = []
            for j in range(int(numBreaks)):
                combo.append(arcsoln[i1][j*sizeBreak:(j+1)*sizeBreak])
            combo.append(arcfit[i1])    
            writer.writerow(combo)           
    
    return arcsoln
    return arcfit
    return combo
    
print "done"

#------------------------------------------------------#

print "\n>> DEFINING THE CUSTOM SEED POPULATION"

best = []
for parcel in parcelLst:
    if parcel[7] == 0:
        best.append(int(0))
    if parcel[7] > 0:
        best.append(int(1))

worst = []
for parcel in parcelLst:
    if parcel[7] == 0:
        worst.append(int(1))
    if parcel[7] > 0:
        worst.append(int(0))
        
randoms = int(0.9 * popSize)
edges = popSize - randoms
bestest = int(edges/4)
worstest = int(edges/4)
ones = int(edges/4)
zeros = popSize - randoms - ones - bestest - worstest

seedLst = []
for i in range(randoms):
    seedLst.append([random.randint(0, 1) for i in range(parcCnt)])
for i in range(ones):
    seedLst.append([1 for i in range(parcCnt)])
for i in range(zeros):
    seedLst.append([0 for i in range(parcCnt)])
for i in range(bestest):
    seedLst.append(best)
for i in range(worstest):
    seedLst.append(worst)

#------------------------------------------------------#

print "\n>> RUN THE ALGORITHM"

prng = random.Random()
prng.seed(time())

ea = inspyred.ec.emo.NSGA2(prng)
ea.variator = [inspyred.ec.variators.n_point_crossover, 
               inspyred.ec.variators.bit_flip_mutation]
ea.observer = CustomObserver
ea.terminator = inspyred.ec.terminators.generation_termination
final_pop = ea.evolve(generator=CustomGenerator, 
                      evaluator=CustomEvaluator,
                      pop_size=2000,
                      seeds=seedLst,
                      maximize=True,
                      nr_inputs = parcCnt,
                      max_generations=50,
                      mutation_rate=0.0009,
                      num_crossover_points=2)

print "done"

#------------------------------------------------------#

# Tidy folders

# clear in_memory again
arcpy.Delete_management("in_memory")

print "\nCleared folders of unneeded files"

#------------------------------------------------------#

# final script speed
stop = time()
diff = (stop-start)/60

print "\nStop the stopwatch at {0}".format(ctime())
print "Final run time = {0} minutes".format(diff)
print "Final run time = {0} hours".format(diff/60)

