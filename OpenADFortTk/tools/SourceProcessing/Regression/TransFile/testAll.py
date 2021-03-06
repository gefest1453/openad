#!/usr/bin/env python

import os
import sys
import shutil
import re

sepLength=80
makeCmd="make"
globalBatchMode=False
globalIgnoreFailingCases=False
globalOfferAcceptAsDefault=False
globalAcceptAll=False
globalDeclineAll = False
globalVerbose=False
globalOkCount=0
globalKnownFailCount=0
globalNewFailCount=0
globalUpdatedReferenceFlag = False
globalDiffCmd='diff'

class MultiColumnOutput:

    ourColumnSeparator=2
    
    @staticmethod
    def __getTermWidth():
        import termios, fcntl, struct, sys
        s = struct.pack("HHHH", 0, 0, 0, 0)
        fd_stdout = sys.stdout.fileno()
        cols=80
	try:
          x = fcntl.ioctl(fd_stdout, termios.TIOCGWINSZ, s)
          (rows,cols,xPix,yPix)=struct.unpack("HHHH", x)
	except IOError:
          debug("Error: cannot detect terminal width\n")
        return cols

    class ColumnInfo:

        ourMinimalWidth=2
        
        def __init__(self):
            self.width=self.ourMinimalWidth # minimal width

    def __init__(self,stringList) :
        self.stringList=stringList
        self.totalWidth=self.__getTermWidth()

    def __computeColumnInfos(self):
        goodColumnInfos=[]
        maxCols=self.totalWidth/self.ColumnInfo.ourMinimalWidth
        listLen=len(self.stringList)
        if listLen==0:
            return goodColumnInfos
        numCols=1 # 1 column to start with
        while (True) :
            if numCols>listLen:
                break
            columnInfos=[]
            colLength=listLen/numCols
            if (listLen%numCols>0):
                colLength+=1
            for i in xrange(0,numCols):
                columnInfos.append(self.ColumnInfo())
            for i in xrange(0,listLen):
                # determine the column:
                colForThis=i/colLength
                if columnInfos[colForThis].width<len(self.stringList[i])+self.ourColumnSeparator :
                    columnInfos[colForThis].width=len(self.stringList[i])+self.ourColumnSeparator
            totalColumnWidth=sum([ c.width for c in columnInfos ])
            if totalColumnWidth>self.totalWidth and numCols>1: 
              break
            goodColumnInfos=columnInfos[:]
            numCols+=1
        return goodColumnInfos
    
    def printIt(self):
        columnInfos=self.__computeColumnInfos()
        listLen=len(self.stringList)
        numCols=len(columnInfos)
        numRows=listLen/numCols
        if (listLen%numCols>0) :
            numRows+=1
        thisRow=0
        while thisRow<numRows:
            listPos=thisRow
            for columnInfo in columnInfos:
                if listPos>=listLen:
                    break
                outString=self.stringList[listPos]
                outString+=(' ' * (columnInfo.width - len(self.stringList[listPos])))
                sys.stdout.write(outString)
                listPos+=numRows
            sys.stdout.write('\n')
            thisRow+=1
        sys.stdout.flush()

class CommandError(Exception):
    """Exception thrown when a system command fails"""


class ComparisonError(Exception):
    """Exception thrown when the output comparison sees differences"""

class MakeError(Exception):
    """Exception thrown when the a make command fails"""

class CommandLineError(Exception):
    """Exception thrown when the script is invoked with bad command line arguments"""

class ConfigError(Exception):
    """Exception thrown when there is a problem with the environment configuration"""


def debug(debugMessage):
    if globalVerbose:
        sys.stdout.write(debugMessage)
        sys.stdout.flush()


def runCmd(cmd):
    debug('running "'+cmd+'"...\n')
    return os.system(cmd)


def verifyFile(aFile):
    if not (os.path.exists(aFile)):
        raise ConfigError, 'file '+aFile+' NOT verified: doesn\'t exist'


def queryFileCopy(queryStr):
    sys.stdout.write(queryStr+' ')
    if globalAcceptAll:
        sys.stdout.write(' (accepted on account of globalAcceptAll)\n')
        sys.stdout.flush()
        return 'y'
    elif globalDeclineAll:
        sys.stdout.write(' (declined on account of globalDeclineAll)\n')
        sys.stdout.flush()
        return 'n'
    else:
	if (globalBatchMode): 
            raise ComparisonError("Batch mode assumes no difference or one has to accept all differences")		
        answer = ''
        if (globalOfferAcceptAsDefault):
            answer = raw_input(' (y)/n: ')
            if (answer != 'n'):
                answer = 'y'
        else:
            answer = raw_input(' y/(n): ')
            if (answer != 'y'):
                answer = 'n'
        return answer


def refFileCopy(newFile,refFile):
    sys.stdout.write(refFile+' not available')
    sys.stdout.flush()
    answer = queryFileCopy(', copy and hg add it?')
    if (answer == "n"):
        return 1
    else:
        global globalUpdatedReferenceFlag
        globalUpdatedReferenceFlag = True
        if not os.path.isdir(os.path.split(refFile)[0]):
            os.mkdir(os.path.split(refFile)[0])
        shutil.copy(newFile,refFile)
        cmd="hg add "+refFile
        if runCmd(cmd): raise CommandError, cmd
        return 0


def fileCompare(newFile,refFile,ignoreString=''):
    verifyFile(newFile)
    # no reference file: ask whether we want to copy the new file and hg add it as the reference
    if not (os.path.exists(refFile)):
        refFileCopy(newFile,refFile)
    else:
        cmd="diff "
        if (ignoreString) : cmd+="-I '"+ignoreString+"' "
        cmd += newFile+' '+refFile        
        hasDiff = runCmd(cmd+" > /dev/null")
        if (hasDiff == 512):
            raise CommandError, "command "+cmd+" not successful"
        elif (hasDiff != 0):
            detailedDiffCmd = globalDiffCmd+' '+newFile+' '+refFile
            if runCmd(detailedDiffCmd) not in [0,256]: raise CommandError, detailedDiffCmd
            sys.stdout.write('Transformation -- '+detailedDiffCmd+'\n')
            if (queryFileCopy('   accept/copy new '+newFile+' to '+refFile+'?') == 'n'):
                sys.stdout.write('skipping change\n')
            else:
                global globalUpdatedReferenceFlag
                globalUpdatedReferenceFlag = True
                shutil.copy(newFile,refFile)
    sys.stdout.flush()


def printSep(sepChar,msg,sepLength):
    sys.stdout.write(msg)
    i = 0
    while (i < sepLength - len(msg)):
	sys.stdout.write(sepChar)
	i = i + 1
    sys.stdout.write("\n")
    sys.stdout.flush()

def numberedList(list):
    numberedList=[]
    digits=len(str(len(list)+1))
    format='%'+str(digits)+'d:%s'
    for (c,e) in enumerate(list):
        numberedName=format%(c+1,e)
        numberedList.append(numberedName)
    return numberedList

def populateExamplesList(args):
    import glob
    allExamples = glob.glob(os.path.join("Tests","*"))
    allExamples = [ os.path.split(e)[1] for e in allExamples]
    allExamples.sort(key=str.lower) # default sort is case sensitive, this one isn't
    rangeStart = 1
    rangeEnd = len(allExamples)
    examples = []
    exampleRegexs = []
    if (len(args) == 0): # no arguments
	if (globalBatchMode):
	    examples = allExamples
	else:
	    done = False
	    while not (done):
		done = True
		sys.stdout.write("pick from the following examples:\n")
		sys.stdout.flush()
                examples = allExamples
                MultiColumnOutput(numberedList(examples)).printIt()    
		examplesInput = raw_input("enter one or more regular expressions here or '(all [%i | %i %i])': ").split()
		if (len(examplesInput) == 0):			# no arguments
		    examples = allExamples
		elif (examplesInput[0] == "all"):		# the first argument is "all"
		    try:
			if (len(examplesInput) >= 2):
			    rangeStart = int(examplesInput[1])
			if (len(examplesInput) >= 3):
			    rangeEnd = int(examplesInput[2])
		    except ValueError:
			sys.stdout.write("\"all\" must be followed by zero, one, or two integers which specify the start and end range, e.g. 'all [%i | %i %i]'\n")
			sys.stdout.flush()
			done = False
			rangeStart = 1
			rangeEnd = len(allExamples)
		    if (rangeStart < 1 or rangeEnd > len(allExamples)):
			sys.stdout.write("invalid range (%i-%i)\n" % (rangeStart,rangeEnd))
			sys.stdout.flush()
			done = False
			rangeStart = 1
			rangeEnd = len(allExamples)
		    examples = allExamples
		else:						# one or more arguments, and the first one isn't "all"
		    exampleRegexs = examplesInput
    else: # at least one argument
	if (args[0] == "all"): # the first argument was all
	    examples = allExamples
	    try:
		if (len(args) >= 2): # A range START is given
		    rangeStart = int(args[1])
		if (len(args) >= 3): # A range END is also given
		    rangeEnd = int(args[2])
	    except ValueError:
		raise CommandLineError, "\"all\" must be followed by zero, one, or two integers which specify the start and end range, e.g. 'all [%i | %i %i]'"
	else: # each argument is treated as a regex
	    exampleRegexs = args

    # user running examples specified by one or more regular expressions
    if (len(examples) == 0):
	for regex in exampleRegexs:
	    for ex in allExamples:
		if (re.search(regex,ex,re.IGNORECASE)):
		    examples.append(ex)
	if (len(examples) == 0):
	    raise RuntimeError, "No examples match the given regular expressions"
	exampleSet = set(examples)
	examples = list(exampleSet)
	examples.sort(key=str.lower)
	rangeStart = 1

	rangeEnd = len(examples)
	printSep("=","",sepLength)
	sys.stdout.write("The following examples match the given regular expression(s):\n")
        MultiColumnOutput(numberedList(examples)).printIt()    
	printSep("=","",sepLength)
    else:
	printSep("=","",sepLength)
	sys.stdout.write("Running all examples with a range of (%i-%i):\n" % (rangeStart,rangeEnd))
        MultiColumnOutput(numberedList(examples)[rangeStart-1:rangeEnd]).printIt()    
	printSep("=","",sepLength)
    return (examples,rangeStart,rangeEnd)


def shouldRunTest(refFile,failFile) :
    global globalNewFailCount
    if os.path.exists(refFile) and not os.path.exists(failFile) :
        return True
    if os.path.exists(refFile) :
        sys.stdout.write("NOTE: both a reference file and a failure reason exist\n")
        sys.stdout.flush()
    if os.path.exists(failFile) :
        global globalKnownFailCount
        globalKnownFailCount += 1
        sys.stdout.write("   failure reason:")
        sys.stdout.flush()
        os.system("cat "+failFile)
        if globalBatchMode or globalIgnoreFailingCases:
            sys.stdout.write("skipping test\n")
            return False
        if globalOfferAcceptAsDefault and raw_input('run it anyway? (y)/n: ') != 'n' :
            sys.stdout.flush()
            globalNewFailCount -= 1
            return True
        elif raw_input('run it anyway? y/(n): ') == 'y' :
            sys.stdout.flush()
            globalNewFailCount -= 1
            return True
        else :
            sys.stdout.write("skipping test (on account of user input)\n")
            return False
    else :
        return True


def runTest(exName,exNum,totalNum,compiler,optimizeFlag,extraObjs):
    printSep("*","** testing %i of %i (%s)" % (exNum,totalNum,exName),sepLength)
    targBase='oad_untouched'
    l=[]
    if (os.path.isfile(os.path.join("Tests",exName,targBase+'.f'))):
        l.append(os.path.join("Tests",exName,targBase+'.f'))
    if (os.path.isfile(os.path.join("Tests",exName,targBase+'.f90'))):
        l.append(os.path.join("Tests",exName,targBase+'.f90'))
    if (len(l)!=1):
        raise ConfigError("found "+str(len(l))+" possible target files in "+exName)
    exOptsFileName=os.path.join("Tests",exName,'options')
    exOpts={}
    if (os.path.exists(exOptsFileName)):
        exOptsFile=open(os.path.join(exOptsFileName))
        try :
            exOpts=eval(exOptsFile.read().strip())
        except Exception, e :
            raise ConfigError, "options file "+exOptsFileName+" does not specify a Python dictionary "+str(e)
        if globalVerbose:
            sys.stdout.write("  extra options are : "+str(exOpts))
        permittedKeys=['OAD_active']
        if (any(map(lambda l: not (l in permittedKeys),exOpts.keys()))):
            raise ConfigError, "options file "+exOptsFileName+" contains key not in "+str(permittedKeys)

    targExtension=(os.path.splitext(l[0]))[1]
    cmd="ln -sf "+os.path.join("Tests",exName,'*.f*') + " ."
    if runCmd(cmd): raise CommandError, cmd

    transformFile=os.path.join(os.environ['OPENADFORTTK_BASE'],'tools','SourceProcessing','transformFile.py')
    transformedSource = targBase+'.ty'+targExtension
    transformedExec   = targBase+'.ty.run'
    transformedOutput = targBase+'.ty.out'

    refTransformedSource=os.path.join("Tests",exName,"refOutput",transformedSource)
    if not shouldRunTest(refTransformedSource,
                         os.path.join("Tests",exName,"FAIL",transformedSource)) :
        return
    verboseTransform = globalVerbose and ' -v' or ''
    # perform transform
    cmd=transformFile+' --check -d '+os.path.join("Tests",exName,'oad_transformed.decls.f90')+' '+targBase+targExtension+' -o '+transformedSource+verboseTransform
    if runCmd(cmd): raise CommandError, cmd
    fileCompare(transformedSource, refTransformedSource)
    
    # prepare the active module
    activeModuleDir="scalar"
    if "OAD_active" in exOpts:
        activeModuleDir=exOpts["OAD_active"]
    cmd="ln -sf "+os.path.join('..','Extras',activeModuleDir,'OAD_active.f90')+" ."
    if runCmd(cmd): raise CommandError, cmd
    cmd=compiler+" "+optimizeFlag+" "+os.environ['F90FLAGS']+" -c OAD_active.f90"
    if runCmd(cmd): raise CommandError, cmd

    # compile + link 
    cmd=compiler+" "+optimizeFlag+" "+os.environ['F90FLAGS']+" -o " +transformedExec+' oad_transformed.f90 '+transformedSource+' driver.f90 ' +extraObjs
    if runCmd(cmd): raise CommandError, cmd
    cmd='./'+transformedExec+' > '+transformedOutput
    if runCmd(cmd): raise CommandError, cmd
    fileCompare(transformedOutput,os.path.join("Tests",exName,"refOutput",transformedOutput))
    global globalOkCount
    globalOkCount+=1


def main():
    import glob
    from optparse import OptionParser
    usage = '%prog [options] '
    compilers=['ifort','gfortran','g95','f95','openf95']
    compilerOpts='[ '
    for i in compilers :
        compilerOpts+=i
        if  i != compilers[-1]:
            compilerOpts+=" | "
    compilerOpts+=" ]"        
    opt = OptionParser(usage=usage)
    opt.add_option('-i','--ignoreFailingCases',dest='ignoreFailingCases',
                   help="don't if we should try to run  the cases known to fail",
                   action='store_true',default=False)
    opt.add_option('-a','--offerAcceptAsDefault',dest='offerAcceptAsDefault',
                   help="offer accept as default for updating reference files",
                   action='store_true',default=False)
    opt.add_option('-A','--acceptAll',dest='acceptAll',
                   help="accept all changes without confirmation",
                   action='store_true',default=False)
    opt.add_option('-D','--declineAll',dest='declineAll',
                   help="decline all changes without confirmation",
                   action='store_true',default=False)
    opt.add_option('-b','--batchMode',dest='batchMode',
                   help="run in batchMode suppressing output",
                   action='store_true',default=False)
    opt.add_option('-d','--diff',dest='diff',
                   help="different diff command (e.g. kdiff3) to show differences in case the regular diff detects differences")
    opt.add_option('-v','--verbose',dest='verbose',
                   help="let the pipeline components produce some extra output",
                   action='store_true',default=False)
    opt.add_option('-c','--compiler',dest='compiler',
                   type='choice', choices=compilers,
                   help="pick a compiler (defaults to ifort) from the following list: " +compilerOpts+" - the compiler should be in PATH; we use F90FLAGS when set in the environment",
                   default='ifort')
    opt.add_option('-O','--optimize',dest='optimize',
                   help="turn compiler optimization on (default off)",
                   action='store_true',default=False)
    opt.add_option('-u','--unstructure',dest='unstructured',
                   help="translate as unstructured control flow (default off)",
                   action='store_true',default=False)
    (options, args) = opt.parse_args()
    global globalNewFailCount
    globalNewFailCount=0
    try:
        if os.environ.has_key('BATCHMODE') or options.batchMode :
            global globalBatchMode
            globalBatchMode=True
        if options.ignoreFailingCases :
            global globalIgnoreFailingCases
            globalIgnoreFailingCases=True
        if options.offerAcceptAsDefault :
            global globalOfferAcceptAsDefault
            globalOfferAcceptAsDefault=True
        if options.acceptAll :
            global globalAcceptAll
            globalAcceptAll=True
        if options.declineAll:
            global globalDeclineAll
            globalDeclineAll = True
        if options.diff :
            global globalDiffCmd
            globalDiffCmd=options.diff 
        if options.verbose :
            global globalVerbose
            globalVerbose=True
        if options.optimize :
            os.environ['OPTIMIZE']='true'
	if not (os.environ.has_key('OPENADFORTTK_BASE')):
	    raise ConfigError, "environment variable OPENADFORTTK_BASE not defined"
	if not (os.environ.has_key('F90FLAGS')):
            os.environ['F90FLAGS']=''
        optimizeFlag='-O0 -g'
        if options.optimize:
	  optimizeFlag='-O3'
        extraFiles = glob.glob(os.path.join('..','Extras','*.f90'))
        extraFiles.sort()
        extraFiles.reverse()
        extraObjs=""
        for extraFile in extraFiles:
            cmd=options.compiler+" "+optimizeFlag+" "+os.environ['F90FLAGS']+" -c "+ extraFile
            if (os.system(cmd)):
                raise MakeError, "Error while executing \"" + cmd + "\""
            extraObjs+=(os.path.splitext(os.path.basename(extraFile)))[0]+".o "
	(examples,rangeStart,rangeEnd) = populateExamplesList(args[0:])
	# Run the examples
	j = rangeStart-1
	while (j < rangeEnd):
	    try:
		runTest(examples[j],j+1,len(examples), options.compiler, optimizeFlag, extraObjs)
	    except ConfigError, errMsg:
		print "ERROR (environment configuration) in test %i of %i (%s): %s" % (j+1,len(examples),examples[j],errMsg)
	        globalNewFailCount+=1
		if not (globalBatchMode):
		    if (raw_input("Do you want to continue? (y)/n: ") == "n"):
			return -1
		else:
		    return -1
	    except MakeError, errMsg:
		print "ERROR in test %i of %i (%s) while executing \"%s\"." % (j+1,len(examples),examples[j],errMsg)
	        globalNewFailCount+=1
		if not (globalBatchMode):
		    if (raw_input("Do you want to continue? (y)/n: ") == "n"):
			return -1
		else:
		    return -1
	    except ComparisonError, errMsg:
		print "ERROR in test %i of %i (%s): %s." % (j+1,len(examples),examples[j],errMsg)
	        globalNewFailCount+=1
		if not (globalBatchMode):
		    if (raw_input("Do you want to continue? (y)/n: ") == "n"):
			return -1
                else: 
	            return -1
            except CommandError, cmd:
                print 'ERROR in test %i of %i (%s): CommandError while running "%s"' % (j+1,len(examples),examples[j],cmd)
	        globalNewFailCount+=1
		if not (globalBatchMode):
		    if (raw_input("Do you want to continue? (y)/n: ") == "n"):
			return -1
		else:
			return -1
	    except RuntimeError, errMsg:
		print "ERROR in test %i of %i (%s): %s." % (j+1,len(examples),examples[j],errMsg)
	        globalNewFailCount+=1
		if globalBatchMode or \
                   raw_input("Do you want to continue? (y)/n: ") == "n" :
		    return -1
            printSep("*","",sepLength)
	    j = j + 1
        # if we have updated any of the reference output, also update the version information
        if globalUpdatedReferenceFlag:
            openadStatusCmd = os.path.join(os.environ['OPENADROOT'],'bin','openadStatus')
            cmd = openadStatusCmd+' -ld | grep -v Regression | grep  SourceProcessing > versionInfo.txt'
            if runCmd(cmd): raise CommandError, cmd
    except CommandError, cmd:
	print 'CommandError: error while running "'+str(cmd)+'"\n'
	return -1
    except ConfigError, errMsg:
	print "ERROR (environment configuration):",errMsg
	return -1
    except CommandLineError, errMsg:
	print "ERROR (command line arguments):",errMsg
	return -1
    except RuntimeError, errMsg:
	print 'caught exception: ',errMsg
	return -1
    print "total: "+str(rangeEnd-rangeStart+1)+", ran  OK:"+str(globalOkCount)+", known errors:"+str(globalKnownFailCount)+", new errors:"+str(globalNewFailCount)
    return 0

if __name__ == "__main__":
    sys.exit(main())

