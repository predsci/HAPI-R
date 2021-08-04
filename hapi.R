hapi <- function(SERVER = NULL, DATASET = NULL, PARAMETERS = NULL, START = NULL, STOP = NULL, OPTS = NULL){
# HAPI - Interface to Heliophysics Data Environment API
#
#   HAPI.m gets metadata and data from a <a href="https://github.com/hapi-server/">HAPI v1.1</a> compliant
#   data server. See <a href="./html/hapi_demo.html">hapi_demo</a> for usage examples.
#
#   This is a command-line only interface.  For a GUI for browsing and
#   searching servers, datasets, and parameters, see http://tsds.org/get/.
#   Select a catalog, dataset, parameter, and time range and then request a
#   MATLAB script as an output.  A script including a call to this script
#   will be generated that can be pasted onto the command line.
#
#   <a href="http://tsds.org/get/#catalog=SSCWeb&dataset=ace&parameters=X_TOD&start=-P2D&stop=2017-08-27&return=script&format=matlab">Example of search result</a>.
#
#   Servers = HAPI() or HAPI() returns a cell array of URL strings
#   or lists data server URLs from <a href="https://github.com/hapi-server/servers/servers.json">known HAPI servers</a>.
#
#   Dataset = HAPI(Server) or HAPI(Server) returns or lists datasets
#   available at server URL Server.
#
#   Meta = HAPI(Server, Dataset) returns metadata for all parameters
#   in dataset Dataset from server Server.
#
#   Meta = HAPI(Server, Dataset, Parameters) returns metadata
#   associated with one or more parameter strings in Parameters.
#   Parameters can be a comma-separated string or cell array.
#
#   [Data,Meta] = HAPI(Server, Dataset, Parameters, Start, Stop) returns
#   the data and metadata for for the requested parameters. If Parameters =
#   '', all parameters are returned.
#
#   Start and Stop must be time stamps of the form YYYY-mm-DDTHH:MM:SS.SSS
#   or YYYY-DDDTHH:MM:SS.SSS and truncated timestamps are allowed (e.g.,
#   YYYY-mm-DD, YYYY-DDD, YYYY-mm-DDTHH, etc.
#
#   An extra field is added to the returned Data structure named
#   DateTimeVector, which is a matrix with columns of at least Year, Month,
#   Day, and then Hour, Minute, Second, Millisecond, Microsecond, depending
#   on the precision of the time stamps in the data returned from the
#   server, e.g., if the timestamps are of the form 2000-01, then
#   DateTimeVector will have only three columns (the day is assumed to be
#   zero). Data.DateTimeVec can be used to directly compute a MATLAB
#   DATENUM using, e.g.,
#
#      datenum(Data.DateTimeVec(:,1:3)) or
#      datenum(Data.DateTimeVec(:,1:6)) or
#      datenum(Data.DateTimeVec(:,1:6)) + Data.DateTimeVec(:,7)/86400
#
#   Data.DateTimeVec will always have either 3 columns or 6+ columns
#   depending on the precision of the returned data.  Note that MATLAB's
#   DATENUM is only accurate to 1 ms resolution, so that
#
#    datenum(Data.DateTimeVec(:,1:6)) ...
#      + Data.DateTimeVec(:,7)/86400 ..
#      + Data.DateTimeVec(:,8)/86400000
#
#   is not meaningful.
#
#   Options are set by passing a structure as the last argument with fields
#
#     logging (default false)   - Log to console
#     cache_hapi (default true) - Cache data in ./hapi-data
#     use_cache (default true)  - Use files in ./hapi-data if found
#
#   Note that file locking is not implemented for ./hapi-data.
#
#   To reverse default options, use
#     OPTS = struct();
#     OPTS.logging      = 1;
#     OPTS.cache_hapi   = 0;
#     OPTS.use_cache    = 0;
#     HAPI(...,OPTS)
#
#   Version 2017-06-18.
#
#   For bug reports and feature requests, see
#   <a href="https://github.com/hapi-server/client-matlab/issues">https://github.com/hapi-server/client-matlab/issues</a>
#
#   See also HAPI_DEMO, DATENUM.

##########################################################################
# Author: R.S Weigel <rweigel@gmu.edu>
# License: This is free and unencumbered software released into the public domain.
# Repository: https://github.com/hapi-server/client-matlab.git
##########################################################################

##########################################################################
# Default Options
#
# Implemented options:
 DOPTS <- list()
 DOPTS$logging <- 0
 DOPTS$cache_mlbin <- 1
 DOPTS$cache_hapi <- 1
 DOPTS$use_cache <- 0
 DOPTS$format <- 'csv'
      
#logging: Save data requested in MATLAB binary for off-line use.
#cache_mlbin: Save responses in files (HAPI CSV, JSON, and Binary) for debugging.
#use_cache: Use cached MATLAB binary file associated with request if found.
#format: If 'csv', request for HAPI CSV will be made even if server supports HAPI Binary. (For debugging.)

 DOPTS$serverlist <- 'https://raw.githubusercontent.com/hapi-server/servers/master/all.txt'
 DOPTS$scripturl <- 'https://raw.githubusercontent.com/hapi-server/client-matlab/master/hapi.m'


 DOPTS$update_script <- 0

# Not implemented:
#DOPTS <- c(DOPTS, hapi_data = "./hapi-data") # Where to store cached data.
#DOPTS <- c(DOPTS, split_long = 0) # Split long requests into chunks and fetch chunks.
#DOPTS <- c(DOPTS, parallel_req  = 0) # Use parallel requests for chunks.
##########################################################################

##########################################################################
# Extract options (TODO: Find better way to do this.)
 nargin <- function() {
   if(sys.nframe()<2) stop("must be called from inside a function")
   length(as.list(sys.call(-1)))-1
 }
 nin <- nargin()
  if ((!is.null(SERVER)) & is.list(SERVER)){
  OPTS <- SERVER
  SERVER <- NULL
  }
 if ((!is.null(DATASET)) & is.list(DATASET)){
   OPTS <- DATASET
   DATASET <- NULL
 }
 if ((!is.null(PARAMETERS)) & is.list(PARAMETERS)){
   OPTS <- PARAMETERS
   PARAMETERS <- NULL
 }
 if ((!is.null(START)) & is.list(START)){
   OPTS <- START
   START <- NULL
 }
 if ((!is.null(STOP)) & is.list(STOP)){
   OPTS <- STOP
   STOP <- NULL
 }


if (!is.null(OPTS)){
    keys <- names(OPTS)
    nin <- nin-1
    if (length(keys) != 0){
        for (i in 1:length(keys)){
            DOPTS <- c(DOPTS, keys[i] = OPTS[[i]])
        }
    }
}
##########################################################################

##########################################################################
# Get latest version of script
# TODO: Consider nagging once per day with message for how to turn off.
if (DOPTS.update_script != 0){
    # MD5 method based on
    # http://www.mathworks.com/matlabcentral/fileexchange/55746-javamd5
    md <- md5sum('MD5')
    fid <- file("hapi.r")
    open(fid, "r")
    as.uint8 <- function(x) {
      i <- suppressWarnings(as.integer(round(x)))
      if (any(is.na(i))) {
        stop("Non-integer values in:", paste(x, collapse=", "))
      }
      if(as.raw(i > 255)){
        x <- as.raw(255)
      }
      else if(as.raw(i < 0)){
        x <- as.raw(0)
      }
      else{
        x <- as.raw(i)
      }  
      attr(x, 'class') <- 'uint8'
      x
    }
    uint8 <- function(length=0) {
      x <- as.raw(integer(length))
      attr(x, 'class') <- 'uint8'
      x
    }
    digest <- as.hexmode(as.uint8(md.digest(fread(fid, inf, '*uint8'))))
    md5a <- tolower(digest)
    utils::download.file(DOPTS.scripturl, ".hapi.r")
    fid <- file(".hapi.r")
    open(fid, "r")
    digest <- as.hexmode(as.uint8(md.digest(fread(fid, inf, '*uint8')),'uint8'))
    md5b <- tolower(digest)
    if (!grepl(md5a,md5b)){
        #reply = input('Newer version of hapi.m found.  Install? [y]/n:','s');
        #if strcmp(lower(reply),'y')
        fname <- toString('.hapi.r')
        fname <- paste(fname,toString(Sys.Date()), sep = ' ')
        my.file.rename <- function(from, to) {
          todir <- dirname(to)
          if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
          file.rename(from = from,  to = to)
        }
        my.file.rename(from = 'hapi.r', to = fname)
        my.file.rename(from = '.hapi.r', to = 'hapi.r')
        pracma::fprintf('Updated hapi.m. Old version saved as %s. Use options to disable updates.\n',file = fname)
        #else
        #    pracma::fprintf('Not updating hapi.m. Update attempts can be disabled with options.\n');
        #end
    }
}
##########################################################################

##########################################################################
# Get list of servers
if (nin == 0){
    if (DOPTS$logging != 0) { Pracma::fprintf('Reading %s ... ',file = DOPTS$serverlist)}
    str <- toString(readLines(DOPTS.serverlist))
    data <- scan(text = str, what = "")
    if (DOPTS$logging != 0) {Pracma::fprintf('Done.\n');}

    if ((DOPTS.logging != 0) || (nargout == 0)){
        Pracma::fprintf('List of HAPI servers in %s:\n', file = DOPTS$serverlist)
        for (i in 1:length(data)){
            Pracma::fprintf('  %s\n',data[i])
        }
        Pracma::fprintf('\n')
    }
    
}
##########################################################################

##########################################################################
# Datasets
if (nin == 1){

    url <- c(SERVER,'/catalog/')
    if (DOPTS$logging != 0) {Pracma::fprintf('Reading %s ... ',url)}

    opts <- weboptions('ContentType', 'json')
    data <- tryCatch({
      data <- webread(url,opts)
      #readLines to string
      
      }, error = function(err) {
        print("Error")
        return(NA)
      }
    ) 

    if (DOPTS$logging != 0 || nargout == 0){
        Pracma::fprintf('\nAvailable datasets from %s:\n',SERVER)
    }

    if (is.list(data$catalog)){
        # Make data.catalog cell array of structs.
        # Why is data.catalog a struct array instead of
        # a cell array of structs like data.parameters?
        # Perhaps when JSON array has objects with only one key?
        ids <- data$catalog$id
        for (i in 1:length(ids)){
           tmp$id <- list(ids[[i]])
        }
        data$catalog <- tmp
    }

    for (i in 1:length(data$catalog)){
        if (DOPTS$logging !=0 || nargout == 0){pramca::fprintf('  %s\n',data$catalog[[i]]$id)}
    
    if (DOPTS.logging !=0 || nargout == 0){pracma::fprintf('\n')}
    
}
##########################################################################

##########################################################################
# Parameters
if (nin == 2){

    url <- c(SERVER,'/info?id <- ',DATASET)

    if (DOPTS$logging != 0) {pracma::fprintf('Downloading %s ... ',url)}
	  opts <- weboptions('ContentType', 'json')
	  data <- tryCatch({
	    data <- webread(url,opts)
	    
	  }, error = function(err) {
	    print("Error")
	    return(NA)
	  }
	  ) 
    if (DOPTS$logging !=0){ pracma::fprintf('Done.\n')}

    start <- data$startDate
    stop  <- data$stopDate

    if (DOPTS.logging !=0 || nargout == 0){
        pracma::fprintf('Available parameters in %s from %s:\n',SERVER)
        pracma::fprintf('Available parameters in %s from %s:\n',DATASET)
        pracma::fprintf('Time range of availability: %s-%s\n',stop)
        pracma::fprintf('Time range of availability: %s-%s\n',start)
        for (i in 1:length(data$parameters)){
            desc <- 'No description'
            if (exists(data$parameters[[i]]$description)){
                desc <- data$parameters[[i]]$description
            }
            pracma::fprintf('  %s - %s\n',data$parameters[[i]]$name,desc)
        }
    }
    
}

##########################################################################
# Data
if (nin == 3 || nin == 5){

    if (is.list(PARAMETERS)){
      tot <- ""
      for(i in length(PARAMETERS)) {str <- toString(PARAMETERS[[i]])
      tot <- paste(tot, str, sep = '')
      }
      tot <- gsub(",","", tot)
      PARAMETERS <- scan(text = tot, what = "")
    }

    if (DOPTS.cache_mlbin != 0 || DOPTS.cache_hapi!= 0 || DOPTS.use_cache!= 0){
        # Create directory name from server URL
        urld <- pracma::regexprep(SERVER,'https*://(.*)','$1');
        urld <- paste('hapi-data/',pracma::regexprep(urld,'/|:','_'))
        if (nin == 5){# Data requested
            fname <- paste(pracma::regexprep(DATASET,'/|:','_'),pracma::regexprep(PARAMETERS,',','-'),pracma::regexprep(START,'-|:\.|Z',''), pracma::regexprep(STOP,'-|:|\.|Z',''), sep = " ")
            fnamecsv  <- paste(urld,'/',fname,'.csv')
            fnamebin  <- paste(urld,'/',fname,'.bin')
            fnamefbin <- paste(urld,'/',fname,'.fbin')
            fnamemat  <- paste(urld,'/',fname,'.mat')
            urlcsv  <- paste(SERVER,'/data?id <- ',DATASET,'&time.min <- ',START,'&time.max <- ',STOP)
            if (length(PARAMETERS) > 0){# Not all parameters wanted
                urlcsv <- paste(urlcsv,'&parameters <- ',PARAMETERS)
            }
            urlfbin <- paste(urlcsv,'&format in fbinary')
            urlbin  <- paste(urlcsv,'&format in binary')
        }
        urljson <- paste(SERVER,'/info?id <- ',DATASET)
        if (length(PARAMETERS) > 0){# Not all parameters wanted
            urljson <- paste(urljson,'&parameters <- ',PARAMETERS)
        }
        fnamejson <- paste(pracma::regexprep(DATASET,'/|:','_'),pracma::regexprep(PARAMETERS,',','-'), sep = " ")
        fnamejson <- paste(urld,'/',fnamejson,'.json')
    }

    if (DOPTS$use_cache && nin == 5){
        # Read cached .mat file and return
        if (file.exists('fnamemat')){
            if (DOPTS$logging != 0) {pracma::fprintf('Reading %s ... ',file = 'fnamemat')}
            load(fnamemat)
            if (DOPTS$logging !=0){ pracma::fprintf('Done.\n')}
            
        }
    }

    if (DOPTS$cache_mlbin || DOPTS$cache_hapi){
        # Create directory
        if (logcondens.mode::dir.exists('hapi-data')){
          icesTAF::mkdir('hapi-data')
        }
      if (logcondens.mode::dir.exists('urld')){
        icesTAF::mkdir('urld')
      }
    }

    if (DOPTS.logging) {pracma::fprintf('Downloading %s ... ',urljson)}
	opts <- weboptions('ContentType', 'json')
	meta <- tryCatch({
	  meta <- webread(urljson,opts)
	  
	}, error = function(err) {
	  print("Error")
	  return(NA)
	}
	) 

    # Catch case where server returns metadata for all parameters
    # instead of just the ones requested and then correct meta.parameters.
    # Remove when that server is fixed (voyager.gsfc.nasa.gov/hapiproto).
    timename <- meta.parameters[[1]]$name
    if (length(PARAMETERS) > 0){# PARAMETERS='' not given
        wanted <- strsplit(PARAMETERS, split = ',')
        k <- 1
        for (i in 1:length(meta$parameters)){
            given[[i]] <- meta.parameters[[i]]$name
            if (any((given[[i]]==wanted) || (given[[i]]==timename))){
                newlist <- list()
                newlist[[k]] <- meta.parameters[[i]]
                k <- k+1
            }
        }
        meta$parameters <- newlist
        if (wanted == timename){
            if (length(newlist) != length(wanted)){
                pracma::fprintf('\n')
                warning('Server returned too many parameters in /info request')
              
            }
        } else {
            if (length(newlist) != (length(wanted)+1)){
                pracma::fprintf('\n')
                warning('Server returned too many parameters in /info request')
            }
        }
    }

    if (DOPTS$logging) {pracma::fprintf('Done.\n')}
    if (nin == 3){
      # Only metadata wanted.  Return it.
        data <- meta
    }

    if (DOPTS$cache_hapi){
        # Ideally the meta variable read above could be serialized to JSON,
        # so second request is not needed to write the JSON file, but
        # serialization is not simple to do and webread can't read from a
        # file. TODO: Look at code for webread() to find out what
        # undocumented function is used to convert JSON to MATLAB
        # structure and use it here instead of websave().
        fnamejson <- read_file(urljson)
        if (DOPTS$logging) {pracma::fprintf('Wrote %s ...\n',fnamejson)}
    

    if (!(DOPTS.format =='csv')){
        # Determine if server supports binary
        url <- paste(SERVER,'/capabilities')
        if (DOPTS$logging) {pracma::fprintf('Reading %s ... ',url)}
        opts <- weboptions('ContentType', 'json')
        caps <- webread(url,opts)
        if (DOPTS$logging){ pracma::fprintf('Done.\n')}
        if (any(caps$outputFormats == 'binary')){
            binaryavailable <- 1
        }
    }

    if (!((DOPTS.format == 'csv') && binaryavailable)){
        # Binary read.
        if (DOPTS$logging) {pracma::fprintf('Downloading %s ... ',urlfbin)}
        # Fastest method based on tests in format_compare.m
        utils::download.file(urlfbin, fnamefbin)
        if (DOPTS$logging) {pracma::fprintf('Done.\n')}
        if (DOPTS$logging) {pracma::fprintf('Reading %s ... ',fnamefbin)}
        fid <- file("fnamefbin")
        open(fid, "r")
        p <- (fread(fid,21,'uint8 <- >char'))
        n <- as.numeric(p[1])
        data <- as.numeric(read.csv(fid))
        if (DOPTS.logging) {pracma::fprintf('Done.\n')}
        psize <- 1 + meta$parameters[[2]].size
        data <- as.matrix(data, nrow = psize)
        zerotime <- p(2:length(p))
        data(:,1) <- datenum(zerotime,'yyyy-mm-ddTHH:MM:SS') + data(:,1)/(86400*10^(3*n))

        if (!DOPTS$cache_hapi){
            file.remove(fnamefbin)
        }

        # Should use _x instead of x_, but _x is not an allowed field name.
        # Extra info needed later.
        meta$x_$format    <- 'csv'
        meta$x_$url       <- urlbin
        meta$x_$cachefile <- fnamebin
    } else {
        # CSV read.
        if (DOPTS.cache_hapi){
            # Save to file and read file
            if (DOPTS.logging) {pracma::fprintf('Downloading %s ... ',urlcsv)}
            #urlwrite(urlcsv,fnamecsv);
            readfile(fnamecsv,urlcsv)
            if (DOPTS.logging) fprintf('Done.\n');}{
            if (DOPTS.logging) fprintf('Reading %s ... ',fnamecsv);}{
            fid <- fopen(fnamecsv,'r')
            str <- fscanf(fid,'%c')
            fclose(fid)
            if (DOPTS.logging) fprintf('Done.\n');}{
        } else {
            # Read into memory directly.
            if (DOPTS.logging) fprintf('Reading %s ... ',urlcsv);}{
            str <- urlread(urlcsv)
            if (DOPTS.logging) fprintf('Done.\n');}{
        }

        # Ifc = index of first comma.
        # Assumes time stamps + whitespace <= 40 characters.
        Ifc <- findstr(str(1:min(40,length(str))),',')
        t1 <- deblank(str(1:Ifc-1))

        [rformat,twformat,na] in timeformat(t1)){

        # Number of time columns that will bre created using twformat read.
        ntc     in length(findstr('d',twformat))){
        lcol(1) <- ntc# Last time column.

        for (i in 2:length(meta.parameters)){# parameters{1} is always Time
            pnames{i-1} <- meta.parameters{i}.name
            ptypes{i-1} <- meta.parameters{i}.type
            psizes{i-1} <- [1]
            if (isfield(meta.parameters{i},'size')){
                psizes{i-1} <- meta.parameters{i}.size
            }
            if (i == 2,a <- ntc;,else,a <- lcol(i-2);,}){
            fcol(i-1) <- a + 1# First column of parameter
            lcol(i-1) <- fcol(i-1)+prod(psizes{i-1})-1# Last column of parameter

            if (strcmp(ptypes{i-1},'integer')){
                rformat in [rformat,repmat('%d ',1,prod(psizes{i-1}))]){
            }
            # float should not be needed, but common error
            if (strcmp(ptypes{i-1},'double') || strcmp(ptypes{i-1},'float')){
                rformat in [rformat,repmat('%f ',1,prod(psizes{i-1}))]){
            }
            if (any(strcmp(ptypes{i-1},{'isotime','string'}))){
                plengths{i-1}  <- meta.parameters{i}.length - 1
                rformat in [rformat,repmat(['%',num2str(plengths{i-1}),'c '],1,prod(psizes{i-1}))]){
            }
        }
        if (DOPTS.logging) fprintf('Parsing %s ... ',fnamecsv);}{
        fid <- fopen(fnamecsv,'r')
        A in textscan(fid,rformat,'Delimiter',',')){
        fclose(fid)

        if (isempty(A{length(A)})){# Catches case when rformat is wrong.
            error(sprintf('\nError in CSV read of %f\n',fnamecsv))
        }

        # Check for correct number of commas. Remove in production.
        # TODO: Use ReturnOnError option of TEXTSCAN.
        [s,r] <- system(sprintf('wc %s | tr -s [:blank:] | cut -d" " -f2',fnamecsv))
        if (0 & s == 0){# TODO: Only works on OS-X and Linux
            # Check A to make sure it has same number of rows
            # as number of rows in file. See hapi_test for example
            # when this error is caught.  Much faster than using
            # native MATLAB function.  Remove for production.
            nrows <- str2num(r)
            for (i in 1:length(A)){
                nread <- size(A{i},1)
                if (nread != nrows){
                    error(sprintf('\nNumber of rows read (%d) does not match number of rows in\n%s (%d).\nPlease report this issue at https://github.com/hapi-server/client-matlab/issues',fnamecsv,nread,nrows))
                }
            }
        }

        DTVec <- transpose(cat(2,A{1:ntc}))
        # DTVec is matrix with columns of Yr,Mo,Dy,Hr,Mn,Sc,Ms,...
        # or Yr,Doy,Hr,Mn,Sc,Ms,....

        # Return exact time strings as found in CSV. Should this even be
        # returned?  Probably won't be used. Doubles the parsing time.
        timelen <- length(t1)
        Time in sprintf(twformat,DTVec(:))){# datestr() is more straightforward, but is very slow.
        if (mod(length(Time),timelen) != 0){
            error(sprintf('\nError in CSV read of %f\n',fnamecsv))
        }
        Time <- reshape(Time,timelen,length(Time)/timelen)'

        # Convert DOY to Month Day in DTVec and make last column the number
        # of milliseconds (if 1-2 decimal places in t1), microseconds (if
        # 4-5 decimal places in t1), etc.
        DTVec <- normalizeDTVec(DTVec,t1,na)

        data <- struct('Time',Time,'DateTimeVector',DTVec')

        for (i in 1:length(pnames)){
            if (any(strcmp(ptypes{i},{'isotime','string'}))){
                # Array parameter of type isotime or string
                pdata <- A(fcol(i):lcol(i))
            } else {
                pdata <- cat(2,A{fcol(i):lcol(i)})
            }
            pdata <- reshape(pdata,[size(pdata,1),psizes{i}(:)'])
            data  <- setfield(data,pnames{i},pdata)
        }

        meta.x_.format    in 'csv'){
        meta.x_.url       <- urlcsv
        meta.x_.cachefile <- fnamecsv

        if (DOPTS.logging) fprintf('Done.\n');}{

    }

    # Save extra metadata about request in MATLAB binary file
    # (_x is more consistent with HAPI spec, but not allowed as field
    # name.)
    meta.x_ <- struct()
    meta.x_.server     <- SERVER
    meta.x_.dataset    <- DATASET
    meta.x_.parameters <- PARAMETERS
    meta.x_.time_min   <- START
    meta.x_.time_max   <- STOP

    if (DOPTS.cache_mlbin){
        if (DOPTS.logging) fprintf('Saving %s ... ',fnamemat);}{
        save(fnamemat,'urlcsv','data','meta')
        if (DOPTS.logging) fprintf('Done.\n');}{
    }

}

normalizeDTVec <- function(DTVec,t1,na){

    DTVec(},:) <- DTVec(},:)*10^na
    DTVec <- DTVec'

    if (size(DTVec,2) > 1){
        if (length(t1) > 4 && !isempty(regexp(t1,'[0-9]{4}-[0-9]{3}'))){
            # Second column of DTVec is day-of-year
            # Make second and third column month and day.
            if (size(DTVec,2) > 2){
                DTVec <- [DTVec(:,1),doy2md(DTVec(:,1:2)),DTVec(:,3:length(DTVec))]
            } else {
                DTVec <- [DTVec(:,1),doy2md(DTVec(:,1:2))]
            }
        }
    }

    if (size(DTVec,2) < 3){
        DTVec <- [DTVec,ones(size(DTVec,1),3-size(DTVec,2))]
    }
    if (size(DTVec,2) > 3 && size(DTVec,2) < 6){
        DTVec <- [DTVec,ones(size(DTVec,1),3-size(DTVec,2))]
    }
    DTVec <- DTVec'

doy2md <- function(ydoy){
# DOY2MD - Convert year/day-of-year to month/day-of-month
#
#   md = DOY2MD(ydoy) where ydoy is a two-column matrix with columns
#   of year and doy and md is a two-column int32 matrix with columns of
#   month and day.

    ydoy <- double(ydoy)
    I                 <- (rem(ydoy(:,1),4) == 0 & rem(ydoy(:,1),100) != 0) | rem(ydoy(:,1),400) == 0
    II                <- [zeros(size(ydoy,1),2) , repmat(I,1,10)]
    day_sum           <- [0 31 59 90 120 151 181 212 243 273 304 334]
    delta             <- repmat(ydoy(:,2),1,12)-(repmat(day_sum,size(ydoy,1),1) + II)
    delta(delta <= 0) <- 32
    [D,M]             <- min(delta')
    md                <- int32([M',floor(D')])

 <- function(t1){
# TIMEFORMAT - Compute a read and write format string
#
#   [trf,twf,tlen,na] = TIMEFORMAT(t), where the sample time string t is a restricted
#   set of ISO 8601 date/time strings. See
#   https://github.com/hapi-server/data-specification/blob/master/HAPI-data-access-spec.md#representation-of-time
#   for a definition of the allowed date/time strings.
#
#   If t = '1999-11-12T00', then
#   tr = '#4d-#2d-#2dT#2d'
#   tw = '#4d-#02d-#02dT#02d'
#   tw is always the read format with #Nd replaced with #0Nd if N > 1.
#
#   When the sample time string has values after the decimal, na is
#   the number of zeros that must be appended to the last number read
#   such that it represents a millisecond, microsecond, nanosecond, etc.
#   For example, if
#   t = '1999-11-12T23:59:59.12', then
#   tr = '#4d-#2d-#2dT#2d:#2d:#2d.#2d'
#   and na = 1 and the last read number should be multiplied by 10^na to
#   make it a millisecond.
#
#   See also TIMEFORMAT_TEST.

    Z <- ''
    if (strcmp(t1(length(t1)),'Z')){# Remove trailing Z.
        Z <- t1(length(t1))
        t1 <- t1(1:}-1)
    }

    # TODO: Test all possible valid time representation lengths.
    if (length(t1) == 7 || length(t1) == 8){
        if (isempty(regexp(t1,'[0-9]{4}-[0-9]{2}')) && isempty(regexp(t1,'[0-9]{4}-[0-9]{3}'))){
            error('Unrecognized time format of first string %s',t1)){
        }
    }

    timelen  <- length(t1) + length(Z)# Length of time string.
    trformat in t1){# Time read format.

    trformat in regexprep(trformat,'^[0-9]{4}','%4d')){
    trformat in regexprep(trformat,'%4d-[0-9][0-9][0-9]','%4d-%3d')){
    trformat in regexprep(trformat,'%4d-[0-9][0-9]','%4d-%2d')){
    trformat in regexprep(trformat,'%4d-%2d-[0-9][0-9]','%4d-%2d-%2d')){
    trformat in regexprep(trformat,'T[0-9][0-9]','T%2d')){
    trformat in regexprep(trformat,'T%2d:[0-9][0-9]','T%2d:%2d')){
    trformat in regexprep(trformat,'T%2d:%2d:[0-9][0-9]','T%2d:%2d:%2d')){

    na <- 0# Number of 0s to be appended
    if (regexp(t1,'\.[0-9]')){# Has one or more digits after decimal
        nd <- length(regexprep(t1,'.*\.([0-9].*)','$1'))# # of decimals
        if (nd > 0){# Has digits after decimal
            nb <- floor(nd/3)# # blocks of 3
            nr <- mod(nd,3)# # remaining after blocked
            # Replace blocks of three digits with #3d
            if (nr > 0){
                na <- 3-(nd-3*nb)# # of 0s to append
                pad <- sprintf('%%%dd',nr)
            } else {
                na <- 0
                pad <- ''
            }
            trformat in regexprep(trformat,'(.*)\..*',...){
                           ['$1.',repmat('%3d',1,nb),pad])
        }
    }
    trformat in [trformat,Z]){# Recover trailing Z
    twformat in regexprep(trformat,'%([2-9])','%0$1')){
