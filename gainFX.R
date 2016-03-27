# This function downloads historical tick data from Gain Capital
# Weekly CSV files are downloaded for input pair, month and year
# from Gain Capital archives, combines and parses it as monthly
# zoo objects; and finally returns it as a list of buy and sell
# XTS objects. The code can be modified for desired output object.

# Please visit Gain Capital's website for the list of instruments.
# pair = instrument, eg. "eurusd", "EURUSD", "EuRUsD", "EUR_USD"
# year and month are numeric, eg. year = 2012; month = 1 or 01
suppressMessages(library(dplyr))
suppressMessages(library(quantmod))

suppressMessages(require("xts"))
gaindata <- function(pair, year, month)
{
  # Required packages, loads both zoo and xts
  # Parsing the arguments
  pair = toupper(pair)
  pair = paste(substr(pair, 1, 3), "_", substr(pair,4, 6), sep="")
  year = as.numeric(year)
  month = as.numeric(month)
  mon = c('January', 'February', 'March', 'April', 'May', 'June', 'July',
          'August', 'September', 'October', 'November', 'December') [month]
  slash = ifelse((month<10), ("/0"), ("/"))
  
  # Download html file; works on linux with
  tmpname = paste("gaindatatempfile", year, month, sep="")
  htmlurl = paste("http://ratedata.gaincapital.com/", year, slash, month,
                  "%20", mon, "/", sep="")#"%20"is the new pattern of url
  download.file(htmlurl, tmpname, quiet=TRUE) # download the html file
  
  # Parse the html file for pair records
  txtl=invisible(readLines(file(tmpname)))
  txtlist = substr(txtl, 145, 157)
  pairlist = grep(pair, txtlist)
  
  # Close the connection and delete the html file
  closeAllConnections()
  unlink (tmpname)
  
  # Preparing to download data files: zip files
  dlurl = paste("http://ratedata.gaincapital.com/", year, slash, month,
                "%20", mon, "/", txtlist[pairlist], ".zip", sep="")
  
  for (i in 1:length(pairlist)){
    i=1
    # Tempororary string variables
    zipfile = paste(txtlist[pairlist[i]], ".zip", sep="")
    csvfile = paste(txtlist[pairlist[i]], ".csv", sep="")
    
    # Download&unzip .zip file and extract the csv
    download.file(dlurl[i], zipfile)
    tmpdata = read.csv(unz(zipfile, csvfile), header=TRUE ,sep=",")#unzip by unz() and readin the csv
    
    # Create zoo objects and append in series
    tempzoo = zoo(tmpdata[, 5:6], as.POSIXct(strptime((tmpdata[,4]),
                                                      "%Y-%m-%d %H:%M:%OS")))
    if (i == 1) {data = tempzoo}
    else {data = rbind(data, tempzoo)}
    
    closeAllConnections()# Close all connections
    unlink (zipfile) #and delete the zip file
  }
  
  # Group the buy and sell series and return the list object
  fx = list("Bid" = as.xts(data[, colnames(data) != "RateBid"])
            , "Ask" = as.xts(data[, colnames(data) != "RateAsk"]))#keep the xts
  fx2 <- data.frame(Bid=log(fx$Bid),Ask=log(fx$Ask))#lose the xts
  fx2 <- mutate(fx2,Mid = (Ask+Bid)/2)#geometric middle price
  fx2 <- .xts(fx2, index(fx$Bid))
  return (fx2)
} 

downloadFX<-function(pair ,year ,month =  01){
  fx<<-gaindata(pair = pair,year =  year,month =  month)  
  save(fx,file = paste("../",pair,year,month,".RData",sep="" ))
}

downloadFX(pair = "eurusd",year =  2015,month =  01)  

class(fx)


