

makeSankey <- function(switch, origin.date, dest.date, file = "sankey.html") {
  colnames(switch) <- c("Origin", "Destination", "Percent")
  
  switch$Origin <- paste(switch$Origin, origin.date)
  switch$Destination <- paste(switch$Destination, dest.date)
  
  party.list <- unique(as.vector(mapply(c, switch$Origin, switch$Destination)))
  party.list <- gsub(paste0(" +", origin.date), "", party.list)
  party.list <- gsub(paste0(" +", dest.date), "", party.list)
  
  party.colours <- c(Conservative = "#0087dc", Labour = "#d50000", `Lib Dem` = "#FDBB30", 
                     UKIP = "#B3009D", Green = "#008066", SNP = "#FFF95D", Plaid = "#3F8428",
                     DK = "#a4a6a8", Other = "#a4a6a8")
  party.list <- paste0("'", party.colours[party.list], "'")
  
  party.list <- paste(party.list, collapse = ", ")
  
  makeLink <- function(label1, label2, count) {
    paste0("[ '", label1, "' , '", label2, "' , ", count, "],")
  }
  switch$row <- NA
  for(ii in 1:nrow(switch)) {
    switch$row[ii] <- makeLink(switch$Origin[ii], switch$Destination[ii], 
                               switch$Percent[ii])
  }
  links <- paste(switch$row, collapse = "\n")
  
  
  head <- "<html>
  <head>
  <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>
  <script type=\"text/javascript\">
  google.load(\"visualization\", \"1.1\", {packages:[\"sankey\"]});
  google.setOnLoadCallback(drawChart);
  
  function drawChart() {
  var data = new google.visualization.DataTable();
  data.addColumn('string', '2010 Vote');
  data.addColumn('string', '2015 Vote');
  data.addColumn('number', 'Vote');
  data.addRows(["
  
  
  
  mid <- "]);
  
  // Sets chart options.
  var colors = [ "
  
  end <- "];
  
  
  var options = {
  height: 500,
  width: 600,
  sankey: {
  node: {
  colors: colors
  },
  link: {
  colorMode: 'gradient',
  colors: colors
  }
  }
  };
  
  // Instantiates and draws our chart, passing in some options.
  var chart = new google.visualization.Sankey(document.getElementById('sankey_basic'));
  chart.draw(data, options);
  }
  </script>
  </head>
  <body>
  <div id=\"sankey_basic\" style=\"width: 900px; height: 300px;\"></div>
  </body>
  </html>"
  
  writeLines(paste(head, 
                   links, 
                   mid, party.list, end, collapse = "\n"), con = file)
}
