function GIQTransRequest()
{
var InputForm = document.forms["InputForm"];
var Lon = InputForm["Longitude"].value;
var Lat = InputForm["Latitude"].value;
if (Lat == null || Lat == "" || Lon == null || Lon == "")
  {
  alert("Latitude and Longitude must be supplied.");
  return false;
  };
var Alt = InputForm["Altitude"].value;
if (Alt == null || Alt == "")
  var InputPoint = '{"type":"Point","coordinates":['+Lon+','+Lat+']}'
else
  var InputPoint = '{"type":"Point","coordinates":['+Lon+','+Lat+','+Alt+']}';
var GIQRequest = new XMLHttpRequest();
GIQRequest.open("POST", "giqtrans", true);
GIQRequest.onload = function()
  {
  var OutputPoint = JSON.parse(this.responseText);
  var CoordForm = document.forms["OutputForm"];
  CoordForm["Easting"].value = OutputPoint.coordinates[0];
  CoordForm["Northing"].value = OutputPoint.coordinates[1];
  if (OutputPoint.coordinates.length==3)
    CoordForm["Elevation"].value = OutputPoint.coordinates[2];
  };
var Parameters = 'SourceSRID=4937&TargetSRID=25830&PreferredDatum=0&Geometry='+JSON.stringify(InputPoint);
GIQRequest.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
GIQRequest.send(Parameters);
};

