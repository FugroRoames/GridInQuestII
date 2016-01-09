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
  var OutputForm = document.forms["OutputForm"];
  OutputForm["Easting"].value = OutputPoint.coordinates[0].toFixed(2);
  OutputForm["Northing"].value = OutputPoint.coordinates[1].toFixed(2);
  if (OutputPoint.coordinates.length==3)
    OutputForm["Elevation"].value = OutputPoint.coordinates[2].toFixed(3);
  };
var Parameters = 'SourceSRID=4937&TargetSRID=25830&PreferredDatum=0&Geometry='+InputPoint;
GIQRequest.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
GIQRequest.send(Parameters);
};

function ClearFormData()
{
var InputForm = document.forms["InputForm"];
InputForm["Longitude"].value = null;
InputForm["Latitude"].value = null;
InputForm["Altitude"].value = null;
var OutputForm = document.forms["OutputForm"];
OutputForm["Easting"].value = null;
OutputForm["Northing"].value = null;
OutputForm["Elevation"].value = null;
}