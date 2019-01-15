
var margin = { top: 80, right: 20, bottom: 30, left: 80 },
  width = 960 - margin.left - margin.right,
  height = 800 - margin.top - margin.bottom;

var x = d3.scaleLinear()
  .range([0, width]);

var y = d3.scaleLinear()
  .range([height, 0]);

var xAxis = d3.axisBottom(x);

var yAxis = d3.axisLeft(y);

var svg = d3.select("body").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

d3.csv("dragons.csv", function (error, data) {
  if (error) throw error;

  data.forEach(function (d) {
    d.life_length = +d.life_length;
    d.BMI = +d.BMI;
  });

  x.domain(d3.extent(data, function (d) { return d.BMI; })).nice();
  y.domain(d3.extent(data, function (d) { return d.life_length; })).nice();

  var div = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);
  var color = d3.scaleOrdinal()
    .range(['#e74c3c', '#2ecc71', '#2c3e50', '#3498db']);

  svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + height + ")")
    .call(xAxis)
    .append("text")
    .attr("class", "label")
    .attr("x", width)
    .attr("y", -6)
    .style("text-anchor", "end")
    .text("BMI");

  svg.append("g")
    .attr("class", "y axis")
    .call(yAxis)
    .append("text")
    .attr("class", "label")
    .attr("transform", "rotate(-90)")
    .attr("y", 6)
    .attr("dy", ".71em")
    .style("text-anchor", "end")
    .text("Life Length");

  svg.selectAll(".dot")
    .data(data)
    .enter().append("circle")
    .attr("class", "dot")
    .attr("r", 3.5)
    .attr("cx", function (d) { return x(d.BMI); })
    .attr("cy", function (d) { return y(d.life_length); })
    .style("fill", function (d) { return color(d.colour); })
    .on("mouseover", function (d) {
      div.transition()
        .duration(200)
        .style("opacity", 0.9);
      div.html("Colour: " + d.colour + "<br/>" +
        "BMI: " + Math.round(d.BMI * 100) / 100 + "<br/>" +
        "Life length: " + Math.round(d.life_length * 100) / 100 + "<br/>" +
        "Height: " + Math.round(d.height * 100) / 100 + " yards <br/>" +
        "Weight: " + Math.round(d.weight * 100) / 100 + " tons <br/>")
        .style("left", (d3.event.pageX) + "px")
        .style("top", (d3.event.pageY) + "px");
    })
    .on("mouseout", function (d) {
      div.transition()
        .duration(500)
        .style("opacity", 0);
    });

  var legend = svg.selectAll(".legend")
    .data(color.domain())
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function (d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
    .attr("x", width - 18)
    .attr("width", 18)
    .style("fill", color)
    .attr("height", 18);

  legend.append("text")
    .attr("x", width - 24)
    .attr("y", 9)
    .attr("dy", ".35em")
    .style("text-anchor", "end")
      .text(function (d) { return d; });

  svg.append("text")
    .attr("x", (width / 2))
    .attr("y", 0 - (margin.top / 2))
    .attr("text-anchor", "middle")
    .style("font-size", "48px")
    .style("text-decoration", "underline")
    .text("Dragons BMI");
});