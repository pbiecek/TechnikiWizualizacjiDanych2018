var margin = {top: 20, right: 20, bottom: 30, left: 40},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var x = d3.scaleLinear()
    .range([0, width]);

var y = d3.scaleLinear()
    .range([height, 0]);

var col = ["red", "green", "black","blue"]

var color = d3.scaleOrdinal(col);

var xAxis = d3.axisBottom(x);

var yAxis = d3.axisLeft(y);

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var div = d3.select("body").append("div")	
    .attr("class", "tooltip")				
    .style("opacity", 0);

d3.csv("dragons.csv", function(error, data) {
  if (error) throw error;

  data.forEach(function(d) {
    d.scars = +d.scars;
    d.number_of_lost_teeth = +d.number_of_lost_teeth;
    d.life_length = +d.life_length;
  });

  x.domain(d3.extent(data, function(d) { return d.scars; })).nice();
  y.domain(d3.extent(data, function(d) { return d.number_of_lost_teeth; })).nice();

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)
    .append("text")
      .attr("class", "label")
      .attr("x", width)
      .attr('fill', 'black')
      .attr("y", -6)
      .attr("font-size","25px")
      .style("text-anchor", "end")
      .text("scars");

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("class", "label")
      .attr("transform", "rotate(-90)")
      .attr('fill', 'black')
      .attr("y", -40)
      .attr("dy", ".71em")
      .attr("font-size","25px")
      .style("text-anchor", "end")
      .text("number of lost teeth")

  svg.selectAll(".dot")
      .data(data)
    .enter().append("circle")
      .attr("class", "dot")
      .attr("r", function(d) { return d.life_length/250})
      .attr("cx", function(d) { return x(d.scars); })
      .attr("cy", function(d) { return y(d.number_of_lost_teeth); })
      .style("fill", function(d) { return color(d.colour); })
      .on("mouseover", function(d) {		
            div.transition()		
                .duration(200)		
                .style("opacity", .9);		
            div	.html("year of birth: " + d.year_of_birth)	
                .style("left", (d3.event.pageX) + "px")		
                .style("top", (d3.event.pageY - 28) + "px");	
            })					
        .on("mouseout", function(d) {		
            div.transition()		
                .duration(500)		
                .style("opacity", 0);	
        });

  svg.append("text")
        .attr("x", (width / 2 - 100))             
        .attr("y", 0 - (margin.top / 2) + 5)
        .attr("text-anchor", "middle")  
        .style("font-size", "16px") 
        .style("text-decoration", "underline")  
        .text("Dragon's life span");

 
});