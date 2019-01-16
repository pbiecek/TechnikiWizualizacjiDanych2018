// przyklad za https://bl.ocks.org/sebg/6f7f1dd55e0c52ce5ee0dac2b2769f4b

var margin = {top: 20, right: 20, bottom: 30, left: 40},
width = 960 - margin.left - margin.right,
height = 500 - margin.top - margin.bottom;

var x = d3.scaleLinear()
.range([0, width]);

var y = d3.scaleLinear()
.range([height, 0]);

var color = d3.scaleOrdinal(d3.schemeCategory10);

var xAxis = d3.axisBottom(x);

var yAxis = d3.axisLeft(y);

var div = d3.select("body").append("div")
.attr("class", "tooltip")
.style("opacity", 0);

var svg = d3.select("body").append("svg")
.attr("width", width + margin.left + margin.right)
.attr("height", height + margin.top + margin.bottom)
.attr('display', 'block')
.attr('margin-left', 'auto')
.attr('margin-right', 'auto')
.append("g")
.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

d3.csv("dragons.csv", function(error, data) {
if (error) throw error;

data.forEach(function(d) {
d.year_of_birth = +d.year_of_birth;
d.height = +d.height;
d.weight = +d.weight;
d.scars = +d.scars;
d.number_of_lost_teeth = +d.number_of_lost_teeth;
d.life_length = +d.life_length;
d.BMI = +d.BMI;
});

x.domain(d3.extent(data, function(d) { return d.year_of_birth})).nice();
y.domain(d3.extent(data, function(d) { return d.BMI; })).nice();

svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + height + ")")
  .call(xAxis)
  .append("text")
  .attr("class", "label")
  .attr("x", width)
  .attr("y", -6)
  .attr("fill", "black")
  .style("text-anchor", "end")
  .text("Year of birth");

svg.append("g")
  .attr("class", "y axis")
  .attr('transform', 'translate(0,0)')
  .call(yAxis)
  .append("text")
  .attr("class", "label")
  .attr("transform", "rotate(-90)")
  .attr("y", 6)
  .attr("dy", ".71em")
  .attr("fill", "black")
  .style("text-anchor", "end")
  .text("BMI");

  d3.selectAll(".choice").on("change",function(){reload(data)});
  reload(data);
});

function reload(data) {

  var choices = [];
  d3.selectAll(".choice").each(function(d){
    cb = d3.select(this);
    if(cb.property("checked")){
      choices.push(cb.property("value"));
    }
  });


  if(choices.length > 0){
    newData = data.filter(function(d,i){return choices.includes(d.colour);});
  } else {
    newData = data;
  }

  newDots = svg.selectAll(".dot").remove();
  newDots = svg.selectAll(".dot").data(newData);

  newDots.enter().append("circle")
    .attr("class", "dot")
    .attr("r", 3.5)
    .attr("cx", function(d) { return x(d.year_of_birth); })
    .attr("cy", function(d) { return y(d.BMI); })
    .on("mouseover", function(d) {
      div.transition()
          .duration(200)
          .style("opacity", .9);
      div	.html(d.colour + "<br/>BMI: " + Number(Math.round(d.BMI + 'e2') + 'e-2') +
       "<br/>weight in tons: " + Number(Math.round(d.weight + 'e2') + 'e-2') +
        "<br/>height in  yards: " + Number(Math.round(d.height + 'e2') + 'e-2'))
          .style("left", (d3.event.pageX) + "px")
          .style("top", (d3.event.pageY - 28) + "px");
      })
    .on("mouseout", function(d) {
      div.transition()
          .duration(500)
          .style("opacity", 0);
    })
    .style("fill", function(d) { return d.colour; });
}
