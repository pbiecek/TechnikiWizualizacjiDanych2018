

var margin = {top: 20, right: 20, bottom: 30, left: 40},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var x = d3.scaleLinear()
    .range([0, width]);

var y = d3.scaleLinear()
        .range([height, 0]);

var xAxis = d3.axisBottom(x);

var yAxis = d3.axisLeft(y);

var div = d3.select("body").append("div")
        .attr("class", "tooltip")
        .style("opacity", 0);

var svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

d3.csv("dragons.csv", function(error, data){
  if (error) throw error;

  data.forEach(function(d){
    d.year_of_birth = +d.year_of_birth;
    d.height = +d.height;
    d.weight = +d.weight;
    d.scars = +d.scars;
    d.year_of_discovery = +d.year_of_discovery;
    d.number_of_lost_teeth = +d.number_of_lost_teeth;
    d.life_length = +d.life_length;
  });

  x.domain(d3.extent(data, function(d) { return d.life_length; })).nice();
  y.domain(d3.extent(data, function(d) { return d.scars; })).nice();

  svg.append("g")
              .attr("class", "x axis")
              .attr("transform", "translate(0," + height + ")")
              .call(xAxis)
            .append("text")
              .attr("class", "label")
              .attr("x", width)
              .attr("y", -6)
              .style("text-anchor", "end")
              .style("fill", "black")
              .text("Długość życia");

  svg.append("g")
              .attr("class", "y axis")
              .call(yAxis)
            .append("text")
              .attr("class", "label")
              .attr("transform", "rotate(-90)")
              .attr("y", 6)
              .attr("dy", ".71em")
              .style("text-anchor", "end")
              .style("fill", "black")
              .text("Liczba blizn")

  d3.select("#mySlider").on("change", function(){update(data);});
  update(data);

});

function update(data){

var news = d3.select(".slider").property("value")
var newdata = data.filter(function(d) {
    return d.year_of_discovery < news;})

newdots = svg.selectAll(".dot").remove();
newdots = svg.selectAll(".dot").data(newdata);
console.log(news)
    newdots.enter().append("circle")
      .attr("class", "dot")
      .attr("r", 3.5)
      .attr("cx", function(d) { return x(d.life_length); })
      .attr("cy", function(d) { return y(d.scars); })
      .style("fill", function(d) { return d.colour; })
      .on("mouseover", function(d) {
        div.transition()
          .duration(100)
          .style("opacity", .9);
        div.html("Rok urodzenia: " + d.year_of_birth +
                  "<br>Długość życia: " + Math.round(d.life_length) +
                  "<br>Rok odkrycia: " + d.year_of_discovery +
                  "<br>Liczba blizn: " + d.scars)
          .style("left", (d3.event.pageX) + "px")
          .style("top", (d3.event.pageY - 28) + "px");
        })
      .on("mouseout", function(d) {
        div.transition()
          .duration(500)
          .style("opacity", 0);
        });

};
