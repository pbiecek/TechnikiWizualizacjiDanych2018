var margin = {top: 40, right: 40, bottom: 60, left: 80},
    width = 1600 - margin.left - margin.right,
    height = 900 - margin.top - margin.bottom;

var x = d3.scaleLinear()
    .range([0, width-80]);

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
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");



d3.csv("dragons.csv", function(error, data) {
  if (error) {
    throw error;
  }
  data.forEach(function(d) {
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
      .attr("x", width-100)
      .attr("y", -6)
      .attr("fill", "black")
      .text("Długość życia smoka (w latach)");

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("class", "label")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .attr("fill", "black")
      .style("text-anchor", "end")
      .text("Liczba blizn");

  svg.select('#tooltip').append("rect");

  d3.selectAll(".myCheckbox").on("change",function(){update(data)});
  update(data);

});


function update(data) {

  var choices = [];
  d3.selectAll(".myCheckbox").each(function(d){
    cb = d3.select(this);
    if(cb.property("checked")){
      choices.push(cb.property("value"));
    }
  });

  if(choices.length >= 0){
    newData = data.filter(function(d,i){return choices.includes(d.colour);});
  } else {
    newData = data;     
  }

  newDots = svg.selectAll(".dot").remove();
  newDots = svg.selectAll(".dot").data(newData);


  newDots.enter()
    .append("circle")
      .attr("class", "dot")
      .classed("enabled", true)
      .attr("r", 6)
      .attr("cx", function(d) { return x(d.life_length); })
      .attr("cy", function(d) { return y(d.scars); })
      .style("fill", function(d) { return d.colour; })
      .on("mouseover", function(d) {
        
        div.transition()
          .duration(200)
          .style("opacity", .9);
        
        d3.selectAll(".dot")
          .transition()
          .duration(200)
          .style("opacity", function(t) {
            if( t == d) return 1
            else return Math.atan(Math.sqrt(Math.pow(x(d.life_length) - x(t.life_length), 2) + Math.pow(y(d.scars) - y(t.scars), 2))/100)/1.25 +0.2;
          })
          .attr("r", function(t) {
            if (t == d) return 12
            else return 6;
          });
        
        div.html("Birth: " + d.year_of_birth + 
                  "<br>Death: "+ Math.round(d.year_of_birth + d.life_length) + 
                  "<br>Discovery: " + d.year_of_discovery )
          .style("left", (d3.event.pageX) + "px")
          .style("top", (d3.event.pageY - 50) + "px");
        })
      .on("mouseout", function(d) {
        d3.selectAll(".dot").transition()
          .duration(200)
          .attr("r", 6)
          .style("opacity", 1);
        div.transition()
          .duration(500)
          .style("opacity", 0);
        });

  newDots.exit()
    .remove();
}

