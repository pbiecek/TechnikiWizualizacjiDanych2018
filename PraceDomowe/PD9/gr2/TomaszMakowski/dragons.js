var margin = {top: 40, right: 20, bottom: 30, left: 40},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var x = d3.scale.ordinal()
    .rangeRoundBands([0, width], .1);

var y = d3.scale.linear()
    .range([height, 0]);

var xAxis = d3.svg.axis()
    .scale(x)
    .orient("bottom");

var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")

var tip = d3.tip()
  .attr('class', 'd3-tip')
  .offset([-10, 0])
  .html(function(d) {
    return "W grupie tej jest " + d.number + " smoków. <br> Najwcześniej urodził się smok w " + d.oldest + " roku, <br> a najpóźniej w " + d.youngest +
          ". <br> Średnie wartości dla smoków z tej grupy: <br>" +
          "Wysokość: " + d.height + "<br> Waga: " + d.weight + "<br> Liczba blizn: " + d.scars +
          "<br> Liczba straconych zębów: " + d.number_of_lost_teeth + "<br> Długość życia: " + d.life_length;
  })

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg.call(tip);

d3.csv("dragons.csv", type, function(error, data) {
  x.domain(data.map(function(d) { return d.colour; }));
  y.domain([0, d3.max(data, function(d) { return d.number; })]);

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Liczba smoków");

  svg.selectAll(".bar")
      .data(data)
    .enter().append("rect")
      .attr("class", "bar")
      .attr("x", function(d) { return x(d.colour); })
      .attr("width", x.rangeBand())
      .attr("y", function(d) { return y(d.number); })
      .attr("height", function(d) { return height - y(d.number); })
      .on('mouseover', tip.show)
      .on('mouseout', tip.hide)

});

function type(d) {
  //d.height = +d.height;
  d.number = +d.number;
  d.oldest = +d.oldest;
  d.youngest = +d.youngest;
  //d.weight = +d.weight;
  //d.scars = +d.scars;
  //d.year_of_discovery = +d.year_of_discovery;
  //d.number_of_lost_teeth = +d.number_of_lost_teeth;
  //d.life_length = +d.life_length;
  return d;
}