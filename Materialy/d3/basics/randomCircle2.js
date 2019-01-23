var width = 600;
var height = 600;

    d3.select('svg')
		.attr('height', height)
		.attr('width', width)
		.selectAll('.dot')
		.data(data)
		.enter().append("circle")
      .attr("class", "dot")
      .attr("r", 20)
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .style("fill", function(d) { return "black"; })
		.transition()
	    .duration(3000)
	    .delay(50)
	    .attr("cx", function(d) { return Math.random() * width; })
	    .attr("cy", function(d) { return Math.random() * height; })
	    .style("fill","red")
	    .attr("r",30);
