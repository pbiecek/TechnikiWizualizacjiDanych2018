var svg = svg.append('g');

var margin = {top: 50, right: 5, bottom: 50, left: 80};
var barWidth = Math.floor((width-margin.left-margin.right) / data.length);
var y = d3.scaleLinear()
.range([(height-margin.bottom), margin.top])
.domain([0, 8000000]);

var x = d3.scaleBand()
        .range([margin.left, width-margin.left])
        .domain(data.map(function(d) { return d.Age; }));
        
var div = d3.select("body").append("div")	
    		.attr("class", "tooltip")				
    		.style("opacity", 0);

svg.selectAll('rect')
.data(data)
.enter().append('rect')
.attr('width', barWidth)
.attr('height', function(d, i) { return height-margin.bottom-y(d.Female); })
.attr('y', function(d, i) { return y(d.Female); })
.attr('x', function(d, i) { return x(d.Age)})
.attr('fill', 'green')

.on("click", function(d) {
    d3.selectAll('rect')
    .transition()
    .duration(2000)
    .attr('height', function(d, i) {return height-margin.bottom-y(d.roznica);})
    .attr('fill', "red");
    d3.select('.title')
    .text("Przewaga populacji kobiet");
})
.on("mouseover", function(d) {
			div.style("opacity", .99);				
            		div.html("Wiek: " + d.Age + "<br/>" + "Populacja kobiet: " + d.Female + "<br/>"  + "Populacja mê¿czyzn: " + d.Male)	
                	.style("right", (d3.event.pageX) + "px")		
                	.style("top", (d3.event.pageY) + "px");	
            	})					
        	.on("mouseout", function(d) {		
            		div.transition()		
                	.duration(500)		
                	.style("opacity", 0);	
        	});

var x_axis = d3.axisBottom()
        	.scale(x);
	var xAxisTranslate = height-margin.bottom;

    	svg.append("g")
            .attr("transform", "translate(0, " + xAxisTranslate  +")")
            .call(x_axis);

            
var y_axis = d3.axisLeft()
        	.scale(y)
        	.tickFormat(d3.format(",d"));
	var yAxisTranslate = margin.left;

    	svg.append("g")
            .attr("transform", "translate("+ yAxisTranslate + ", 0" +")")
            .call(y_axis);
            
  svg.append("text")
		.attr("class", "title")
       		.attr("x", (width / 2))             
        	.attr("y", 30)
        	.style("text-anchor", "middle")
		      .style("text decoration", "bold")
        	.text("Populacja kobiet w danym wieku");
        	
  svg.append("text")             
      .attr("transform",
            "translate(" + (width/2) + " ," + 
                           (height - margin.top + 40) + ")")
      .style("text-anchor", "middle")
      .text("Wiek");
      
      svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("x", 0 - height/2)
      .attr("y", 0)      
      .attr("dy", "1em")
      .style("text-anchor", "middle")
.text("Liczba kobiet"); 