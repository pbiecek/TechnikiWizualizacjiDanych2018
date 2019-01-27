

$("acontent").empty();



var w = 600,                       
    h = 600,                            
    r = 150,                            
    color = d3.scale.category20c();     

    data = [{"label":"Lwowska", "value":8}, 
            {"label":"Warszawska", "value":20}, 
            {"label":"Brak", "value":13}];
    
    var vis = d3.select("acontent")
        .append("svg:svg")              
        .data([data])                   
            .attr("width", w)           
            .attr("height", h)
        .append("svg:g")                
            .attr("transform", "translate(200,200)")  
		

    var arc = d3.svg.arc()          
        .outerRadius(r);

    var pie = d3.layout.pie()           
        .value(function(d) { return d.value; });    

    var arcs = vis.selectAll("g.slice")    
        .data(pie)                           
        .enter()                            
            .append("svg:g")                
                .attr("class", "slice");    

        arcs.append("svg:path")
                .attr("fill", function(d, i) { return color(i); } ) 
                .attr("d", arc)				
				.append("svg:title") 
					.text(function(d) { return d.value; });
        arcs.append("svg:text")
		.transition()
		.delay(function (d) {return Math.random()*1500;})
		.duration(1500)
                .attr("transform", function(d) {                    
                d.innerRadius = 0;
                d.outerRadius = r;
                return "translate(" + arc.centroid(d) + ")";				
            })
            .attr("text-anchor", "middle")                         
            .text(function(d, i) { return data[i].label +": " + (100*data[i].value/41).toFixed(0) + "%"; });
			
		
			


