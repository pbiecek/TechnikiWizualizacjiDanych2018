const data = [{"name":"110+","male":2,"female":3},{"name":"109","male":3,"female":6},{"name":"108","male":6,"female":13},{"name":"107","male":12,"female":28},{"name":"106","male":24,"female":56},{"name":"105","male":45,"female":112},{"name":"104","male":82,"female":214},{"name":"103","male":146,"female":399},{"name":"102","male":253,"female":718},{"name":"101","male":429,"female":1253},{"name":"100","male":709,"female":2117},{"name":"99","male":1142,"female":3466},{"name":"98","male":1794,"female":5504},{"name":"97","male":2752,"female":8485},{"name":"96","male":4124,"female":12711},{"name":"95","male":6041,"female":18526},{"name":"94","male":8714,"female":26378},{"name":"93","male":12359,"female":36679},{"name":"92","male":17157,"female":49808},{"name":"91","male":23272,"female":66093},{"name":"90","male":30844,"female":85734},{"name":"89","male":40175,"female":109115},{"name":"88","male":51521,"female":136515},{"name":"87","male":65099,"female":168018},{"name":"86","male":81129,"female":203797},{"name":"85","male":99705,"female":243909},{"name":"84","male":120966,"female":288228},{"name":"83","male":145032,"female":336605},{"name":"82","male":171957,"female":388880},{"name":"81","male":201794,"female":444819},{"name":"80","male":234558,"female":504184},{"name":"79","male":270214,"female":566743},{"name":"78","male":308738,"female":632253},{"name":"77","male":350097,"female":700453},{"name":"76","male":394206,"female":771103},{"name":"75","male":441007,"female":844006},{"name":"74","male":490461,"female":918966},{"name":"73","male":542495,"female":995781},{"name":"72","male":597031,"female":1074258},{"name":"71","male":653995,"female":1154262},{"name":"70","male":713321,"female":1235686},{"name":"69","male":774955,"female":1318423},{"name":"68","male":838801,"female":1402360},{"name":"67","male":904742,"female":1487411},{"name":"66","male":972687,"female":1573514},{"name":"65","male":1042626,"female":1660624},{"name":"64","male":1114498,"female":1748682},{"name":"63","male":1188201,"female":1837624},{"name":"62","male":1263685,"female":1927414},{"name":"61","male":1340848,"female":2018012},{"name":"60","male":1419613,"female":2109364},{"name":"59","male":1499894,"female":2201407},{"name":"58","male":1581620,"female":2294086},{"name":"57","male":1664725,"female":2387359},{"name":"56","male":1749106,"female":2481183},{"name":"55","male":1834668,"female":2575509},{"name":"54","male":1921334,"female":2670297},{"name":"53","male":2009038,"female":2765512},{"name":"52","male":2097703,"female":2861118},{"name":"51","male":2187252,"female":2957090},{"name":"50","male":2277634,"female":3053397},{"name":"49","male":2368772,"female":3150000},{"name":"48","male":2460595,"female":3246872},{"name":"47","male":2553060,"female":3343983},{"name":"46","male":2646101,"female":3441306},{"name":"45","male":2739657,"female":3538823},{"name":"44","male":2833684,"female":3636509},{"name":"43","male":2928126,"female":3734343},{"name":"42","male":3022961,"female":3832310},{"name":"41","male":3118176,"female":3930402},{"name":"40","male":3213730,"female":4028610},{"name":"39","male":3309588,"female":4126920},{"name":"38","male":3405725,"female":4225322},{"name":"37","male":3502106,"female":4323798},{"name":"36","male":3598700,"female":4422339},{"name":"35","male":3695496,"female":4520946},{"name":"34","male":3792486,"female":4619611},{"name":"33","male":3889647,"female":4718325},{"name":"32","male":3986966,"female":4817085},{"name":"31","male":4084434,"female":4915890},{"name":"30","male":4182046,"female":5014736},{"name":"29","male":4279786,"female":5113621},{"name":"28","male":4377640,"female":5212542},{"name":"27","male":4475604,"female":5311494},{"name":"26","male":4573679,"female":5410474},{"name":"25","male":4671862,"female":5509479},{"name":"24","male":4770149,"female":5608509},{"name":"23","male":4868542,"female":5707567},{"name":"22","male":4967048,"female":5806650},{"name":"21","male":5065664,"female":5905758},{"name":"20","male":5164392,"female":6004890},{"name":"19","male":5263229,"female":6104047},{"name":"18","male":5362166,"female":6203230},{"name":"17","male":5461185,"female":6302441},{"name":"16","male":5560260,"female":6401675},{"name":"15","male":5659376,"female":6500931},{"name":"14","male":5758528,"female":6600210},{"name":"13","male":5857703,"female":6699509},{"name":"12","male":5956894,"female":6798825},{"name":"11","male":6056101,"female":6898154},{"name":"10","male":6155321,"female":6997493},{"name":"9","male":6254557,"female":7096844},{"name":"8","male":6353808,"female":7196206},{"name":"7","male":6453075,"female":7295583},{"name":"6","male":6552355,"female":7394975},{"name":"5","male":6651653,"female":7494379},{"name":"4","male":6750968,"female":7593795},{"name":"3","male":6850296,"female":7693229},{"name":"2","male":6949644,"female":7792681},{"name":"1","male":7049023,"female":7892158},{"name":"0","male":7148455,"female":7991684}] 
  const margin =  { top: 10, right: 10, bottom: 20, left: 50}
  const width = 1800 - margin.left - margin.right
  const height = 1000 - margin.top - margin.bottom
  
  const xScale = d3.scaleLinear()
      .domain([0, 8000000])
      .range([0, width])
  
  const yScale = d3.scaleBand()
      .domain(data.map(d => d.name))
      .range([0, height])
  
  const svg = d3.select('#chart')
      .append('svg')
      .attr('width', width + margin.left + margin.right)
      .attr('height', height + margin.top + margin.bottom)
      .style('position', 'absolute')
      .style('top', 0)
      .style('left', 0)
      .on("mouseover", function(d) {		
        div.transition()		
            .duration(200)		
            .style("opacity", .9);		
        div	.html("<br/>"  + d.male)	
            .style("left", (d3.event.pageX) + "px")		
            .style("top", (d3.event.pageY - 28) + "px");	
        })					
    .on("mouseout", function(d) {		
        div.transition()		
            .duration(500)		
            .style("opacity", 0);	
    });


  const axisContainer = svg.append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`)



    
    
    axisContainer.append('g')
    .attr('transform', `translate(0, ${height})`)
    .call(d3.axisBottom(xScale))
    
    axisContainer.append('g')
    .call(d3.axisLeft(yScale))

    svg.append('text')
    .attr('class', 'label')
    .attr('x', 800)
    .attr('y', 1000)
    .attr('font-size', 15)
    .attr('text-anchor', 'middle')
    .text('Liczba ludności')

    svg.append('text')
    .attr('class', 'label')
    .attr('x', -500)
    .attr('y', 20)
    .attr('font-size', 15)
    .attr('transform', 'rotate(-90)')
    .attr('text-anchor', 'middle')
    .text('Wiek')


    const render = (subject) => {
        
        var tooltip = d3.select("body")
        .append("div")
        .style("position", "absolute")
        .style("z-index", "10")
        .style("visibility", "hidden")
        .text("a simple tooltip");

      const bars = d3.select('#chart')
      .selectAll('div')
      .data(data, d => d.name)
    
      const newBars = bars
      .enter() 
      .append('div')
      .attr('class', 'bar')
      .style('width', 0)



      newBars.merge(bars)
      .transition()
      .style('width', d => `${xScale(d[subject])}px`)
      .style('height', d => `${yScale.bandwidth() - 2}px`)

  }
  
  render('male')