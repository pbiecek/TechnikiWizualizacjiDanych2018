var drawGraph = function () {
    var barHeight = height / level;

    svg.selectAll('rect')
        .data(data)
        .enter()
        .append('rect')
        .attr('width', function (d) { return d.Tx * width / (2 * localMax); })
        .attr('x', function (d) { return (1 - (+ (d.Gender == 'Female')) * d.Tx / localMax) * width / 2; })
        .attr('height', Math.floor(barHeight))
        .attr('y', function (d, i) { return height - ((i % level) + 1) * barHeight; })
        // zamiana kolorow jest umyslna i ma wywolywac poczucie dyskomfortu
        .attr('fill', function (d) { return d.Gender == 'Male' ? 'pink' : 'blue'; })
        .on('mousemove', function (d) {
            tooltip
                .style('left', function (d) { return (d3.event.pageX + 15) + "px" })
                .style('top', function (d) { return d3.event.pageY + "px" })
                .style('opacity', 1)
                .style('width', '120px')
                .text(d.Tx + (d.Gender == 'Male' ? ' mê¿czyzn ' : ' kobiet ') + 'w wieku ' + d.Age + ' lat.');
        })
        .on('mouseout', function (d) {
            tooltip
                .style('opacity', 0);
        })
        .on('click', function (d, i) {
            svg.selectAll('rect').remove();
            data = originalData.slice((111 - level + i % level), 111).concat(originalData.slice((111 - level + i % level) + 111, 222));
            localMax = data[0].Tx;
            level = level - i % level;
            update();
        });
}

var cont = d3.select('body');

function update() {
    cont.call(drawGraph);
}

var localMax = data[0].Tx;

// swoisty "back-up"
var originalData = data;

var level = data.length / 2;

var tooltip = d3.select('body').append('div')
tooltip
    .text("<pusty tooltip>")
    .style('position', 'absolute')
    .style('background', 'lightsteelblue')
    .style('left', '100px')
    .style('opacity', 0)
    .style('top', '100px')
    .style('width', '120px')
    .style('height', '40px')
    .style('padding', '3px')
    .style('border-radius', '8px');

update();

svg.append('text')
    .attr('x', width / 2)
    .attr('y', 15)
    .attr('font-size', 20)
    .attr('text-anchor', 'middle')
    .text('Minimalistyczny wykres demograficzny nie za du¿ej populacji');

svg.selectAll('circle')
    .data([0, 1])
    .enter()
    .append('circle')
    .attr('cx', function (d, i) { return 10 + i * (width - 20); })
    .attr('cy', 10)
    .attr('r', 8)
    .style('fill', 'black')
    .on('mousemove', function (d, i) {
        tooltip
            .style('left', function (d) { return (d3.event.pageX + 15 - i * 230) + "px" })
            .style('top', function (d) { return d3.event.pageY + "px" })
            .style('opacity', 1)
            .style('width', '200px')
            .text(function (d) { if (i === 1) return 'Naciœnij, aby przywróciæ pocz¹tkowy stan wykresu.'; else return 'Kliknij na s³upek, aby ograniczyæ zakres wykresu.'; });
    })
    .on('mouseout', function (d, i) {
        tooltip
            .style('opacity', 0);
    })
    .on('click', function (d) {
        svg.selectAll('rect').remove();
        data = originalData;
        localMax = data[0].Tx;
        level = 111;
        update();
    });