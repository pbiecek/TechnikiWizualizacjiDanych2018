$(document).ready(function () {
    // slider
    const $slidesContainer = $('.slides-container')
    $slidesContainer.slick()
    const bodyClasses = ['sleep', 'calories', 'footsteps']
    $slidesContainer.on('beforeChange', function (event, slick, currentSlide, nextSlide) {
        $('body').attr('class', bodyClasses[nextSlide])
    })

    // sleeping
    var sleepingCtx = document.getElementById('sleep-chart').getContext('2d')
    var sleepingChart = new Chart(sleepingCtx, {
        type: 'bar',
        data: {
            labels: [
                '2018-01-07', '2018-01-08', '2018-01-09', '2018-01-10', '2018-01-11', '2018-01-12', '2018-01-13',
                '2018-01-14', '2018-01-15', '2018-01-16', '2018-01-17', '2018-01-18', '2018-01-19', '2018-01-20'
            ],
            datasets: [{
                label: 'Hours of sleep',
                data: [7, 7, 7.5, 5.5, 8, 9, 7.5, 7.5, 6, 8, 6.5, 7.5, 6, 7],
                backgroundColor: 'rgba(255, 206, 0, 0.5)',
                borderColor: 'rgba(255, 206, 0, 1)',
                borderWidth: 1
            }]
        },
        options: {
            scales: {
                yAxes: [{
                    ticks: {
                        beginAtZero: true,
                        callback: function (value) {
                            return value + 'h'
                        }
                    }
                }]
            },
            legend: {
                display: false
            }
        }
    })

    // studying
    var studyingCtx = document.getElementById('studying-chart').getContext('2d')
    var studyingChart = new Chart(studyingCtx, {
        type: 'radar',
        data: {
            labels: [
                'Wstęp do bioinformatyki',
                'Zaawansowane Metody Uczenia Maszynowego',
                'Przetwarzanie danych w platformach Big Data',
                'Praca dyplomowa',
                'Techniki wizualizacji danych',
                'Warsztaty badawcze',
            ],
            datasets: [{
                label: 'Hours spent',
                data: [2, 9, 3, 5, 7, 4],
                backgroundColor: 'rgba(34, 38, 41, 0.5)',
                borderColor: 'rgba(34, 38, 41, 1)',
                borderWidth: 1
            }]
        },
        options: {
            scale: {
                reverse: false,
                ticks: {
                    beginAtZero: true,
                    callback: function (value) {
                        return value + 'h'
                    }
                }
            },
            legend: {
                display: false
            }
        }
    })

    // traveling
    var travelingCtx = document.getElementById('traveling-chart').getContext('2d')
    var travelingChart = new Chart(travelingCtx, {
        type: 'line',
        data: {
            labels: [
                '2018-01-07', '2018-01-08', '2018-01-09', '2018-01-10', '2018-01-11', '2018-01-12', '2018-01-13',
                '2018-01-14', '2018-01-15', '2018-01-16', '2018-01-17', '2018-01-18', '2018-01-19', '2018-01-20'
            ],
            datasets: [
                {
                    label: 'On foot',
                    data: [
                        2.1, 5.4, 8, 10.6, 11.6, 15.3, 21.3, 24.2, 26.8, 29.7, 32.9,
                        37.7, 39.8, 45.3
                    ],
                    backgroundColor: 'rgba(52, 144, 214, 0.5)',
                    borderColor: 'rgba(52, 144, 214, 1)',
                    borderWidth: 1
                },
                {
                    label: 'Public transport',
                    data: [
                        7.2, 16.6, 23.8, 31., 31., 31., 33.8, 41., 48.2, 57.7, 69.6,
                        75.7, 79.1, 79.1
                    ],
                    backgroundColor: 'rgba(222, 97, 98, 0.5)',
                    borderColor: 'rgba(222, 97, 98, 1)',
                    borderWidth: 1
                }
            ],
        },
        options: {
            scales: {
                yAxes: [{
                    ticks: {
                        beginAtZero: true,
                        callback: function (value) {
                            return value + 'km'
                        }
                    }
                }]
            },
            tooltips: {
                callbacks: {
                    label: function (tooltipItems, data) {
                        return tooltipItems.yLabel + 'km'
                    }
                }
            }
        }
    })
})


