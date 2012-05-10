function draw_perf(container, name, per) {
  Highcharts.setOptions({global: { useUTC: false }});

  return new Highcharts.Chart({
    chart: {
      renderTo: container,
      marginRight: 10
    },
    title: { text: name },
    xAxis: {
      type: 'datetime',
      tickPixelInterval: 150
    },
    yAxis: {
      title: { text: per },
      plotLines: [{
        value: 0,
        width: 1,
        color: '#808080'
      }]
    },
    tooltip: {
      formatter: function() {
        return '<b>'+ this.series.name +'</b><br/>'+
          Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) +'<br/>'+
          Highcharts.numberFormat(this.y, 2);
      }
    },
    legend: {
      enabled: false
    },
    series: [{
      name: per,
      type: 'spline',
      data: []
    }]
  });
};

function draw_stock(container, data, name) {
  Highcharts.setOptions({global: { useUTC: true }});

  // split the data set into ohlc and volume
  var ohlc = [],
    volume = [],
    dataLength = data.length;

  for (i = 0; i < dataLength; i++) {
    ohlc.push([
      data[i].from, // the date
      parseFloat(pf(data[i].begin)), // open
      parseFloat(pf(data[i].max)), // high
      parseFloat(pf(data[i].min)), // low
      parseFloat(pf(data[i].end)) // close
    ]);

    volume.push([
      data[i].from, // the date
      data[i].summary // the volume
    ])
  }

  // create the chart
  chart = new Highcharts.StockChart({
    chart: {
      renderTo: container,
      alignTicks: false,
      height: 290
    },

    navigator: { enabled: false },
    scrollbar: { enabled: false },
    rangeSelector: { enabled: false },
    exporting: { enabled: false },
    title: { text: name },

    plotOptions: {
      series: {
        dataGrouping : {
          groupPixelWidth: 10
        }
      }
    },

    yAxis: [{
        title: { text: 'Price' },
        height: 100,
        lineWidth: 2
    },

    {
      title: { text: 'Volume' },
      top: 150,
      height: 100,
      offset: 0,
      lineWidth: 2
    }],

    series: [{
      type: 'candlestick',
      name: name,
      data: ohlc
    },
    {
      type: 'column',
      name: 'Volume',
      data: volume,
      yAxis: 1
    }]
  });
};
