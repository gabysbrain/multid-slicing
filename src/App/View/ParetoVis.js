
const React = require('react');
const d3 = require('d3');

const Component = React.Component;
const PropTypes = React.PropTypes;

function _initialState() {
  var xScale = d3.scaleLinear();
  var yScale = d3.scaleLinear();

  return {
    width: 163,
    height: 163,
    margin: {left: 35, top: 10, right: 10, bottom: 40},

    x: xScale,
    y: yScale,

    xAxis: d3.axisBottom(xScale)
             .ticks(5)
             .tickFormat(d3.format('0.2f')),
    yAxis: d3.axisLeft(yScale)
             .ticks(5)
             .tickFormat(d3.format('0.2f'))
  };
}

function createChart(self) {
  var svg = d3.select(self.refs.chartContainer);

  // the actual visual components get put in here
  var chart = svg.append('g');
  chart.attr('transform', 'translate(' + self.state.margin.left + ',' + self.state.margin.top + ')')
       .attr('class', 'container');

  chart.append('g')
       .attr('class', 'x axis');
  chart.append('g')
       .attr('class', 'y axis');
}

function updateChart(self) {
  var svg = d3.select(self.refs.chartContainer);
  svg.attr('width', self.state.width)
     .attr('height', self.state.height);
  
  // set up the sizes for things
  var visWidth = self.state.width - self.state.margin.left - self.state.margin.right;
  var visHeight = self.state.height - self.state.margin.top - self.state.margin.bottom;

  var paretoPoints = self.props['data-paretopoints'];
  var paretoLines = self.props['data-paretopaths'];
  var maxX = self.props['data-maxX'];
  var maxY = self.props['data-maxY'];
  
  // update axes and scales
  self.state.x.range([0, visWidth])
              .domain([0, maxX]);
  self.state.y.range([visHeight, 0])
              .domain([0, maxY]);

  var xAxis = svg.select('g.x.axis');
  xAxis.attr('transform', 'translate(0,'+self.state.y.range()[0]+')')
       .call(self.state.xAxis);
  var yAxis = svg.select('g.y.axis');
  yAxis.call(self.state.yAxis);

  var chartContainer = svg.select('g.container');
  drawParetoPoints(self, chartContainer, paretoPoints);
  drawParetoLines(self, chartContainer, paretoLines);
}

function drawParetoLines(self, elem, data) {
  var handleHover = self.props.onFrontHover;

  var line = d3.line()
    .x(function(d) {
      return self.state.x(d.x);
    })
    .y(function(d) {
      return self.state.y(d.y);
    });

  var lines = elem.selectAll('.pareto-front.path').data(data);
  lines.enter()
    .append('path')
      .on('mouseover', function() {
        handleHover(d3.select(this).data());
      })
      .on('mouseout', function() {
        handleHover([]);
      })
      .attr('class', 'pareto-front path')
      .attr('stroke-width', 1)
      .attr('fill', 'none')
      .attr('stroke', function(d) {return d.selected ? 'red' : 'black';})
      .attr('d', function(d) {
        return line(d.points);
      });
  lines
    .attr('d', function(d) {
      return line(d.points);
    })
    .attr('stroke', function(d) {return d.selected ? 'red' : 'black';});
  lines.exit().remove();
}

function drawParetoPoints(self, elem, data) {
  var handleHover = self.props.onPointHover;

  var points = elem.selectAll('.pareto-front.point').data(data);
  points.enter()
    .append('circle')
      .on('mouseover', function() {
        handleHover(d3.select(this).data());
      })
      .on('mouseout', function() {
        handleHover([]);
      })
      .attr('class', 'pareto-front point')
      .attr('cx', function(d) {return self.state.x(d.x);})
      .attr('cy', function(d) {return self.state.y(d.y);})
      .attr('r', function(d) {return d.selected ? 5 : 3;})
      .attr('fill', function(d) {return d.selected ? 'red' : 'black';});
  points
    .attr('cx', function(d) {return self.state.x(d.x);})
    .attr('cy', function(d) {return self.state.y(d.y);})
    .attr('r', function(d) {return d.selected ? 5 : 3;})
    .attr('fill', function(d) {return d.selected ? 'red' : 'black';});
  points.exit().remove();
}

exports.paretoVisComponent = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    //this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return _initialState();
  },

  componentDidMount: function() {
    createChart(this);
    updateChart(this);
  },

  componentDidUpdate: function() {
    updateChart(this);
  },

  render: function() {
    var props = Object.assign({}, this.props, {ref: 'chartContainer'});
    return (
      React.createElement('svg', props)
    );
  }
});

