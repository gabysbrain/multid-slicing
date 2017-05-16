
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

  var paretoGroups = self.props['data-paretopaths'];
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
  drawParetoLines(self, chartContainer, paretoGroups);
}

function drawParetoLines(self, elem, data) {
  var line = d3.line()
    .x(function(d) {
      return self.state.x(d[0])
    })
    .y(function(d) {
      return self.state.y(d[1])
    });

  var lines = elem.selectAll('.pareto-front.path').data(data);
  lines.enter()
    .append('path')
      .attr('class', 'pareto-front path')
      .attr('stroke', 'black')
      .attr('stroke-width', 1)
      .attr('fill', 'none')
      .attr('d', function(d) {
        return line(d);
      });
  lines.attr('d', function(d) {
    return line(d);
  });
  lines.exit().remove();
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

