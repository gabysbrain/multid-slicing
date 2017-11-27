
const React = require('react');
const d3 = require('d3');
const cbColors = require('d3-scale-chromatic');

const Component = React.Component;
const PropTypes = React.PropTypes;

function _initialState() {
  var xScale = d3.scaleLinear();
  var yScale = d3.scaleLinear();

  var fpColorScale = d3.scaleOrdinal(cbColors.schemeDark2);

  return {
    width: 163,
    height: 163,
    margin: {left: 35, top: 10, right: 10, bottom: 40},

    x: xScale,
    y: yScale,
    fpColor: fpColorScale,

    xAxis: d3.axisBottom(xScale)
             .ticks(5)
             .tickFormat(d3.format('0.2f')),
    yAxis: d3.axisLeft(yScale)
             .ticks(5)
             .tickFormat(d3.format('0.2f')),

    isDragging: false
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

  var hullLines = self.props['data-hullpaths'];
  var focusPoints = self.props['data-focuspoints'];
  var maxX = self.props['data-maxX'];
  var maxY = self.props['data-maxY'];

  // update axes and scales
  self.state.x.range([0, visWidth])
              //.domain([0, maxX]);
              .domain([-1, 1]);
  self.state.y.range([visHeight, 0])
              .domain([-1, 1]);
              //.domain([0, maxY]);

  var xAxis = svg.select('g.x.axis');
  xAxis.attr('transform', 'translate(0,'+self.state.y.range()[0]+')')
       .call(self.state.xAxis);
  var yAxis = svg.select('g.y.axis');
  yAxis.call(self.state.yAxis);

  var chartContainer = svg.select('g.container');
  var targetFps = self.props['data-fp-targets'];
  if(targetFps && self.state.isDragging) {
    drawTargetFocusPoints(self, chartContainer, targetFps);
  } else {
    drawTargetFocusPoints(self, chartContainer, []);
  }

  if(focusPoints) {
    drawFocusPoints(self, chartContainer, focusPoints);
  } else {
    drawFocusPoints(self, chartContainer, []);
  }
  
  drawHullLines(self, chartContainer, hullLines);

  var addlLines = self.props['data-fppaths'];
  if(addlLines) {
    drawAddlLines(self, chartContainer, addlLines);
  }
}

function isSelected(d, fps) {
  return fps.has(d.focusPointId);
}

function drawTargetFocusPoints(self, elem, data) {
  var points = elem.selectAll('.target.point').data(data);
  points.enter()
    .append('circle')
      .attr('class', 'target point')
      .attr('fill', '#c0c0c0')
      .attr('r', 1.5)
      .attr('cx', function(d) { return self.state.x(d.row[0]); })
      .attr('cy', function(d) { return self.state.y(d.row[1]); });
  points.exit().remove();
}

function drawFocusPoints(self, elem, data) {
  var handleDrag = self.props.onPointDrag;
  var handleUpdate = self.props.onPointRelease;

  // set up drag behavior
  var dragEvt = d3.drag()
    .on('drag', function() {
      if(handleDrag) {
        self.setState({isDragging: true});
        var newx = self.state.x.invert(d3.event.x);
        var newy = self.state.y.invert(d3.event.y);

        // FIXME: hack to handle bug in pux
        // see https://github.com/alexmingoia/purescript-pux/issues/122
        var evtData = new Object();
        evtData.nativeEvent = d3.select(this).datum();
        evtData.nativeEvent.row = [newx, newy];
        handleDrag(evtData);
      }
    })
    .on('end', function() {
      self.setState({isDragging: false});

      if(handleUpdate) {
        var newx = self.state.x.invert(d3.event.x);
        var newy = self.state.y.invert(d3.event.y);

        // FIXME: hack to handle bug in pux
        // see https://github.com/alexmingoia/purescript-pux/issues/122
        var evtData = new Object();
        evtData.nativeEvent = d3.select(this).datum();
        evtData.nativeEvent.row = [newx, newy];
        handleUpdate(evtData);
      }
    });
  var points = elem.selectAll('.focus-point.point').data(data);
  points.enter()
    .append('circle')
      .attr('class', 'focus-point point')
      .attr('fill', function(d) {return self.state.fpColor(d.rowId-1);})
      .attr('r', 5)
      .attr('cx', function(d) {return self.state.x(d.row[0]);})
      .attr('cy', function(d) {return self.state.y(d.row[1]);})
      .call(dragEvt);
  points
    .attr('cx', function(d) {return self.state.x(d.row[0]);})
    .attr('cy', function(d) {return self.state.y(d.row[1]);});
  points.exit().remove();
}

function drawAddlLines(self, elem, data) {
  //var handleHover = self.props.onHullHover;
  //var handleClick = self.props.onHullClick;
  //var selectedFPs = new Set(self.props['data-selectedfps']);

  var lines = elem.selectAll('.user-selected.path').data(data);
  lines.enter()
    .append('line')
      .attr('class', 'user-selected path')
      .attr('stroke-width', 1)
      //.attr('stroke-opacity', '0.6')
      .attr('fill', 'none')
      .attr('stroke', function(d) {return self.state.fpColor(d.focusPointId-1);})
      .attr('x1', function(d) { return self.state.x(d.x1Min); })
      .attr('x2', function(d) { return self.state.x(d.x1Max); })
      .attr('y1', function(d) { return self.state.y(d.x2Min); })
      .attr('y2', function(d) { return self.state.y(d.x2Max); });
  lines
    .attr('x1', function(d) { return self.state.x(d.x1Min); })
    .attr('x2', function(d) { return self.state.x(d.x1Max); })
    .attr('y1', function(d) { return self.state.y(d.x2Min); })
    .attr('y2', function(d) { return self.state.y(d.x2Max); });
  //lines
    //.attr('stroke-width', function(d) {return isSelected(d, selectedFPs) ? 1.5 : 1;})
    //.attr('stroke', function(d) {return isSelected(d, selectedFPs) ? 'red' : 'black';});
  lines.exit().remove();
}

function drawHullLines(self, elem, data) {
  var handleHover = self.props.onHullHover;
  var handleClick = self.props.onHullClick;
  var selectedFPs = new Set(self.props['data-selectedfps']);

  var lines = elem.selectAll('.pareto-front.path').data(data);
  lines.enter()
    .append('line')
      .on('mouseover', function() {
        if(handleHover) {
          // FIXME: hack to handle bug in pux
          // see https://github.com/alexmingoia/purescript-pux/issues/122
          var evtData = new Object();
          evtData.nativeEvent = d3.select(this).data();
          handleHover(evtData);
        }
      })
      .on('mouseout', function() {
        if(handleHover) {
          // FIXME: hack to handle bug in pux
          // see https://github.com/alexmingoia/purescript-pux/issues/122
          var evtData = new Object();
          evtData.nativeEvent = [];
          handleHover(evtData);
        }
      }).on('click', function() {
        if(handleClick) {
          var evtData = new Object();
          evtData.nativeEvent = d3.select(this).datum();
          handleClick(evtData);
          //console.log(evtData);
        }
      })
      .attr('class', 'pareto-front path')
      .attr('stroke-width', function(d) {return isSelected(d, selectedFPs) ? 1.5 : 1;})
      .attr('stroke-opacity', '0.6')
      .attr('fill', 'none')
      .attr('stroke', function(d) {return isSelected(d, selectedFPs) ? 'red' : 'black';})
      .attr('x1', function(d) { return self.state.x(d.x1Min); })
      .attr('x2', function(d) { return self.state.x(d.x1Max); })
      .attr('y1', function(d) { return self.state.y(d.x2Min); })
      .attr('y2', function(d) { return self.state.y(d.x2Max); });
  lines
    .attr('stroke-width', function(d) {return isSelected(d, selectedFPs) ? 1.5 : 1;})
    .attr('stroke', function(d) {return isSelected(d, selectedFPs) ? 'red' : 'black';});
  lines.exit().remove();
}

exports.slicePanelComponent = React.createClass({
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

