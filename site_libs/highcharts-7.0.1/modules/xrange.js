<<<<<<< HEAD
/*
 Highcharts JS v7.0.1 (2018-12-19)
 X-range series

 (c) 2010-2018 Torstein Honsi, Lars A. V. Cabrera

 License: www.highcharts.com/license
*/
(function(g){"object"===typeof module&&module.exports?module.exports=g:"function"===typeof define&&define.amd?define(function(){return g}):g("undefined"!==typeof Highcharts?Highcharts:void 0)})(function(g){(function(c){var g=c.addEvent,t=c.defined,x=c.Color,u=c.seriesTypes.column,y=c.correctFloat,v=c.isNumber,r=c.isObject,p=c.merge,n=c.pick,z=c.seriesType,A=c.Axis,f=c.Point,B=c.Series;z("xrange","column",{colorByPoint:!0,dataLabels:{verticalAlign:"middle",inside:!0,formatter:function(){var a=this.point.partialFill;
r(a)&&(a=a.amount);t(a)||(a=0);return y(100*a)+"%"}},tooltip:{headerFormat:'\x3cspan style\x3d"font-size: 10px"\x3e{point.x} - {point.x2}\x3c/span\x3e\x3cbr/\x3e',pointFormat:'\x3cspan style\x3d"color:{point.color}"\x3e\u25cf\x3c/span\x3e {series.name}: \x3cb\x3e{point.yCategory}\x3c/b\x3e\x3cbr/\x3e'},borderRadius:3,pointRange:0},{type:"xrange",parallelArrays:["x","x2","y"],requireSorting:!1,animate:c.seriesTypes.line.prototype.animate,cropShoulder:1,getExtremesFromAll:!0,autoIncrement:c.noop,getColumnMetrics:function(){function a(){d.series.forEach(function(a){var b=
a.xAxis;a.xAxis=a.yAxis;a.yAxis=b})}var b,d=this.chart;a();b=u.prototype.getColumnMetrics.call(this);a();return b},cropData:function(a,b,d,e){b=B.prototype.cropData.call(this,this.x2Data,b,d,e);b.xData=a.slice(b.start,b.end);return b},translatePoint:function(a){var b=this.xAxis,d=this.yAxis,e=this.columnMetrics,l=this.options,c=l.minPointLength||0,m=a.plotX,k=n(a.x2,a.x+(a.len||0)),h=b.translate(k,0,0,0,1),k=Math.abs(h-m),w=this.chart.inverted,g=n(l.borderWidth,1)%2/2,q=e.offset,f=Math.round(e.width);
c&&(c-=k,0>c&&(c=0),m-=c/2,h+=c/2);m=Math.max(m,-10);h=Math.min(Math.max(h,-10),b.len+10);t(a.options.pointWidth)&&(q-=(Math.ceil(a.options.pointWidth)-f)/2,f=Math.ceil(a.options.pointWidth));l.pointPlacement&&v(a.plotY)&&d.categories&&(a.plotY=d.translate(a.y,0,1,0,1,l.pointPlacement));a.shapeArgs={x:Math.floor(Math.min(m,h))+g,y:Math.floor(a.plotY+q)+g,width:Math.round(Math.abs(h-m)),height:f,r:this.options.borderRadius};d=a.shapeArgs.x;l=d+a.shapeArgs.width;0>d||l>b.len?(d=Math.min(b.len,Math.max(0,
d)),l=Math.max(0,Math.min(l,b.len)),b=l-d,a.dlBox=p(a.shapeArgs,{x:d,width:l-d,centerX:b?b/2:null})):a.dlBox=null;a.tooltipPos[0]+=w?0:k/2;a.tooltipPos[1]-=w?-k/2:e.width/2;if(b=a.partialFill)r(b)&&(b=b.amount),v(b)||(b=0),e=a.shapeArgs,a.partShapeArgs={x:e.x,y:e.y,width:e.width,height:e.height,r:this.options.borderRadius},a.clipRectArgs={x:e.x,y:e.y,width:Math.max(Math.round(k*b+(a.plotX-m)),0),height:e.height}},translate:function(){u.prototype.translate.apply(this,arguments);this.points.forEach(function(a){this.translatePoint(a)},
this)},drawPoint:function(a,b){var d=this.options,e=this.chart.renderer,c=a.graphic,g=a.shapeType,m=a.shapeArgs,k=a.partShapeArgs,h=a.clipRectArgs,f=a.partialFill,n=a.selected&&"select",q=d.stacking&&!d.borderRadius;if(a.isNull)c&&(a.graphic=c.destroy());else{if(c)a.graphicOriginal[b](p(m));else a.graphic=c=e.g("point").addClass(a.getClassName()).add(a.group||this.group),a.graphicOriginal=e[g](m).addClass(a.getClassName()).addClass("highcharts-partfill-original").add(c);k&&(a.graphicOverlay?(a.graphicOverlay[b](p(k)),
a.clipRect.animate(p(h))):(a.clipRect=e.clipRect(h.x,h.y,h.width,h.height),a.graphicOverlay=e[g](k).addClass("highcharts-partfill-overlay").add(c).clip(a.clipRect)));this.chart.styledMode||(a.graphicOriginal.attr(this.pointAttribs(a,n)).shadow(d.shadow,null,q),k&&(r(f)||(f={}),r(d.partialFill)&&(f=p(f,d.partialFill)),b=f.fill||x(a.color||this.color).brighten(-.3).get(),a.graphicOverlay.attr(this.pointAttribs(a,n)).attr({fill:b}).shadow(d.shadow,null,q)))}},drawPoints:function(){var a=this,b=a.getAnimationVerb();
a.points.forEach(function(d){a.drawPoint(d,b)})},getAnimationVerb:function(){return this.chart.pointCount<(this.options.animationLimit||250)?"animate":"attr"}},{applyOptions:function(){var a,b=f.prototype.applyOptions.apply(this,arguments),d=b.series;if(d.options.colorByPoint&&!b.options.color){a=d.options.colors||d.chart.options.colors;var c=b.y%(a?a.length:d.chart.options.chart.colorCount);a=a&&a[c];d.chart.styledMode||(b.color=a);b.options.colorIndex||(b.colorIndex=c)}return b},init:function(){f.prototype.init.apply(this,
arguments);this.y||(this.y=0);return this},setState:function(){f.prototype.setState.apply(this,arguments);this.series.drawPoint(this,this.series.getAnimationVerb())},getLabelConfig:function(){var a=f.prototype.getLabelConfig.call(this),b=this.series.yAxis.categories;a.x2=this.x2;a.yCategory=this.yCategory=b&&b[this.y];return a},tooltipDateKeys:["x","x2"],isValid:function(){return"number"===typeof this.x&&"number"===typeof this.x2}});g(A,"afterGetSeriesExtremes",function(){var a=this.series,b,c;this.isXAxis&&
(b=n(this.dataMax,-Number.MAX_VALUE),a.forEach(function(a){a.x2Data&&a.x2Data.forEach(function(a){a>b&&(b=a,c=!0)})}),c&&(this.dataMax=b))})})(g)});
//# sourceMappingURL=xrange.js.map
=======
/*
 Highcharts JS v7.0.1 (2018-12-19)
 X-range series

 (c) 2010-2018 Torstein Honsi, Lars A. V. Cabrera

 License: www.highcharts.com/license
*/
(function(g){"object"===typeof module&&module.exports?module.exports=g:"function"===typeof define&&define.amd?define(function(){return g}):g("undefined"!==typeof Highcharts?Highcharts:void 0)})(function(g){(function(c){var g=c.addEvent,t=c.defined,x=c.Color,u=c.seriesTypes.column,y=c.correctFloat,v=c.isNumber,r=c.isObject,p=c.merge,n=c.pick,z=c.seriesType,A=c.Axis,f=c.Point,B=c.Series;z("xrange","column",{colorByPoint:!0,dataLabels:{verticalAlign:"middle",inside:!0,formatter:function(){var a=this.point.partialFill;
r(a)&&(a=a.amount);t(a)||(a=0);return y(100*a)+"%"}},tooltip:{headerFormat:'\x3cspan style\x3d"font-size: 10px"\x3e{point.x} - {point.x2}\x3c/span\x3e\x3cbr/\x3e',pointFormat:'\x3cspan style\x3d"color:{point.color}"\x3e\u25cf\x3c/span\x3e {series.name}: \x3cb\x3e{point.yCategory}\x3c/b\x3e\x3cbr/\x3e'},borderRadius:3,pointRange:0},{type:"xrange",parallelArrays:["x","x2","y"],requireSorting:!1,animate:c.seriesTypes.line.prototype.animate,cropShoulder:1,getExtremesFromAll:!0,autoIncrement:c.noop,getColumnMetrics:function(){function a(){d.series.forEach(function(a){var b=
a.xAxis;a.xAxis=a.yAxis;a.yAxis=b})}var b,d=this.chart;a();b=u.prototype.getColumnMetrics.call(this);a();return b},cropData:function(a,b,d,e){b=B.prototype.cropData.call(this,this.x2Data,b,d,e);b.xData=a.slice(b.start,b.end);return b},translatePoint:function(a){var b=this.xAxis,d=this.yAxis,e=this.columnMetrics,l=this.options,c=l.minPointLength||0,m=a.plotX,k=n(a.x2,a.x+(a.len||0)),h=b.translate(k,0,0,0,1),k=Math.abs(h-m),w=this.chart.inverted,g=n(l.borderWidth,1)%2/2,q=e.offset,f=Math.round(e.width);
c&&(c-=k,0>c&&(c=0),m-=c/2,h+=c/2);m=Math.max(m,-10);h=Math.min(Math.max(h,-10),b.len+10);t(a.options.pointWidth)&&(q-=(Math.ceil(a.options.pointWidth)-f)/2,f=Math.ceil(a.options.pointWidth));l.pointPlacement&&v(a.plotY)&&d.categories&&(a.plotY=d.translate(a.y,0,1,0,1,l.pointPlacement));a.shapeArgs={x:Math.floor(Math.min(m,h))+g,y:Math.floor(a.plotY+q)+g,width:Math.round(Math.abs(h-m)),height:f,r:this.options.borderRadius};d=a.shapeArgs.x;l=d+a.shapeArgs.width;0>d||l>b.len?(d=Math.min(b.len,Math.max(0,
d)),l=Math.max(0,Math.min(l,b.len)),b=l-d,a.dlBox=p(a.shapeArgs,{x:d,width:l-d,centerX:b?b/2:null})):a.dlBox=null;a.tooltipPos[0]+=w?0:k/2;a.tooltipPos[1]-=w?-k/2:e.width/2;if(b=a.partialFill)r(b)&&(b=b.amount),v(b)||(b=0),e=a.shapeArgs,a.partShapeArgs={x:e.x,y:e.y,width:e.width,height:e.height,r:this.options.borderRadius},a.clipRectArgs={x:e.x,y:e.y,width:Math.max(Math.round(k*b+(a.plotX-m)),0),height:e.height}},translate:function(){u.prototype.translate.apply(this,arguments);this.points.forEach(function(a){this.translatePoint(a)},
this)},drawPoint:function(a,b){var d=this.options,e=this.chart.renderer,c=a.graphic,g=a.shapeType,m=a.shapeArgs,k=a.partShapeArgs,h=a.clipRectArgs,f=a.partialFill,n=a.selected&&"select",q=d.stacking&&!d.borderRadius;if(a.isNull)c&&(a.graphic=c.destroy());else{if(c)a.graphicOriginal[b](p(m));else a.graphic=c=e.g("point").addClass(a.getClassName()).add(a.group||this.group),a.graphicOriginal=e[g](m).addClass(a.getClassName()).addClass("highcharts-partfill-original").add(c);k&&(a.graphicOverlay?(a.graphicOverlay[b](p(k)),
a.clipRect.animate(p(h))):(a.clipRect=e.clipRect(h.x,h.y,h.width,h.height),a.graphicOverlay=e[g](k).addClass("highcharts-partfill-overlay").add(c).clip(a.clipRect)));this.chart.styledMode||(a.graphicOriginal.attr(this.pointAttribs(a,n)).shadow(d.shadow,null,q),k&&(r(f)||(f={}),r(d.partialFill)&&(f=p(f,d.partialFill)),b=f.fill||x(a.color||this.color).brighten(-.3).get(),a.graphicOverlay.attr(this.pointAttribs(a,n)).attr({fill:b}).shadow(d.shadow,null,q)))}},drawPoints:function(){var a=this,b=a.getAnimationVerb();
a.points.forEach(function(d){a.drawPoint(d,b)})},getAnimationVerb:function(){return this.chart.pointCount<(this.options.animationLimit||250)?"animate":"attr"}},{applyOptions:function(){var a,b=f.prototype.applyOptions.apply(this,arguments),d=b.series;if(d.options.colorByPoint&&!b.options.color){a=d.options.colors||d.chart.options.colors;var c=b.y%(a?a.length:d.chart.options.chart.colorCount);a=a&&a[c];d.chart.styledMode||(b.color=a);b.options.colorIndex||(b.colorIndex=c)}return b},init:function(){f.prototype.init.apply(this,
arguments);this.y||(this.y=0);return this},setState:function(){f.prototype.setState.apply(this,arguments);this.series.drawPoint(this,this.series.getAnimationVerb())},getLabelConfig:function(){var a=f.prototype.getLabelConfig.call(this),b=this.series.yAxis.categories;a.x2=this.x2;a.yCategory=this.yCategory=b&&b[this.y];return a},tooltipDateKeys:["x","x2"],isValid:function(){return"number"===typeof this.x&&"number"===typeof this.x2}});g(A,"afterGetSeriesExtremes",function(){var a=this.series,b,c;this.isXAxis&&
(b=n(this.dataMax,-Number.MAX_VALUE),a.forEach(function(a){a.x2Data&&a.x2Data.forEach(function(a){a>b&&(b=a,c=!0)})}),c&&(this.dataMax=b))})})(g)});
//# sourceMappingURL=xrange.js.map
>>>>>>> f031b43e38d98207a3ca883aebac68cdeea7ec37
