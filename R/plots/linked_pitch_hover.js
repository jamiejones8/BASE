function(el, x, options) {
  el._baseHoverGroup = options.group;
  // Re-renders replace handlers, rather than accumulating listeners.
  if (el._basePitchHover) el.removeListener('plotly_hover', el._basePitchHover);
  if (el._basePitchUnhover) el.removeListener('plotly_unhover', el._basePitchUnhover);
  const originals = window._basePitchHoverOriginals || (window._basePitchHoverOriginals = new WeakMap());
  const array = value => Array.isArray(value) || ArrayBuffer.isView(value) ? Array.from(value) : [value];
  const peers = () => Array.from(document.querySelectorAll('.js-plotly-plot'))
    .filter(plot => plot._baseHoverGroup === options.group);
  function style(plot, key) {
    const traces = plot.data || [];
    const hasMatch = key !== null && traces.some(t => t.customdata != null && array(t.customdata).some(k => String(k) === key));
    traces.forEach((trace, index) => {
      if (trace.customdata == null || !trace.marker) return;
      if (!originals.has(trace)) originals.set(trace, {
        size: trace.marker.size == null ? 7 : trace.marker.size,
        opacity: trace.marker.opacity == null ? 1 : trace.marker.opacity
      });
      const original = originals.get(trace);
      const keys = array(trace.customdata);
      const sizes = array(original.size);
      const update = hasMatch ? {
        'marker.size': [keys.map((k, i) => String(k) === key ? Number(sizes[i % sizes.length]) + 7 : sizes[i % sizes.length])],
        'marker.opacity': [keys.map(k => String(k) === key ? 1 : 0.12)]
      } : {'marker.size': [original.size], 'marker.opacity': [original.opacity]};
      Plotly.restyle(plot, update, [index]);
    });
  }
  el._basePitchHover = function(event) {
    const point = event && event.points && event.points[0];
    if (!point || point.customdata == null) return;
    const key = String(point.customdata);
    peers().forEach(plot => style(plot, key));
  };
  el._basePitchUnhover = function() { peers().forEach(plot => style(plot, null)); };
  el.on('plotly_hover', el._basePitchHover);
  el.on('plotly_unhover', el._basePitchUnhover);
}
