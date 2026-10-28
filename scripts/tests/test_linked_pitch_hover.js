// Pure event/Plotly mock: run with node scripts/tests/test_linked_pitch_hover.js.
const fs = require('fs');
const vm = require('vm');
const assert = require('assert');
const plots = [];
function graph(keys, group='metrics') {
  const handlers = {};
  const plot = {data:[{customdata:keys,marker:{size:7,opacity:.35}}],_baseHoverGroup:group,
    on:(name,fn)=>handlers[name]=fn,removeListener:name=>delete handlers[name],handlers};
  plots.push(plot); return plot;
}
const context = {window:{},ArrayBuffer,document:{querySelectorAll:()=>plots},Plotly:{restyle:(plot,update,indices)=> {
  for(const i of indices) for(const [key,value] of Object.entries(update)) plot.data[i].marker[key.split('.')[1]]=value[0];
}}};
const attach = vm.runInNewContext('('+fs.readFileSync('R/plots/linked_pitch_hover.js','utf8')+')',context);
const movement=graph(['p1','p2','avg:Fastball']);
const release=graph(['p2','p1','avg:Fastball']);
const extension=graph(['p2']); // p1 has no extension measurement.
const unrelated=graph(['p1'],'bullpen');
for(const p of plots) attach(p,{}, {group:p._baseHoverGroup});
movement.handlers.plotly_hover({points:[{customdata:'p1'}]});
assert.deepEqual(Array.from(release.data[0].marker.size),[7,14,7]);
assert.equal(extension.data[0].marker.size,7);
assert.equal(unrelated.data[0].marker.opacity,.35);
// Hover a different chart before unhover: base sizes must not accumulate.
release.handlers.plotly_hover({points:[{customdata:'avg:Fastball'}]});
assert.deepEqual(Array.from(movement.data[0].marker.size),[7,7,14]);
release.handlers.plotly_unhover();
assert.equal(movement.data[0].marker.size,7);
assert.equal(movement.data[0].marker.opacity,.35);
attach(movement,{}, {group:'metrics'});
assert.equal(Object.keys(movement.handlers).length,2);
console.log('Linked hover identity, missing measurements, groups, averages, reset and rebinding passed.');
