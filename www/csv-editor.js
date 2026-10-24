// Client-side table: edits update cells without a draw, preserving the viewport.
var root = $(table.table().container()), enabled = false, drag = null;
var headers = table.columns().header().toArray().map(function(x) { return $(x).text(); });
var halfCol = headers.findIndex(function(x) { return /^(Top\/Bottom|Top.Bottom|TopBottom)$/i.test(x); });
var inningCol = headers.indexOf('Inning');
var gameCols = ['GameID', 'Date', 'HomeTeam', 'AwayTeam'].map(function(x) { return headers.indexOf(x); }).filter(function(x) { return x >= 0; });
var groups = [], halves = [], previous = null, group = 0;
table.rows().every(function(i) {
  var row = this.data(), half = halfCol < 0 ? '' : String(row[halfCol]).trim().toLowerCase();
  var valid = ['top','bottom','bot'].indexOf(half) >= 0 && inningCol >= 0 && row[inningCol] !== '';
  var key = JSON.stringify(gameCols.map(function(c) { return row[c]; }).concat([row[inningCol], half]));
  if (!valid || key !== previous) group++;
  groups[i] = valid ? group : null; halves[i] = half; previous = key;
});
function paint() {
  table.rows({page:'current'}).every(function(i) {
    $(this.node()).find('td').css('color', enabled ? (halves[i] === 'top' ? '#c62828' : ['bottom','bot'].indexOf(halves[i]) >= 0 ? '#1565c0' : '') : '');
  });
}
$('#dp_csv_fill').off('.csvFill').on('click.csvFill', function() {
  enabled = !enabled; drag = null;
  $(this).attr('aria-pressed', String(enabled)).toggleClass('btn-primary', enabled);
  root.css('user-select', enabled ? 'none' : ''); paint();
});
$('#dp_csv_fill').attr('aria-pressed', 'false').removeClass('btn-primary');
table.on('draw.csvFill', paint);
root.on('mousedown.csvFill', 'tbody td', function(e) {
  if (!enabled || e.button !== 0 || $(e.target).is('input,textarea,select')) return;
  var cell = table.cell(this), idx = cell.index();
  if (!idx || groups[idx.row] == null) return;
  drag = {row:idx.row, col:idx.column, value:cell.data(), text:this.textContent, end:idx};
});
root.on('mouseover.csvFill', 'tbody td', function() {
  if (!drag) return;
  var idx = table.cell(this).index();
  if (!idx || groups[idx.row] !== groups[drag.row]) return;
  drag.end = idx;
  root.find('td').css('outline','');
  table.cell(this).nodes().to$().css('outline','2px solid #555');
});
$(document).off('mouseup.csvFill').on('mouseup.csvFill', function() {
  if (!drag) return;
  var start = drag; drag = null; root.find('td').css('outline','');
  var displayed = table.rows({order:'applied',search:'applied',page:'current'}).indexes().toArray();
  var a = displayed.indexOf(start.row), b = displayed.indexOf(start.end.row);
  var rows = displayed.slice(Math.min(a,b),Math.max(a,b)+1);
  if (a < 0 || b < 0 || rows.some(function(r) { return groups[r] !== groups[start.row]; })) return;
  var cols = [];
  for (var c=Math.min(start.col,start.end.column);c<=Math.max(start.col,start.end.column);c++) cols.push(c);
  if (rows.length === 1 && cols.length === 1) return;
  // Keep inning and game identifiers protected in fill mode.
  if (cols.some(function(c) { return c === halfCol || c === inningCol || gameCols.indexOf(c) >= 0; })) return;
  rows.forEach(function(r) { cols.forEach(function(c) {
    var cell = table.cell(r,c); cell.data($('<div>').text(start.text).html());
    $(cell.node()).text(start.text);
  }); });
  Shiny.setInputValue('dp_csv_fill_edit', {rows:rows.map(function(r){return r+1;}), cols:cols.map(function(c){return c+1;}), source:start.row+1, sourceCol:start.col+1}, {priority:'event'});
  paint();
});
