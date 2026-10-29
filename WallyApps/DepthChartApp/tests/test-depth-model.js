const assert=require('node:assert/strict');
const M=require('../www/depth-model.js');
const player=(id,positions,entryClass=1,entryYear=2027,extra={})=>({id,positions,entryClass,entryYear,status:'Signed',...extra});
const nolan=player('nolan',['LHP'],2,2027,{bats:'L',throws:'L'});
const senior=player('senior',['RHP'],5);
const ss=player('ss',['SS','2B'],1,2027,{bats:'S',throws:'R'});
const second=player('second',['2B'],2);
const bench=player('bench',['1B'],1,2027,{bats:'L',throws:'R'});
const future=player('recruit',['RHP'],1,2028,{status:'Offered'});
const people=[nolan,senior,ss,second,bench,future];
const base=Object.fromEntries(M.positions.map(p=>[p,[]]));
Object.assign(base,{SP:['senior','nolan',null,null],SS:['ss'], '2B':['second','ss'],DH:['ss','second','bench']});
assert.deepEqual(M.dhCandidates(base),['bench']);
assert.equal(M.lineup(base).DH,'bench');
const changedStarter=JSON.parse(JSON.stringify(base));changedStarter.SS=[];
assert.ok(M.dhCandidates(changedStarter).includes('ss'));
const twoWayStarter={...base,DH:['nolan','bench']};assert.deepEqual(M.dhCandidates(twoWayStarter),['bench']);assert.equal(M.lineup(twoWayStarter).DH,'bench');
assert.equal(M.nameClass(nolan,'SP'),'name-left');
assert.equal(M.nameClass(nolan),'name-left');
assert.equal(M.nameClass(ss,'SS'),'name-switch');
assert.equal(M.nameClass(bench,'DH'),'name-left');
assert.equal(M.nameClass(player('r',['C'],1,2027,{bats:'R'}),'C'),'');
assert.equal(M.nameClass(player('two',['LHP','1B'],1,2027,{bats:'S',throws:'L'}),'SP'),'name-left');
assert.equal(M.nameClass(player('two',['LHP','1B'],1,2027,{bats:'S',throws:'L'}),'1B'),'name-switch');
const charts={2027:base,2028:{...M.initial(people,2028),SP:['recruit'],Bullpen:['nolan']}};
M.forward(charts,people,2027,2031);
for(const y of [2028,2029,2030]){
 assert.equal(charts[y].SP[1],'nolan');
 assert.ok(!charts[y].LHP.includes('nolan'));
 assert.equal(charts[y].SS[0],'ss');
 assert.equal(charts[y]['2B'][0],'second');
 assert.ok(!Object.values(charts[y]).flat().includes('senior'));
}
assert.ok(!Object.values(charts[2031]).flat().includes('nolan'));
assert.equal(charts[2031].SS[0],'ss');
assert.ok(Object.values(charts[2028]).flat().includes('recruit'));
assert.equal(charts[2028].SP[0],'recruit');
const reopened=JSON.parse(JSON.stringify(charts));
assert.equal(M.normalize(reopened[2029],people,2029).SP[1],'nolan');
// Editing a later season leaves earlier saved seasons alone.
reopened[2029].SP[1]=null;reopened[2029].Closer=['nolan'];
M.forward(reopened,people,2029,2031);
assert.equal(reopened[2028].SP[1],'nolan');
assert.equal(reopened[2030].Closer[0],'nolan');
assert.ok(!reopened[2030].SP.includes('nolan'));
console.log('DH exclusions, handedness, fixed pitching slots, future arrivals, eligibility, reload, and forward-only changes passed.');
// Positional needs use projected eligibility and count multi-role players once per category.
const utility=player('utility',['C','SS','2B','OF','LHP'],1,2027,{bats:'S',throws:'L'});
const needs=[utility,utility,player('corner',['1B','3B'],1,2027,{bats:'R'}),
 player('grad',['C'],5),player('arrival',['C'],1,2028,{bats:'L'}),
 player('departed',['OF'],1,2027,{removedFrom:2028}),
 player('unsigned',['C'],1,2028,{status:'Recruiting'}),player('arm',['RHP'],1,2027,{bats:'R'})];
const counts=Object.fromEntries(M.positionalBreakdown(needs,2028).map(x=>[x.label,x.count]));
assert.deepEqual(counts,{Total:4,C:2,OF:1,MIF:1,CIF:1,LHP:1,RHP:1,LHH:2,RHH:2});
assert.deepEqual(M.positionalBreakdown([],2028).map(x=>x.target),[34,3,6,4,4,8,8,6,6]);
assert.equal(M.signingClass({entryYear:2028}),2027);
assert.equal(M.signingClass({entryYear:'2029'}),2028);
assert.equal(needs[4].entryYear,2028);
console.log('Positional counts, two-way/switch hitters, future eligibility, and signing classes passed.');
