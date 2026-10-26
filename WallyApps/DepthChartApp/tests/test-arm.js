const assert=require('node:assert/strict');const M=require('../www/depth-model.js');
const p={id:'two',first:'Two',last:'Way',positions:['1B','RHP'],throws:'R',bats:'R',entryYear:2027,entryClass:2,status:'Signed',heightIn:72,releaseHeightFt:74/12,releaseSideFt:2};
const c=M.normalize({ '1B':['two']},[p],2027);assert.ok(c['1B'].includes(p.id));assert.ok(c.RHP.includes(p.id));
assert.equal(M.arms.flatMap(pos=>c[pos]).filter(id=>id===p.id).length,1);
const point=M.armPoint(p,2027);assert.equal(point.x,2);assert.equal(point.anchorY,62/12);assert.ok(Math.abs(point.angle-26.565051)<.00001);assert.equal(point.classYear,2);
assert.equal(M.armPoint({...p,throws:'L'},2027).x,-2);
for(const key of ['heightIn','releaseHeightFt','throws'])assert.equal(M.armPoint({...p,[key]:null},2027),null);
assert.equal(M.armPoint({...p,removedFrom:2028},2028),null);assert.equal(M.armPoint(p,2031),null);
assert.equal(M.armPoint({...p,status:'Recruiting'},2027),null);
assert.equal(M.armPoint({...p,status:'Committed'},2027).status,'Committed');
assert.ok(M.armPoint({...p,releaseHeightFt:4},2027).angle<0);
const next=M.project(c,null,[p],2027,2028);assert.equal(next['1B'][0],p.id);assert.ok(next.RHP.includes(p.id));
const funding={budgets:{2027:100000},awards:{two:{2027:65000}}};assert.equal(M.budgetSummary(funding,[p],2027).used,65000);assert.equal(M.budgetBreakdown(funding,[p],2027,c).reduce((sum,g)=>sum+g.value,0),65000);
console.log('Two-way placement, inferred angle math, left/right mirroring, missing data, class progression, and unique allocation passed.');

for(const missing of [null,undefined,0]) {
 const profile={...p,releaseSideFt:missing};
 const point=M.armPoint(profile,2027);
 assert.equal(point.x,1.5);
 assert.equal(M.armPoint({...profile,throws:'L'},2027).x,-1.5);
 assert.equal(profile.releaseSideFt,missing);
 assert.ok(Math.abs(point.angle-Math.atan2(p.releaseHeightFt-(p.heightIn-10)/12,1.5)*180/Math.PI)<1e-9);
}
