const assert=require('node:assert/strict');const M=require('../www/depth-model.js');
const c={C:['a','b','c'],SP:['p','q',null,null],LongRelief:[null,null,null],LHP:['l'],RHP:['r','s'],Closer:[null,null,null]};
for(let i=0;i<100;i++){assert.ok(M.reorderDepth(c,'C','b',-1));assert.ok(M.reorderDepth(c,'C','b',1));assert.equal(c.C.length,3);assert.equal(new Set(c.C).size,3);}
assert.deepEqual(c.C,['a','b','c']);assert.equal(M.reorderDepth(c,'C','a',-1),false);
for(let i=0;i<100;i++){M.moveDepth(c,'b','C','C',0);M.moveDepth(c,'b','C','C',1);assert.equal(c.C.length,3);assert.equal(c.C.filter(Boolean).length,3);}
M.reorderDepth(c,'SP','q',1);assert.deepEqual(c.SP,['p',null,'q',null]);assert.equal(c.SP.length,4);
M.moveDepth(c,'r','RHP','LongRelief',0);assert.deepEqual(c.RHP,['s']);assert.deepEqual(c.LongRelief,['r',null,null]);
M.moveDepth(c,'r','LongRelief','RHP',0);assert.deepEqual(c.RHP,['r','s']);assert.equal(c.LongRelief.length,3);assert.ok(!c.LongRelief.includes('r'));
console.log('Repeated depth reorders and position changes keep unique players and stable slot counts.');
