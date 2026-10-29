(function(root){
'use strict';
const defense=['C','1B','2B','3B','SS','LF','CF','RF'];
const arms=['SP','LongRelief','LHP','RHP','Closer'];
const positions=[...defense,'DH',...arms];
const limits={SP:4,LongRelief:3,Closer:3};
const labels={SP:'Starting pitchers',LongRelief:'Long relief',LHP:'Left handed',RHP:'Right handed',Closer:'Closer'};
const grade=(p,y)=>Number(p.entryClass)+Number(y)-Number(p.entryYear);
const active=(p,y)=>grade(p,y)>=1&&grade(p,y)<=5&&p.status!=='Recruiting'&&(!p.removedFrom||Number(y)<Number(p.removedFrom));
const throwingHand=p=>p.throws||(p.positions.includes('LHP')?'L':'R');
const reliefGroup=p=>throwingHand(p)==='L'?'LHP':'RHP';
const eligible=(p,pos)=>p.positions.includes(pos)||(arms.includes(pos)&&p.positions.some(x=>['RHP','LHP','P'].includes(x)))||(defense.slice(1,5).includes(pos)&&p.positions.includes('INF'))||(['LF','CF','RF'].includes(pos)&&p.positions.includes('OF'))||(pos==='DH'&&p.positions.some(x=>!['RHP','LHP','P'].includes(x)));
function nameClass(p,role){
 const pitcher=arms.includes(role)||role==='Bullpen'||(!role&&p.positions.every(x=>['RHP','LHP','P'].includes(x)));
 const hand=pitcher?throwingHand(p):p.bats;
 return !pitcher&&hand==='S'?'name-switch':hand==='L'?'name-left':'';
}
function lineup(c){
 const used=new Set(),result={};
 for(const pos of [...defense,'DH']){const id=(c[pos]||[]).find(id=>id&&!used.has(id)&&(pos!=='DH'||!(c.SP||[]).includes(id)));result[pos]=id||null;if(id)used.add(id);}
 return result;
}
function dhCandidates(c){const starters=new Set(Object.entries(lineup(c)).filter(([p])=>p!=='DH').map(([,id])=>id));(c.SP||[]).filter(Boolean).forEach(id=>starters.add(id));return (c.DH||[]).filter(id=>id&&!starters.has(id));}
function initial(players,y){
 const available=players.filter(p=>active(p,y)),c=Object.fromEntries(positions.map(p=>[p,[]])),used=new Set();
 for(const pos of [...defense,'DH']){
  const candidates=available.filter(p=>eligible(p,pos)),first=candidates.find(p=>!used.has(p.id));
  if(first){c[pos].push(first.id);used.add(first.id);}c[pos].push(...candidates.filter(p=>p!==first).map(p=>p.id));
 }
 const pitchers=available.filter(p=>eligible(p,'SP'));c.SP=pitchers.slice(0,4).map(p=>p.id);
 pitchers.slice(4).forEach(p=>c[reliefGroup(p)].push(p.id));return normalize(c,players,y);
}
function normalize(c,players,y){
 const result={},available=players.filter(p=>active(p,y)),valid=new Set(available.map(p=>p.id)),byId=new Map(players.map(p=>[p.id,p]));
 // Nulls retain exact depth numbers after a graduation or departure.
 for(const pos of positions)result[pos]=(c[pos]||[]).map(id=>valid.has(id)?id:null);
 for(const id of c.Bullpen||[]){if(valid.has(id)&&!arms.some(pos=>result[pos].includes(id)))result[reliefGroup(byId.get(id))].push(id);}
 const used=new Set();
 for(const pos of arms)result[pos]=result[pos].map(id=>{if(!id||used.has(id))return null;used.add(id);return id;});
 for(const p of available){
  if(![...defense,'DH'].some(pos=>result[pos].includes(p.id))){
   for(const pos of [...defense,'DH'].filter(pos=>eligible(p,pos)))result[pos].push(p.id);
  }
  if(eligible(p,'SP')&&!arms.some(pos=>result[pos].includes(p.id)))result[reliefGroup(p)].push(p.id);
 }
 for(const [pos,size] of Object.entries(limits))while(result[pos].length<size)result[pos].push(null);
 return result;
}
function project(source,target,players,from,to){
 source=normalize(source,players,from);
 const baseIds=new Set(positions.flatMap(pos=>source[pos]).filter(Boolean));
 const valid=new Set(players.filter(p=>active(p,to)).map(p=>p.id)),byId=new Map(players.map(p=>[p.id,p])),result={};
 const overflow=[];
 for(const pos of positions){
  result[pos]=source[pos].map(id=>valid.has(id)?id:null);
  const arrivals=(target?.[pos]||[]).filter(id=>id&&valid.has(id)&&!baseIds.has(id));
  for(const id of arrivals){
   if(limits[pos]){const slot=result[pos].indexOf(null);if(slot>=0)result[pos][slot]=id;else overflow.push(id);}
   else if(!result[pos].includes(id))result[pos].push(id);
  }
 }
 for(const id of overflow)result[reliefGroup(byId.get(id))].push(id);
 return normalize(result,players,to);
}
function forward(charts,players,from,horizon,overrides={}){
 const last=Math.max(horizon,...players.map(p=>Number(p.entryYear)+5-Number(p.entryClass)),...Object.keys(charts).map(Number));
 for(let y=from+1;y<=last;y++)charts[y]=overrides[y]&&charts[y]?normalize(charts[y],players,y):project(charts[y-1],charts[y],players,y-1,y);
 return charts;
}
function reorderDepth(c,pos,id,offset){
 const ids=limits[pos]?[...c[pos]]:c[pos].filter(Boolean),index=ids.indexOf(id),target=index+offset;
 if(index<0||target<0||target>=ids.length)return false;
 [ids[index],ids[target]]=[ids[target],ids[index]];c[pos]=ids;return true;
}
function moveDepth(c,id,from,to,index){
 if(from===to){const ids=c[to].filter(value=>value&&value!==id);ids.splice(Math.min(index,ids.length),0,id);if(limits[to])while(ids.length<limits[to])ids.push(null);c[to]=ids;return;}
 c[from]=limits[from]?c[from].map(value=>value===id?null:value):c[from].filter(value=>value&&value!==id);
 if(arms.includes(to))for(const pos of arms)c[pos]=limits[pos]?c[pos].map(value=>value===id?null:value):c[pos].filter(value=>value&&value!==id);
 if(limits[to]){while(c[to].length<limits[to])c[to].push(null);const empty=c[to].indexOf(null);if(empty<0)return;const dest=Math.min(index,limits[to]-1);c[to].splice(empty,1);c[to].splice(dest,0,id);}
 else{c[to]=c[to].filter(value=>value&&value!==id);c[to].splice(Math.min(index,c[to].length),0,id);}
}
function removeFrom(state,id,season){
 state.departures??={};state.departures[id]=season;
 for(const store of [state.charts,...Object.values(state.matchups||{})])for(const [y,c] of Object.entries(store||{})){
  if(Number(y)>=season)for(const pos of Object.keys(c))c[pos]=c[pos].map(value=>value===id?null:value);
 }
 const awards=state.funding?.awards?.[id];
 if(awards)for(const y of Object.keys(awards))if(Number(y)>=season)delete awards[y];
 if(awards&&!Object.keys(awards).length)delete state.funding.awards[id];
}
const categories=['LHP','RHP','Catchers','Outfielders','Middle infielders','Corner infielders'];
function budgetCategory(p,c){
 if(arms.some(pos=>(c[pos]||[]).includes(p.id))||p.positions.every(pos=>['LHP','RHP','P'].includes(pos)))return reliefGroup(p);
 const chosen=Object.entries(lineup(c)).find(([pos,id])=>id===p.id&&pos!=='DH')?.[0];
 const pos=chosen||p.positions.find(pos=>!['P','LHP','RHP','DH'].includes(pos));
 if(pos==='C')return 'Catchers';
 if(!pos)return 'Unassigned';
 if(['LF','CF','RF','OF'].includes(pos))return 'Outfielders';
 if(['SS','2B','INF'].includes(pos))return 'Middle infielders';
 return 'Corner infielders';
}
function budgetBreakdown(funding,players,y,c){
 const totals=Object.fromEntries(categories.map(name=>[name,0]));
 for(const p of new Map(players.filter(p=>active(p,y)).map(p=>[p.id,p])).values()){const category=budgetCategory(p,c);totals[category]=(totals[category]||0)+Math.round(allocation(funding,p,y)*100);}
 return Object.entries(totals).map(([name,cents])=>({name,value:cents/100}));
}
// Year-keyed amounts inherit forward; explicit later-season overrides remain intact.
function amountAt(schedule,y,fallback=0){
 const years=Object.keys(schedule||{}).map(Number).filter(n=>n<=y).sort((a,b)=>b-a);
 return years.length?Number(schedule[years[0]]):fallback;
}
function allocation(funding,p,y){
 if(!active(p,y))return 0;
 return amountAt(funding?.awards?.[p.id],y,Number(p.offerPackage||0));
}
function budgetSummary(funding,players,y){
 const unique=[...new Map(players.filter(p=>active(p,y)).map(p=>[p.id,p])).values()];
 const usedCents=unique.reduce((total,p)=>total+Math.round(allocation(funding,p,y)*100),0);
 const budget=amountAt(funding?.budgets,y,null),used=usedCents/100;
 return {budget,used,remaining:budget===null?null:(Math.round(budget*100)-usedCents)/100,
  percent:budget>0?used/budget*100:null,
  incoming:unique.filter(p=>p.recruit&&Number(p.entryYear)===Number(y)).reduce((n,p)=>n+Math.round(allocation(funding,p,y)*100),0)/100};
}
// Count each eligible player once per category; multi-position players can fill multiple needs.
function positionalBreakdown(players,y){
 const unique=[...new Map(players.filter(p=>active(p,y)).map(p=>[p.id,p])).values()];
 const has=(p,positions)=>p.positions.some(pos=>positions.includes(pos));
 const hitters=p=>has(p,[...defense,'DH','INF','OF','MIF','CIF']);
 const groups=[['Total',34,()=>true],['C',3,p=>has(p,['C'])],
  ['OF',6,p=>has(p,['OF','LF','CF','RF'])],['MIF',4,p=>has(p,['MIF','INF','SS','2B'])],
  ['CIF',4,p=>has(p,['CIF','INF','1B','3B'])],
  ['LHP',8,p=>eligible(p,'SP')&&throwingHand(p)==='L'],['RHP',8,p=>eligible(p,'SP')&&throwingHand(p)==='R'],
  ['LHH',6,p=>hitters(p)&&['L','S'].includes(p.bats)],['RHH',6,p=>hitters(p)&&['R','S'].includes(p.bats)]];
 return groups.map(([label,target,matches])=>({label,target,count:unique.filter(matches).length}));
}
const signingClass=p=>Number(p.entryYear)-1;
function compactMoney(value){
 const n=Number(value);
 if(n>=1e6)return '$'+Number((n/1e6).toFixed(1))+'m';
 if(n>=1e3)return '$'+Number((n/1e3).toFixed(1))+'k';
 return '$'+Number(n.toFixed(2));
}
function armPoint(p,y){
 const has=value=>typeof value==='number'&&Number.isFinite(value);
 if(!active(p,y)||!eligible(p,'SP')||!has(p.heightIn)||!has(p.releaseHeightFt)||!['L','R'].includes(p.throws))return null;
 const x=(p.throws==='L'?-1:1)*(has(p.releaseSideFt)&&p.releaseSideFt!==0?Math.abs(p.releaseSideFt):1.5),anchorY=(p.heightIn-10)/12;
 return {id:p.id,x,y:p.releaseHeightFt,anchorY,angle:Math.atan2(p.releaseHeightFt-anchorY,Math.abs(x))*180/Math.PI,classYear:grade(p,y),status:p.status};
}
const api={positionalBreakdown,signingClass,positions,defense,arms,limits,labels,grade,active,eligible,throwingHand,reliefGroup,nameClass,lineup,dhCandidates,initial,normalize,project,forward,removeFrom,categories,budgetCategory,budgetBreakdown,amountAt,allocation,budgetSummary,compactMoney,armPoint,reorderDepth,moveDepth};
if(typeof module!=='undefined'&&module.exports)module.exports=api;else root.RosterModel=api;
})(typeof window!=='undefined'?window:globalThis);
