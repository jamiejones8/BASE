(() => {
'use strict';
const POS=window.RosterModel.positions;
const FIELD=['LF','CF','RF','SS','2B','3B','SP','1B','DH','C'];
const CLASSES=['Freshman','Sophomore','Junior','Senior','5th year'];
let roster=[],state,year,currentYear,page='field',signingYear,dirty=false,drag=null,selected=null,pending=false,version=0,savedVersion=0,viewKey='',mode='General',lineupDrag=null;
const el=(tag,cls,text)=>{const e=document.createElement(tag);if(cls)e.className=cls;if(text!==undefined)e.textContent=text;return e;};
const button=(text,fn,cls='')=>{const b=el('button',cls,text);b.type='button';b.onclick=fn;return b;};
const all=()=>[...roster,...state.profiles].map(p=>({...p,...(!p.recruit?state.playerDetails?.[p.id]:{}),removedFrom:state.departures?.[p.id]}));
const grade=(p,y=year)=>Number(p.entryClass)+y-Number(p.entryYear);
const active=()=>all().filter(p=>M.active(p,year));
const M=window.RosterModel;
const eligible=M.eligible;
const money=n=>n===null?'Not set':new Intl.NumberFormat('en-US',{style:'currency',currency:'USD',maximumFractionDigits:Number.isInteger(n)?0:2}).format(n);
const allocation=p=>M.allocation(state.funding,p,year);
function markDirty(){dirty=true;version++;const status=document.querySelector('.save-status');if(status)status.textContent='Unsaved changes';}
function moneyInput(label,value,onChange){
 const input=el('input','money-input');input.type='number';input.min='0';input.max='1000000000';input.step='0.01';input.required=true;
 input.setAttribute('aria-label',label);input.value=value===null?'':String(value);
 input.oninput=()=>{if(!input.validity.valid)return;onChange(Number(input.value));markDirty();refreshFunding();};
 return input;
}
function budgetCard(y){
 const summary=M.budgetSummary(state.funding,all(),y),card=el('section','budget-card');card.setAttribute('aria-label',`${y} REV Share Budget`);
 card.append(el('div','eyebrow','REV Share Budget'));
 const stats=el('div','budget-stats');
 for(const [label,value] of [['Budget',summary.budget],['Used',summary.used],['Remaining',summary.remaining]]){const item=el('div');item.append(el('span','budget-label',label),el('strong',label==='Remaining'&&value<0?'over-budget':'',money(value)));stats.append(item);}
 card.append(stats);
 const track=el('div','budget-track'),fill=el('div',summary.remaining!==null&&summary.remaining<0?'budget-fill over':'budget-fill');
 const pct=summary.percent??(summary.used>0&&summary.budget===0?100:0);fill.style.width=`${Math.min(100,Math.max(0,pct))}%`;track.append(fill);
 track.setAttribute('role','progressbar');track.setAttribute('aria-label','REV Share Budget used');track.setAttribute('aria-valuemin','0');track.setAttribute('aria-valuemax','100');track.setAttribute('aria-valuenow',String(Math.min(100,pct)));
 const label=summary.budget===null?'Set a total budget to see available room':summary.budget===0?(summary.used>0?'Allocations exceed the $0 budget':'0% of Budget Used'):`${Math.round(summary.percent)}% of Budget Used`;
 track.setAttribute('aria-valuetext',label);card.append(track,el('div','budget-caption',label));return card;
}
function rosterSummary(y){
 const row=el('div','roster-summary'),card=el('section','budget-card positional-card');
 card.setAttribute('aria-label',`${y} Positional Breakdown`);card.append(el('div','eyebrow','Positional Breakdown'));
 const grid=el('div','positional-grid');
 for(const {label,target,count} of M.positionalBreakdown(all(),y)){
  const item=el('div','positional-count'),number=el('strong',y>currentYear&&count<target?'roster-shortage':'',String(count));
  item.append(el('span','position-label',`${label}: `),number,el('span','',`/${target}`));grid.append(item);
 }
 card.append(grid);row.append(budgetCard(y),card);return row;
}
function fundingOutlook(){
 const table=el('table','funding-outlook');const head=el('tr');['Season','Budget','Used','Incoming class','Remaining'].forEach(t=>head.append(el('th','',t)));const thead=el('thead');thead.append(head);table.append(thead);const body=el('tbody');
 for(let y=currentYear;y<currentYear+5;y++){const s=M.budgetSummary(state.funding,all(),y),row=el('tr',y===year?'selected-season':'');[String(y),money(s.budget),money(s.used),money(s.incoming),money(s.remaining)].forEach((v,i)=>row.append(el('td',i===4&&s.remaining!==null&&s.remaining<0?'over-budget':'',v)));body.append(row);}table.append(body);return table;
}
function refreshFunding(){
 document.querySelectorAll('[data-budget-summary]').forEach(e=>e.replaceChildren(budgetCard(year)));
 document.querySelectorAll('[data-funding-outlook]').forEach(e=>e.replaceChildren(fundingOutlook()));
}
function financeView(root){
 root.append(button('← Back to depth chart',()=>{page='field';render();},'back'),el('h2','',`${year} Scholarships and NIL`),el('p','muted','Annual allocations carry into later eligible seasons. Edit a later season to set a different amount from that year onward. Offered and Committed packages reserve room; Recruiting profiles are excluded.'));
 const summary=el('div');summary.dataset.budgetSummary='';summary.append(budgetCard(year));root.append(summary);
 const budgetLabel=el('label','budget-input-label','Total annual budget ($)');budgetLabel.append(moneyInput('Total annual budget ($)',M.amountAt(state.funding.budgets,year,null),value=>{state.funding.budgets[year]=value;}));root.append(budgetLabel);
 root.append(el('h2','','Five-season budget outlook'));const outlook=el('div','table-scroll');outlook.dataset.fundingOutlook='';outlook.append(fundingOutlook());root.append(outlook);
 root.append(el('h2','',`Player allocations · ${active().length}`),el('p','muted','Amounts are annual totals for Scholarships and NIL. Each player counts once. Save changes to keep these allocations.'));
 const table=el('table','allocation-table');const head=el('tr');['Player','Class / positions','Status','Annual amount ($)'].forEach(t=>head.append(el('th','',t)));const thead=el('thead');thead.append(head);table.append(thead);const body=el('tbody');
 for(const p of active()){const row=el('tr'),name=el('td',M.nameClass(p),`${p.first} ${p.last}`),amount=el('td');amount.append(moneyInput(`Annual amount for ${p.first} ${p.last}`,allocation(p),value=>{state.funding.awards[p.id]??={};state.funding.awards[p.id][year]=value;}));row.append(name,el('td','',`${CLASSES[grade(p)-1]} · ${p.positions.join('/')}`),el('td','',p.status),amount);body.append(row);}table.append(body);const scroll=el('div','table-scroll');scroll.append(table);root.append(scroll);
}

function chartStore(){return mode==='General'?state.charts:state.matchups[mode];}
function chart(){
 const store=chartStore(),key=String(year);
 if(!state.charts[currentYear])state.charts[currentYear]=M.initial(all(),currentYear);
 if(!store[key]){
  const previous=Object.keys(store).map(Number).filter(y=>y<year).sort((a,b)=>b-a)[0];
  if(previous)store[key]=M.project(store[previous],null,all(),previous,year);
  else store[key]=mode==='General'?M.initial(all(),year):M.normalize(state.charts[key]||M.project(state.charts[currentYear],null,all(),currentYear,year),all(),year);
 }
 store[key]=M.normalize(store[key],all(),year);return store[key];
}
function carryForward(){chart();M.forward(chartStore(),all(),year,currentYear+4,state.planEdits[mode]);}
function battingOrder(){
 const store=state.lineupOrders[mode];
 if(!store[year]){const previous=Object.keys(store).map(Number).filter(y=>y<year).sort((a,b)=>b-a)[0];store[year]=[...(previous?store[previous]:state.lineupOrders.General[year]||[...M.defense,'DH'])];}
 return store[year];
}
function reorderLineup(from,to){
 const order=battingOrder(),index=order.indexOf(from);if(index<0)return;
 order.splice(index,1);order.splice(to,0,from);state.lineupEdits[mode][year]=true;
 for(let y=year+1;y<=currentYear+4;y++){if(state.lineupEdits[mode][y])break;state.lineupOrders[mode][y]=[...order];}
 changed();
}
function changed(){dirty=true;version++;render();}
function plate(id,pos,index,role=pos){
 const p=all().find(p=>p.id===id);if(!p)return el('div');
 const card=el('div',`plate class-${grade(p)} status-${p.status.toLowerCase()}`);
 card.draggable=false;card.tabIndex=0;
 card.title=`${p.first} ${p.last} · ${CLASSES[grade(p)-1]} · ${p.positions.join('/')} · ${p.status} · B/T: ${p.bats||'?'}/${p.throws||'?'}`;
 const amount=el('span','player-amount',M.compactMoney(allocation(p)));amount.title=`Annual allocation: ${money(allocation(p))}`;
 card.append(el('span',`player-name ${M.nameClass(p,role)}`,`${p.first} ${p.last}`),el('span','class-label',['FR','SO','JR','SR','5TH'][grade(p)-1]),amount);
 if(pos){
 card.onclick=()=>{selected={id,pos,index};render();};
 card.onkeydown=e=>{if(e.target!==card)return;if(e.key==='Enter'||e.key===' '){e.preventDefault();selected={id,pos,index};render();}};
 const controls=el('div','depth-controls');const ids=M.limits[pos]?chart()[pos]:(pos==='DH'?M.dhCandidates(chart()):chart()[pos].filter(Boolean));const at=ids.indexOf(id);
 for(const [text,offset] of [['↑',-1],['↓',1]]){const b=button(text,e=>{});b.onclick=e=>{e.stopPropagation();const c=chart();if(pos==='DH'){const visible=M.dhCandidates(c),from=visible.indexOf(id),other=visible[from+offset];if(!other)return;const a=c.DH.indexOf(id),z=c.DH.indexOf(other);[c.DH[a],c.DH[z]]=[c.DH[z],c.DH[a]];}else if(!M.reorderDepth(c,pos,id,offset))return;selected=null;state.planEdits[mode][year]=true;carryForward();changed();};b.setAttribute('aria-label',`Move ${p.first} ${p.last} ${offset<0?'up':'down'} in ${M.labels[pos]||pos}`);b.disabled=at+offset<0||at+offset>=ids.length;controls.append(b);}card.append(controls);
 if(selected?.id===id&&selected.pos===pos)card.classList.add('selected');
 }return card;
}
function move(source,to,index){
 if(!source)return;const c=chart(),arms=M.arms;
 const player=all().find(p=>p.id===source.id);
 if(arms.includes(to)&&!M.eligible(player,'SP')){alert('Choose a pitching position for a pitcher.');return;}
 if(['LHP','RHP'].includes(to)&&M.reliefGroup(player)!==to){alert('Use the relief group matching this pitcher’s throwing hand.');return;}
 if(to==='DH'&&(c.SP.includes(source.id)||Object.entries(M.lineup(c)).some(([pos,id])=>pos!=='DH'&&id===source.id))){alert('Move this player out of the starting lineup before assigning DH.');return;}
 const limit=M.limits[to]||Infinity;
 if(source.pos!==to&&!c[to].includes(source.id)&&c[to].filter(Boolean).length>=limit){alert(`${to} has ${limit} slots. Move a player out first.`);return;}
 M.moveDepth(c,source.id,source.pos,to,index);
 selected=null;state.planEdits[mode][year]=true;carryForward();changed();
}
function stack(pos){
 const c=chart(),box=el('section',`position pos-${pos}`);box.setAttribute('aria-label',`${pos} depth chart`);
 const ids=pos==='DH'?M.dhCandidates(c):M.limits[pos]?c[pos]:c[pos].filter(Boolean);
 const h=el('div','position-title');h.append(el('strong','',M.labels[pos]||pos),el('span','count',`${ids.filter(Boolean).length}${M.limits[pos]?' / '+M.limits[pos]:''}`));box.append(h);
 const list=el('div','depth-list');ids.forEach((id,i)=>{if(id)list.append(plate(id,pos,c[pos].indexOf(id)));else list.append(el('div','empty-slot',`Open ${M.labels[pos]||pos} slot ${i+1}`));});
 if(!ids.length)list.append(el('div','empty-slot',`No players at ${M.labels[pos]||pos}`));
 box.append(list);return box;
}
function fieldView(root){
 const workspace=el('div','workspace'),left=el('div','field-column'),field=el('div','field');
 const backdrop=el('img','field-art');backdrop.src='home-park.svg';backdrop.alt='';backdrop.setAttribute('aria-hidden','true');field.append(backdrop);
 FIELD.forEach(pos=>field.append(stack(pos)));left.append(field);
 const pitching=el('div','pitching');pitching.append(stack('LongRelief'),stack('LHP'),stack('RHP'),stack('Closer'));
 const side=el('aside','sidebar'),tabs=el('div','matchup-tabs');
 for(const variant of ['General','v LHP','v RHP'])tabs.append(button(variant,()=>{mode=variant;selected=null;render();},mode===variant?'active':''));
 side.append(tabs,el('div','eyebrow','GAME DAY'),el('h2','','Projected lineup'),el('p','muted',`${mode} · Use arrows to set batting order.`));
 const c=chart(),used=new Set(),lineup=M.lineup(c);
 battingOrder().forEach((pos,i)=>{
 const row=el('div','lineup-row');row.draggable=false;row.dataset.position=pos;row.setAttribute('aria-label',`Batting slot ${i+1}, ${pos}`);
 row.append(el('span','order',String(i+1)),el('span','lineup-position',pos));const id=lineup[pos];if(id){used.add(id);const hitter=plate(id,undefined,undefined,pos);hitter.draggable=false;row.append(hitter);}else row.append(el('span','muted','Open spot'));
 const controls=el('div','batting-controls');for(const [text,offset] of [['↑',-1],['↓',1]]){const b=button(text,()=>reorderLineup(pos,i+offset));b.setAttribute('aria-label',`Move batting slot ${i+1} ${offset<0?'up':'down'}`);b.disabled=i+offset<0||i+offset>8;controls.append(b);}row.append(controls);side.append(row);
 });
 const bench=active().filter(p=>!used.has(p.id)&&p.positions.some(x=>!['RHP','LHP','P'].includes(x)));
 side.append(el('h2','bench-title',`Bench · ${bench.length}`));const benchList=el('div','bench-list');bench.forEach(p=>benchList.append(plate(p.id)));if(!bench.length)benchList.append(el('p','muted','No remaining position players.'));side.append(benchList);
 side.append(el('p','muted small','Select a depth-chart name to change its position or edit its profile.'));
 workspace.append(left,side);root.append(workspace,pitching,armVisualization());
 root.append(budgetVisualization());
 if(selected){const bar=el('div','move-bar');bar.append(el('strong','',`Move ${all().find(p=>p.id===selected.id)?.last}`));const select=el('select');POS.forEach(p=>{const o=el('option','',M.labels[p]||p);o.value=p;select.append(o);});select.value=selected.pos;bar.append(select,button('Move to top',()=>move(selected,select.value,0)),button('Move to bottom',()=>move(selected,select.value,chart()[select.value].length)),button('Edit profile',()=>profileForm(all().find(p=>p.id===selected.id))),button('Remove player…',()=>removalDialog(all().find(p=>p.id===selected.id))),button('Cancel',()=>{selected=null;render();}));root.append(bar);}
}
function profileForm(existing){
 const isCurrent=!!existing&&!existing.recruit;const dialog=el('dialog'),form=el('form','profile-form');form.append(el('h2','',existing?'Edit player profile':'Create player profile'));
 const inputs={};
 function input(label,key,type,options,value){const l=el('label','',label),e=el(options?'select':'input');if(options)options.forEach(([v,t])=>{const o=el('option','',t);o.value=v;e.append(o);});else{e.type=type;e.required=true;e.maxLength=60;}e.value=value??'';l.append(e);form.append(l);inputs[key]=e;}
 input('First name','first','text',null,existing?.first);input('Last name','last','text',null,existing?.last);
 if(isCurrent){inputs.first.readOnly=true;inputs.last.readOnly=true;}
 input('Roster entry season','entryYear',null,Array.from({length:5},(_,i)=>[currentYear+i,currentYear+i]),existing?.entryYear||(signingYear+1));
 input('Class on arrival','entryClass',null,CLASSES.map((c,i)=>[i+1,c]),existing?.entryClass||1);
 input('Bats','bats',null,[['','Unknown'],['R','Right'],['L','Left'],['S','Switch']],existing?.bats||'');
 input('Throws','throws',null,[['','Unknown'],['R','Right'],['L','Left']],existing?.throws||'');
 input('Status','status',null,['Recruiting','Offered','Committed','Signed'].map(x=>[x,x]),existing?.status||'Recruiting');
 input('Offer Package ($ per year)','offerPackage','number',null,existing?.offerPackage??0);inputs.offerPackage.min='0';inputs.offerPackage.max='1000000000';inputs.offerPackage.step='0.01';form.append(el('p','muted small','Offer Package is the default annual allocation. Amounts set in Scholarships and NIL take priority for their season onward.'));
 if(isCurrent)for(const key of ['entryYear','entryClass','status','offerPackage'])inputs[key].disabled=true;
 for(const [label,key,min,max] of [['Height (inches)','heightIn',36,100],['Release height (ft)','releaseHeightFt',0.01,10],['Release side (ft from center)','releaseSideFt',0,10]]){
  input(label,key,'number',null,existing?.[key]??'');inputs[key].required=false;inputs[key].step='0.0001';inputs[key].min=String(min);inputs[key].max=String(max);
 }
 form.append(el('p','muted small','Pitchers appear on the arm-angle chart when height, both release measurements, and throwing hand are filled. Left-handers plot left of center; right-handers plot right.'));
 const positions=el('fieldset');positions.append(el('legend','','Position(s)'));const checks=[];
 ['C','1B','2B','3B','SS','LF','CF','RF','DH','INF','OF','RHP','LHP'].forEach(p=>{const l=el('label','check'),c=el('input');c.type='checkbox';c.value=p;c.checked=existing?.positions.includes(p)||false;checks.push(c);l.append(c,document.createTextNode(p));positions.append(l);});form.append(positions);
 const error=el('p','error');form.append(error);const actions=el('div','actions');const submit=el('button','primary','Apply profile');submit.type='submit';actions.append(submit,button('Cancel',()=>dialog.close()));form.append(actions,el('p','muted small','Use Save changes to persist profiles and depth charts.'));
 form.onsubmit=e=>{e.preventDefault();const ps=checks.filter(c=>c.checked).map(c=>c.value);if(!ps.length){error.textContent='Choose at least one position.';return;}if(!inputs.first.value.trim()||!inputs.last.value.trim()){error.textContent='Enter first and last name.';return;}
 const p={id:existing?.id||`recruit-${Date.now()}-${Math.random().toString(36).slice(2)}`,first:inputs.first.value.trim(),last:inputs.last.value.trim(),entryYear:Number(inputs.entryYear.value),entryClass:Number(inputs.entryClass.value),status:inputs.status.value,offerPackage:Number(inputs.offerPackage.value),bats:inputs.bats.value,throws:inputs.throws.value,positions:ps,recruit:true};
 const measurements={};for(const key of ['heightIn','releaseHeightFt','releaseSideFt'])measurements[key]=inputs[key].value===''?null:Number(inputs[key].value);
 if(isCurrent)state.playerDetails[existing.id]={positions:ps,bats:p.bats,throws:p.throws,...measurements};
 else{Object.assign(p,measurements);if(existing)state.profiles[state.profiles.findIndex(x=>x.id===p.id)]=p;else state.profiles.push(p);}
 dialog.close();changed();};
 dialog.append(form);document.body.append(dialog);dialog.onclose=()=>dialog.remove();dialog.showModal();
}
function recruitsView(root){
 root.append(button('← Back to depth chart',()=>{page='field';render();},'back'));
 const head=el('div','section-heading');head.append(el('div','', 'RECRUITING PIPELINE'),button('Create player profile',()=>profileForm(),'primary'));root.append(head);
 const tabs=el('nav','years');for(let y=currentYear-1;y<currentYear+4;y++)tabs.append(button(String(y),()=>{signingYear=y;render();},signingYear===y?'active':''));root.append(tabs);
 const list=all().filter(p=>p.recruit&&M.signingClass(p)===signingYear&&(!p.removedFrom||Number(p.entryYear)<p.removedFrom));root.append(el('h2','',`${signingYear} signing class`),el('p','muted',`Roster entry: ${signingYear+1} season. Offered, Committed and Signed players project into that season based on eligibility.`));
 if(!list.length){const empty=el('div','empty-recruits');empty.append(el('h2','','Build the next class'),el('p','muted','No player profiles yet.'),button('Create player profile',()=>profileForm(),'primary'));root.append(empty);}
 list.forEach(p=>{const row=el('div','recruit-row');row.append(el('strong',M.nameClass(p),`${p.first} ${p.last}`),el('span','',`${CLASSES[p.entryClass-1]} · ${p.positions.join(' / ')} · Offer ${money(Number(p.offerPackage||0))}/yr`),el('span',`badge ${p.status.toLowerCase()}`,p.status),el('div','profile-actions'));const actions=row.lastChild;actions.append(button('Edit profile',()=>profileForm(p)),button('Remove…',()=>removalDialog(p)));root.append(row);});
}
function initializePlans(){
 const object=value=>value&&!Array.isArray(value)?value:{};
 state.playerDetails=object(state.playerDetails);state.departures=object(state.departures);state.matchups=object(state.matchups);state.planEdits=object(state.planEdits);state.lineupOrders=object(state.lineupOrders);state.lineupEdits=object(state.lineupEdits);
 for(const variant of ['General','v LHP','v RHP']){
  state.planEdits[variant]=object(state.planEdits[variant]);state.lineupOrders[variant]=object(state.lineupOrders[variant]);state.lineupEdits[variant]=object(state.lineupEdits[variant]);
  if(variant!=='General')state.matchups[variant]=object(state.matchups[variant]);
 }
 for(const store of [state.charts,...Object.values(state.matchups)])for(const y of Object.keys(store))store[y]=M.normalize(store[y],all(),Number(y));
 if((state.schemaVersion||0)<2){
  if(!state.charts[currentYear])state.charts[currentYear]=M.initial(all(),currentYear);
  M.forward(state.charts,all(),currentYear,currentYear+4);
  state.schemaVersion=2;dirty=true;version++;
 }
}
function removalDialog(p){
 const dialog=el('dialog'),form=el('form','profile-form');form.append(el('h2','',`Remove ${p.first} ${p.last}`));
 const label=el('label','','Remove beginning with season'),select=el('select');select.setAttribute('aria-label','Remove beginning with season');
 for(let y=2027;y<=Math.max(currentYear+5,Number(p.entryYear)+5);y++){const option=el('option','',String(y));option.value=y;select.append(option);}select.value=String(Math.max(year+1,Number(p.entryYear)));label.append(select);form.append(label);
 const explanation=el('p','muted'),confirm=el('button','primary');confirm.type='submit';
 const update=()=>{const y=Number(select.value);explanation.textContent=`Remove this player from all ${y} and later depth charts and matchup plans, and stop counting their NIL allocation from ${y}. Seasons before ${y} keep their roster history and dollar amounts. Save changes to keep the removal.`;confirm.textContent=`Remove from ${y} onward`;};select.onchange=update;update();form.append(explanation);
 const actions=el('div','actions');actions.append(confirm,button('Cancel',()=>dialog.close()));form.append(actions);
 form.onsubmit=e=>{e.preventDefault();M.removeFrom(state,p.id,Number(select.value));selected=null;dialog.close();changed();};
 dialog.append(form);document.body.append(dialog);dialog.onclose=()=>dialog.remove();dialog.showModal();
}
function rosterView(root){
 root.append(button('← Back to depth chart',()=>{page='field';render();},'back'),el('h2','',`${year} roster management`),el('p','muted','Remove a player beginning with a chosen season. Previous seasons remain available. Removal defaults to next season.'));
 const table=el('table','allocation-table'),header=el('tr');['Player','Positions','Status','Roster action'].forEach(t=>header.append(el('th','',t)));const head=el('thead');head.append(header);table.append(head);const body=el('tbody');
 for(const p of all().filter(p=>grade(p)>=1&&grade(p)<=5&&(!p.removedFrom||year<p.removedFrom))){const row=el('tr'),action=el('td');action.append(button('Edit profile',()=>profileForm(p)),button('Remove player…',()=>removalDialog(p)));if(p.removedFrom)action.append(el('p','muted',`Leaves in ${p.removedFrom}`));row.append(el('td',M.nameClass(p),`${p.first} ${p.last}`),el('td','',p.positions.join('/')),el('td','',p.status),action);body.append(row);}table.append(body);const scroll=el('div','table-scroll');scroll.append(table);root.append(scroll);
}
function armVisualization(){
 const section=el('section','arm-visualization');section.append(el('h2','',`${year} pitcher release and inferred arm angle`));
 const pitchers=active().filter(p=>M.eligible(p,'SP')),points=pitchers.map(p=>({p,point:M.armPoint(p,year)})).filter(d=>d.point);
 section.append(el('p','muted',`${points.length} plotted · ${pitchers.length-points.length} awaiting complete measurements. Release positions are in feet; shoulder anchor is player height minus 10 inches. Unknown release side uses 1.5 ft for this chart only.`));
 const colors=['#fff','#d8dce0','#cce7b7','#f7e28e','#e7a3a0'];
 const key=el('div','arm-key');CLASSES.forEach((name,i)=>{const item=el('span','arm-key-item'),dot=el('span','arm-class-dot');dot.style.background=colors[i];item.append(dot,document.createTextNode(name));key.append(item);});key.append(el('span','','Solid: Signed'),el('span','','Long dash: Offered'),el('span','','Short dash: Committed'));section.append(key);
 if(points.length){
  const xmax=Math.max(3,Math.ceil((Math.max(...points.map(d=>Math.abs(d.point.x)))+.45)*2)/2);
  const ymin=Math.floor((Math.min(...points.flatMap(d=>[d.point.y,d.point.anchorY]))-.3)*2)/2;
  const ymax=Math.ceil((Math.max(...points.flatMap(d=>[d.point.y,d.point.anchorY]))+.3)*2)/2;
  const scale=1000/(2*xmax),width=1200,height=(ymax-ymin)*scale+155;
  const X=x=>80+(x+xmax)*scale,Y=y=>70+(ymax-y)*scale;
  const ns='http://www.w3.org/2000/svg',svg=document.createElementNS(ns,'svg');svg.classList.add('arm-plot');svg.setAttribute('viewBox',`0 0 ${width} ${height}`);svg.setAttribute('role','img');svg.setAttribute('aria-label',`${year} release height versus release side with inferred arm lines for ${points.length} pitchers`);
  function shape(tag,attrs,text){const n=document.createElementNS(ns,tag);for(const [k,v] of Object.entries(attrs))n.setAttribute(k,String(v));if(text!==undefined)n.textContent=text;return n;}
  for(let x=Math.ceil(-xmax);x<=xmax;x++){svg.append(shape('line',{x1:X(x),x2:X(x),y1:Y(ymin),y2:Y(ymax),class:'arm-grid',...(x===0?{stroke:'#9ba59f','stroke-width':2}:{})}),shape('text',{x:X(x),y:Y(ymin)+25,'text-anchor':'middle',class:'arm-axis'},String(x)));}
  for(let y=ymin;y<=ymax+.001;y+=.5)svg.append(shape('line',{x1:X(-xmax),x2:X(xmax),y1:Y(y),y2:Y(y),class:'arm-grid'}),shape('text',{x:65,y:Y(y)+4,'text-anchor':'end',class:'arm-axis'},y.toFixed(1)));
  svg.append(shape('text',{x:X(0),y:height-15,'text-anchor':'middle',class:'arm-axis'},'Release side (ft) · LHP left / RHP right'),shape('text',{x:20,y:height/2,transform:`rotate(-90 20 ${height/2})`,'text-anchor':'middle',class:'arm-axis'},'Release height (ft)'));
  // Draw all arm segments before points and names so every release endpoint stays visible.
  points.forEach(({p,point:d})=>{
   const dash=p.status==='Offered'?'9 6':p.status==='Committed'?'3 5':'';
   const attrs={x1:X(0),y1:Y(d.anchorY),x2:X(d.x),y2:Y(d.y),'stroke-dasharray':dash,'stroke-linecap':'round'};
   svg.append(shape('line',{...attrs,stroke:'#56665c','stroke-width':5,opacity:.55}),shape('line',{...attrs,stroke:colors[d.classYear-1],'stroke-width':3}));
  });
  const nameLocations=[];
  points.forEach(({p,point:d},i)=>{
   const group=shape('g',{class:'arm-point',tabindex:0,role:'button','aria-label':`Edit ${p.first} ${p.last} pitcher measurements`});
   group.append(shape('title',{},`${p.first} ${p.last} · ${CLASSES[d.classYear-1]} · ${p.status}\nRelease: ${d.y.toFixed(2)} ft high, ${d.x.toFixed(2)} ft side\nHeight: ${p.heightIn} in · Inferred angle: ${d.angle.toFixed(1)}°\n${p.releaseSource||'Profile measurements'}`));
   group.append(shape('circle',{cx:X(d.x),cy:Y(d.y),r:6,fill:colors[d.classYear-1],stroke:'#34473c','stroke-width':1.5}));
   let lx=X(d.x),ly=Y(d.y)-13;
   while(nameLocations.some(v=>Math.abs(v.x-lx)<120&&Math.abs(v.y-ly)<17))ly-=18;
   nameLocations.push({x:lx,y:ly});
   if(ly<Y(d.y)-15)group.append(shape('line',{x1:X(d.x),y1:Y(d.y)-7,x2:lx,y2:ly+4,stroke:'#a1aaa4','stroke-width':1}));
   group.append(shape('text',{x:lx,y:ly,'text-anchor':d.x<0?'end':'start',class:'arm-name'},`${p.first} ${p.last}`));
   group.onclick=()=>profileForm(p);group.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();profileForm(p);}};svg.append(group);
  });
  const scroll=el('div','arm-scroll');scroll.append(svg);section.append(scroll);
 }else section.append(el('p','muted','Add height, release height, release side, and throwing hand to a pitcher profile to include them here.'));
 const actions=el('div','arm-profile-actions'),select=el('select');select.setAttribute('aria-label','Pitcher profile to edit');for(const p of pitchers){const o=el('option','',`${p.first} ${p.last}`);o.value=p.id;select.append(o);}const edit=button('Edit pitcher profile',()=>{const p=all().find(p=>p.id===select.value);if(p)profileForm(p);});edit.disabled=!pitchers.length;actions.append(select,edit);section.append(actions,el('p','muted small','Arm angle is inferred from the line between (0, height − 10 inches) and the release point, measured from horizontal. It is a planning estimate. Imported defaults use Fall 2026 mean release measurements and the existing pitcher-height profiles.'));
 return section;
}

function budgetVisualization(){
 const section=el('section','budget-visualization');section.append(el('h2','',`${year} Rev Share by position`));
 const summary=M.budgetSummary(state.funding,all(),year),groups=M.budgetBreakdown(state.funding,all(),year,chart());
 const available=Math.max(0,summary.remaining||0);groups.push({name:'Available',value:available});
 const total=Math.max(summary.used,summary.budget||0),colors=['#31575b','#6b8c91','#9d3549','#c3a25c','#789054','#786587','#dfe3e0','#999'];
 const layout=el('div','pie-layout'),svg=document.createElementNS('http://www.w3.org/2000/svg','svg');svg.setAttribute('viewBox','0 0 500 500');svg.setAttribute('role','img');svg.setAttribute('aria-label',`${year} budget by position. ${groups.map(g=>`${g.name}: ${money(g.value)}`).join('. ')}`);
 function shape(tag,attrs){const node=document.createElementNS('http://www.w3.org/2000/svg',tag);for(const [k,v] of Object.entries(attrs))node.setAttribute(k,String(v));return node;}
 let angle=-Math.PI/2;const pieLabels=[];
 if(!total)svg.append(shape('circle',{cx:250,cy:250,r:215,fill:'#e3e7e3'}));
 groups.forEach((g,i)=>{if(!g.value||!total)return;const end=angle+g.value/total*Math.PI*2;let slice;if(g.value===total)slice=shape('circle',{cx:250,cy:250,r:215,fill:colors[i]});else slice=shape('path',{d:`M250 250 L${250+215*Math.cos(angle)} ${250+215*Math.sin(angle)} A215 215 0 ${end-angle>Math.PI?1:0} 1 ${250+215*Math.cos(end)} ${250+215*Math.sin(end)} Z`,fill:colors[i],stroke:'#fff','stroke-width':2});const title=shape('title',{});title.textContent=`${g.name}: ${money(g.value)} (${(g.value/total*100).toFixed(1)}%)`;slice.append(title);svg.append(slice);pieLabels.push({g,i,angle:(angle+end)/2});angle=end;});
 const shortNames={'Middle infielders':'Middle INF','Corner infielders':'Corner INF','Outfielders':'Outfield','Catchers':'Catchers'};
 const smallLabelLocations=[];let pieMinY=0,pieMaxY=500;
 for(const {g,i,angle:mid} of pieLabels){
  const small=g.value/total<0.07,r=small?243:141,x=250+r*Math.cos(mid);let y=250+r*Math.sin(mid);
  if(small){while(smallLabelLocations.some(p=>Math.abs(p.x-x)<110&&Math.abs(p.y-y)<35))y-=36;smallLabelLocations.push({x,y});pieMinY=Math.min(pieMinY,y-25);pieMaxY=Math.max(pieMaxY,y+25);}
  if(small)svg.append(shape('line',{x1:250+205*Math.cos(mid),y1:250+205*Math.sin(mid),x2:x,y2:y,stroke:'#56605b','stroke-width':1}));
  const label=shape('text',{x,y:y-8,'text-anchor':'middle',fill:small||i===3||i===4||i>=6?'#26332d':'#fff','font-size':small?10:12,'font-weight':600,class:'pie-direct-label'});
  for(const [line,text] of [(shortNames[g.name]||g.name),`${M.compactMoney(g.value)} · ${(g.value/total*100).toFixed(1)}%`].entries()){const span=shape('tspan',{x,dy:line===0?0:15});span.textContent=text;label.append(span);}svg.append(label);
 }
 svg.setAttribute('viewBox',`-35 ${pieMinY} 570 ${pieMaxY-pieMinY}`);
 const legend=el('div','pie-legend');groups.forEach((g,i)=>{const row=el('div','pie-legend-row'),swatch=el('span','pie-swatch');swatch.style.background=colors[i];row.append(swatch,el('span','',g.name),el('strong','',money(g.value)),el('span','muted',total?`${(g.value/total*100).toFixed(1)}%`:'0%'));legend.append(row);});
 layout.append(svg,legend);section.append(layout,el('p','muted',summary.budget===null?'Set a total budget in Scholarships and NIL to show the full budget and available room.':summary.remaining<0?`Allocations exceed the budget by ${money(-summary.remaining)}. The pie represents total allocations; there is no available slice.`:`Full budget: ${money(summary.budget)} · Allocated: ${money(summary.used)} · Available: ${money(available)}`),el('p','muted small','Each eligible player counts once. Pitchers use throwing hand; field starters use their assigned position; remaining hitters use their first listed position. Two-way players assigned to the pitching staff count as pitchers.'));
 return section;
}

function render(){
 const root=document.getElementById('studio');root.replaceChildren();
 const header=el('header');const brand=el('div');brand.append(el('div','eyebrow','TEXAS STATE BASEBALL'),el('h1','','Rosters & Recruiting'));const actions=el('div','actions');actions.append(el('span','save-status',pending?'Saving…':dirty?'Unsaved changes':'All changes saved'),button('Save changes',save,'primary'));header.append(brand,actions);root.append(header);
 const nav=el('nav','years');for(let y=currentYear;y<currentYear+5;y++)nav.append(button(String(y),()=>{year=y;selected=null;if(!['finance','roster'].includes(page))page='field';render();},year===y&&page!=='recruits'?'active':''));root.append(nav);
 if(currentYear>2027){const history=el('select','history-select');history.setAttribute('aria-label','Past season');history.append(el('option','','Past seasons…'));for(let y=2027;y<currentYear;y++){const o=el('option','',String(y));o.value=y;history.append(o);}history.onchange=()=>{if(Number(history.value)){year=Number(history.value);page='field';selected=null;render();}};root.append(history);}
 const legend=el('div','legend');CLASSES.forEach((c,i)=>legend.append(el('span',`legend-chip class-${i+1}`,c)));legend.append(el('span','legend-chip name-left','Left: red'),el('span','legend-chip name-switch','Switch hitter: green'));legend.append(el('span','legend-chip status-offered','Offered'),el('span','legend-chip status-committed','Committed'));root.append(legend);
 const navigation=el('nav','roster-navigation');navigation.setAttribute('aria-label','Roster tools');navigation.append(button('View Recruiting Classes',()=>{page='recruits';render();},'primary'),button('Scholarships and NIL',()=>{page='finance';selected=null;render();},'primary'),button('Manage Roster',()=>{page='roster';render();}));root.append(navigation);
 if(page==='field'){const heading=el('div','section-heading');heading.append(el('h2','',`${year} depth chart`),el('span','muted',`${active().length} projected players · Five-year planning model`));root.append(heading,el('p','intro','DH excludes field starters. Saving carries this season’s roles and depth order forward until eligibility ends.'));root.append(rosterSummary(year));fieldView(root);}else if(page==='finance')financeView(root);else if(page==='roster')rosterView(root);else recruitsView(root);
 const nextView=`${page}:${year}:${signingYear}`;if(viewKey!==nextView){window.scrollTo(0,0);viewKey=nextView;}
}
function save(){if(pending)return;const invalid=document.querySelector('#studio input:invalid');if(invalid){invalid.reportValidity();return;}if(page==='field'){state.planEdits[mode][year]=true;carryForward();battingOrder();}pending=true;savedVersion=version;render();Shiny.setInputValue('save_state',JSON.parse(JSON.stringify(state)),{priority:'event'});}
window.studioBoot=data=>{roster=data.roster.map(p=>({...p,positions:Array.isArray(p.positions)?p.positions:[p.positions]}));state=data.state;state.profiles=Array.isArray(state.profiles)?state.profiles:[];state.charts=state.charts&&!Array.isArray(state.charts)?state.charts:{};state.funding??={};state.funding.budgets=state.funding.budgets&&!Array.isArray(state.funding.budgets)?state.funding.budgets:{};state.funding.awards=state.funding.awards&&!Array.isArray(state.funding.awards)?state.funding.awards:{};currentYear=data.currentYear;year=currentYear;signingYear=currentYear;initializePlans();render();};
window.studioSaved=data=>{pending=false;if(data.ok){state.revision=data.revision;dirty=version!==savedVersion;}else alert(`Save failed: ${data.message}`);render();};
window.addEventListener('beforeunload',e=>{if(dirty){e.preventDefault();e.returnValue='';}});
window.DepthModel={grade,eligible};
})();
