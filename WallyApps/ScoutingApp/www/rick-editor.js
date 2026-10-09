(() => {
  'use strict';
  const NS = 'http://www.w3.org/2000/svg';
  let doc = {key:null, notes:'', shapes:[]}, selected=null, drag=null;
  const el = id => document.getElementById(id);
  const svgEl = (tag, attrs={}) => {const n=document.createElementNS(NS,tag); for(const [k,v] of Object.entries(attrs)) n.setAttribute(k,v); return n;};
  const center = zone => zone === 'Pre2k' ? 150 : 450;
  const color = c => c === 'red' ? '#cf202f' : '#18833c';
  function send(id='rick_editor_change') {
    if(doc.key && window.Shiny) Shiny.setInputValue(id, {...doc, nonce:Date.now()}, {priority:'event'});
  }
  function point(e) { const p=el('rick_canvas').createSVGPoint(); p.x=e.clientX;p.y=e.clientY; return p.matrixTransform(el('rick_canvas').getScreenCTM().inverse()); }
  function clamp(s) {
    s.w=Math.max(.15,Math.min(2.4,s.w));s.h=Math.max(.15,Math.min(4,s.h));
    if(s.type!=='rectangle') s.h=s.w=Math.min(s.w,s.h);
    s.x=Math.max(-1.2+s.w/2,Math.min(1.2-s.w/2,s.x));
    s.z=Math.max(s.h/2,Math.min(4-s.h/2,s.z));
    return s;
  }
  function paint() {
    const svg=el('rick_canvas'); if(!svg) return;
    svg.replaceChildren();
    for(const zone of ['Pre2k','2k']) {
      const cx=center(zone), text=svgEl('text',{x:cx,y:24,'text-anchor':'middle','font-weight':'bold'}); text.textContent=zone;svg.append(text);
      svg.append(svgEl('rect',{x:cx-108,y:50,width:216,height:360,fill:'#fafafa',stroke:'#ddd'}));
      svg.append(svgEl('rect',{x:cx-.708*90,y:410-3.5*90,width:1.416*90,height:180,fill:'white',stroke:'#333','stroke-width':2}));
      svg.append(svgEl('path',{d:`M ${cx-63.9} 365 L ${cx+63.9} 365 L ${cx+63.9} 378.5 L ${cx} 396.5 L ${cx-63.9} 378.5 Z`,fill:'white',stroke:'#333'}));
    }
    for(const s of doc.shapes) {
      const cx=center(s.zone)-s.x*90,cy=410-s.z*90,w=s.w*90,h=s.h*90;
      const g=svgEl('g',{'data-id':s.id});
      const attrs={fill:color(s.color),'fill-opacity':.27,stroke:color(s.color),'stroke-width':2,cursor:'move'};
      g.append(s.type==='circle' ? svgEl('ellipse',{...attrs,cx,cy,rx:w/2,ry:h/2}) : svgEl('rect',{...attrs,x:cx-w/2,y:cy-h/2,width:w,height:h}));
      // A visible corner grip lets coaches resize every shape directly.
      g.append(svgEl('rect',{x:cx+w/2-5,y:cy+h/2-5,width:10,height:10,fill:'white',stroke:color(s.color),'stroke-width':2,'data-resize':'true',cursor:'nwse-resize'}));
      if(selected===s.id) g.append(svgEl('rect',{x:cx-w/2-3,y:cy-h/2-3,width:w+6,height:h+6,fill:'none',stroke:'#444','stroke-dasharray':'3 3','pointer-events':'none'}));
      svg.append(g);
    }
  }
  function add(type,zone,p) {
    if(!doc.key) return;
    const s=clamp({id:'s'+Date.now()+Math.random().toString(36).slice(2),type,zone,color:'green',x:p ? (center(zone)-p.x)/90 : 0,z:p ? (410-p.y)/90 : 2.5,w:1.416,h:type==='rectangle'?.75:1.416});
    doc.shapes.push(s); selected=s.id;paint();send();
  }
  function init() {
    const svg=el('rick_canvas');if(!svg || svg.dataset.ready) return;svg.dataset.ready='1';
    document.querySelectorAll('[data-rick-shape]').forEach(button=>{
      button.addEventListener('click',()=>add(button.dataset.rickShape,el('rick_target').value));
      button.addEventListener('dragstart',e=>e.dataTransfer.setData('text/plain',button.dataset.rickShape));
    });
    svg.addEventListener('dragover',e=>e.preventDefault());
    svg.addEventListener('drop',e=>{e.preventDefault();const type=e.dataTransfer.getData('text/plain');if(!['circle','square','rectangle'].includes(type)) return;const p=point(e);add(type,p.x<300?'Pre2k':'2k',p);});
    svg.addEventListener('pointerdown',e=>{
      const g=e.target.closest('[data-id]');if(!g || !doc.key)return;
      e.preventDefault();const s=doc.shapes.find(s=>s.id===g.dataset.id);selected=s.id;
      drag={id:s.id,start:point(e),original:{...s},resize:e.target.hasAttribute('data-resize'),moved:false};
      svg.setPointerCapture(e.pointerId);paint();
    });
    svg.addEventListener('pointermove',e=>{
      if(!drag)return;const p=point(e),dx=p.x-drag.start.x,dy=p.y-drag.start.y;
      if(Math.hypot(dx,dy)>3)drag.moved=true;
      const s=doc.shapes.find(s=>s.id===drag.id),o=drag.original;
      if(drag.resize){
        // Resize from the lower-right corner; preserve the opposite corner.
        let w=Math.max(.15,Math.min(2.4,o.w+dx/90)),h=Math.max(.15,Math.min(4,o.h+dy/90));
        if(s.type!=='rectangle')w=h=Math.min(w,h);
        s.w=w;s.h=h;s.x=o.x-(w-o.w)/2;s.z=o.z-(h-o.h)/2;
      } else {s.x=o.x-dx/90;s.z=o.z-dy/90;}
      clamp(s);paint();
    });
    function finish(e,cancel=false){if(!drag)return;const s=doc.shapes.find(s=>s.id===drag.id);if(cancel)Object.assign(s,drag.original);else if(!drag.moved&&!drag.resize)s.color=s.color==='green'?'red':'green';drag=null;paint();send();}
    svg.addEventListener('pointerup',e=>finish(e));svg.addEventListener('pointercancel',e=>finish(e,true));
    el('rick_clear').addEventListener('click',()=>{doc.shapes=[];selected=null;paint();send();});
    el('rick_editor_notes').addEventListener('input',e=>{doc.notes=e.target.value;send();});
    el('rick_save').addEventListener('click',()=>send('rick_editor_save'));
    paint();
  }
  function register(){
    init();if(!window.Shiny)return;
    Shiny.addCustomMessageHandler('rick_document',message=>{init();doc={key:message.key,notes:message.notes||'',shapes:message.shapes||[]};selected=null;drag=null;el('rick_editor_notes').value=doc.notes;el('rick_editor_panel').style.borderColor=message.side==='L'?'#cf202f':'#111';paint();});
  }
  if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',register);else register();
})();
