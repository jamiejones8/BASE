(() => {
  'use strict';
  let ready=false, boot;
  const frame = () => document.getElementById('base-roster-frame');
  const send = (type,payload) => frame()?.contentWindow.postMessage({source:'base-roster-host',type,payload},location.origin);
  window.addEventListener('message',event => {
    if (event.origin !== location.origin || event.source !== frame()?.contentWindow || event.data?.source !== 'base-roster-studio') return;
    const {type,payload}=event.data;
    if(type==='ready'){ready=true;if(boot)send('boot',boot);}
    if(type==='input' && payload?.name==='save_state') Shiny.setInputValue('rosters-save_state',payload.value,{priority:'event'});
  });
  $(function(){
    Shiny.addCustomMessageHandler('rosters-boot',data=>{boot=data;if(ready)send('boot',data);});
    Shiny.addCustomMessageHandler('rosters-saved',data=>send('saved',data));
  });
})();
