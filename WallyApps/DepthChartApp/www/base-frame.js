(() => {
  'use strict';
  const send = (type, payload) => parent.postMessage({source:'base-roster-studio',type,payload},location.origin);
  window.Shiny = {setInputValue:(name,value) => send('input',{name,value})};
  window.addEventListener('message', event => {
    if (event.source !== parent || event.origin !== location.origin || event.data?.source !== 'base-roster-host') return;
    if (event.data.type === 'boot') window.studioBoot(event.data.payload);
    if (event.data.type === 'saved') window.studioSaved(event.data.payload);
  });
  send('ready');
})();
