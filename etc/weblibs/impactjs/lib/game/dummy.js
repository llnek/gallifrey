
ig.module('game.dummy').requires(

'impact.animation',
'impact.background-map',
'impact.collision-map',
'impact.entity',
'impact.entity-pool',
'impact.font',
'impact.game',
'impact.image',
'impact.impact',
'impact.input',
'impact.loader',
'impact.map',
'impact.sound',
'impact.system',
'impact.timer'
//'impact.debug.debug',
//'impact.debug.entities-panel',
//'impact.debug.graph-panel',
//'impact.debug.maps-panel',
//'impact.debug.menu'
).defines(function() {
  if (console && console.log) { console.log('[impactjs] heeloo'); }
});
