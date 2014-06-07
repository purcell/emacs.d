$(document).ready(function (){
  var updateUI=function(){
    var v=$('#filter').val().replace(/^\s+|\s+$/g, ''),n,d;
    for (var i=1, len=dic.length; i<=len; i++) {
      n=$('#n'+i);
      d=$('#d'+i);
      if(v===''){
        n.show();
        d.show();
        continue;
      }
      if(dic[i-1].indexOf(v)!==-1){
        n.show();
        d.show();
        continue;
      }
      n.hide();
      d.hide();
    }
  };
  $('#filter').keyup(updateUI);
  $('#reset').click(function(){
    $('#filter').val('');
    updateUI();
  });
});
// Local Variables:
// coding: utf-8
// indent-tabs-mode: nil
// mode: js2-mode
// tab-width: 2
// js2-basic-offset: 2
// End:
// vim: set fs=javascript fenc=utf-8 et ts=2 sts=2 sw=2// vim: set fs=javascript fenc=utf-8 et ts=2 sts=2 sw=2