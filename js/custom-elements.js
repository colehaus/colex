webpackJsonp([1],{1175:function(t,e,a){"use strict";Object.defineProperty(e,"__esModule",{value:!0});a(1176),a(1177),a(241),a(464),a(1179)},1176:function(t,e,a){"use strict";var n=a(31),r=a.n(n),s=a(41),o=(a.n(s),a(103)),i=a(464),c=a(241),l=a(161);const p=Object(s.create)({checkTypes:!1,env:s.env}),d=t=>{const e=r()(t.target);return p.map(t=>{const a=r()(r()(t).children().get(e.index())),n=a.hasClass("open")?{tag:"SELECTEDOPEN"}:a.parent().is(":visible")?{tag:"SELECTEDNEWVISIBLE",from:a.siblings(".open"),to:a,parent:a.parent()}:{tag:"INVISIBLEPARENT",from:a.siblings(".open"),to:a};new Promise((t=>(e,a)=>{switch(t.tag){case"INVISIBLEPARENT":t.from.removeClass("open"),t.to.addClass("open"),e();break;case"SELECTEDOPEN":e();break;case"SELECTEDNEWVISIBLE":const a=t,n=a.parent.height(),r=300,s=()=>Promise.resolve().then(()=>{const t=Object(o.h)(n,a.parent.height());return Object(l.b)(r,e=>{a.parent.css("height",t(e))})}),i=()=>Promise.resolve().then(()=>(a.to.css("opacity",0),a.to.addClass("open"),Object(l.b)(r,t=>{a.to.css("opacity",t)}))),c=()=>{a.parent.removeAttr("style"),a.from.removeAttr("style"),a.to.removeAttr("style")};(()=>Object(l.b)(r,t=>a.from.css("opacity",1-t)).then(()=>{a.from.css("opacity",0),a.from.removeClass("open")}))().then(()=>Promise.all([i(),s()])).then(c).then(e)}})(n)).then(()=>{c.a.setNotes(),c.a.fixNotes()})})(r()(`[data-menu="${e.closest("menu").attr("id")}"]`).toArray()),i.a.defaultHandlers(e),!1};r()(()=>{p.map(t=>r()(t).click(({pageY:t,pageX:e,target:a})=>(i.a.getMenu((t=>HTMLElement=t)(a)).offset({top:t,left:e}).children("ul.menu").children().off().click(d),!1)))(r()('[type="menu"]').toArray()),MathJax.Hub.Queue(()=>{r()(".MathJax_MathContainer").css("display","inline"),r()(".MathJax_MathContainer > span").css("display","inline");const t=r()(".switch.inline > li.open");t.css("display","inline-block"),t.offset(),t.css("display","inline"),t.removeAttr("style")})})},1177:function(t,e,a){"use strict";var n=a(31),r=a.n(n),s=a(41),o=(a.n(s),a(241)),i=a(161),c=a(1178);const l=Object(s.create)({checkTypes:!1,env:s.env}),p=function(){const t=r()(this),e=t.parent(),a="."+e.attr("class").replace(" ","."),[n,s]="swap-down"===t.attr("class")?[e,e.nextAll(a+":first")]:[e.prevAll(a+":first"),e];new Promise(((t,e)=>(a,n)=>{const r=t.nextUntil(e),{bottomToTop:s,between:o,topToBottom:l}=((t,e)=>{const a=t.offset().top-e.offset().top,n=e.height()-t.height();return{bottomToTop:a,between:n,topToBottom:e.offset().top-t.offset().top+n}})(t,e),p=Math.PI/4,d=t=>{const e=t<=0?Math.PI+p:p,{center:a,radius:n}=Object(c.a)(e,e-2*p,{x:0,y:0},{x:0,y:t});return t=>{const{x:r,y:s}=Object(c.b)(a,n,e-2*p*t);return`translate(${r}px, ${-s}px)`}},h=d(l),u=d(s),f=d(o);Promise.all([Object(i.b)(300,e=>t.css("transform",h(e))).then(()=>t.removeAttr("style")),Object(i.b)(300,t=>e.css("transform",u(t))).then(()=>e.removeAttr("style")),Object(i.b)(300,t=>r.css("transform",f(t))).then(()=>r.removeAttr("style"))]).then(a)})(n,s)).then(()=>{d(n,s),o.a.fixNotes(),r()(".swap-up, .swap-down").remove(),h()})},d=(t,e)=>{const a=t.next();a[0]===e[0]?(e.before(t),t.before(e)):(e.before(t),a.before(e))},h=()=>{const t=r()('<span class="swap-up"></span>'),e=r()('<span class="swap-down"></span>'),a=r()(".swap");l.groupBy(l.on(l.equals)(t=>r()(t).attr("class")))(a.toArray()).forEach(a=>{l.pipe([l.tail,l.map(l.map(e=>r()(e).prepend(t.clone())))])(a),l.pipe([l.init,l.map(l.map(t=>r()(t).append(e.clone())))])(a)}),r()(".swap-up, .swap-down").click(p)};r()(h)},1178:function(t,e,a){"use strict";a.d(e,"a",function(){return n}),a.d(e,"b",function(){return r});const n=(t,e,a,n)=>{const s=((t,e)=>Math.hypot(t.x-e.x,t.y-e.y))(a,n)/Math.sin(Math.abs(e-t)/2)/2;return{radius:s,center:r(a,s,t+Math.PI)}},r=({x:t,y:e},a,n)=>({x:t+Math.cos(n)*a,y:e+Math.sin(n)*a})},1179:function(t,e,a){"use strict";var n=a(31),r=a.n(n),s=a(103),o=a(1180),i=a.n(o),c=a(41),l=(a.n(c),a(1181)),p=a(161);const d=Object(c.create)({checkTypes:!1,env:c.env}),h=[l.f(l.e),l.f(l.a),l.f(l.b),l.f(l.c),l.f(l.d),l.f(l.g)],u=t=>{const{target:e,source:a}=t,n=e.x-a.x,r=e.y-a.y,s=Math.hypot(n,r);return`M${a.x},${a.y}A${s},${s} 0 0,1 ${e.x},${e.y}`},f=t=>t,m=t=>t,g=t=>e=>{const a=t.findIndex(t=>t.type===e);if(null==a)throw Error(`Couldn't find node type ${e} in ${t.toString()}`);return h[a](13)},y=(t,e,a)=>(t=>Promise.all([s.a(t+"/nodes.csv"),s.a(t+"/links.csv"),s.a(t+"/node-types.csv"),s.a(t+"/link-types.csv")]))(e).then(([e,n,o,i])=>{const c={width:r()(a).width(),height:r()(a).height()},l=s.l(a).append("svg").attr("id",t).attr("width",c.width).attr("height",c.height),h=(({width:t,height:e})=>s.g().force("link",s.e().id(t=>t.id).strength(1).distance(t/5)).force("charge",s.f().strength(-250)).force("center",s.d(t/2,e/2)).alphaDecay(.015))(c);((t,e,a)=>{const n=Object(p.c)(a.map(t=>t.type)),r=e.append("defs");r.append("g").selectAll("marker").data(n).enter().append("marker").attr("id",e=>`${t}-marker-${e}`).attr("viewBox","0 -5 10 10").attr("refX",15).attr("refY",-1.5).attr("markerWidth",6).attr("markerHeight",6).attr("orient","auto").append("path").attr("d","M0,-5L10,0L0,5"),r.append("g").selectAll("linearGradient").data(n).enter().append("linearGradient").attr("id",e=>`${t}-gradient-${e}`).append("stop")})(t,l,i);const y=((t,e,a)=>e.append("g").attr("class","links").selectAll("path").data(d.map(m)(a)).enter().append("path").attr("class",t=>t.type).classed("link",!0).attr("marker-end",e=>`url(#${t}-marker-${e.type})`).attr("stroke",e=>`url(#${t}-gradient-${e.type})`))(t,l,n),I=((t,e,a,n)=>{const r=t.append("g").classed("nodes",!0).selectAll("g.node").data(d.map(f)(e)).enter().append("g").classed("node",!0).on("dblclick",M).call(s.b().on("start",v(n)).on("drag",b).on("end",w(n)));return r.append("polygon").attr("class",t=>t.type).attr("points",t=>g(a)(t.type)),x(t=>t.label,18)(r.append("a").attr("xlink:href",t=>"#"+t.id).append("text")),r})(l,e,o,h);return h.nodes(e).on("tick",((t,e)=>()=>{t.attr("d",u),e.attr("transform",t=>`translate(${t.x}, ${t.y})`)})(y,I)),h.force("link").links(n),E(t,l,o,i),h}),v=t=>e=>{t.alphaTarget(.3).restart(),e.fx=e.x,e.fy=e.y},b=t=>{t.fx=s.c.x,t.fy=s.c.y},M=function(t){s.l(this).select("polygon").classed("fixed",!1),t.fx=null,t.fy=null},w=function(t){return function(){s.l(this).select("polygon").classed("fixed",!0),t.alphaTarget(0)}},E=(t,e,a,n)=>{const r=e.append("g").attr("transform","translate(80, 50)").selectAll(".legend").data(a).enter().append("g").classed("legend",!0).attr("transform",(t,e)=>"translate(0, "+30*e+")");r.append("polygon").attr("points",t=>g(a)(t.type)),x(t=>t.label,18)(r.append("text"));const s=e.append("g").attr("transform","translate(80, 180)").selectAll(".legend").data(d.map(m)(n)).enter().append("g").classed("legend",!0).attr("transform",(t,e)=>"translate(0, "+30*e+")");s.append("path").attr("d",u({source:{x:-13,y:13},target:{x:13,y:-13}})).attr("class",t=>t.type).attr("stroke",e=>`url(#${t}-gradient-${e.type}`).classed("link",!0),x(t=>t.label,18)(s.append("text"))},x=(t,e)=>a=>a.selectAll("tspan").data(e=>i()(t(e),20)).enter().append("tspan").attr("y",(t,e)=>.5+1.6*e+"ex").attr("x",e).text(t=>t),I=t=>()=>{r()("#arg-map a").removeAttr("style"),r()("#underlay").removeClass("inactive"),r()("#overlay").addClass("inactive"),history.replaceState(null,"",window.location.pathname),t.stop()},k=t=>e=>{if(r()("#underlay").addClass("inactive"),r()("#overlay").removeClass("inactive"),r()("#arg-map a").click(I(t)),r()("#arg-map > svg, #overlay").click(function(e){e.target===this&&I(t)()}),null!=e){const t=r()(e.currentTarget).attr("id");null!=t&&(r()(".node > a").removeClass("entry-point"),r()(".node > a").filter((e,a)=>a.href.baseVal==="#"+t).addClass("entry-point"))}},P={};p.a.then(()=>{r()(window).resize(()=>{r()("#arg-map svg").width(r()("#arg-map").width()),r()("#arg-map svg").height(r()("#arg-map").height())}),(()=>r()("a.arg-map").click(t=>{const e=r()(t.currentTarget).attr("href").slice(1);r()("#overlay svg").hide(),null==P[e]?y(e,r()(`a[href="#${e}"][data-data-src]`).data("data-src"),"#overlay #arg-map").then(a=>{P[e]=a,k(a)(t)}):(r()("#"+e).show(),k(P[e])(t),P[e].restart())}))(),(()=>{if(location.hash.endsWith("-map")){const t=location.hash.slice(1);y(t,r()(`a[href="#${t}"]`).data("data-src"),"#overlay #arg-map").then(e=>{P[t]=e,k(e)()})}})()})},1180:function(t,e){function a(t,e,a){var r=t.split(/\s/),s=[],o="";if(a=n(a),e<2)throw new Error("width must be at least 2 characters");return r.forEach(function(t){var n;if(o.length?o.length+1+t.length<=e?o+=" "+t:!a.splitWords||~a.preservedWords.indexOf(t)?(o.length&&s.push(o),o=t):o.length+5<=e&&t.length>5?(n=(n=e-o.length-2)>t.length-3?t.length-3:n,o+=" "+t.substring(0,n)+"-",s.push(o),o=t.substring(n)):(s.push(o),o=t):o=t,a.splitWords&&!~a.preservedWords.indexOf(t))for(;o.length>e;)s.push(o.substring(0,e-1)+"-"),o=o.substring(e-1)}),s.push(o),s}function n(t){var e={splitWords:!0,preservedWords:[]};if("object"!=typeof t&&void 0!==t)throw new Error("options must be an object");if("splitWords"in(t=t||{})&&(e.splitWords=!!t.splitWords),"preservedWords"in t){if(!Array.isArray(t.preservedWords))throw new Error("options.preservedWords must be an array");t.preservedWords.forEach(function(t){if("string"!=typeof t)throw new Error("options.preservedWords entries must be strings");e.preservedWords.push(t)})}return e}t.exports=a,a.normalizeOptions=n},1181:function(t,e,a){"use strict";a.d(e,"f",function(){return s}),a.d(e,"g",function(){return o}),a.d(e,"e",function(){return i}),a.d(e,"b",function(){return c}),a.d(e,"d",function(){return l}),a.d(e,"c",function(){return p}),a.d(e,"a",function(){return d});var n=a(41);a.n(n);const r=Object(n.create)({checkTypes:!1,env:n.env}),s=t=>e=>r.pipe([r.map(t=>Math.cos(t)*e+" "+Math.sin(t)*e),r.joinWith(", ")])(t),o=[.5*Math.PI,7/6*Math.PI,11/6*Math.PI],i=[.25*Math.PI,.75*Math.PI,5/4*Math.PI,7/4*Math.PI],c=[0*Math.PI,.5*Math.PI,1*Math.PI,1.5*Math.PI],l=[.3*Math.PI,.7*Math.PI,1.1*Math.PI,1.5*Math.PI,1.9*Math.PI],p=[1/6*Math.PI,.5*Math.PI,5/6*Math.PI,7/6*Math.PI,1.5*Math.PI,11/6*Math.PI],d=r.map(t=>2*Math.PI/16*t)(r.range(0,16))},241:function(t,e,a){"use strict";var n=a(31),r=a.n(n),s=a(41);a.n(s);const o=Object(s.create)({checkTypes:!1,env:s.env}),i=t=>o.pipe([o.toMaybe,o.map(t=>r()("#"+t.slice(1))),o.maybeToNullable])(t.find("a").last().attr("href")),c=()=>{o.reduce(t=>e=>{const a=r()(e);return a.offset((e,{top:n,left:r})=>o.pipe([o.toMaybe,o.maybe_(()=>(t=n+a.outerHeight(!0),{top:n,left:r}))(e=>{const n=o.max(e.prev().offset().top)(t);return t=n+a.outerHeight(!0),{top:n,left:r}})])(i(a))),t})(0)(r()(".sidenote").toArray())},l=()=>{r()(".footnotes").hide(),r()("details").each((t,e)=>{p.observe(e,{attributes:!0})}),r()(".sidenote").not("#warnings").remove(),r()(".footnotes > ol > li").each((t,e)=>{(t=>{const e=i(t);if(null!=e){const a=e.prev();if(a.is(":visible")){const e=a.closest("p");(0===e.length?a:e).before('<aside class="sidenote">'+r()(t).html()+"</aside>")}}})(r()(e))}),r()(".noted").next().hide(),r()(".sidenote").each((t,e)=>{r()(e).find("a").last().hide()})},p=new MutationObserver(c),d=()=>{r()(window).width()/parseFloat(r()("html").css("font-size"))>60?(l(),document.fonts.ready.then(c),MathJax.Hub.Queue(()=>{c()})):(p.disconnect(),r()(".sidenote").not("#warnings").remove(),r()(".noted").next().show(),r()(".footnotes").show())};r()(()=>{d(),r()(window).resize(d)}),e.a={setNotes:l,fixNotes:c}},464:function(t,e,a){"use strict";var n=a(31),r=a.n(n),s=a(41);a.n(s);const o=Object(s.create)({checkTypes:!1,env:s.env}),i=t=>r()("#"+r()(t).closest('[type="menu"]').attr("data-menu")),c=t=>{switch(t.tag){case"HR":return r()("<hr/>");case"MENUITEM":const e=r()("<li/>").text(t.label).attr("type",t.type);return t.active?e.attr("checked","checked"):e.removeAttr("checked"),e;default:throw Error(t.toString())}},l=t=>{switch(t.tag){case"CLOSED":return null;case"OPEN":return r()('<ul class="menu"></ul>').append(o.map(c)(t.items))}},p=t=>HTMLElement=t,d=t=>t.data("active")?{tag:"CLOSED"}:{tag:"OPEN",items:o.map(t=>(t=>{switch(t.get(0).nodeName){case"HR":return{tag:"HR"};case"MENUITEM":return{tag:"MENUITEM",label:t.attr("label"),type:t.attr("type"),active:"checked"===t.attr("checked")};default:throw Error(t.toString())}})(r()(t)))(t.children("menuitem, hr").toArray())},h=t=>{switch(t.tag){case"MENUCLICK":o.pipe([d,l,o.toMaybe,o.maybe_(()=>{r()("ul.menu").remove(),t.menu.data("active",!1)})(e=>{t.menu.append(e),t.menu.data("active",!0)})])(t.menu);break;case"ITEMSELECT":r()("ul.menu").remove(),t.menu.data("active",!1),o.map(t=>r()(t).removeAttr("checked"))(t.menu.children().toArray()),t.item.attr("checked","checked")}},u=t=>{if(["TEXTAREA","BUTTON","INPUT","OPTION","SELECT","DETAILS","SUMMARY","A"].some(e=>p(t.target).nodeName===e))return;const e=i(p(t.target)).filter((t,e)=>(t=>"MENU"===r()(t).get(0).nodeName&&"popup"===r()(t).attr("type"))(e));null!=e&&h({tag:"MENUCLICK",menu:r()(e)})};r()(()=>{r()('[type="menu"]').each((t,e)=>{r()(e).click(u)})}),e.a={getMenu:i,defaultHandlers:t=>{if("radio"===t.attr("type")){const e=t.closest("menu"),a=e.find(`[label="${t.text()}"]`);h({tag:"ITEMSELECT",menu:e,item:a})}}}}},[1175]);
//# sourceMappingURL=custom-elements.js.map