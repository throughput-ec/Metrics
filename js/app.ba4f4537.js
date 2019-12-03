(function(t){function e(e){for(var n,i,s=e[0],l=e[1],c=e[2],d=0,p=[];d<s.length;d++)i=s[d],Object.prototype.hasOwnProperty.call(o,i)&&o[i]&&p.push(o[i][0]),o[i]=0;for(n in l)Object.prototype.hasOwnProperty.call(l,n)&&(t[n]=l[n]);u&&u(e);while(p.length)p.shift()();return a.push.apply(a,c||[]),r()}function r(){for(var t,e=0;e<a.length;e++){for(var r=a[e],n=!0,s=1;s<r.length;s++){var l=r[s];0!==o[l]&&(n=!1)}n&&(a.splice(e--,1),t=i(i.s=r[0]))}return t}var n={},o={app:0},a=[];function i(e){if(n[e])return n[e].exports;var r=n[e]={i:e,l:!1,exports:{}};return t[e].call(r.exports,r,r.exports,i),r.l=!0,r.exports}i.m=t,i.c=n,i.d=function(t,e,r){i.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:r})},i.r=function(t){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},i.t=function(t,e){if(1&e&&(t=i(t)),8&e)return t;if(4&e&&"object"===typeof t&&t&&t.__esModule)return t;var r=Object.create(null);if(i.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var n in t)i.d(r,n,function(e){return t[e]}.bind(null,n));return r},i.n=function(t){var e=t&&t.__esModule?function(){return t["default"]}:function(){return t};return i.d(e,"a",e),e},i.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},i.p="/Metrics/";var s=window["webpackJsonp"]=window["webpackJsonp"]||[],l=s.push.bind(s);s.push=e,s=s.slice();for(var c=0;c<s.length;c++)e(s[c]);var u=l;a.push([0,"chunk-vendors"]),r()})({0:function(t,e,r){t.exports=r("56d7")},2309:function(t,e,r){},"56d7":function(t,e,r){"use strict";r.r(e);r("cadf"),r("551c"),r("f751"),r("097d");var n=r("2b0e"),o=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",{staticClass:"mainpage",attrs:{id:"app"}},[r("Header"),r("parentPage")],1)},a=[],i=(r("b165"),r("2309"),r("f9e3"),r("2dd8"),function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",{staticClass:"hello"},[r("h1",[t._v("Throughput Metrics")]),t._v("\n  This project is to be evaluated using Project Metrics defined in the\n  Throughput project proposal\n    "),r("b-button",{attrs:{variant:"primary",size:"sm",href:"https://nsf.gov/awardsearch/showAward?AWD_ID=1928366"}},[t._v("\n        NSF-1928366\n    ")]),t._v(".\n\n  "),r("br"),r("met-anno"),r("br"),r("met-collab"),r("br"),r("ec-cook"),r("br"),r("cross-dis")],1)}),s=[],l=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",[r("b-card",{attrs:{title:"Annotation infrastructure"}},[r("b-card",{attrs:{title:"Goal"}},[r("b-card-text",[t._v("\n        Improving the cyberinfrastructure required to maintain and serve an\n        Annotation database for the Earth Sciences.\n      ")])],1),r("b-card",{attrs:{title:"Metrics"}},[r("b-card-text",[r("ul",[r("li",[t._v("Development of a testing suite for the database and fully\n            integrated development/production system.")]),r("li",[t._v("Clear documentation for online collaboration and protocols for\n            bug reporting.")]),r("li",[t._v("Adoption and management of a Project Board system.\n            ["),r("a",{attrs:{href:"https://github.com/orgs/throughput-ec/projects"}},[t._v("Throughput Project Boards")]),t._v("]")]),r("li",[t._v("Public database landing page with key reporting metrics (data\n            volume, query speeds, etc.)")])])])],1)],1)],1)},c=[],u={name:"annotationVue",props:{msg:String}},d=u,p=r("2877"),b=Object(p["a"])(d,l,c,!1,null,null,null),f=b.exports,h=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",[r("b-card",{attrs:{title:"Throughput Collaboration"}},[r("b-card",{attrs:{title:"Core Concepts"}},[r("b-card-text",[t._v("\n        Throughput is, fundamentally, a database that links several core data objects.\n        These objects include:\n        "),r("ul",[r("li",[t._v("Objects")]),r("li",[t._v("Annotations")]),r("li",[t._v("Types")]),r("li",[t._v("Language")]),r("li",[t._v("XXXX")])])])],1),r("b-card",{attrs:{title:"Adding New Resources"}}),r("b-card",{attrs:{title:"Bug Reporting"}}),r("b-card",{attrs:{title:"Goal"}},[r("b-card-text",[t._v("\n        Building user interfaces to support user groups within the earth sciences including Direct Annotation, The Earth Science Cookbook\n      ")]),r("b-card",{attrs:{title:"Metrics"}},[r("b-card-text",[r("ul",[r("li",[t._v("Direct annotation landing page built and tagged in a version control system (GitHub), with Usability report from the UMN Usability Lab.")]),r("li",[t._v("Documentation surrounding the usability report.")]),r("li",[t._v("Earth Science Cookbook UI built and tagged in version control system.")]),r("li",[t._v("Usability report from the Usability Lab.")])])])],1)],1)],1)],1)},g=[],v={name:"collaborationVue",props:{msg:String}},m=v,_=Object(p["a"])(m,h,g,!1,null,null,null),y=_.exports,j=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",[r("b-card",{attrs:{title:"Workflow Discovery"}},[r("b-card",{attrs:{title:"Goal"}},[r("b-card-text",[t._v("\n        Establishing a platform to support the addition of workflow elements\n        to the TAE and to link primary data resources in the geosciences\n        through publicly available code.\n      ")]),r("b-card",{attrs:{title:"Metrics"}},[r("b-card-text",[r("ul",[r("li",[t._v("Number of scripts recovered by programmatic workflows")]),r("li",[t._v("Number of user-submitted scripts")]),r("li",[t._v("Number of data resources connected through scripts")]),r("li",[t._v("Number of users accessing scripts through the system")]),r("li",[t._v("Number of workshops (and number of participants) that use annotation as an element in instruction.")])])])],1)],1)],1)],1)},w=[],k={name:"ECcook",props:{msg:String}},x=k,O=Object(p["a"])(x,j,w,!1,null,null,null),T=O.exports,P=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",[r("b-card",{attrs:{title:"Cross-Disciplinary Access"}},[r("b-card",{attrs:{title:"Goals"}},[r("b-card-text",[t._v("\n        To improve cross-disciplinary research outcomes by linking data across repositories using a range of tools, and vocabularies.\n      ")]),r("b-card",{attrs:{title:"Metrics"}},[r("b-card-text",[t._v("\n        Clear documentation of workflows for GeoDeepDive-based annotation\n        Number of annotations generated by cross-disciplinary linking activity\n        Number of end user workshops held and number of participants\n        Number of data repositories linked through cross-disciplinary annotations.\n        ")])],1)],1)],1)],1)},S=[],E={name:"crossDiscip",props:{msg:String}},M=E,C=Object(p["a"])(M,P,S,!1,null,null,null),D=C.exports,N={name:"parentPage",props:{msg:String},components:{"met-anno":f,"met-collab":y,"ec-cook":T,"cross-dis":D}},A=N,$=Object(p["a"])(A,i,s,!1,null,null,null),G=$.exports,U=function(){var t=this,e=t.$createElement,r=t._self._c||e;return r("div",[r("b-jumbotron",{attrs:{header:"Throughput Metrics",lead:"Resource page for Throughput to provide ongoing assessment of project metrics."}})],1)},B=[],X={name:"header",data:function(){return{msg:"Throughput header"}}},H=X,I=Object(p["a"])(H,U,B,!1,null,null,null),L=I.exports,R={name:"app",components:{parentPage:G,Header:L}},q=R,J=Object(p["a"])(q,o,a,!1,null,null,null),V=J.exports,W=r("5f5b");n["default"].use(W["a"]),n["default"].config.productionTip=!1,new n["default"]({render:function(t){return t(V)}}).$mount("#app")},b165:function(t,e,r){}});
//# sourceMappingURL=app.ba4f4537.js.map