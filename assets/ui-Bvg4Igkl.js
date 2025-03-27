import{r as s,j as v,R as w,a as Ce}from"./vendor-DrbvkEHX.js";function K(e,t){if(typeof e=="function")return e(t);e!=null&&(e.current=t)}function W(...e){return t=>{let o=!1;const n=e.map(r=>{const c=K(r,t);return!o&&typeof c=="function"&&(o=!0),c});if(o)return()=>{for(let r=0;r<n.length;r++){const c=n[r];typeof c=="function"?c():K(e[r],null)}}}}function E(...e){return s.useCallback(W(...e),e)}var P=s.forwardRef((e,t)=>{const{children:o,...n}=e,r=s.Children.toArray(o),c=r.find(be);if(c){const a=c.props.children,u=r.map(l=>l===c?s.Children.count(a)>1?s.Children.only(null):s.isValidElement(a)?a.props.children:null:l);return v.jsx(j,{...n,ref:t,children:s.isValidElement(a)?s.cloneElement(a,void 0,u):null})}return v.jsx(j,{...n,ref:t,children:o})});P.displayName="Slot";var j=s.forwardRef((e,t)=>{const{children:o,...n}=e;if(s.isValidElement(o)){const r=Me(o),c=Re(n,o.props);return o.type!==s.Fragment&&(c.ref=t?W(t,r):r),s.cloneElement(o,c)}return s.Children.count(o)>1?s.Children.only(null):null});j.displayName="SlotClone";var xe=({children:e})=>v.jsx(v.Fragment,{children:e});function be(e){return s.isValidElement(e)&&e.type===xe}function Re(e,t){const o={...t};for(const n in t){const r=e[n],c=t[n];/^on[A-Z]/.test(n)?r&&c?o[n]=(...u)=>{c(...u),r(...u)}:r&&(o[n]=r):n==="style"?o[n]={...r,...c}:n==="className"&&(o[n]=[r,c].filter(Boolean).join(" "))}return{...e,...o}}function Me(e){var n,r;let t=(n=Object.getOwnPropertyDescriptor(e.props,"ref"))==null?void 0:n.get,o=t&&"isReactWarning"in t&&t.isReactWarning;return o?e.ref:(t=(r=Object.getOwnPropertyDescriptor(e,"ref"))==null?void 0:r.get,o=t&&"isReactWarning"in t&&t.isReactWarning,o?e.props.ref:e.props.ref||e.ref)}/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Ie=e=>e.replace(/([a-z0-9])([A-Z])/g,"$1-$2").toLowerCase(),q=(...e)=>e.filter((t,o,n)=>!!t&&n.indexOf(t)===o).join(" ");/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */var we={xmlns:"http://www.w3.org/2000/svg",width:24,height:24,viewBox:"0 0 24 24",fill:"none",stroke:"currentColor",strokeWidth:2,strokeLinecap:"round",strokeLinejoin:"round"};/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Te=s.forwardRef(({color:e="currentColor",size:t=24,strokeWidth:o=2,absoluteStrokeWidth:n,className:r="",children:c,iconNode:a,...u},l)=>s.createElement("svg",{ref:l,...we,width:t,height:t,stroke:e,strokeWidth:n?Number(o)*24/Number(t):o,className:q("lucide",r),...u},[...a.map(([i,p])=>s.createElement(i,p)),...Array.isArray(c)?c:[c]]));/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const b=(e,t)=>{const o=s.forwardRef(({className:n,...r},c)=>s.createElement(Te,{ref:c,iconNode:t,className:q(`lucide-${Ie(e)}`,n),...r}));return o.displayName=`${e}`,o};/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Qe=b("ArrowRight",[["path",{d:"M5 12h14",key:"1ays0h"}],["path",{d:"m12 5 7 7-7 7",key:"xquz4c"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const et=b("BookOpen",[["path",{d:"M12 7v14",key:"1akyts"}],["path",{d:"M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4 4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3 3 3 0 0 0-3-3z",key:"ruj8y"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const tt=b("ChevronLeft",[["path",{d:"m15 18-6-6 6-6",key:"1wnfg3"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const nt=b("Code",[["polyline",{points:"16 18 22 12 16 6",key:"z7tu5w"}],["polyline",{points:"8 6 2 12 8 18",key:"1eg1df"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ot=b("Expand",[["path",{d:"m21 21-6-6m6 6v-4.8m0 4.8h-4.8",key:"1c15vz"}],["path",{d:"M3 16.2V21m0 0h4.8M3 21l6-6",key:"1fsnz2"}],["path",{d:"M21 7.8V3m0 0h-4.8M21 3l-6 6",key:"hawz9i"}],["path",{d:"M3 7.8V3m0 0h4.8M3 3l6 6",key:"u9ee12"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const rt=b("Github",[["path",{d:"M15 22v-4a4.8 4.8 0 0 0-1-3.5c3 0 6-2 6-5.5.08-1.25-.27-2.48-1-3.5.28-1.15.28-2.35 0-3.5 0 0-1 0-3 1.5-2.64-.5-5.36-.5-8 0C6 2 5 2 5 2c-.3 1.15-.3 2.35 0 3.5A5.403 5.403 0 0 0 4 9c0 3.5 3 5.5 6 5.5-.39.49-.68 1.05-.85 1.65-.17.6-.22 1.23-.15 1.85v4",key:"tonef"}],["path",{d:"M9 18c-4.51 2-5-2-7-2",key:"9comsn"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const st=b("GripVertical",[["circle",{cx:"9",cy:"12",r:"1",key:"1vctgf"}],["circle",{cx:"9",cy:"5",r:"1",key:"hp0tcf"}],["circle",{cx:"9",cy:"19",r:"1",key:"fkjjf6"}],["circle",{cx:"15",cy:"12",r:"1",key:"1tmaij"}],["circle",{cx:"15",cy:"5",r:"1",key:"19l28e"}],["circle",{cx:"15",cy:"19",r:"1",key:"f4zoj3"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ct=b("Maximize2",[["polyline",{points:"15 3 21 3 21 9",key:"mznyad"}],["polyline",{points:"9 21 3 21 3 15",key:"1avn1i"}],["line",{x1:"21",x2:"14",y1:"3",y2:"10",key:"ota7mn"}],["line",{x1:"3",x2:"10",y1:"21",y2:"14",key:"1atl0r"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const at=b("Menu",[["line",{x1:"4",x2:"20",y1:"12",y2:"12",key:"1e0a9i"}],["line",{x1:"4",x2:"20",y1:"6",y2:"6",key:"1owob3"}],["line",{x1:"4",x2:"20",y1:"18",y2:"18",key:"yk5zj1"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const it=b("Minimize2",[["polyline",{points:"4 14 10 14 10 20",key:"11kfnr"}],["polyline",{points:"20 10 14 10 14 4",key:"rlmsce"}],["line",{x1:"14",x2:"21",y1:"10",y2:"3",key:"o5lafz"}],["line",{x1:"3",x2:"10",y1:"21",y2:"14",key:"1atl0r"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ut=b("Parentheses",[["path",{d:"M8 21s-4-3-4-9 4-9 4-9",key:"uto9ud"}],["path",{d:"M16 3s4 3 4 9-4 9-4 9",key:"4w2vsq"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const lt=b("Play",[["polygon",{points:"6 3 20 12 6 21 6 3",key:"1oa8hb"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const dt=b("Shrink",[["path",{d:"m15 15 6 6m-6-6v4.8m0-4.8h4.8",key:"17vawe"}],["path",{d:"M9 19.8V15m0 0H4.2M9 15l-6 6",key:"chjx8e"}],["path",{d:"M15 4.2V9m0 0h4.8M15 9l6-6",key:"lav6yq"}],["path",{d:"M9 4.2V9m0 0H4.2M9 9 3 3",key:"1pxi2q"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ft=b("Terminal",[["polyline",{points:"4 17 10 11 4 5",key:"akl6gq"}],["line",{x1:"12",x2:"20",y1:"19",y2:"19",key:"q2wloq"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const pt=b("Users",[["path",{d:"M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2",key:"1yyitq"}],["circle",{cx:"9",cy:"7",r:"4",key:"nufk8"}],["path",{d:"M22 21v-2a4 4 0 0 0-3-3.87",key:"kshegd"}],["path",{d:"M16 3.13a4 4 0 0 1 0 7.75",key:"1da9ce"}]]);/**
 * @license lucide-react v0.453.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const mt=b("X",[["path",{d:"M18 6 6 18",key:"1bl5f8"}],["path",{d:"m6 6 12 12",key:"d8bk6v"}]]);function M(e,t,{checkForDefaultPrevented:o=!0}={}){return function(r){if(e==null||e(r),o===!1||!r.defaultPrevented)return t==null?void 0:t(r)}}function V(e,t=[]){let o=[];function n(c,a){const u=s.createContext(a),l=o.length;o=[...o,a];const i=d=>{var g;const{scope:m,children:y,...f}=d,h=((g=m==null?void 0:m[e])==null?void 0:g[l])||u,x=s.useMemo(()=>f,Object.values(f));return v.jsx(h.Provider,{value:x,children:y})};i.displayName=c+"Provider";function p(d,m){var h;const y=((h=m==null?void 0:m[e])==null?void 0:h[l])||u,f=s.useContext(y);if(f)return f;if(a!==void 0)return a;throw new Error(`\`${d}\` must be used within \`${c}\``)}return[i,p]}const r=()=>{const c=o.map(a=>s.createContext(a));return function(u){const l=(u==null?void 0:u[e])||c;return s.useMemo(()=>({[`__scope${e}`]:{...u,[e]:l}}),[u,l])}};return r.scopeName=e,[n,Se(r,...t)]}function Se(...e){const t=e[0];if(e.length===1)return t;const o=()=>{const n=e.map(r=>({useScope:r(),scopeName:r.scopeName}));return function(c){const a=n.reduce((u,{useScope:l,scopeName:i})=>{const d=l(c)[`__scope${i}`];return{...u,...d}},{});return s.useMemo(()=>({[`__scope${t.scopeName}`]:a}),[a])}};return o.scopeName=t.scopeName,o}function ke(e){const t=e+"CollectionProvider",[o,n]=V(t),[r,c]=o(t,{collectionRef:{current:null},itemMap:new Map}),a=y=>{const{scope:f,children:h}=y,x=w.useRef(null),g=w.useRef(new Map).current;return v.jsx(r,{scope:f,itemMap:g,collectionRef:x,children:h})};a.displayName=t;const u=e+"CollectionSlot",l=w.forwardRef((y,f)=>{const{scope:h,children:x}=y,g=c(u,h),C=E(f,g.collectionRef);return v.jsx(P,{ref:C,children:x})});l.displayName=u;const i=e+"CollectionItemSlot",p="data-radix-collection-item",d=w.forwardRef((y,f)=>{const{scope:h,children:x,...g}=y,C=w.useRef(null),T=E(f,C),S=c(i,h);return w.useEffect(()=>(S.itemMap.set(C,{ref:C,...g}),()=>void S.itemMap.delete(C))),v.jsx(P,{[p]:"",ref:T,children:x})});d.displayName=i;function m(y){const f=c(e+"CollectionConsumer",y);return w.useCallback(()=>{const x=f.collectionRef.current;if(!x)return[];const g=Array.from(x.querySelectorAll(`[${p}]`));return Array.from(f.itemMap.values()).sort((S,N)=>g.indexOf(S.ref.current)-g.indexOf(N.ref.current))},[f.collectionRef,f.itemMap])}return[{Provider:a,Slot:l,ItemSlot:d},m,n]}var D=globalThis!=null&&globalThis.document?s.useLayoutEffect:()=>{},Ne=Ce.useId||(()=>{}),Ae=0;function Y(e){const[t,o]=s.useState(Ne());return D(()=>{e||o(n=>n??String(Ae++))},[e]),e||(t?`radix-${t}`:"")}var Ee=["a","button","div","form","h2","h3","img","input","label","li","nav","ol","p","span","svg","ul"],k=Ee.reduce((e,t)=>{const o=s.forwardRef((n,r)=>{const{asChild:c,...a}=n,u=c?P:t;return typeof window<"u"&&(window[Symbol.for("radix-ui")]=!0),v.jsx(u,{...a,ref:r})});return o.displayName=`Primitive.${t}`,{...e,[t]:o}},{});function U(e){const t=s.useRef(e);return s.useEffect(()=>{t.current=e}),s.useMemo(()=>(...o)=>{var n;return(n=t.current)==null?void 0:n.call(t,...o)},[])}function X({prop:e,defaultProp:t,onChange:o=()=>{}}){const[n,r]=Pe({defaultProp:t,onChange:o}),c=e!==void 0,a=c?e:n,u=U(o),l=s.useCallback(i=>{if(c){const d=typeof i=="function"?i(e):i;d!==e&&u(d)}else r(i)},[c,e,r,u]);return[a,l]}function Pe({defaultProp:e,onChange:t}){const o=s.useState(e),[n]=o,r=s.useRef(n),c=U(t);return s.useEffect(()=>{r.current!==n&&(c(n),r.current=n)},[n,r,c]),o}var Fe=s.createContext(void 0);function Z(e){const t=s.useContext(Fe);return e||t||"ltr"}var O="rovingFocusGroup.onEntryFocus",_e={bubbles:!1,cancelable:!0},F="RovingFocusGroup",[L,H,Oe]=ke(F),[je,J]=V(F,[Oe]),[De,Le]=je(F),Q=s.forwardRef((e,t)=>v.jsx(L.Provider,{scope:e.__scopeRovingFocusGroup,children:v.jsx(L.Slot,{scope:e.__scopeRovingFocusGroup,children:v.jsx(Ve,{...e,ref:t})})}));Q.displayName=F;var Ve=s.forwardRef((e,t)=>{const{__scopeRovingFocusGroup:o,orientation:n,loop:r=!1,dir:c,currentTabStopId:a,defaultCurrentTabStopId:u,onCurrentTabStopIdChange:l,onEntryFocus:i,preventScrollOnEntryFocus:p=!1,...d}=e,m=s.useRef(null),y=E(t,m),f=Z(c),[h=null,x]=X({prop:a,defaultProp:u,onChange:l}),[g,C]=s.useState(!1),T=U(i),S=H(o),N=s.useRef(!1),[me,z]=s.useState(0);return s.useEffect(()=>{const R=m.current;if(R)return R.addEventListener(O,T),()=>R.removeEventListener(O,T)},[T]),v.jsx(De,{scope:o,orientation:n,dir:f,loop:r,currentTabStopId:h,onItemFocus:s.useCallback(R=>x(R),[x]),onItemShiftTab:s.useCallback(()=>C(!0),[]),onFocusableItemAdd:s.useCallback(()=>z(R=>R+1),[]),onFocusableItemRemove:s.useCallback(()=>z(R=>R-1),[]),children:v.jsx(k.div,{tabIndex:g||me===0?-1:0,"data-orientation":n,...d,ref:y,style:{outline:"none",...e.style},onMouseDown:M(e.onMouseDown,()=>{N.current=!0}),onFocus:M(e.onFocus,R=>{const ye=!N.current;if(R.target===R.currentTarget&&ye&&!g){const B=new CustomEvent(O,_e);if(R.currentTarget.dispatchEvent(B),!B.defaultPrevented){const _=S().filter(I=>I.focusable),ve=_.find(I=>I.active),he=_.find(I=>I.id===h),ge=[ve,he,..._].filter(Boolean).map(I=>I.ref.current);ne(ge,p)}}N.current=!1}),onBlur:M(e.onBlur,()=>C(!1))})})}),ee="RovingFocusGroupItem",te=s.forwardRef((e,t)=>{const{__scopeRovingFocusGroup:o,focusable:n=!0,active:r=!1,tabStopId:c,...a}=e,u=Y(),l=c||u,i=Le(ee,o),p=i.currentTabStopId===l,d=H(o),{onFocusableItemAdd:m,onFocusableItemRemove:y}=i;return s.useEffect(()=>{if(n)return m(),()=>y()},[n,m,y]),v.jsx(L.ItemSlot,{scope:o,id:l,focusable:n,active:r,children:v.jsx(k.span,{tabIndex:p?0:-1,"data-orientation":i.orientation,...a,ref:t,onMouseDown:M(e.onMouseDown,f=>{n?i.onItemFocus(l):f.preventDefault()}),onFocus:M(e.onFocus,()=>i.onItemFocus(l)),onKeyDown:M(e.onKeyDown,f=>{if(f.key==="Tab"&&f.shiftKey){i.onItemShiftTab();return}if(f.target!==f.currentTarget)return;const h=$e(f,i.orientation,i.dir);if(h!==void 0){if(f.metaKey||f.ctrlKey||f.altKey||f.shiftKey)return;f.preventDefault();let g=d().filter(C=>C.focusable).map(C=>C.ref.current);if(h==="last")g.reverse();else if(h==="prev"||h==="next"){h==="prev"&&g.reverse();const C=g.indexOf(f.currentTarget);g=i.loop?ze(g,C+1):g.slice(C+1)}setTimeout(()=>ne(g))}})})})});te.displayName=ee;var Ue={ArrowLeft:"prev",ArrowUp:"prev",ArrowRight:"next",ArrowDown:"next",PageUp:"first",Home:"first",PageDown:"last",End:"last"};function Ge(e,t){return t!=="rtl"?e:e==="ArrowLeft"?"ArrowRight":e==="ArrowRight"?"ArrowLeft":e}function $e(e,t,o){const n=Ge(e.key,o);if(!(t==="vertical"&&["ArrowLeft","ArrowRight"].includes(n))&&!(t==="horizontal"&&["ArrowUp","ArrowDown"].includes(n)))return Ue[n]}function ne(e,t=!1){const o=document.activeElement;for(const n of e)if(n===o||(n.focus({preventScroll:t}),document.activeElement!==o))return}function ze(e,t){return e.map((o,n)=>e[(t+n)%e.length])}var Be=Q,Ke=te;function We(e,t){return s.useReducer((o,n)=>t[o][n]??o,e)}var oe=e=>{const{present:t,children:o}=e,n=qe(t),r=typeof o=="function"?o({present:n.isPresent}):s.Children.only(o),c=E(n.ref,Ye(r));return typeof o=="function"||n.isPresent?s.cloneElement(r,{ref:c}):null};oe.displayName="Presence";function qe(e){const[t,o]=s.useState(),n=s.useRef({}),r=s.useRef(e),c=s.useRef("none"),a=e?"mounted":"unmounted",[u,l]=We(a,{mounted:{UNMOUNT:"unmounted",ANIMATION_OUT:"unmountSuspended"},unmountSuspended:{MOUNT:"mounted",ANIMATION_END:"unmounted"},unmounted:{MOUNT:"mounted"}});return s.useEffect(()=>{const i=A(n.current);c.current=u==="mounted"?i:"none"},[u]),D(()=>{const i=n.current,p=r.current;if(p!==e){const m=c.current,y=A(i);e?l("MOUNT"):y==="none"||(i==null?void 0:i.display)==="none"?l("UNMOUNT"):l(p&&m!==y?"ANIMATION_OUT":"UNMOUNT"),r.current=e}},[e,l]),D(()=>{if(t){let i;const p=t.ownerDocument.defaultView??window,d=y=>{const h=A(n.current).includes(y.animationName);if(y.target===t&&h&&(l("ANIMATION_END"),!r.current)){const x=t.style.animationFillMode;t.style.animationFillMode="forwards",i=p.setTimeout(()=>{t.style.animationFillMode==="forwards"&&(t.style.animationFillMode=x)})}},m=y=>{y.target===t&&(c.current=A(n.current))};return t.addEventListener("animationstart",m),t.addEventListener("animationcancel",d),t.addEventListener("animationend",d),()=>{p.clearTimeout(i),t.removeEventListener("animationstart",m),t.removeEventListener("animationcancel",d),t.removeEventListener("animationend",d)}}else l("ANIMATION_END")},[t,l]),{isPresent:["mounted","unmountSuspended"].includes(u),ref:s.useCallback(i=>{i&&(n.current=getComputedStyle(i)),o(i)},[])}}function A(e){return(e==null?void 0:e.animationName)||"none"}function Ye(e){var n,r;let t=(n=Object.getOwnPropertyDescriptor(e.props,"ref"))==null?void 0:n.get,o=t&&"isReactWarning"in t&&t.isReactWarning;return o?e.ref:(t=(r=Object.getOwnPropertyDescriptor(e,"ref"))==null?void 0:r.get,o=t&&"isReactWarning"in t&&t.isReactWarning,o?e.props.ref:e.props.ref||e.ref)}var G="Tabs",[Xe,yt]=V(G,[J]),re=J(),[Ze,$]=Xe(G),se=s.forwardRef((e,t)=>{const{__scopeTabs:o,value:n,onValueChange:r,defaultValue:c,orientation:a="horizontal",dir:u,activationMode:l="automatic",...i}=e,p=Z(u),[d,m]=X({prop:n,onChange:r,defaultProp:c});return v.jsx(Ze,{scope:o,baseId:Y(),value:d,onValueChange:m,orientation:a,dir:p,activationMode:l,children:v.jsx(k.div,{dir:p,"data-orientation":a,...i,ref:t})})});se.displayName=G;var ce="TabsList",ae=s.forwardRef((e,t)=>{const{__scopeTabs:o,loop:n=!0,...r}=e,c=$(ce,o),a=re(o);return v.jsx(Be,{asChild:!0,...a,orientation:c.orientation,dir:c.dir,loop:n,children:v.jsx(k.div,{role:"tablist","aria-orientation":c.orientation,...r,ref:t})})});ae.displayName=ce;var ie="TabsTrigger",ue=s.forwardRef((e,t)=>{const{__scopeTabs:o,value:n,disabled:r=!1,...c}=e,a=$(ie,o),u=re(o),l=fe(a.baseId,n),i=pe(a.baseId,n),p=n===a.value;return v.jsx(Ke,{asChild:!0,...u,focusable:!r,active:p,children:v.jsx(k.button,{type:"button",role:"tab","aria-selected":p,"aria-controls":i,"data-state":p?"active":"inactive","data-disabled":r?"":void 0,disabled:r,id:l,...c,ref:t,onMouseDown:M(e.onMouseDown,d=>{!r&&d.button===0&&d.ctrlKey===!1?a.onValueChange(n):d.preventDefault()}),onKeyDown:M(e.onKeyDown,d=>{[" ","Enter"].includes(d.key)&&a.onValueChange(n)}),onFocus:M(e.onFocus,()=>{const d=a.activationMode!=="manual";!p&&!r&&d&&a.onValueChange(n)})})})});ue.displayName=ie;var le="TabsContent",de=s.forwardRef((e,t)=>{const{__scopeTabs:o,value:n,forceMount:r,children:c,...a}=e,u=$(le,o),l=fe(u.baseId,n),i=pe(u.baseId,n),p=n===u.value,d=s.useRef(p);return s.useEffect(()=>{const m=requestAnimationFrame(()=>d.current=!1);return()=>cancelAnimationFrame(m)},[]),v.jsx(oe,{present:r||p,children:({present:m})=>v.jsx(k.div,{"data-state":p?"active":"inactive","data-orientation":u.orientation,role:"tabpanel","aria-labelledby":l,hidden:!m,id:i,tabIndex:0,...a,ref:t,style:{...e.style,animationDuration:d.current?"0s":void 0},children:m&&c})})});de.displayName=le;function fe(e,t){return`${e}-trigger-${t}`}function pe(e,t){return`${e}-content-${t}`}var vt=se,ht=ae,gt=ue,Ct=de;export{Qe as A,et as B,nt as C,ot as E,rt as G,ht as L,at as M,ut as P,vt as R,P as S,ft as T,pt as U,mt as X,lt as a,st as b,it as c,ct as d,dt as e,gt as f,Ct as g,tt as h};
