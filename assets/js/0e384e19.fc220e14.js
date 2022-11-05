"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[671],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>d});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var u=r.createContext({}),c=function(e){var t=r.useContext(u),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},s=function(e){var t=c(e.components);return r.createElement(u.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},h=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,u=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),h=c(n),d=a,m=h["".concat(u,".").concat(d)]||h[d]||p[d]||o;return n?r.createElement(m,i(i({ref:t},s),{},{components:n})):r.createElement(m,i({ref:t},s))}));function d(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=h;var l={};for(var u in t)hasOwnProperty.call(t,u)&&(l[u]=t[u]);l.originalType=e,l.mdxType="string"==typeof e?e:a,i[1]=l;for(var c=2;c<o;c++)i[c]=n[c];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}h.displayName="MDXCreateElement"},59881:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>u,contentTitle:()=>i,default:()=>p,frontMatter:()=>o,metadata:()=>l,toc:()=>c});var r=n(87462),a=(n(67294),n(3905));const o={sidebar_position:1},i="Introduction",l={unversionedId:"intro",id:"intro",title:"Introduction",description:"This is an advanced B\xe9zier curves module for Luau, which supports Vector3 curves of any degree.",source:"@site/docs/intro.md",sourceDirName:".",slug:"/intro",permalink:"/bezier/docs/intro",draft:!1,editUrl:"https://github.com/bstummer/bezier/edit/main/docs/intro.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"defaultSidebar"},u={},c=[{value:"Installation",id:"installation",level:2},{value:"How to create a B\xe9zier curve",id:"how-to-create-a-b\xe9zier-curve",level:2},{value:"Get a point on the curve",id:"get-a-point-on-the-curve",level:2},{value:"Arc Length Parameterization",id:"arc-length-parameterization",level:2},{value:"Things to consider",id:"things-to-consider",level:2}],s={toc:c};function p(e){let{components:t,...n}=e;return(0,a.kt)("wrapper",(0,r.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"introduction"},"Introduction"),(0,a.kt)("p",null,"This is an advanced B\xe9zier curves module for Luau, which supports Vector3 curves of any degree.\nIt provides all functions that you need for game development."),(0,a.kt)("p",null,"Although it encompasses many features, it is very lightweight. It won't waste any calculations on things you don't use."),(0,a.kt)("h2",{id:"installation"},"Installation"),(0,a.kt)("p",null,"Get the module ",(0,a.kt)("a",{parentName:"p",href:"https://www.roblox.com/library/11467361559"},"here")," and insert into your game via the Toolbox.\nYou can also just paste this in the command bar:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-lua"},'game:GetObjects("rbxassetid://11467361559")[1].Parent=game.ReplicatedStorage\n')),(0,a.kt)("h2",{id:"how-to-create-a-b\xe9zier-curve"},"How to create a B\xe9zier curve"),(0,a.kt)("p",null,"There are multiple constructors:"),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"Bezier.new")," is the default one, which simply requires an array of Vector3 points."),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"Bezier.fromParts")," can be used to create a curve from a folder of Parts. The parts must have a number as their name (1, 2, 3, ...) so the order of the points is clear."),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"Bezier.random")," allows you to generate a random curve based on the generation settings."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-lua"},"local Bezier = require(game.ReplicatedStorage.Bezier)\n\nlocal curve = Bezier.fromParts(workspace.BezierCurve)\n")),(0,a.kt)("h2",{id:"get-a-point-on-the-curve"},"Get a point on the curve"),(0,a.kt)("admonition",{type:"tip"},(0,a.kt)("p",{parentName:"admonition"},"For testing and visualization purposes, you can get my visualization module ",(0,a.kt)("a",{parentName:"p",href:"https://www.roblox.com/library/11467394566"},"here"),".")),(0,a.kt)("p",null,"The recommended way to calculate a point on the curve is ",(0,a.kt)("inlineCode",{parentName:"p"},"BezierCurve:DeCasteljau")," (named after De Casteljau's algorithm)."),(0,a.kt)("p",null,"It takes a ",(0,a.kt)("inlineCode",{parentName:"p"},"t")," value of 0 to 1 as input, where 0 is the first point of the curve and 1 the last."),(0,a.kt)("p",null,"You can visualize the curve with this code:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-lua"},'local Bezier = require(game.ReplicatedStorage.Bezier)\nlocal curve = Bezier.fromParts(workspace.BezierCurve)\n\nlocal function createPoint(pos: Vector3, color: Color3?)\n    local part = Instance.new("Part")\n    part.Anchored = true\n    part.Color = color or Color3.fromRGB(170, 0, 0)\n    part.Shape = Enum.PartType.Ball\n    part.Position = pos\n    part.Size = Vector3.new(1, 1, 1)\n    part.Name = "Point"\n    part.TopSurface = Enum.SurfaceType.Smooth\n    part.BottomSurface = Enum.SurfaceType.Smooth\n    part.Parent = workspace\nend\n\nlocal iterations = 50\nfor i = 0, iterations do\n    local t = i / iterations\n    createPoint(curve:DeCasteljau(t))\nend\n')),(0,a.kt)("h2",{id:"arc-length-parameterization"},"Arc Length Parameterization"),(0,a.kt)("p",null,"If you visualize the path of the curve with the code above, you will notice that the points don't have an equal distance to each other. They are not evenly distributed although the points are spread at equal intervals."),(0,a.kt)("p",null,"This means that t = 0.5 is usually not at 50% of the B\xe9zier curve's length."),(0,a.kt)("p",null,"This can be solved approximately using arc length parameterization. I created the function ",(0,a.kt)("inlineCode",{parentName:"p"},"ConvertT"),", which takes a length as input and returns the time t at which this length occurs."),(0,a.kt)("p",null,"Now we can distribute the points evenly along the curve:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-lua"},"local iterations = 50\nfor i = 0, iterations do\n    local t = i / iterations\n    local convertedT = curve:ConvertT(t)\n    createPoint(curve:DeCasteljau(convertedT))\nend\n")),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"}," Continue to the API section ",(0,a.kt)("a",{parentName:"strong",href:"https://bstummer.github.io/bezier/api/Bezier"},"here"),"!")),(0,a.kt)("h2",{id:"things-to-consider"},"Things to consider"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"There are no checks of the arguments you pass in functions. You should look at the type annotations and understand what the function does. Furthermore, you can look at the source code."),(0,a.kt)("li",{parentName:"ul"},"The use of ",(0,a.kt)("inlineCode",{parentName:"li"},"GetExtrema")," requires the installation of a complex numbers module, more on that in the API section."),(0,a.kt)("li",{parentName:"ul"},"It is always possible to modify the control points. However, if you use the functions ",(0,a.kt)("inlineCode",{parentName:"li"},"ConvertT"),", ",(0,a.kt)("inlineCode",{parentName:"li"},"GetNormal")," or the property ",(0,a.kt)("inlineCode",{parentName:"li"},"Length"),", you have to call ",(0,a.kt)("inlineCode",{parentName:"li"},"BezierCurve:UpdateLUT")," after the modifications. This is due to the fact that the internal lookup tables won't be accurate anymore.")))}p.isMDXComponent=!0}}]);