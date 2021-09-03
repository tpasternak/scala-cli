(window.webpackJsonp=window.webpackJsonp||[]).push([[27],{102:function(e,t,n){"use strict";n.d(t,"a",(function(){return u})),n.d(t,"b",(function(){return m}));var a=n(0),r=n.n(a);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=r.a.createContext({}),p=function(e){var t=r.a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},u=function(e){var t=p(e.components);return r.a.createElement(s.Provider,{value:t},e.children)},b={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},d=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,i=e.parentName,s=c(e,["components","mdxType","originalType","parentName"]),u=p(n),d=a,m=u["".concat(i,".").concat(d)]||u[d]||b[d]||o;return n?r.a.createElement(m,l(l({ref:t},s),{},{components:n})):r.a.createElement(m,l({ref:t},s))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=d;var l={};for(var c in t)hasOwnProperty.call(t,c)&&(l[c]=t[c]);l.originalType=e,l.mdxType="string"==typeof e?e:a,i[1]=l;for(var s=2;s<o;s++)i[s]=n[s];return r.a.createElement.apply(null,i)}return r.a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},95:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return i})),n.d(t,"metadata",(function(){return l})),n.d(t,"toc",(function(){return c})),n.d(t,"default",(function(){return p}));var a=n(3),r=n(7),o=(n(0),n(102)),i={title:"IDE support",sidebar_position:10},l={unversionedId:"ide",id:"ide",isDocsHomePage:!1,title:"IDE support",description:"IDE support for sources managed by the Scala CLI is experimental, and limited to",source:"@site/docs/ide.md",sourceDirName:".",slug:"/ide",permalink:"/scala-cli/docs/ide",editUrl:"https://github.com/VirtuslabRnD/scala-cli/edit/master/website/docs/ide.md",version:"current",sidebarPosition:10,frontMatter:{title:"IDE support",sidebar_position:10},sidebar:"tutorialSidebar",previous:{title:"REPL",permalink:"/scala-cli/docs/repl"},next:{title:"Scala.JS",permalink:"/scala-cli/docs/scala-js"}},c=[{value:"VSCode",id:"vscode",children:[{value:"Setup",id:"setup",children:[]},{value:"Activating the Scala CLI support",id:"activating-the-scala-cli-support",children:[]}]},{value:"Neovim",id:"neovim",children:[{value:"Setup",id:"setup-1",children:[]}]}],s={toc:c};function p(e){var t=e.components,n=Object(r.a)(e,["components"]);return Object(o.b)("wrapper",Object(a.a)({},s,n,{components:t,mdxType:"MDXLayout"}),Object(o.b)("p",null,"IDE support for sources managed by the Scala CLI is experimental, and limited to\n",Object(o.b)("a",{parentName:"p",href:"https://scalameta.org/metals/"},"Metals")," with ",Object(o.b)("a",{parentName:"p",href:"https://scalameta.org/metals/docs/editors/vscode"},"VS\nCode")," or\n",Object(o.b)("a",{parentName:"p",href:"https://github.com/scalameta/nvim-metals"},"Neovim")," for now."),Object(o.b)("h2",{id:"vscode"},"VSCode"),Object(o.b)("h3",{id:"setup"},"Setup"),Object(o.b)("p",null,"Scala CLI support in Metals / VSCode requires the latest Metals VSCode extension (>= ",Object(o.b)("inlineCode",{parentName:"p"},"1.10.8"),"). Ensure\nit is installed and up-to-date, or install or update it from the Extension panel in VSCode."),Object(o.b)("p",null,'Scala CLI support relies on a custom Metals server for now. To enable it in the current project,\nrun the command "Create New Integrated Terminal (in Active Workspace)", and type'),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-bash"},'mkdir -p .vscode\ncat > .vscode/settings.json << EOF\n{\n  "metals.serverVersion": "org.virtuslab:metals_2.12:0.10.5+65-f2a9927c-SNAPSHOT",\n  "metals.serverProperties": [\n    "-Xmx512m",\n    "-Dmetals.scala-cli.launcher=$(which scala-cli)"\n  ]\n}\nEOF\n')),Object(o.b)("p",null,'A window reload should be needed for this change to be taken into account. If Metals doesn\'t\nsuggest it, run the "Developer: Reload window" command from the command palette.'),Object(o.b)("h3",{id:"activating-the-scala-cli-support"},"Activating the Scala CLI support"),Object(o.b)("p",null,"In order for Metals to assume a ",Object(o.b)("inlineCode",{parentName:"p"},".scala")," or ",Object(o.b)("inlineCode",{parentName:"p"},".sc")," file is handled by the Scala CLI,\na ",Object(o.b)("inlineCode",{parentName:"p"},"scala.conf")," file or a file ending in ",Object(o.b)("inlineCode",{parentName:"p"},".scala.conf")," needs to exist in the same\ndirectory as the ",Object(o.b)("inlineCode",{parentName:"p"},".scala")," or ",Object(o.b)("inlineCode",{parentName:"p"},".sc")," files, or in a parent directory of theirs. Beware\nthat it needs to be in the Metals workspace though (so you can't put it at the root\nof your filesystem, for example). This file can be empty."),Object(o.b)("p",null,"Upon opening a ",Object(o.b)("inlineCode",{parentName:"p"},".scala")," or ",Object(o.b)("inlineCode",{parentName:"p"},".sc")," file while ",Object(o.b)("inlineCode",{parentName:"p"},"scala.conf")," or a ",Object(o.b)("inlineCode",{parentName:"p"},"*.scala.conf")," file exists,\nMetals should open a dialog offering to:"),Object(o.b)("ul",null,Object(o.b)("li",{parentName:"ul"},"Import Scala CLI projects automatically"),Object(o.b)("li",{parentName:"ul"},"Import"),Object(o.b)("li",{parentName:"ul"},"Dismiss")),Object(o.b)("p",null,"Pick any of the first two options, and enjoy IDE support for your Scala CLI-managed sources!"),Object(o.b)("p",null,"The following Metals features are expected to work, among others:"),Object(o.b)("ul",null,Object(o.b)("li",{parentName:"ul"},"go-to-source"),Object(o.b)("li",{parentName:"ul"},"diagnostics"),Object(o.b)("li",{parentName:"ul"},"find usages")),Object(o.b)("p",null,"Upon adding new dependencies, via ",Object(o.b)("inlineCode",{parentName:"p"},"scala.conf")," or via ",Object(o.b)("inlineCode",{parentName:"p"},"import $dep")," in Scala sources, the\nnew dependencies should be automatically downloaded and be available right after in Metals."),Object(o.b)("h2",{id:"neovim"},"Neovim"),Object(o.b)("h3",{id:"setup-1"},"Setup"),Object(o.b)("p",null,"You can get Scala CLI support in Neovim by using\n",Object(o.b)("a",{parentName:"p",href:"https://github.com/scalameta/nvim-metals"},Object(o.b)("inlineCode",{parentName:"a"},"nvim-metals")),". Scala CLI support\nrelies on a custom Metals server for now. To enable it make sure to set the\n",Object(o.b)("inlineCode",{parentName:"p"},"g:metals_server_org")," to ",Object(o.b)("inlineCode",{parentName:"p"},"org.virtuslab"),", and update the\n",Object(o.b)("inlineCode",{parentName:"p"},"g:metals_server_version")," to the desired version."),Object(o.b)("p",null,"You'll also need to set a server property to detect the location of ",Object(o.b)("inlineCode",{parentName:"p"},"scala-cli"),".\nYou can set this in your settings table like so:"),Object(o.b)("pre",null,Object(o.b)("code",{parentName:"pre",className:"language-lua"},'Metals_config = require("metals").bare_config\n\nMetals_config.settings = {\n  serverProperties = {\n    "-Dmetals.scala-cli.launcher=<location of your installed scala-cli>"\n  }\n}\n')),Object(o.b)("p",null,"After updating these values make sure to run a ",Object(o.b)("inlineCode",{parentName:"p"},":MetalsInstall")," command to\ninstall the custom Metals server. You'll then either need to restart the server\nwith a ",Object(o.b)("inlineCode",{parentName:"p"},":MetalsRestartServer")," or just close the project and reopen."))}p.isMDXComponent=!0}}]);