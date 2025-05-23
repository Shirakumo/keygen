class Keygen{
    constructor(){
        this.cache = {};
        this.loading = {};
        if(console.log === undefined)
            this.log = ()=>{};
        else
            this.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Keygen]");
                return console.log.apply(console, args);
            };
        
        this.log("Init");

        this.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!this.apiRoot){
            this.log("Failed to retrieve API root. WTF?");
        }
        this.registerElements();
    }

    registerElements(element){
        element = element || document;
        this.registerAll(element, ".button.confirm", this.registerConfirm);
        this.registerAll(element, "form", this.registerForm);
        this.registerAll(element, ".dynamic-list", this.registerDynamicList);
        this.registerAll(element, "a.code", this.registerCode);
    }

    registerAll(element, query, regger){
        var self = this;
        var elements = element.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    registerForm(element){
        var self = this;
        var save = element.querySelector("input[type=submit]");
        if(!save || save.dataset.nofetch) return;
        if(element.classList.contains("search")) return;
        save.addEventListener("click", (ev)=>{
            var target = save.getAttribute("formaction") || element.getAttribute("action");
            ev.preventDefault();
            if(!self.loading[target]){
                if(element.checkValidity()){
                    self.showSpinner();
                    self.loading[target] =
                        self.chunkUpload(target, element, {progress: (loaded,total)=>
                            self.showSpinner({progress: Math.round(loaded*100/total)})
                        })
                        .then((r)=>{if(r.target)window.location.replace(r.target);},
                              (r)=>{self.showError(r.message || new DOMParser().parseFromString(r, "text/html").querySelector("title").innerText);})
                        .finally(()=>{delete self.loading[target];
                                      self.showSpinner();});
                }else{
                    element.reportValidity();
                }
            }
            return false;
        });
    }

    registerCode(element){
        var self = this;
        var code = element.innerText;
        var href = element.getAttribute("href");
        element.addEventListener("click", (ev)=>{
            ev.preventDefault();
            navigator.clipboard.writeText(href||code).then(()=>{
                self.showInfo(code+" copied to clipboard!");
            });
            return true;
        });
        if(href)
            element.addEventListener("dblclick", (ev)=>{
                window.location.replace(href);
                return true;
            });
    }

    registerConfirm(element){
        element.addEventListener("click", (ev)=>{
            if(confirm("Are you sure?")){
                return true;
            }else{
                ev.preventDefault();
                return false;
            }
        });
    }

    registerDynamicList(element){
        var self = this;
        var make = ()=>{
            let copy = self.instantiateTemplate(element);
            element.querySelector("ul").appendChild(reg(copy));
            return copy;
        };
        var reg = (subelement)=>{
            self.registerElements(subelement);
            var remove = subelement.querySelector(".remove-self");
            if(remove)
                remove.addEventListener("click",()=>{
                    subelement.parentNode.removeChild(subelement);
                });
            return subelement;
        };
        [].forEach.call(element.querySelectorAll("li"), (el)=>{
            if(!el.classList.contains("template")) reg(el);
        });
        element.querySelector("a.new").addEventListener("click",make);
    }

    instantiateTemplate(element){
        var copy = element.querySelector(".template").cloneNode(true);
        var context = {};
        copy.classList.remove("template");
        copy.removeAttribute("data-name");
        var resolveValue = (descriptor)=>{
            return function(){return eval(descriptor);}.call(context);
        };
        [].forEach.call(element.querySelectorAll("[data-var]"), (el)=>{
            context[el.dataset["var"]] = el;
        });
        [].forEach.call(copy.querySelectorAll("[data-name]"), (el)=>{
            el.setAttribute("name", el.dataset.name);
            el.removeAttribute("data-name");
        });
        [].forEach.call(copy.querySelectorAll("[data-value]"), (el)=>{
            el.value = resolveValue(el.dataset.value);
            el.removeAttribute("data-value");
        });
        [].forEach.call(copy.querySelectorAll("[data-text]"), (el)=>{
            el.innerText = resolveValue(el.dataset.text);
            el.removeAttribute("data-text");
        });
        return copy;
    }

    showInfo(content){
        var box = document.querySelector(".box.info");
        var updated = box.cloneNode(true);
        updated.innerText = content;
        box.parentNode.replaceChild(updated, box);
    }

    showError(content){
        var box = document.querySelector(".box.error");
        var updated = box.cloneNode(true);
        updated.innerText = content;
        box.parentNode.replaceChild(updated, box);
    }

    showSpinner(options){
        var spinner = document.querySelector(".spinner");
        options = options || {activate: (spinner)? false : true};
        if(options.activate === undefined) options.activate = true;
        if(options.activate){
            if(!spinner)
                spinner = this.constructElement("div", {
                    classes: ["popup", "spinner", options.classes],
                    elements: [
                        {
                            tag: "div",
                            text: options.message || "Please Wait",
                            classes: ["container"],
                            elements: [{tag:"div", classes: ["anim"]},
                                       {tag:"div", classes: ["anim"]}, {
                                           tag: "div",
                                           classes: ["progress"],
                                           elements: [{tag:"div"}]
                                       }]
                        }
                    ]
                });
            if(options.message) spinner.querySelector(".container").innerText = options.message;
            if(options.progress !== undefined) spinner.querySelector(".progress div").style.width = options.progress+"%";
            document.querySelector("body").appendChild(spinner);
        }else if(spinner){
            spinner.parentElement.removeChild(spinner);
        }
        return spinner;
    }

    constructElement(tag, options){
        var self = this;
        var el = document.createElement(options.tag || tag);
        (options.classes||[]).forEach(function(clas){
            if(clas) el.classList.add(clas);
        });
        if(options.text) el.innerText = options.text;
        if(options.html) el.innerHTML = options.html;
        for(var attr in (options.attributes||{})){
            if(options.attributes[attr])
                el.setAttribute(attr, options.attributes[attr]);
        }
        (options.elements||[]).forEach(function(element){
            el.appendChild(self.constructElement(element.tag, element));
        });
        return el;
    }

    chunkUpload(endpoint, args, options){
        options = options || {};
        var chunkSize = options.chunkSize || (1024*1024)*5;
        var input = args.querySelector("input[type=file]");
        if(!input || input.files.length <= 0 || args.classList.contains("direct"))
            return this.apiCall(endpoint, args, options);
        var file = input.files[0];
        var filePart = input.getAttribute("name");
        var promise = Promise.resolve();
        var progress = options.progress;
        console.log("Will chunk the file into",Math.ceil(file.size/chunkSize),"chunks of",chunkSize,"bytes");
        for(let i=0,c=0; i<file.size; i+=chunkSize, c++){
            let start = i;
            let chunk = c;
            promise = promise.then(()=>{
                var end = Math.min(start+chunkSize, file.size);
                var formData = new FormData(args);
                formData.delete("browser");
                formData.set(filePart, file.slice(start, end));
                formData.set("chunk", chunk+"");
                if(progress){
                    options.progress = (loaded,total)=>progress(start+loaded, file.size);
                }
                return this.apiCall(endpoint, formData, options);
            });
        };
        return promise.then(()=>{
            var formData = new FormData(args);
            formData.delete("browser");
            formData.set(filePart, "end");
            formData.set("chunk", "end");
            return this.apiCall(endpoint, formData, options);
        });
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = this.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");

            if(methodArgs.progress)
                request.upload.addEventListener("progress", (ev)=>
                    methodArgs.progress(ev.loaded, ev.total));
            
            request.addEventListener("load", ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            });
            self.log("Sending request to", endpoint);
            request.open("POST", endpoint);
            request.send(formData);
        });
    }
}

var keygen;
document.addEventListener("DOMContentLoaded", ()=>keygen = keygen || new Keygen());
