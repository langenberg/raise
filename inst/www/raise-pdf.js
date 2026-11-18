// PDF preview in modals: every page drawn with PDF.js at the full width of
// the modal (browsers' built-in viewers, e.g. Safari's, shrink multi-page PDFs
// to the frame height and ignore #view=FitH). Pages are rendered when they
// scroll into view. Without PDF.js (offline), falls back to an <iframe>.
(function () {
  var PDFJS_WORKER = "https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.worker.min.js";

  function fallback(box, src) {
    box.innerHTML = "";
    var f = document.createElement("iframe");
    f.src = src;
    f.style.cssText = "width:100%;height:78vh;border:0;";
    box.appendChild(f);
  }

  function render(box) {
    if (box.dataset.rendered) return;
    box.dataset.rendered = "1";
    var src = box.dataset.src;
    if (!window.pdfjsLib) { fallback(box, src); return; }
    window.pdfjsLib.GlobalWorkerOptions.workerSrc = PDFJS_WORKER;
    var width = box.clientWidth - 2;
    var dpr = window.devicePixelRatio || 1;
    window.pdfjsLib.getDocument(src).promise.then(function (pdf) {
      box.textContent = "";
      var observer = new IntersectionObserver(function (entries) {
        entries.forEach(function (e) {
          if (!e.isIntersecting || e.target.dataset.done) return;
          e.target.dataset.done = "1";
          observer.unobserve(e.target);
          var n = Number(e.target.dataset.page);
          pdf.getPage(n).then(function (page) {
            var base = page.getViewport({ scale: 1 });
            var vp = page.getViewport({ scale: (width / base.width) * dpr });
            var canvas = e.target;
            canvas.width = vp.width;
            canvas.height = vp.height;
            canvas.style.aspectRatio = vp.width + " / " + vp.height;
            page.render({ canvasContext: canvas.getContext("2d"), viewport: vp });
          });
        });
      }, { root: box, rootMargin: "800px 0px" });
      for (var i = 1; i <= pdf.numPages; i++) {
        var c = document.createElement("canvas");
        c.dataset.page = i;
        c.className = "raise-pdf-page";
        // placeholder height (A4 ratio) until the page is drawn
        c.style.cssText = "width:100%;aspect-ratio:1/1.414;display:block;margin:0 0 8px;" +
                          "box-shadow:0 1px 3px rgba(0,0,0,.25);background:#fff;";
        box.appendChild(c);
        observer.observe(c);
      }
    }).catch(function () { fallback(box, src); });
  }

  document.addEventListener("shown.bs.modal", function (e) {
    e.target.querySelectorAll(".raise-pdf[data-src]").forEach(render);
  });
})();
