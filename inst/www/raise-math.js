// Math in the chat and in the modals.
//
// shinychat (0.3.x) renders chat messages with marked.js, which treats LaTeX as
// Markdown: `\_` loses its backslash, `_x_` turns into italics. Inside every
// math span, each ASCII punctuation character gets a backslash; marked removes
// exactly those backslashes, so MathJax receives the LaTeX unchanged.
// R/markdown_math.R applies the same rule to the modals. Keep both in sync.
(function () {
  var PATTERN = new RegExp([
    "(`{3,}|~{3,})[^\\n]*\\n[\\s\\S]*?(?:\\n\\1[`~]*[ \\t]*(?=\\n|$)|$)",
    "`+[^`\\n]*`+",
    "\\\\\\$",
    "\\$\\$[\\s\\S]+?\\$\\$",
    "\\\\\\[[\\s\\S]+?\\\\\\]",
    "\\\\\\([\\s\\S]+?\\\\\\)",
    "\\$(?:\\\\.|[^$\\\\])+?\\$"
  ].join("|"), "g");
  var PUNCT = /[!-\/:-@\[-`{-~]/g;

  function protectMath(s) {
    if (typeof s !== "string" || !/[$\\]/.test(s)) return s;
    return s.replace(PATTERN, function (m) {
      var c = m.charAt(0);
      if (c === "`" || c === "~") return m;
      if (m === "\\$") return "\\\\\\$";
      return m.replace(PUNCT, "\\$&");
    });
  }
  window.raiseProtectMath = protectMath;

  function typeset(el) {
    if (window.MathJax && window.MathJax.Hub) {
      window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub, el]);
    }
  }

  // Chat typesetting is batched: Lit re-creates nested elements (e.g. the
  // Markdown inside a tool card) on every streamed chunk, and typesetting an
  // element that is removed mid-way leaves MathJax 2's queue stuck for good.
  // So: collect elements, wait until nothing streams and no update came for
  // 300 ms, then typeset each outermost element that is still in the page.
  var pendingEls = new Set();
  var flushTimer = null;
  function scheduleTypeset(el) {
    pendingEls.add(el);
    clearTimeout(flushTimer);
    flushTimer = setTimeout(flushTypeset, 300);
  }
  function flushTypeset() {
    flushTimer = null;
    if (document.querySelector("shiny-chat-message[streaming], shiny-markdown-stream[streaming]")) {
      flushTimer = setTimeout(flushTypeset, 300);
      return;
    }
    var els = Array.from(pendingEls).filter(function (e) { return e.isConnected; });
    pendingEls.clear();
    els.filter(function (e) {
      return !els.some(function (o) { return o !== e && o.contains(e); });
    }).forEach(typeset);
  }

  // Patch the `content` setter of <shiny-markdown-stream>. The chat sets the
  // `content` attribute, which Lit copies to the property, so the attribute
  // keeps the original text (streamed chunks are appended to it) and only the
  // text marked.js parses is protected. Typeset once a message stops streaming:
  // during streaming every chunk replaces the rendered HTML.
  function patchMarkdownStream(cls) {
    var proto = cls.prototype;
    var p = proto, desc = null;
    while (p && !(desc = Object.getOwnPropertyDescriptor(p, "content"))) {
      p = Object.getPrototypeOf(p);
    }
    if (!desc || !desc.set || !desc.get || typeof proto.updated !== "function") {
      console.warn("RAISE: unexpected shinychat version; math in the chat is not protected.");
      return false;
    }
    if (desc.set.__raise) return false;
    var setter = function (v) { desc.set.call(this, protectMath(v)); };
    setter.__raise = true;
    Object.defineProperty(proto, "content", {
      configurable: true, enumerable: desc.enumerable, get: desc.get, set: setter
    });
    var updated = proto.updated;
    proto.updated = function (changed) {
      updated.call(this, changed);
      if (!this.streaming && changed && (changed.has("content") || changed.has("streaming"))) {
        scheduleTypeset(this);
      }
    };
    return true;
  }

  if (window.customElements) {
    customElements.whenDefined("shiny-markdown-stream").then(function () {
      if (!patchMarkdownStream(customElements.get("shiny-markdown-stream"))) return;
      document.querySelectorAll("shiny-markdown-stream").forEach(function (el) {
        var a = el.getAttribute("content");
        if (a !== null) el.content = a;
      });
    });
  }

  // Modals (question set, export and study details) are built on the server;
  // typeset them once they are shown.
  document.addEventListener("shown.bs.modal", function (e) { typeset(e.target); });
})();
