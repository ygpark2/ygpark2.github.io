(() => {
  function hasContent(el) {
    if (!el) return false;
    return (el.textContent || "").trim().length > 0;
  }

  function setStatus(msg) {
    let status = document.getElementById("post-fallback-status");
    if (!status) {
      status = document.createElement("div");
      status.id = "post-fallback-status";
      status.setAttribute("role", "alert");
      status.className =
        "fixed right-5 top-5 z-[9999] max-w-xs border-l-4 border-orange-500 bg-orange-100 p-4 text-orange-700 shadow-lg transition-opacity duration-200";
      status.style.opacity = "0";
      document.body.appendChild(status);
    }
    if (!msg) {
      status.style.opacity = "0";
      status.textContent = "";
      return;
    }
    status.innerHTML = "<p class=\"font-bold\">Be Warned</p><p></p>";
    const body = status.querySelector("p:last-child");
    if (body) {
      body.textContent = msg;
    }
    status.style.opacity = "1";
  }

  function renderFromJson(data) {
    const container = document.querySelector(".post-body");
    if (!container) return;
    const html = data && data.html;
    const plain = data && data.plain;
    if (html) {
      container.innerHTML = html;
    } else if (plain) {
      container.textContent = plain;
    }
    if (data && data.title) {
      const base = document.title.split("::")[1];
      document.title = base ? `${data.title} ::${base}` : data.title;
    }
    setStatus("");
  }

  function fetchJson(url) {
    if (!url) return;
    setStatus("본문을 불러오는 중입니다...");
    fetch(url, { cache: "no-store" })
      .then((res) => {
        if (!res.ok) throw new Error(`status ${res.status}`);
        return res.json();
      })
      .then(renderFromJson)
      .catch((err) => {
        console.error(err);
        setStatus("본문을 불러오지 못했습니다. 정적 HTML을 확인하세요.");
      });
  }

  document.addEventListener("DOMContentLoaded", () => {
    const jsonUrl = document.body ? document.body.dataset.jsonUrl : null;
    const title = document.body ? document.body.dataset.postTitle : "";
    const container = document.querySelector(".post-body");
    if (!jsonUrl) return;
    if (hasContent(container)) return;
    setStatus(`정적 HTML이 없어 JSON 폴백을 시도합니다${title ? `: ${title}` : ""}`);
    fetchJson(jsonUrl);
  });
})();
