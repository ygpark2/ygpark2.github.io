(() => {
  const tocRoot = document.getElementById("post-toc");
  if (!tocRoot) {
    return;
  }

  const content = document.querySelector(".post-body");
  if (!content) {
    tocRoot.remove();
    return;
  }

  const headings = Array.from(content.querySelectorAll("h2, h3, h4"));
  if (!headings.length) {
    const card = tocRoot.closest(".post-toc__card") || tocRoot.parentElement;
    if (card) {
      card.style.display = "none";
    } else {
      tocRoot.style.display = "none";
    }
    return;
  }

  const list = document.createElement("ul");
  list.className = "space-y-2 text-sm";
  headings.forEach((heading, index) => {
    if (!heading.id) {
      heading.id = `toc-${index + 1}`;
    }

    const item = document.createElement("li");
    const link = document.createElement("a");
    const level = Number(heading.tagName.replace("H", "")) || 2;

    link.textContent = heading.textContent?.trim() || `Section ${index + 1}`;
    link.href = `#${heading.id}`;
    link.className = "text-slate-700 hover:text-slate-900";

    item.classList.add(`toc-level-${level}`);
    if (level === 3) {
      item.classList.add("ml-3", "text-xs", "text-slate-500");
    } else if (level >= 4) {
      item.classList.add("ml-6", "text-xs", "text-slate-400");
    }
    item.appendChild(link);
    list.appendChild(item);
  });

  tocRoot.appendChild(list);
})();
