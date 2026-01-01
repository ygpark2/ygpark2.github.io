(() => {
  const state = {
    posts: [],
    filtered: [],
    tags: [],
    tagCounts: new Map(),
    selectedTags: new Set(),
    query: "",
    sort: "latest",
  };

  const els = {};

  function $(selector) {
    return document.querySelector(selector);
  }

  function createEl(tag, className = "", text = "") {
    const el = document.createElement(tag);
    if (className) el.className = className;
    if (text) el.textContent = text;
    return el;
  }

  function setStatus(msg) {
    if (els.status) els.status.textContent = msg;
  }

  function fetchPosts() {
    setStatus("로딩 중...");
    fetch("/assets/data/posts-index.json", { cache: "no-store" })
      .then((res) => {
        if (!res.ok) throw new Error(`status ${res.status}`);
        return res.json();
      })
      .then((data) => {
        state.posts = Array.isArray(data.posts) ? data.posts : [];
        state.tags = dedupeTags(state.posts);
        state.tagCounts = getTagCounts(state.posts);
        initTags();
        applyFilters();
        setStatus(`총 ${state.posts.length}건 로드됨`);
      })
      .catch((err) => {
        console.error(err);
        setStatus("데이터를 불러오지 못했습니다. 잠시 후 다시 시도하세요.");
      });
  }

  function dedupeTags(posts) {
    const set = new Set();
    posts.forEach((p) => (p.tags || []).forEach((t) => set.add(t)));
    return Array.from(set).sort();
  }

  function getTagCounts(posts) {
    const counts = new Map();
    posts.forEach((p) => {
      (p.tags || []).forEach((tag) => {
        counts.set(tag, (counts.get(tag) || 0) + 1);
      });
    });
    return counts;
  }

  function getTagFontSize(tag) {
    if (!state.tagCounts.size) return "0.95rem";
    const values = Array.from(state.tagCounts.values());
    const min = Math.min(...values);
    const max = Math.max(...values);
    if (min === max) return "0.95rem";
    const count = state.tagCounts.get(tag) || min;
    const ratio = (count - min) / (max - min);
    const size = 0.85 + ratio * 0.6;
    return `${size.toFixed(2)}rem`;
  }

  function getTagTone(tag) {
    if (!state.tagCounts.size) {
      return { weight: "600", color: "text-slate-600" };
    }
    const values = Array.from(state.tagCounts.values());
    const min = Math.min(...values);
    const max = Math.max(...values);
    if (min === max) {
      return { weight: "600", color: "text-slate-600" };
    }
    const count = state.tagCounts.get(tag) || min;
    const ratio = (count - min) / (max - min);
    const weight = ratio > 0.66 ? "800" : ratio > 0.33 ? "700" : "600";
    const color = ratio > 0.66 ? "text-sky-700" : ratio > 0.33 ? "text-sky-600" : "text-slate-600";
    return { weight, color };
  }

  function initControls() {
    els.search = $("#search-input");
    els.sort = $("#sort-select");
    els.tagSection = $("#tag-filter-section");
    els.postsContainer = $("#posts-container");
    els.status = $("#status");

    if (els.search) {
      els.search.addEventListener("input", (e) => {
        state.query = e.target.value || "";
        applyFilters();
      });
    }

    if (els.sort) {
      els.sort.addEventListener("change", (e) => {
        state.sort = e.target.value;
        applyFilters();
      });
    }
  }

  function initTags() {
    if (!els.tagSection) return;
    els.tagSection.innerHTML = "";
    if (!state.tags.length) {
      els.tagSection.textContent = "태그가 없습니다.";
      return;
    }
    const heading = createEl("p", "tag-heading", "태그 필터");
    els.tagSection.appendChild(heading);
    state.tags.forEach((tag) => {
      const tone = getTagTone(tag);
      const button = createEl(
        "button",
        `btn btn-sm rounded-full border border-slate-200 bg-white px-3 py-1 text-sm font-semibold shadow-sm transition hover:border-sky-300 hover:text-sky-700 ${tone.color}`,
        tag
      );
      button.style.fontWeight = tone.weight;
      button.style.fontSize = getTagFontSize(tag);
      button.type = "button";
      button.setAttribute("aria-pressed", "false");
      button.setAttribute("aria-label", `${tag} 태그 필터`);
      button.addEventListener("click", () => {
        if (state.selectedTags.has(tag)) {
          state.selectedTags.delete(tag);
          button.classList.remove("is-selected", "border-sky-500", "bg-sky-100", "text-sky-800", "shadow-sm");
          button.setAttribute("aria-pressed", "false");
        } else {
          state.selectedTags.add(tag);
          button.classList.add("is-selected", "border-sky-500", "bg-sky-100", "text-sky-800", "shadow-sm");
          button.setAttribute("aria-pressed", "true");
        }
        applyFilters();
      });
      els.tagSection.appendChild(button);
    });
  }

  function applyFilters() {
    const q = state.query.trim().toLowerCase();
    const hasQuery = q.length > 0;
    const selected = state.selectedTags;

    state.filtered = state.posts.filter((p) => {
      const matchesQuery = !hasQuery
        || (p.title && p.title.toLowerCase().includes(q))
        || (p.description && p.description.toLowerCase().includes(q))
        || (p.excerpt && p.excerpt.toLowerCase().includes(q))
        || (p.plain && p.plain.toLowerCase().includes(q))
        || (Array.isArray(p.tags) && p.tags.some((t) => t.toLowerCase().includes(q)));

      const matchesTags =
        selected.size === 0 ||
        (Array.isArray(p.tags) && Array.from(selected).every((t) => p.tags.includes(t)));

      return matchesQuery && matchesTags;
    });

    sortFiltered();
    renderPosts();
  }

  function sortFiltered() {
    const sort = state.sort;
    state.filtered.sort((a, b) => {
      if (sort === "title") {
        return (a.title || "").localeCompare(b.title || "");
      }
      if (sort === "tag") {
        const at = (a.tags && a.tags[0]) || "";
        const bt = (b.tags && b.tags[0]) || "";
        return at.localeCompare(bt);
      }
      // "popular" falls back to date desc (latest)
      return new Date(b.date || 0) - new Date(a.date || 0);
    });
  }

  function renderPosts() {
    if (!els.postsContainer) return;
    els.postsContainer.innerHTML = "";
    if (!state.filtered.length) {
      els.postsContainer.textContent = "결과가 없습니다.";
      setStatus("검색 결과 없음");
      return;
    }
    setStatus(`총 ${state.filtered.length}건`);
    const frag = document.createDocumentFragment();
    state.filtered.forEach((p) => {
      const article = createEl(
        "article",
        "rounded-2xl border border-slate-200 bg-white px-5 py-4 shadow-sm"
      );
      const title = createEl("h2", "text-lg font-semibold text-slate-900");
      const link = createEl("a", "", p.title || "제목 없음");
      link.href = p.url || "#";
      link.setAttribute("aria-label", `${p.title || "제목 없음"} 열기`);
      link.className = "hover:text-amber-600";
      title.appendChild(link);

      article.appendChild(title);

      const tagsRow = createEl("div", "mt-2 flex flex-wrap justify-end gap-2");
      if (Array.isArray(p.tags) && p.tags.length) {
        p.tags.forEach((tag) => {
          const chip = createEl(
            "span",
            "rounded-md border border-slate-200 bg-slate-50 px-2 py-1 text-[11px] font-semibold text-slate-600",
            tag
          );
          tagsRow.appendChild(chip);
        });
      }
      article.appendChild(tagsRow);

      frag.appendChild(article);
    });
    els.postsContainer.appendChild(frag);
  }

  function formatDate(dateStr) {
    if (!dateStr) return "";
    const date = new Date(dateStr);
    if (Number.isNaN(date.getTime())) return dateStr;
    return date.toLocaleDateString("ko-KR");
  }

  document.addEventListener("DOMContentLoaded", () => {
    initControls();
    fetchPosts();
  });
})();
