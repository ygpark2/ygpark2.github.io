(() => {
  const state = {
    posts: [],
    filtered: [],
    tags: [],
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
      // Initialize Materialize select if available
      if (window.M && M.FormSelect) {
        M.FormSelect.init(els.sort);
      }
    }
  }

  function initTags() {
    if (!els.tagSection) return;
    els.tagSection.innerHTML = "";
    if (!state.tags.length) {
      els.tagSection.textContent = "태그가 없습니다.";
      return;
    }
    const heading = createEl("p", "grey-text", "태그 필터");
    els.tagSection.appendChild(heading);
    state.tags.forEach((tag) => {
      const id = `tag-${tag.replace(/[^a-z0-9]/gi, "-")}`;
      const wrapper = createEl("div", "chip tag-chip");
      const checkbox = createEl("input");
      checkbox.type = "checkbox";
      checkbox.id = id;
      checkbox.value = tag;
      checkbox.setAttribute("aria-label", `${tag} 태그 필터`);
      checkbox.addEventListener("change", (e) => {
        if (e.target.checked) state.selectedTags.add(tag);
        else state.selectedTags.delete(tag);
        applyFilters();
      });
      const label = createEl("label", "", tag);
      label.htmlFor = id;
      wrapper.appendChild(checkbox);
      wrapper.appendChild(label);
      els.tagSection.appendChild(wrapper);
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
      const col = createEl("div", "col s12 m6");
      const card = createEl("div", "card hoverable");
      const content = createEl("div", "card-content");
      const title = createEl("span", "card-title");
      const link = createEl("a", "", p.title || "제목 없음");
      link.href = p.url || "#";
      link.setAttribute("aria-label", `${p.title || "제목 없음"} 열기`);
      title.appendChild(link);
      const meta = createEl("p", "grey-text text-darken-1", formatDate(p.date));
      const desc = createEl("p", "", p.excerpt || p.description || "");
      content.appendChild(title);
      content.appendChild(meta);
      content.appendChild(desc);

      if (Array.isArray(p.tags) && p.tags.length) {
        const tagsRow = createEl("div", "section");
        p.tags.forEach((tag) => {
          const chip = createEl("div", "chip", tag);
          chip.setAttribute("role", "text");
          tagsRow.appendChild(chip);
        });
        content.appendChild(tagsRow);
      }

      card.appendChild(content);
      col.appendChild(card);
      frag.appendChild(col);
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
