(() => {
  const mapEl = document.getElementById("visited-map");
  const statusEl = document.getElementById("visited-map-status");
  if (!mapEl || !window.L) return;

  const setStatus = (msg) => {
    if (statusEl) statusEl.textContent = msg;
  };

  const map = L.map(mapEl, {
    scrollWheelZoom: false,
  }).setView([36.5, 127.8], 5);

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  }).addTo(map);

  setStatus("방문 데이터 로딩 중...");
  fetch("/assets/data/visited.geojson", { cache: "no-store" })
    .then((res) => {
      if (!res.ok) throw new Error(`status ${res.status}`);
      return res.json();
    })
    .then((data) => {
      const layer = L.geoJSON(data, {
        pointToLayer: (feature, latlng) =>
          L.circleMarker(latlng, {
            radius: 6,
            fillColor: "#f59e0b",
            color: "#b45309",
            weight: 1,
            opacity: 1,
            fillOpacity: 0.9,
          }),
        style: () => ({
          color: "#f59e0b",
          weight: 2,
          fillOpacity: 0.2,
        }),
        onEachFeature: (feature, layer) => {
          const props = feature.properties || {};
          const title = props.title || props.name || "";
          const url = props.url || "";
          const visited = props.visited || "";
          const location = props.location || "";
          if (title || location) {
            const lines = [];
            if (title && url) {
              lines.push(`<a href="${url}">${title}</a>`);
            } else if (title) {
              lines.push(title);
            }
            if (location && location !== title) {
              lines.push(location);
            }
            if (visited) lines.push(`방문: ${visited}`);
            layer.bindPopup(lines.join("<br>"));
          }
        },
      }).addTo(map);

      const bounds = layer.getBounds();
      if (bounds.isValid()) {
        map.fitBounds(bounds.pad(0.2));
      }
      setStatus(`총 ${data.features ? data.features.length : 0}건 표시됨`);
    })
    .catch((err) => {
      console.error(err);
      setStatus("방문 데이터를 불러오지 못했습니다.");
    });
})();
