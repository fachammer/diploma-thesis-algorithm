const putInCache = async (request, response) => {
  const cache = await caches.open("v1");
  await cache.put(request, response);
};

const cacheFirst = async ({ request, fallbackUrl }) => {
  if (false) {
    const responseFromCache = await caches.match(request, {
      ignoreSearch: true,
    });
    if (responseFromCache) {
      return responseFromCache;
    }
  }

  try {
    const responseFromNetwork = await fetch(request);
    putInCache(request, responseFromNetwork.clone());
    return responseFromNetwork;
  } catch (error) {
    const fallbackResponse = await caches.match(fallbackUrl);
    if (fallbackResponse) {
      return fallbackResponse;
    }

    return new Response("Network error happened", {
      status: 408,
      headers: { "Content-Type": "text/plain" },
    });
  }
};

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open("v1")
      .then((cache) =>
        cache.addAll([
          ".",
          "index.html",
          "diploma-thesis.pdf",
          "worker.js",
          "fonts/Roboto-Regular.woff2",
          "pkg/thesis_algorithm_bg.wasm",
          "pkg/thesis_algorithm.js",
        ])
      )
  );
});

self.addEventListener("fetch", (event) => {
  event.respondWith(
    cacheFirst({
      request: event.request,
      fallbackUrl: ".",
    })
  );
});
