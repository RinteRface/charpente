/*
Copyright 2015, 2019, 2020 Google LLC. All Rights Reserved.
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 http://www.apache.org/licenses/LICENSE-2.0
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

// Incrementing OFFLINE_VERSION will kick off the install event and force
// previously cached resources to be updated from the network.
const OFFLINE_VERSION = 1;
const CACHE_NAME = "offline";
// Customize this with a different URL if needed.
const OFFLINE_URL = "offline.html";

async function cacheResources() {
  const cache = await caches.open(CACHE_NAME);
  const resources = [
    new Request(OFFLINE_URL, { cache: "reload" }),
    new Request("shinyMobile-2.0.1/dist/shinyMobile.min.css", { cache: "reload" }),
    new Request("shinyMobile-2.0.1/dist/shinyMobile.min.js", { cache: "reload" }),
    new Request("jquery-3.6.0/jquery.min.js", { cache: "reload" }),
    new Request("shiny-javascript-1.8.1.1/shiny.min.js", { cache: "reload" })
  ];

  for (const resource of resources) {
    try {
      await cache.add(resource);
    } catch (error) {
      console.error(`Failed to cache ${resource.url}:`, error);
    }
  }
}

self.addEventListener("install", (event) => {
  event.waitUntil(cacheResources());
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    (async () => {
      // Enable navigation preload if it's supported.
      // See https://developers.google.com/web/updates/2017/02/navigation-preload
      if ("navigationPreload" in self.registration) {
        await self.registration.navigationPreload.enable();
      }
    })()
  );

  // Tell the active service worker to take control of the page immediately.
  self.clients.claim();
});

self.addEventListener("fetch", (event) => {

  // Fix service-worker bug: https://github.com/sveltejs/sapper-template/blob/7e028c825d46f3e633d0378d4d952c9bb1f61068/app/service-worker.js#L61
  if (event.request.cache === 'only-if-cached') return;

  // We only want to call event.respondWith() if this is a navigation request
  // for an HTML page.
  if (event.request.mode === "navigate") {
    event.respondWith(
      (async () => {
        try {
          // First, try to use the navigation preload response if it's supported.
          const preloadResponse = await event.preloadResponse;
          if (preloadResponse) {
            return preloadResponse;
          }

          // Always try the network first.
          const networkResponse = await fetch(event.request);
          return networkResponse;
        } catch (error) {
          // catch is only triggered if an exception is thrown, which is likely
          // due to a network error.
          // If fetch() returns a valid HTTP response with a response code in
          // the 4xx or 5xx range, the catch() will NOT be called.
          console.log("Fetch failed; returning offline page instead.", error);

          const cache = await caches.open(CACHE_NAME);
          const cachedResponse = await cache.match(OFFLINE_URL);
          return cachedResponse;
        }
      })()
    );
  } else {
    // also serve other cached assets (not a navigation request)
    event.respondWith(
      (async () => {

        try {
          // Always try the network first.
          const networkResponse = await fetch(event.request);
          return networkResponse;

        } catch (error) {
          // try to get from the cache
          const cache = await caches.open(CACHE_NAME);
          const cachedResponse = await cache.match(event.request);
          if (cachedResponse) return cachedResponse;

        }
      })()
    );
  }
});
