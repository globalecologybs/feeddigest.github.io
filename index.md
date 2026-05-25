---
layout: default
title: "Global Ecology Digest - Bluesky biodiversity & conservation science"
description: "Fortnightly curated digest of the Bluesky Global Ecology feed: biodiversity, ecosystems, conservation. Terrestrial, freshwater & marine realms."
image: https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg
permalink: /
sitemap:
  changefreq: weekly
  priority: 1.0
---

<style>
.post-row { display: flex; gap: 1rem; align-items: flex-start; margin: 0.5rem 0 1rem 0; }
.post-text { flex: 1 1 auto; min-width: 0; }
.post-image { flex: 0 0 140px; }
.post-image a { display: block; }
.post-image img {
  width: 140px; height: 140px;
  object-fit: cover;
  border-radius: 6px;
  display: block;
  cursor: zoom-in;
  background: #f3f3f3;
  border: 1px solid #eee;
  transition: opacity 0.15s, transform 0.15s;
}
.post-image a:hover img { opacity: 0.9; transform: scale(1.02); }
@media (max-width: 600px) {
  .post-row { flex-direction: column; }
  .post-image img { width: 120px; height: 120px; }
}
.lightbox {
  display: none;
  position: fixed; top: 0; left: 0; right: 0; bottom: 0;
  background: rgba(0,0,0,0.92);
  z-index: 9999;
  padding: 2rem;
  cursor: zoom-out;
  text-align: center;
  text-decoration: none;
}
.lightbox:target { display: flex; align-items: center; justify-content: center; }
.lightbox img {
  max-width: 100%; max-height: 100%;
  width: auto !important; height: auto !important;
  object-fit: contain;
  box-shadow: 0 8px 40px rgba(0,0,0,0.5);
  border-radius: 4px;
  cursor: zoom-out;
}
.tag-row { margin: 0.3rem 0 0.5rem 0; line-height: 1.9; }
.tag {
  display: inline-block;
  font-size: 0.74rem;
  font-weight: 600;
  text-transform: lowercase;
  padding: 2px 8px;
  border-radius: 10px;
  margin-right: 5px;
  background: #eef;
  color: #334;
}
/* -- Ecosystems: marine blues -- */
.tag-marine{background:#ddeef8;color:#0e4d6b}.tag-coral-reef{background:#fde8d8;color:#7a2a0a}
.tag-open-ocean{background:#d0e4f5;color:#0a3358}.tag-coastal{background:#d8f0ea;color:#0a4a38}
/* -- Ecosystems: freshwater teals -- */
.tag-freshwater{background:#e3f6fa;color:#0a5667}.tag-rivers-streams{background:#d8eeec;color:#0a4450}
.tag-lakes{background:#dce8f5;color:#1a3a5a}.tag-wetlands{background:#d8eedd;color:#1a4a2a}
/* -- Ecosystems: terrestrial forests -- */
.tag-tropical-forest{background:#d4edcc;color:#1a4a12}.tag-temperate-forest{background:#dceedd;color:#2a4a1a}
.tag-boreal{background:#dde8df;color:#2a4a2a}
/* -- Ecosystems: other terrestrial -- */
.tag-grassland-savanna{background:#f0e8d0;color:#5a3a10}.tag-shrubland{background:#e8e0d0;color:#4a3820}
.tag-desert-dryland{background:#f5ead8;color:#6a4820}.tag-alpine-mountain{background:#e8e8f2;color:#2a2a5a}
.tag-tundra-arctic{background:#deeef5;color:#1a3a4a}.tag-soil{background:#efe4d4;color:#5b3d18}
.tag-urban{background:#e5e5e8;color:#3a3a4a}
/* -- Organisms -- */
.tag-plants{background:#e4f0d8;color:#2e5612}.tag-animals{background:#f0e4d8;color:#5a3812}
.tag-birds{background:#f5ead8;color:#5a3200}.tag-fish{background:#d8e8f5;color:#1a3a58}
.tag-fungi{background:#ead8f0;color:#4a1a5a}.tag-microbiome{background:#e4daf2;color:#46248a}
.tag-pollinator{background:#fff0c4;color:#7a5300}
/* -- Science topics -- */
.tag-climate{background:#fde8d8;color:#8a3a0d}.tag-invasives{background:#fbe0e0;color:#8c1f1f}
.tag-conservation{background:#d8efe2;color:#1f5e3c}.tag-evolution{background:#f5ecd5;color:#5c3a00}
.tag-macroecology{background:#f0e8d8;color:#5c3a10}.tag-networks{background:#dce8f5;color:#1a3a5e}
.tag-traits{background:#e6efd8;color:#2e4a12}.tag-policy{background:#ece1f4;color:#4c2773}
.tag-ecosystem-services{background:#d8f0e8;color:#1a5a3a}.tag-genetics{background:#f0d8f5;color:#5a1a6a}
.tag-movement{background:#d8e8f5;color:#1a3a5a}.tag-disease{background:#f5d8d8;color:#6a1a1a}
.tag-biogeochemistry{background:#f0e8d0;color:#4a3a10}
/* -- Approach / discipline -- */
.tag-methods{background:#e5e7eb;color:#374151}.tag-modelling{background:#e8e8f2;color:#2a2a5a}
.tag-remote-sensing{background:#d8eef5;color:#1a3a4a}.tag-citizen-science{background:#f5ecd8;color:#4a3a10}
.tag-synthesis{background:#ece8f0;color:#3a2a5a}
/* -- Post types -- */
.tag-jobs{background:#fff3c4;color:#6e5400}.tag-events{background:#fcdef0;color:#7a1c5a}
.tag-data{background:#d8f0ef;color:#1a4a4a}.tag-book{background:#f0ead8;color:#4a3a10}
.tag-opinion{background:#f5e8d8;color:#5a3a10}.tag-preprint{background:#e8f0f5;color:#1a3a5a}
</style>

<div style='width:100%; text-align:center; margin-bottom:20px;'>
  <img src='https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology_banner.png' alt='Global Ecology Banner' style='width:100%; height:auto;'>
</div>

<p style='text-align:center;font-size:0.95rem;margin:0 0 1.3rem;padding-bottom:0.7rem;border-bottom:1px solid #eee;'>
  <a href='/feeddigest.github.io/' style='text-decoration:none;margin:0 0.6rem;'>Home</a>
  &middot;
  <a href='/feeddigest.github.io/archives/' style='text-decoration:none;margin:0 0.6rem;'>Archive</a>
</p>

# Global Ecology Digest

Curated digest of the 🦋 <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> on biodiversity, ecosystems & conservation at large scales. New issue roughly every two weeks.

---

## Digests

<div style='border:1px solid #e5e5e5;border-radius:10px;padding:1.1rem 1.3rem;margin:0.6rem 0 1.1rem;background:#fafbfc;'>
  <div style='font-size:1.15rem;font-weight:700;'>Digest #1</div>
  <div style='color:#666;font-size:0.92rem;margin:0.25rem 0 0.9rem;'>May 11, 2026 &ndash; May 25, 2026 &middot; 29 posts curated</div>
  <a href='/feeddigest.github.io/archives/digest-1/' style='display:inline-block;padding:10px 18px;background:#2d6cdf;color:white;border-radius:6px;text-decoration:none;font-weight:600;'>Read Digest #1 →</a>
</div>

<p><a href='/feeddigest.github.io/archives/' style='display:inline-block;padding:8px 16px;border:1px solid #2d6cdf;color:#2d6cdf;border-radius:6px;text-decoration:none;'>Browse the full archive →</a></p>

---

## Global Ecology ecosystem

<p style='font-size:0.95rem;color:#444;'>Science-only curated digest (publications, data, jobs) from the 🦋 <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> 🌐. Not on BlueSky? <a href='mailto:global.ecology.bs@gmail.com'>Email us</a> to receive updates. On BlueSky? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank' rel='noopener'>@global-ecology.bsky.social</a> to contribute. <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Like &amp; pin the feed</a>. Starter packs: <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank' rel='noopener'>Vol. 1</a>, <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank' rel='noopener'>Vol. 2</a>, <a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>Vol. 3</a>, <a href='https://go.bsky.app/Dsk4TQ3' target='_blank' rel='noopener'>Vol. 4</a>.</p>

---

<p style='font-size:0.95rem;'><a href='/feeddigest.github.io/archives/'>📚 All digests</a></p>

<div style='text-align:center; margin:1.5rem 0;'>
  <a href='https://hits.sh/globalecologybs.github.io/feeddigest.github.io/' target='_blank' rel='noopener'>
    <img alt='Visitor count' src='https://hits.sh/globalecologybs.github.io/feeddigest.github.io.svg?style=flat-square&label=visitors&color=2d6cdf&labelColor=555'>
  </a>
</div>

<div style='text-align:center; font-size:small; color:gray;'>
  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>
</div>

