---
layout: default
title: "Global Ecology Digest #1 - May 12 to May 26, 2026"
description: "31 curated posts from the Bluesky Global Ecology feed (May 12 - May 26, 2026): biodiversity, ecosystems, conservation -- terrestrial, freshwater & marine."
date: 2026-05-26
year: "2026"
digest_num: 1
image: https://github.com/globalecologybs/feeddigest.github.io/raw/main/global_ecology.jpg
permalink: /archives/digest-1/
sitemap:
  changefreq: monthly
  priority: 0.6
---

<style>
html, body { overflow-x: hidden; max-width: 100%; }
.post-row { display: flex; gap: 1rem; align-items: flex-start; margin: 0.5rem 0 1rem 0; max-width: 100%; }
.post-text { flex: 1 1 auto; min-width: 0; word-break: break-word; overflow-wrap: break-word; }
.post-image { flex: 0 0 140px; max-width: 140px; }
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
@media (max-width: 700px) {
  .post-row { flex-direction: column; }
  .post-image { max-width: 100%; }
  .post-image img { width: 120px; height: 120px; max-width: 100%; }
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

<p style='font-size:0.95rem;color:#444;'>Science-only curated digest (publications, data, jobs) from the 🦋 <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Bluesky Global Ecology feed</a> 🌐. Not on BlueSky? <a href='mailto:global.ecology.bs@gmail.com'>Email us</a> to receive updates. On BlueSky? DM <a href='https://bsky.app/profile/global-ecology.bsky.social' target='_blank' rel='noopener'>@global-ecology.bsky.social</a> to contribute. <a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/feed/global.ecology' target='_blank' rel='noopener'>Like &amp; pin the feed</a>. Starter packs: <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3lfum2bjpab24' target='_blank' rel='noopener'>Vol. 1</a>, <a href='https://bsky.app/starter-pack/nmouquet.bsky.social/3ld2m2csaai2x' target='_blank' rel='noopener'>Vol. 2</a>, <a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>Vol. 3</a>, <a href='https://go.bsky.app/Dsk4TQ3' target='_blank' rel='noopener'>Vol. 4</a>.</p>

# Digest #1

Feeds are from **May 12, 2026** to **May 26, 2026**. Total posts: **31**.

In this digest, climate-driven change features prominently across ecosystems. [Seagrass mass flowering](#post-2) in the Mediterranean followed 2022 marine heatwaves, while [invasive seaweeds](#post-7) may functionally compensate for endemic macroalgal losses under warming. [Alpine shrubification](#post-29) is documented using GLORIA network data, and [plant functional strategies](#post-6) in European grasslands shift under climate projections. [Tree diversity lowers soil carbon temperature sensitivity](#post-11) through microbial stabilization, and [soil microbiome indicators](#post-30) inform ecosystem health assessments. [Bumblebee phylogenomics](#post-12) advances understanding of pollinator macroecology, and [metawebs](#post-9) are proposed as a macroecological modelling framework. [Home range scaling](#post-27) with population density and [functional diversity scaling laws](#post-23) extend macroecological theory. [Toadfish symbiotic bacteria](#post-26) are implicated in carbonate precipitation, [chemodiversity emergent properties](#post-31) are examined for ecological significance, and [marine ecosystem ecology reviews](#post-13) synthesise recent advances. [Biological control stability](#post-25) is enhanced in strip cropping systems, [cultural legacies shape wildlife values](#post-15), and [aquatic eDNA](#post-1) is positioned within global biodiversity policy alongside [Antarctic governance](#post-8) and [AI in conservation science](#post-14). [Origin-of-life perspectives](#post-21) inform microbial evolutionary ecology, and a [mountain research network anniversary](#post-4) marks longstanding alpine collaboration.

Methodological contributions include an [R package for spatial data subdivision](#post-3), a [practical guide to Frescalo trend detection](#post-10), [chemical timber tracing](#post-17) combining tree-genera data, a [taxon-stratified GBIF sampling-effort dataset](#post-22) for bias-aware species distribution models, and a framework for [reproducible biodiversity science infrastructure](#post-19).

A [postdoctoral position in vegetation mapping](#post-16) is advertised at the Greenland Institute of Natural Resources, a [biological invasions webinar](#post-5) connects OneSTOP and GuardIAS initiatives, a [biogeography seminar](#post-28) features Michael Landis, a [botany atlas book event](#post-24) is announced, [tropical forest trait workshops](#post-18) are offered through FRB-CESAB, and [Global Ecology Vol. 3](#post-20) is available as a macroecological reference volume.

Many thanks to all who contribute to the Global Ecology feed by sharing their science on Bluesky.

---

<div id='post-1'></div>

##### 📄 Utilizing aquatic environmental DNA to address global biodiversity targets

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Florian Altermatt** <a href='https://bsky.app/profile/florianaltermatt.bsky.social' target='_blank' rel='noopener'>@florianaltermatt.bsky.social</a> &middot; <time datetime='2026-05-25'>2026-05-25</time> &middot; 💚 6 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-freshwater'>freshwater</span><span class='tag tag-policy'>policy</span></div>
    {% raw %}A year ago we published our perspective "Utilizing aquatic environmental DNA 🧬 to address global 🌍 🌐 biodiversity 🦐 🐟 🦠 targets 🎯 " in Nature Reviews Biodiversity: 1/10🧵{% endraw %}
<br><b>link:</b> <a href='https://www.nature.com/articles/s44358-025-00044-x' target='_blank' rel='noopener'>https://www.nature.com/articles/s44358-025-00044-x</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:ydzmya3tppgd3h2udjzu4iph/post/3mmoilvc2jk2y' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-949a5629f6' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:ydzmya3tppgd3h2udjzu4iph/bafkreibo3n5yv7t7ttatmu7xiuugtdddd5tviyecypo57lrclzkjgjp4f4' alt='A year ago we published our perspective &quot;Utilizing aquatic environmental DNA 🧬 t' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-949a5629f6' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:ydzmya3tppgd3h2udjzu4iph/bafkreibo3n5yv7t7ttatmu7xiuugtdddd5tviyecypo57lrclzkjgjp4f4' alt='A year ago we published our perspective &quot;Utilizing aquatic environmental DNA 🧬 t'>
</a>

---

<div id='post-2'></div>

##### 📄 Mass flowering of the seagrass Posidonia oceanica after 2022 record-breaking marine heatwaves, a Pan-Mediterranean study

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Jérémy Carlot** <a href='https://bsky.app/profile/jerem-carlot.bsky.social' target='_blank' rel='noopener'>@jerem-carlot.bsky.social</a> &middot; <time datetime='2026-05-25'>2026-05-25</time> &middot; 💚 10 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-coastal'>coastal</span><span class='tag tag-climate'>climate</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Our study is now out in @commsearth.nature.com. 2022 heatwaves triggered a record mass flowering of Mediterranean Posidonia oceanica. 🌊 A sign of resilience or a red flag for ocean health? The answer here: Study led by Patrick Austruch 🌱 🌐{% endraw %}
<br><b>link:</b> <a href='https://doi.org/10.1038/s43247-026-03631-8' target='_blank' rel='noopener'>https://doi.org/10.1038/s43247-026-03631-8</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:geytktsrkn74y3b6lpwszt66/post/3mmogrfiwjc2d' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-bedc98f80e' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:geytktsrkn74y3b6lpwszt66/bafkreignsvqothuu54whi7yktavnhurn3q2mnnnqwnj3gyqk434kkgm7uy' alt='Our study is now out in @commsearth.nature.com. 2022 heatwaves triggered a recor' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-bedc98f80e' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:geytktsrkn74y3b6lpwszt66/bafkreignsvqothuu54whi7yktavnhurn3q2mnnnqwnj3gyqk434kkgm7uy' alt='Our study is now out in @commsearth.nature.com. 2022 heatwaves triggered a recor'>
</a>

---

<div id='post-3'></div>

##### 📄 Hierarchical Spatial Data Subdivision into Topologically Contiguous Units [R package hespdiv version 1.2.10]

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Andrej Spiridonov** <a href='https://bsky.app/profile/andrejpaleo.bsky.social' target='_blank' rel='noopener'>@andrejpaleo.bsky.social</a> &middot; <time datetime='2026-05-24'>2026-05-24</time> &middot; 💚 13 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-methods'>methods</span><span class='tag tag-macroecology'>macroecology</span><span class='tag tag-data'>data</span></div>
    {% raw %}The R package 'hespdiv' is now in the CRAN! 🥳 Now automatic hierarchical and statistically testable contiguous bioregionalization is available for everyone! Bretskyan_hiearchy 🧪⚒️ EvoBio Paleobio Geology Biogeography Macroecology{% endraw %}
<br><b>link:</b> <a href='https://cran.rstudio.com/web/packages/hespdiv/index.html' target='_blank' rel='noopener'>https://cran.rstudio.com/web/packages/hespdiv/index.html</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:qb6x3o2dt7vkteqeqrqpjtxc/post/3mmmq6rpp4c2p' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-193d97204f' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:qb6x3o2dt7vkteqeqrqpjtxc/bafkreidfuh7okpgysjvqx5kw6nwaa4ypbgajyvw7g6gr2jdqoehivp4j6e' alt='The R package &#39;hespdiv&#39; is now in the CRAN! 🥳 Now automatic hierarchical and sta' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-193d97204f' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:qb6x3o2dt7vkteqeqrqpjtxc/bafkreidfuh7okpgysjvqx5kw6nwaa4ypbgajyvw7g6gr2jdqoehivp4j6e' alt='The R package &#39;hespdiv&#39; is now in the CRAN! 🥳 Now automatic hierarchical and sta'>
</a>

---

<div id='post-4'></div>

##### 📄 A beautiful birthday

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Jonas Lembrechts** <a href='https://bsky.app/profile/jlembrechts.bsky.social' target='_blank' rel='noopener'>@jlembrechts.bsky.social</a> &middot; <time datetime='2026-05-24'>2026-05-24</time> &middot; 💚 13 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-alpine-mountain'>alpine-mountain</span><span class='tag tag-networks'>networks</span></div>
    {% raw %}The Mountain Invasion Research Network (MIREN) just turned 20! 🎉 We celebrate with a new paper in Biol Invasions on what 20 years of mountain monitoring taught us - and how ecological networks can survive (and thrive). Blog: Paper:{% endraw %}
<br><b>link:</b> <a href='https://the3dlab.org/2026/05/24/a-beautiful-birthday/' target='_blank' rel='noopener'>https://the3dlab.org/2026/05/24/a-beautiful-birthday/</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:3fbqr6o45njrwteegpbwuzir/post/3mmmokkjq6s2u' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-d09e159904' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:3fbqr6o45njrwteegpbwuzir/bafkreif6wmpg4py7yej6e4is2lkpl56cnn3itxcmcksp4qqc5no7c4unne' alt='The Mountain Invasion Research Network (MIREN) just turned 20! 🎉 We celebrate wi' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-d09e159904' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:3fbqr6o45njrwteegpbwuzir/bafkreif6wmpg4py7yej6e4is2lkpl56cnn3itxcmcksp4qqc5no7c4unne' alt='The Mountain Invasion Research Network (MIREN) just turned 20! 🎉 We celebrate wi'>
</a>

---

<div id='post-5'></div>

##### 📄 WEBINAR: Leading the global response to biological invasions: A dialogue between OneSTOP & GuardIAS

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Stelios Katsanevakis** <a href='https://bsky.app/profile/skatsanevakis.bsky.social' target='_blank' rel='noopener'>@skatsanevakis.bsky.social</a> &middot; <time datetime='2026-05-24'>2026-05-24</time> &middot; 💚 2 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-policy'>policy</span></div>
    {% raw %}The GuardIAS - OneStop webinar is now available on YouTube. The two projects were presented by me and @lorilawson.bsky.social, followed by engaging discussions on biosecurity and InvasiveSpecies management, moderated by @sgomezmaldonado.bsky.social. 👉 youtu.be/bC1DOp5Wt2o bioinvasions 🌍🌐🧪{% endraw %}
<br><b>link:</b> <a href='https://youtu.be/bC1DOp5Wt2o' target='_blank' rel='noopener'>https://youtu.be/bC1DOp5Wt2o</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:7fpeunfp74zwh6keq63zmdgt/post/3mmmjf46oz22s' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-6e2df3c800' aria-label='Enlarge image'>
      <img src='https://i.ytimg.com/vi/bC1DOp5Wt2o/maxresdefault.jpg' alt='The GuardIAS - OneStop webinar is now available on YouTube. The two projects wer' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-6e2df3c800' aria-label='Close enlarged image'>
  <img src='https://i.ytimg.com/vi/bC1DOp5Wt2o/maxresdefault.jpg' alt='The GuardIAS - OneStop webinar is now available on YouTube. The two projects wer'>
</a>

---

<div id='post-6'></div>

##### 📄 Plant strategy distributions in European grasslands under climate change <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Milan Chytrý** <a href='https://bsky.app/profile/milanchytry.bsky.social' target='_blank' rel='noopener'>@milanchytry.bsky.social</a> &middot; <time datetime='2026-05-24'>2026-05-24</time> &middot; 💚 16 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-traits'>traits</span><span class='tag tag-climate'>climate</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Our new study, led by Xiao-Peng Tan, maps the distribution of plant strategies across European grasslands and projects how current patterns will be affected by ongoing climate change. Open-access article: @ecography.bsky.social{% endraw %}
<br><b>link:</b> <a href='https://doi.org/10.1002/ecog.08334' target='_blank' rel='noopener'>https://doi.org/10.1002/ecog.08334</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:hm2e3dgvcx4jjk5iqcriel3v/post/3mmlvbojdoc2c' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-32258b9f9f' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:hm2e3dgvcx4jjk5iqcriel3v/bafkreictvyto3ax66i4rmoxrtih2ys4s7ci4d3xwpnhvz57cagr6onuc7q' alt='Our new study, led by Xiao-Peng Tan, maps the distribution of plant strategies a' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-32258b9f9f' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:hm2e3dgvcx4jjk5iqcriel3v/bafkreictvyto3ax66i4rmoxrtih2ys4s7ci4d3xwpnhvz57cagr6onuc7q' alt='Our new study, led by Xiao-Peng Tan, maps the distribution of plant strategies a'>
</a>

---

<div id='post-7'></div>

##### 📄 Invasive seaweeds may functionally compensate for endemic species loss in the warming Mediterranean Sea <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Annals of Botany** <a href='https://bsky.app/profile/annbot.bsky.social' target='_blank' rel='noopener'>@annbot.bsky.social</a> &middot; <time datetime='2026-05-24'>2026-05-24</time> &middot; 💚 2 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-coastal'>coastal</span><span class='tag tag-climate'>climate</span><span class='tag tag-traits'>traits</span></div>
    {% raw %}🔥ADVANCE ACCESS🔥: Invasive seaweeds may functionally compensate for the expected loss of endemic temperate species in the fast-warming Mediterranean Sea PlantScience{% endraw %}
<br><b>link:</b> <a href='https://doi.org/10.1093/aob/mcag105' target='_blank' rel='noopener'>https://doi.org/10.1093/aob/mcag105</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:qowz7hsvbml54nxg6hzo7rrg/post/3mmlt73atr32w' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-92fba70e55' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:qowz7hsvbml54nxg6hzo7rrg/bafkreicxd3qwnp7sdhte4p7iiubuceyzu7vxmr26nky4z23kowcv5eisge' alt='🔥ADVANCE ACCESS🔥: Invasive seaweeds may functionally compensate for the expected' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-92fba70e55' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:qowz7hsvbml54nxg6hzo7rrg/bafkreicxd3qwnp7sdhte4p7iiubuceyzu7vxmr26nky4z23kowcv5eisge' alt='🔥ADVANCE ACCESS🔥: Invasive seaweeds may functionally compensate for the expected'>
</a>

---

<div id='post-8'></div>

##### 📄 Antarctic Treaty System needs a disaster management authority for the continent <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Proceedings of the National Academy of Sciences** <a href='https://bsky.app/profile/pnas.org' target='_blank' rel='noopener'>@pnas.org</a> &middot; <time datetime='2026-05-23'>2026-05-23</time> &middot; 💚 6 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-policy'>policy</span><span class='tag tag-climate'>climate</span></div>
    {% raw %}Opinion: Antarctica and the scientists working there aren’t prepared for disasters on the remote continent. The AntarcticTreatySystem needs a disaster management authority to address biological & physical threats. https://ow.ly/NySP50Z3fZE biosecurity ClimateChange{% endraw %}
<br><b>link:</b> <a href='https://ow.ly/NySP50Z3fZE' target='_blank' rel='noopener'>https://ow.ly/NySP50Z3fZE</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:cvhduyk4ukre2drmboh2z3mi/post/3mmjweuzmbi2t' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-a9bbe59360' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:cvhduyk4ukre2drmboh2z3mi/bafkreig2tiunx5t2tvqk7lbowzg5smnlsyizkahjogr2l5pyz7qhqyrtti' alt='Opinion: Antarctica and the scientists working there aren’t prepared for disaste' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-a9bbe59360' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:cvhduyk4ukre2drmboh2z3mi/bafkreig2tiunx5t2tvqk7lbowzg5smnlsyizkahjogr2l5pyz7qhqyrtti' alt='Opinion: Antarctica and the scientists working there aren’t prepared for disaste'>
</a>

---

<div id='post-9'></div>

##### 📄 Metawebs as an ecological modeling framework in macroecology and biogeography <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Vinicius Bastazini** <a href='https://bsky.app/profile/vinibastazini.bsky.social' target='_blank' rel='noopener'>@vinibastazini.bsky.social</a> &middot; <time datetime='2026-05-23'>2026-05-23</time> &middot; 💚 18 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Our new study synthesizes the role of metawebs as an ecological modeling framework in macroecology and biogeography, outlining their applications, limitations, and future directions. 🧪🦤🌎🌐 biodiversity ecology NetSci conservation ecologicalnetworks{% endraw %}
<br><b>link:</b> <a href='https://www.sciencedirect.com/science/article/pii/S0304380026002024?via%3Dihub' target='_blank' rel='noopener'>https://www.sciencedirect.com/science/article/pii/S0304380026002024?via%3Dihub</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:vb4z72agepotbk4sbdl5dfrd/post/3mmju72ymus2p' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-7384398aeb' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:vb4z72agepotbk4sbdl5dfrd/bafkreidnbmofoconf64tygkyfnphpq35ljffpnuvwazwgj4wnbyaz3qlym' alt='Our new study synthesizes the role of metawebs as an ecological modeling framewo' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-7384398aeb' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:vb4z72agepotbk4sbdl5dfrd/bafkreidnbmofoconf64tygkyfnphpq35ljffpnuvwazwgj4wnbyaz3qlym' alt='Our new study synthesizes the role of metawebs as an ecological modeling framewo'>
</a>

---

<div id='post-10'></div>

##### 📄 A practical guide to species trend detection using Frescalo local frequency scaling <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Pablo Garcia-Diaz** <a href='https://bsky.app/profile/pablo-ecology.bsky.social' target='_blank' rel='noopener'>@pablo-ecology.bsky.social</a> &middot; <time datetime='2026-05-22'>2026-05-22</time> &middot; 💚 6 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-methods'>methods</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}A practical guide to species trend detection with unstructured data using local frequency scaling (Frescalo) 🌐🌎🧪{% endraw %}
<br><b>link:</b> <a href='https://nsojournals.onlinelibrary.wiley.com/doi/10.1002/ecog.08270' target='_blank' rel='noopener'>https://nsojournals.onlinelibrary.wiley.com/doi/10.1002/ecog.08270</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/post/3mmh7mqexq222' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-a0fc5b7e82' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreihhetldy2safhobdm6yo7vanjn6lh7c4d7agqt3fxok43xg2gcr6q' alt='A practical guide to species trend detection with unstructured data using local ' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-a0fc5b7e82' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreihhetldy2safhobdm6yo7vanjn6lh7c4d7agqt3fxok43xg2gcr6q' alt='A practical guide to species trend detection with unstructured data using local '>
</a>

---

<div id='post-11'></div>

##### 📄 Greater tree diversity lowers soil carbon temperature sensitivity via microbial stabilization mechanisms <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Journal of Ecology** <a href='https://bsky.app/profile/journalofecology.bsky.social' target='_blank' rel='noopener'>@journalofecology.bsky.social</a> &middot; <time datetime='2026-05-22'>2026-05-22</time> &middot; 💚 15 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-temperate-forest'>temperate-forest</span><span class='tag tag-soil'>soil</span><span class='tag tag-microbiome'>microbiome</span><span class='tag tag-climate'>climate</span><span class='tag tag-biogeochemistry'>biogeochemistry</span><span class='tag tag-traits'>traits</span></div>
    {% raw %}🌳 Greater tree diversity lowers soil carbon Q₁₀ by enhancing carbon stabilization and shifting microbial strategies. Diverse forests help lock away carbon and reduce climate feedbacks 👉️ buff.ly/DcNVNNW{% endraw %}
<br><b>link:</b> <a href='https://buff.ly/DcNVNNW' target='_blank' rel='noopener'>https://buff.ly/DcNVNNW</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:wkytvpjriqhr7q54cr4wqk7t/post/3mmh3w4sd2223' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-971e9b25d0' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:wkytvpjriqhr7q54cr4wqk7t/bafkreidmhmg3q67lu6sza7cubpkvtyd64fhll3zons6sj37ocmg54a2ow4' alt='🌳 Greater tree diversity lowers soil carbon Q₁₀ by enhancing carbon stabilizatio' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-971e9b25d0' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:wkytvpjriqhr7q54cr4wqk7t/bafkreidmhmg3q67lu6sza7cubpkvtyd64fhll3zons6sj37ocmg54a2ow4' alt='🌳 Greater tree diversity lowers soil carbon Q₁₀ by enhancing carbon stabilizatio'>
</a>

---

<div id='post-12'></div>

##### 📄 Comprehensive evolutionary phylogeny of global bumblebee species integrating nuclear, mitochondrial, and genomic data <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Paul Williams** <a href='https://bsky.app/profile/paulwilliamsnhm.bsky.social' target='_blank' rel='noopener'>@paulwilliamsnhm.bsky.social</a> &middot; <time datetime='2026-05-22'>2026-05-22</time> &middot; 💚 38 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}New evolutionary tree for all of the world's bumblebee species, from (1) slow-evolving nuclear genes, (2) fast-evolving mitochondrial genes, and (3) results from genomic data, to provide a complete starting point for comparative studies of all bumblebees{% endraw %}
<br><b>link:</b> <a href='https://www.researchgate.net/publication/405148391_Evolutionary_Tree_for_All_Bumblebee_Species_World-Wide_Estimated_by_Combining_Information_from_Fast-Evolving_Genes_Slow-Evolving_Genes_and_Genomic_Data_Apidae_Bombus' target='_blank' rel='noopener'>https://www.researchgate.net/publication/405148391_Evolutionary_Tree_for_All_Bumblebee_Species_World-Wide_Estimated_by_Combining_Information_from_Fast-Evolving_Genes_Slow-Evolving_Genes_and_Genomic_Data_Apidae_Bombus</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:rp5kuopusr2duaiy6ifrjaua/post/3mmgz5vg6ds2h' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-24a17739df' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rp5kuopusr2duaiy6ifrjaua/bafkreihdv4pqg6ateelugm24leqttfdl2jaf3gppfeylvy5so2vciok3da' alt='New evolutionary tree for all of the world&#39;s bumblebee species, from (1) slow-ev' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-24a17739df' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:rp5kuopusr2duaiy6ifrjaua/bafkreihdv4pqg6ateelugm24leqttfdl2jaf3gppfeylvy5so2vciok3da' alt='New evolutionary tree for all of the world&#39;s bumblebee species, from (1) slow-ev'>
</a>

---

<div id='post-13'></div>

##### 📄 Frontiers | Reviews in Marine Ecosystem Ecology: 2026

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Stelios Katsanevakis** <a href='https://bsky.app/profile/skatsanevakis.bsky.social' target='_blank' rel='noopener'>@skatsanevakis.bsky.social</a> &middot; <time datetime='2026-05-21'>2026-05-21</time> &middot; 💚 8 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-marine'>marine</span><span class='tag tag-synthesis'>synthesis</span></div>
    {% raw %}Excited to co-edit the Research Topic “Reviews in Marine Ecosystem Ecology: 2026” in Frontiers in Marine Science! We invite high-quality review papers on MarineBiodiversity, ecosystem functioning, advances in monitoring, CumulativeImpacts & more. 👉 🌊🐟🪸🌐🌍{% endraw %}
<br><b>link:</b> <a href='https://www.frontiersin.org/research-topics/80087/reviews-in-marine-ecosystem-ecology-2026' target='_blank' rel='noopener'>https://www.frontiersin.org/research-topics/80087/reviews-in-marine-ecosystem-ecology-2026</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:7fpeunfp74zwh6keq63zmdgt/post/3mmf5g5bo6k25' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-e11810fe6b' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:7fpeunfp74zwh6keq63zmdgt/bafkreiey5ajrj7uclelm5kworoajq7jkn4iccrebp7ihrcc7etg7vvsgmm' alt='Excited to co-edit the Research Topic “Reviews in Marine Ecosystem Ecology: 2026' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-e11810fe6b' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:7fpeunfp74zwh6keq63zmdgt/bafkreiey5ajrj7uclelm5kworoajq7jkn4iccrebp7ihrcc7etg7vvsgmm' alt='Excited to co-edit the Research Topic “Reviews in Marine Ecosystem Ecology: 2026'>
</a>

---

<div id='post-14'></div>

##### 📄 Safeguarding human roles in conservation science in the age of AI <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Pablo Garcia-Diaz** <a href='https://bsky.app/profile/pablo-ecology.bsky.social' target='_blank' rel='noopener'>@pablo-ecology.bsky.social</a> &middot; <time datetime='2026-05-21'>2026-05-21</time> &middot; 💚 6 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-policy'>policy</span></div>
    {% raw %}Safeguarding the role of humans in conservation science in the age of AI 🌎🌐🧪{% endraw %}
<br><b>link:</b> <a href='https://www.sciencedirect.com/science/article/pii/S0006320726002041' target='_blank' rel='noopener'>https://www.sciencedirect.com/science/article/pii/S0006320726002041</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/post/3mmf4ddrknc2q' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-0a8244754e' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreiajr3ovgfmrwbwd7lnmq6qq3rnrleflp62wltgwdtmfutgbjrdn7y' alt='Safeguarding the role of humans in conservation science in the age of AI 🌎🌐🧪' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-0a8244754e' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreiajr3ovgfmrwbwd7lnmq6qq3rnrleflp62wltgwdtmfutgbjrdn7y' alt='Safeguarding the role of humans in conservation science in the age of AI 🌎🌐🧪'>
</a>

---

<div id='post-15'></div>

##### 📄 Enduring cultural legacies affect Euro-American wildlife values

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Pablo Garcia-Diaz** <a href='https://bsky.app/profile/pablo-ecology.bsky.social' target='_blank' rel='noopener'>@pablo-ecology.bsky.social</a> &middot; <time datetime='2026-05-21'>2026-05-21</time> &middot; 💚 7 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-policy'>policy</span></div>
    {% raw %}Enduring cultural legacies affect Euro-American wildlife values 🌎🌐🧪{% endraw %}
<br><b>link:</b> <a href='https://www.nature.com/articles/s41893-026-01825-8' target='_blank' rel='noopener'>https://www.nature.com/articles/s41893-026-01825-8</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/post/3mmf422rrkc27' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-463d8416eb' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreiafv7yxiiieljzxlguslvnysn4lv5q4ejsu7rucm3zaduegnvrmoy' alt='Enduring cultural legacies affect Euro-American wildlife values 🌎🌐🧪' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-463d8416eb' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jvl2oy4s2hpizxkbd7ngq7ty/bafkreiafv7yxiiieljzxlguslvnysn4lv5q4ejsu7rucm3zaduegnvrmoy' alt='Enduring cultural legacies affect Euro-American wildlife values 🌎🌐🧪'>
</a>

---

<div id='post-16'></div>

##### 📄 Greenland Institute of Natural Resources seeks a Postdoctoral Researcher with experience in vegetation mapping and herbivory.

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Brian J. Enquist** <a href='https://bsky.app/profile/bjenquist.bsky.social' target='_blank' rel='noopener'>@bjenquist.bsky.social</a> &middot; <time datetime='2026-05-20'>2026-05-20</time> &middot; 💚 15 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-jobs'>jobs</span><span class='tag tag-traits'>traits</span></div>
    {% raw %}Looks like a fantastic postdoc...deadline soon 'Greenland Institute of Natural Resources seeks a Postdoctoral Researcher with experience in vegetation mapping and herbivory' Drones, traits and Greenland PostDocJobs 🧪🌐🌾{% endraw %}
<br><b>link:</b> <a href='https://naalakkersuisut.emply.net/recruitment/vacancyAd.aspx?publishingId=52944607-ef05-471b-b36c-5cd4ba2071d3&languageKey=en-GB' target='_blank' rel='noopener'>https://naalakkersuisut.emply.net/recruitment/vacancyAd.aspx?publishingId=52944607-ef05-471b-b36c-5cd4ba2071d3&languageKey=en-GB</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:rca6ykp24ubm3alkgmcblslc/post/3mmcg3sstds2b' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-19f6c1c5dd' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreigikl2pzb6nbw3ajbvhspulary6zlz2ehhulpkdnim55rpbzt4pxy' alt='Looks like a fantastic postdoc...deadline soon &#39;Greenland Institute of Natural R' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-19f6c1c5dd' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreigikl2pzb6nbw3ajbvhspulary6zlz2ehhulpkdnim55rpbzt4pxy' alt='Looks like a fantastic postdoc...deadline soon &#39;Greenland Institute of Natural R'>
</a>

---

<div id='post-17'></div>

##### 📄 Chemical timber tracing: combining tree-genera information lowers reference data needs and makes harvest location identification more accurate

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Pieter Zuidema** <a href='https://bsky.app/profile/pieterzuidema.bsky.social' target='_blank' rel='noopener'>@pieterzuidema.bsky.social</a> &middot; <time datetime='2026-05-20'>2026-05-20</time> &middot; 💚 8 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-methods'>methods</span><span class='tag tag-temperate-forest'>temperate-forest</span></div>
    {% raw %}Where does our timber come from? In new study led by Jakub Truszkowski, we show timber tracing gets more accurate if wood chemical data from different tree species are combined. Data gaps of 1 species are filled by the others. 🍁🌐🌍 Timtrace forest woodtrade{% endraw %}
<br><b>link:</b> <a href='https://link.springer.com/article/10.1186/s13595-026-01341-x' target='_blank' rel='noopener'>https://link.springer.com/article/10.1186/s13595-026-01341-x</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:dkwchij2aqfq2mngtbzbes2n/post/3mmbz7pe5dk22' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-4f012f0888' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:dkwchij2aqfq2mngtbzbes2n/bafkreihdusliks2besqdfa4ms6f3bufmoyaj3in3tanh3m54e6ezrzfsza' alt='Where does our timber come from? In new study led by Jakub Truszkowski, we show ' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-4f012f0888' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:dkwchij2aqfq2mngtbzbes2n/bafkreihdusliks2besqdfa4ms6f3bufmoyaj3in3tanh3m54e6ezrzfsza' alt='Where does our timber come from? In new study led by Jakub Truszkowski, we show '>
</a>

---

<div id='post-18'></div>

##### 📄 [FRB-CESAB] CESABINARs - Fondation pour la recherche sur la biodiversité

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **FRB｜Fondation pour la recherche sur la biodiversité** <a href='https://bsky.app/profile/frbiodiv.bsky.social' target='_blank' rel='noopener'>@frbiodiv.bsky.social</a> &middot; <time datetime='2026-05-20'>2026-05-20</time> &middot; 💚 5 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-tropical-forest'>tropical-forest</span><span class='tag tag-traits'>traits</span></div>
    {% raw %}[🌐 Webinar] The next CESABINAR will be on the 28th of May 2026 on the results of the INTRACO group 🗣️ Unravelling the role of intraspecific variability in tree species coexistence in tropical forest 🔎All info: bit.ly/40zrT7v 🧪🌐 @idiv-research.bsky.social{% endraw %}
<br><b>link:</b> <a href='https://bit.ly/40zrT7v' target='_blank' rel='noopener'>https://bit.ly/40zrT7v</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:niursuowepii6gcvzzwhupfn/post/3mmbvx7vcck2j' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-a3078a6f71' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:niursuowepii6gcvzzwhupfn/bafkreia5dndn6p2rdzfgvrbqgn6eao52nriffagyxlqp46aqs34tddjwhe' alt='[🌐 Webinar] The next CESABINAR will be on the 28th of May 2026 on the results of' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-a3078a6f71' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:niursuowepii6gcvzzwhupfn/bafkreia5dndn6p2rdzfgvrbqgn6eao52nriffagyxlqp46aqs34tddjwhe' alt='[🌐 Webinar] The next CESABINAR will be on the 28th of May 2026 on the results of'>
</a>

---

<div id='post-19'></div>

##### 📄 Building the Infrastructure for Reproducible Biodiversity Science

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Brian J. Enquist** <a href='https://bsky.app/profile/bjenquist.bsky.social' target='_blank' rel='noopener'>@bjenquist.bsky.social</a> &middot; <time datetime='2026-05-20'>2026-05-20</time> &middot; 💚 15 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-data'>data</span><span class='tag tag-traits'>traits</span><span class='tag tag-macroecology'>macroecology</span><span class='tag tag-methods'>methods</span></div>
    {% raw %}BIEN 4.2: A Reproducible Standard for Global Plant Biodiversity Data 🧪🌐 Traits ObservationRecords PlotData CitizenScience @methodsinecoevol.bsky.social{% endraw %}
<br><b>link:</b> <a href='https://methodsblog.com/2026/03/30/building-the-infrastructure-for-reproducible-biodiversity-science/' target='_blank' rel='noopener'>https://methodsblog.com/2026/03/30/building-the-infrastructure-for-reproducible-biodiversity-science/</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:rca6ykp24ubm3alkgmcblslc/post/3mmbtmax2rk2o' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-0ee418903c' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreibpobdq23wsmwgbnefzwysq5hzib5gfdnqxhawhxq32nzoorvwkje' alt='BIEN 4.2: A Reproducible Standard for Global Plant Biodiversity Data 🧪🌐 Traits O' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-0ee418903c' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreibpobdq23wsmwgbnefzwysq5hzib5gfdnqxhawhxq32nzoorvwkje' alt='BIEN 4.2: A Reproducible Standard for Global Plant Biodiversity Data 🧪🌐 Traits O'>
</a>

---

<div id='post-20'></div>

##### 📄 Global Ecology Vol. 3

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Nicolas Mouquet** <a href='https://bsky.app/profile/nmouquet.bsky.social' target='_blank' rel='noopener'>@nmouquet.bsky.social</a> &middot; <time datetime='2026-05-20'>2026-05-20</time> &middot; 💚 13 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Global Ecology 🌐 starter pack Vol. 3 is full and curated !! ✨💚 Vol. 3 👉 go.bsky.app/MkLHiKU Let’s start a Vol. 4 🕺🏼 👉 go.bsky.app/Dsk4TQ3 simply reply or DM if you want to be in ! Please share so they will fly into the bluesky 🦋🌈 🌐🧪🌍🦤🍁🦑🪴🦉🐍🌾{% endraw %}
<br><b>link:</b> <a href='https://go.bsky.app/MkLHiKU' target='_blank' rel='noopener'>https://go.bsky.app/MkLHiKU</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:ppsghcl5bbpgjcljnhra353s/post/3mmbt6f2ju22i' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-2df69ad56c' aria-label='Enlarge image'>
      <img src='https://ogcard.cdn.bsky.app/start/did:plc:ppsghcl5bbpgjcljnhra353s/3lhycfce4ar2p' alt='Global Ecology 🌐 starter pack Vol. 3 is full and curated !! ✨💚 Vol. 3 👉 go.bsky.' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-2df69ad56c' aria-label='Close enlarged image'>
  <img src='https://ogcard.cdn.bsky.app/start/did:plc:ppsghcl5bbpgjcljnhra353s/3lhycfce4ar2p' alt='Global Ecology 🌐 starter pack Vol. 3 is full and curated !! ✨💚 Vol. 3 👉 go.bsky.'>
</a>

---

<div id='post-21'></div>

##### 📄 The Origin of Life in the Light of Evolution

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Ricard Solé** <a href='https://bsky.app/profile/ricardsole.bsky.social' target='_blank' rel='noopener'>@ricardsole.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 42 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-microbiome'>microbiome</span></div>
    {% raw %}How can evolutionary theory guide our understanding of life origins? Here, Betül Kaçar @kacarlab.bsky.social and co. propose that, since LUCA was already a complex, adapted population, life must have deep evolutionary roots preceding it. arxiv.org/abs/2605.05464{% endraw %}
<br><b>link:</b> <a href='https://arxiv.org/abs/2605.05464' target='_blank' rel='noopener'>https://arxiv.org/abs/2605.05464</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:pghayd3bor4lqoum3o6sgcxf/post/3mmaighjdcs2g' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-675b68ed2d' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:pghayd3bor4lqoum3o6sgcxf/bafkreige7d5fyp7urbn2iusqulxiqs5tzdm7cvvhmwrt5n3iwgmswwmonq' alt='How can evolutionary theory guide our understanding of life origins? Here, Betül' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-675b68ed2d' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:pghayd3bor4lqoum3o6sgcxf/bafkreige7d5fyp7urbn2iusqulxiqs5tzdm7cvvhmwrt5n3iwgmswwmonq' alt='How can evolutionary theory guide our understanding of life origins? Here, Betül'>
</a>

---

<div id='post-22'></div>

##### 📄 Global taxon-stratified high-resolution GBIF sampling-effort dataset for bias-aware SDMs <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Ahmed El-Gabbas** <a href='https://bsky.app/profile/ahmed-elgabbas.bsky.social' target='_blank' rel='noopener'>@ahmed-elgabbas.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 2 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-data'>data</span><span class='tag tag-macroecology'>macroecology</span><span class='tag tag-methods'>methods</span></div>
    {% raw %}1/ 🚨 New open resource for biodiversity & SDMs 🚨 My paper is now published in Diversity and Distributions: "A global, taxon-stratified, high-resolution sampling-effort dataset from GBIF for bias-aware ecological modelling"{% endraw %}
<br><b>link:</b> <a href='https://doi.org/10.1111/ddi.70205' target='_blank' rel='noopener'>https://doi.org/10.1111/ddi.70205</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:m4ilafth4dpjqoni6unlrkb4/post/3mm7kwbnnbc2s' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-625650f50f' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:m4ilafth4dpjqoni6unlrkb4/bafkreia5plrvbtvgumlfk5k65ddo2xydvdb2egzydkrc66u36ojyu4r3re' alt='1/ 🚨 New open resource for biodiversity &amp; SDMs 🚨 My paper is now published in Di' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-625650f50f' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:m4ilafth4dpjqoni6unlrkb4/bafkreia5plrvbtvgumlfk5k65ddo2xydvdb2egzydkrc66u36ojyu4r3re' alt='1/ 🚨 New open resource for biodiversity &amp; SDMs 🚨 My paper is now published in Di'>
</a>

---

<div id='post-23'></div>

##### 📄 Scaling laws for functional diversity and specialization across complex systems <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Brian J. Enquist** <a href='https://bsky.app/profile/bjenquist.bsky.social' target='_blank' rel='noopener'>@bjenquist.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 18 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span><span class='tag tag-traits'>traits</span></div>
    {% raw %}Scaling laws for function diversity and specialization across socioeconomic and biological complex system "Once functions are introduced, their growth follows a remarkably universal pattern across all systems" @sfiscience.bsky.social 🧪🌐{% endraw %}
<br><b>link:</b> <a href='https://www.pnas.org/doi/10.1073/pnas.2509729123' target='_blank' rel='noopener'>https://www.pnas.org/doi/10.1073/pnas.2509729123</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:rca6ykp24ubm3alkgmcblslc/post/3mm7cvqeauc26' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-79df91b8a3' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/avatar/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreicvixey3tlyzlbtyktwcukq4bakyzj3zmxeu2anfdzc76u63x2y5y' alt='Scaling laws for function diversity and specialization across socioeconomic and ' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:50%;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
    <div style='text-align:center;font-size:0.68rem;color:#999;margin-top:3px;'>author</div>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-79df91b8a3' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/avatar/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreicvixey3tlyzlbtyktwcukq4bakyzj3zmxeu2anfdzc76u63x2y5y' alt='Scaling laws for function diversity and specialization across socioeconomic and '>
</a>

---

<div id='post-24'></div>

##### 📄 Atlas of Botany Discussion & Book Signing

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Jess Rickenback** <a href='https://bsky.app/profile/jess-rickenback.bsky.social' target='_blank' rel='noopener'>@jess-rickenback.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 10 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-book'>book</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Join me and the other authors of the Atlas of Botany at the Royal Botanic Garden Edinburgh on June 11th for some fascinating stories of ancient evolution, innovative adaptation, and how plants shape the world around us today. Plus a signing! 🌎🌐 botany biogeography{% endraw %}
<br><b>link:</b> <a href='https://rbgeshop.org/products/atlas-of-botany-book-signing?pr_prod_strat=e5_desc&pr_rec_id=5c4328909&pr_rec_pid=15862224388469&pr_ref_pid=15710013325685&pr_seq=uniform' target='_blank' rel='noopener'>https://rbgeshop.org/products/atlas-of-botany-book-signing?pr_prod_strat=e5_desc&pr_rec_id=5c4328909&pr_rec_pid=15862224388469&pr_ref_pid=15710013325685&pr_seq=uniform</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:siieb5m57znytybo22elbkjh/post/3mm7bpamxsc2h' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-d604d8729b' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:siieb5m57znytybo22elbkjh/bafkreihqaj7c4ymaqe5rh6nl2eqkgi35aojyo6vxs5thpgizfwqkh44ooi' alt='Join me and the other authors of the Atlas of Botany at the Royal Botanic Garden' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-d604d8729b' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:siieb5m57znytybo22elbkjh/bafkreihqaj7c4ymaqe5rh6nl2eqkgi35aojyo6vxs5thpgizfwqkh44ooi' alt='Join me and the other authors of the Atlas of Botany at the Royal Botanic Garden'>
</a>

---

<div id='post-25'></div>

##### 📄 Higher and more stable biological control of herbivores in diversified strip cropping systems <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Brian J. Enquist** <a href='https://bsky.app/profile/bjenquist.bsky.social' target='_blank' rel='noopener'>@bjenquist.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 13 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-ecosystem-services'>ecosystem-services</span></div>
    {% raw %}Higher and more stable biological control of multiple herbivore species in diversified strip cropping systems 🌐🧪https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.70381{% endraw %}
<br><b>link:</b> <a href='https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.70381' target='_blank' rel='noopener'>https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2664.70381</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:rca6ykp24ubm3alkgmcblslc/post/3mm7235zn7c2b' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-b2ba7cf656' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreibetvcgzse6pvshoj2pgxuj6tewlm43epldscwdi5io55xrd2brmm' alt='Higher and more stable biological control of multiple herbivore species in diver' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-b2ba7cf656' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:rca6ykp24ubm3alkgmcblslc/bafkreibetvcgzse6pvshoj2pgxuj6tewlm43epldscwdi5io55xrd2brmm' alt='Higher and more stable biological control of multiple herbivore species in diver'>
</a>

---

<div id='post-26'></div>

##### 📄 Symbiotic bacteria may support calcium carbonate precipitation in the Gulf toadfish

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **PLOS Biology** <a href='https://bsky.app/profile/plosbiology.org' target='_blank' rel='noopener'>@plosbiology.org</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 14 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-fish'>fish</span><span class='tag tag-microbiome'>microbiome</span><span class='tag tag-biogeochemistry'>biogeochemistry</span></div>
    {% raw %}What role do fish play in the oceanic CarbonCycle via calcium carbonate precipitation? Study of the toadfish gut by @delcampolab.bsky.social reveals Vibrio bacteria that could aid CaCO3 precipitation, expanding the role of symbiosis in marine biomineralization @plosbiology.org 🧪 plos.io/49aY5SH{% endraw %}
<br><b>link:</b> <a href='https://plos.io/49aY5SH' target='_blank' rel='noopener'>https://plos.io/49aY5SH</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:5522ztebtekoor5efelihqhb/post/3mm6vzmoefs2s' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-e5685198a0' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:5522ztebtekoor5efelihqhb/bafkreihbz2rrrwdueavmhe7acvfykampc5wnzlath544pm2o4o7ipcyzpq' alt='What role do fish play in the oceanic CarbonCycle via calcium carbonate precipit' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-e5685198a0' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:5522ztebtekoor5efelihqhb/bafkreihbz2rrrwdueavmhe7acvfykampc5wnzlath544pm2o4o7ipcyzpq' alt='What role do fish play in the oceanic CarbonCycle via calcium carbonate precipit'>
</a>

---

<div id='post-27'></div>

##### 📄 Population density shapes home range size and overlap in animal movement <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Luca Santini** <a href='https://bsky.app/profile/lsantinieco.bsky.social' target='_blank' rel='noopener'>@lsantinieco.bsky.social</a> &middot; <time datetime='2026-05-19'>2026-05-19</time> &middot; 💚 11 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}What is the relationship between population density and home range? and what are the implications in terms of home range overlap? Our latest paper tackling these questions is finally out! @giannijacca.bsky.social @marleetucker.bsky.social{% endraw %}
<br><b>link:</b> <a href='https://nsojournals.onlinelibrary.wiley.com/doi/10.1002/ecog.07936' target='_blank' rel='noopener'>https://nsojournals.onlinelibrary.wiley.com/doi/10.1002/ecog.07936</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:uklfqjiv2ymojvoa4x3fnqnt/post/3mm6r4dznps2q' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-9389c514fc' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:uklfqjiv2ymojvoa4x3fnqnt/bafkreieuamrb2qjmnsm3w7yda3g4uzcte65bwpbzxeob67gkprd24e43iu' alt='What is the relationship between population density and home range? and what are' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-9389c514fc' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:uklfqjiv2ymojvoa4x3fnqnt/bafkreieuamrb2qjmnsm3w7yda3g4uzcte65bwpbzxeob67gkprd24e43iu' alt='What is the relationship between population density and home range? and what are'>
</a>

---

<div id='post-28'></div>

##### 📄 May 2026 Funk Biogeography Seminar — Michael Landis

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **The International Biogeography Society** <a href='https://bsky.app/profile/biogeography.bsky.social' target='_blank' rel='noopener'>@biogeography.bsky.social</a> &middot; <time datetime='2026-05-18'>2026-05-18</time> &middot; 💚 27 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}Join us on May 27th for the next Funk Biogeography Seminar! Dr. Michael Landis from Washington University in St. Louis will be speaking about the use of phylogenetic models to explore the biogeography of the past. Learn more and register here:{% endraw %}
<br><b>link:</b> <a href='https://www.biogeography.org/news/news/may-2026-funk-biogeography-seminar-michael-landis/' target='_blank' rel='noopener'>https://www.biogeography.org/news/news/may-2026-funk-biogeography-seminar-michael-landis/</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:3ggu2dvau7qi4ptkdynzho3x/post/3mm5jbni4ac2p' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-4a22823316' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:3ggu2dvau7qi4ptkdynzho3x/bafkreicvrh4ym73pg2bl3cqu25eozftbjazkv66um63cavbwjx4i22shmm' alt='Join us on May 27th for the next Funk Biogeography Seminar! Dr. Michael Landis f' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-4a22823316' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:3ggu2dvau7qi4ptkdynzho3x/bafkreicvrh4ym73pg2bl3cqu25eozftbjazkv66um63cavbwjx4i22shmm' alt='Join us on May 27th for the next Funk Biogeography Seminar! Dr. Michael Landis f'>
</a>

---

<div id='post-29'></div>

##### 📄 Alpine shrubification documented across European mountains using GLORIA network data <small style='color:#888;font-weight:normal;font-size:0.7em;vertical-align:middle;'>✨ AI title</small>

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Dr. Mariana García Criado** <a href='https://bsky.app/profile/nanitundra.bsky.social' target='_blank' rel='noopener'>@nanitundra.bsky.social</a> &middot; <time datetime='2026-05-18'>2026-05-18</time> &middot; 💚 5 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-climate'>climate</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}🏔️ European mountains get shrubbier 🏔️ In this Research Highlight @globalchangebio.bsky.social, Sarah Elmendorf and myself discuss Vanneste et al. (2026)'s wonderful paper, who found extensive alpine shrubification based on data from the GLORIA network. 🌐🧪🌱🌍{% endraw %}
<br><b>link:</b> <a href='http://dx.doi.org/10.1111/gcb.70922' target='_blank' rel='noopener'>http://dx.doi.org/10.1111/gcb.70922</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:aqbuq4s7636ofwdji3vjafqm/post/3mm57j53un226' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-a4fb8124bf' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/avatar_thumbnail/plain/did:plc:aqbuq4s7636ofwdji3vjafqm/bafkreieyhpmuvhoujm23noogzlzsjpbux4in5d7lulfvn7cq3orehyy6zq' alt='🏔️ European mountains get shrubbier 🏔️ In this Research Highlight @globalchangeb' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-a4fb8124bf' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/avatar_thumbnail/plain/did:plc:aqbuq4s7636ofwdji3vjafqm/bafkreieyhpmuvhoujm23noogzlzsjpbux4in5d7lulfvn7cq3orehyy6zq' alt='🏔️ European mountains get shrubbier 🏔️ In this Research Highlight @globalchangeb'>
</a>

---

<div id='post-30'></div>

##### 📄 An underground census: what bacteria and fungi tell us about ecosystem health – Soil & Landscape Science Lab

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Raphael Viscarra Rossel** <a href='https://bsky.app/profile/ravr19.bsky.social' target='_blank' rel='noopener'>@ravr19.bsky.social</a> &middot; <time datetime='2026-05-18'>2026-05-18</time> &middot; 💚 8 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-soil'>soil</span><span class='tag tag-microbiome'>microbiome</span><span class='tag tag-ecosystem-services'>ecosystem-services</span></div>
    {% raw %}I've written a short blog post on the bacterial-to-fungal richness ratio and the implications of the research for ecosystem health assessments. 🌏🌐🧪🔬 SoilHealth SoilScience Microbiome @slsl-curtinuni.bsky.social 📝 Blog:{% endraw %}
<br><b>link:</b> <a href='https://ravr19.github.io/slsl_blog/posts/2026-05-18-bactfung/' target='_blank' rel='noopener'>https://ravr19.github.io/slsl_blog/posts/2026-05-18-bactfung/</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:v66fqvnkwos7gwuivu4wzgs5/post/3mm56qymdak2b' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-32e34236cb' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/avatar_thumbnail/plain/did:plc:v66fqvnkwos7gwuivu4wzgs5/bafkreihqhs7xka3to7kf2u64btzx5vcymzmmgwvf5i4ve62mcygyoi6kii' alt='I&#39;ve written a short blog post on the bacterial-to-fungal richness ratio and the' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-32e34236cb' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/avatar_thumbnail/plain/did:plc:v66fqvnkwos7gwuivu4wzgs5/bafkreihqhs7xka3to7kf2u64btzx5vcymzmmgwvf5i4ve62mcygyoi6kii' alt='I&#39;ve written a short blog post on the bacterial-to-fungal richness ratio and the'>
</a>

---

<div id='post-31'></div>

##### 📄 Ecological role of emergent properties in the chemodiversity landscape

<p style='font-size:0.88em;color:#666;margin:-0.3em 0 0.8em 0;'>Shared by **Robin Heinen** <a href='https://bsky.app/profile/robinheinennl.bsky.social' target='_blank' rel='noopener'>@robinheinennl.bsky.social</a> &middot; <time datetime='2026-05-18'>2026-05-18</time> &middot; 💚 52 likes on Bluesky</p>

<div class='post-row' style='display:flex;gap:1rem;align-items:flex-start;margin:0.5rem 0 1rem 0;'>
  <div class='post-text' style='flex:1 1 auto;min-width:0;'>
    <div class='tag-row'><span class='tag tag-traits'>traits</span><span class='tag tag-ecosystem-services'>ecosystem-services</span><span class='tag tag-macroecology'>macroecology</span></div>
    {% raw %}An exciting day today. Our work, co-led by @mhanusch.bsky.social and @thomasdussarrat.bsky.social was published in Nature Ecology & Evolution. In this review, we explore the concept of chemodiversity and what role it could play for ecological functions at landscape-level.{% endraw %}
<br><b>link:</b> <a href='https://doi.org/10.1038/s41559-026-03057-7' target='_blank' rel='noopener'>https://doi.org/10.1038/s41559-026-03057-7</a><br>
    <br><span style='display:flex;justify-content:space-between;align-items:baseline;'><a href='https://bsky.app/profile/did:plc:jggts57ydp66iwg2pbddu4n5/post/3mm4z7w7lnc2f' target='_blank' rel='noopener'>View Original Post on Bluesky</a><a href='#' style='font-size:0.8rem;color:#aaa;text-decoration:none;' title='Back to top'>&#8593; Up</a></span>
  </div>
  <div class='post-image' style='flex:0 0 140px;'>
    <a href='#lb-80bcfae87c' aria-label='Enlarge image'>
      <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jggts57ydp66iwg2pbddu4n5/bafkreig5t4erhb2ytdl73g5kiz6omaj62xde3uhzkrzfi7vdotczdjk5tu' alt='An exciting day today. Our work, co-led by @mhanusch.bsky.social and @thomasduss' loading='lazy' width='140' height='140' style='width:140px;height:140px;object-fit:cover;border-radius:6px;display:block;cursor:zoom-in;background:#f3f3f3;border:1px solid #eee;'>
    </a>
  </div>
</div>
<a href='#_' class='lightbox' id='lb-80bcfae87c' aria-label='Close enlarged image'>
  <img src='https://cdn.bsky.app/img/feed_thumbnail/plain/did:plc:jggts57ydp66iwg2pbddu4n5/bafkreig5t4erhb2ytdl73g5kiz6omaj62xde3uhzkrzfi7vdotczdjk5tu' alt='An exciting day today. Our work, co-led by @mhanusch.bsky.social and @thomasduss'>
</a>

---

---

<p style='font-size:0.95rem;'><a href='/feeddigest.github.io/'>🏠 Back to home</a> &nbsp;&middot;&nbsp; <a href='/feeddigest.github.io/archives/'>📚 All digests</a></p>

<div style='text-align:center; margin:1.5rem 0;'>
  <a href='https://hits.sh/globalecologybs.github.io/feeddigest.github.io/' target='_blank' rel='noopener'>
    <img alt='Visitor count' src='https://hits.sh/globalecologybs.github.io/feeddigest.github.io.svg?style=flat-square&label=visitors&color=2d6cdf&labelColor=555'>
  </a>
</div>

<div style='text-align:center; font-size:small; color:gray;'>
  This page is maintained by <a href='http://nicolasmouquet.free.fr/' target='_blank' rel='noopener' style='color:gray;'>Nicolas Mouquet</a>
</div>

