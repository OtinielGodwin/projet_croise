const BASE_URL = "http://172.22.215.130:8080";
const countries = ["PT","CH","DE","GB","CZ","FR","DK","LV","RU","HR","SI","GR","IT","RO","LT","SE","ES","BE","NO","FI","PL","NL","BY","LU","UA","AL","IE","AT","EE","RS","HU","ME","BG","SK","MD","IS"];

for (const country of countries) {
  const dir = `data_social/${country}`;
  try { await Deno.mkdir(dir, { recursive: true }); } catch (e) {}

  for (let y = 1980; y <= 2026; y++) {
    for (const q of ["Q1", "Q2", "Q3", "Q4"]) {
      const url = `${BASE_URL}/archived/social/${country}/${y}${q}`;
      try {
        const res = await fetch(url);
        if (res.status === 200) {
          const txt = await res.text();
          await Deno.writeTextFile(`${dir}/soc_${y}${q}.txt`, txt);
          console.log(`✅ SOCIAL ${country}: ${y}${q}`);
        }
      } catch (e) {}
    }
  }
}