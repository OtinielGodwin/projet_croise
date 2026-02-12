const BASE_URL = "http://172.22.215.130:8080";
const countries = ["PT","CH","DE","GB","CZ","FR","DK","LV","RU","HR","SI","GR","IT","RO","LT","SE","ES","BE","NO","FI","PL","NL","BY","LU","UA","AL","IE","AT","EE","RS","HU","ME","BG","SK","MD","IS"];
const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

for (const country of countries) {
  for (let year = 1980; year <= 2026; year++) {
    const folderPath = `./data_sequences/${country}/${year}`;
    for (const q of ["Q1", "Q2", "Q3", "Q4"]) {
      const trimester = `${year}${q}`;
      const filePath = `${folderPath}/${trimester}.fasta`;
      try { await Deno.stat(filePath); continue; } catch {
        try {
          const res = await fetch(`${BASE_URL}/archived/sequences/${country}/${trimester}`);
          if (res.status === 200) {
            const txt = await res.text();
            if (txt.trim()) {
              await Deno.mkdir(folderPath, { recursive: true });
              await Deno.writeTextFile(filePath, txt);
              console.log(`SEQ OK: ${filePath}`);
            }
          }
          await sleep(500);
        } catch { await sleep(2000); }
      }
    }
  }
}