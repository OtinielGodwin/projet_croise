const BASE_URL = "http://172.22.215.130:8080";
const datasets = ["sequences", "cancer", "biomedical", "social"];
const countries = [
  "PT","CH","DE","GB","CZ","FR","DK","LV","RU","HR","SI","GR","IT","RO",
  "LT","SE","ES","BE","NO","FI","PL","NL","BY","LU","UA","AL","IE","AT",
  "EE","RS","HU","ME","BG","SK","MD","IS"
];

const startYear = 1980;
const endYear = 2026;
const trimesters = ["Q1", "Q2", "Q3", "Q4"];

for (const dataset of datasets) {
  for (const country of countries) {
    for (let year = startYear; year <= endYear; year++) {
      const folderPath = `./data/${dataset}/${country}/${year}`;
      
      for (const q of trimesters) {
        const trimester = `${year}${q}`;
        const url = `${BASE_URL}/archived/${dataset}/${country}/${trimester}`;

        try {
          const response = await fetch(url);
          
          if (response.status === 200) {
            const content = await response.text();
            
            if (content && content.trim().length > 0) {
              await Deno.mkdir(folderPath, { recursive: true });

              const ext = dataset === "sequences" ? "fasta" : "txt";
              const filePath = `${folderPath}/${trimester}.${ext}`;
              
              await Deno.writeTextFile(filePath, content);
              console.log(`OK: ${filePath}`);
            }
          }
        } catch (err) {
        }
      }
    }
  }
}