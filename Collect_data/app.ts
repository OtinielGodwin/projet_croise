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

console.log("Demarrage de l'extraction structuree...");

for (const dataset of datasets) {
  for (const country of countries) {
    // Creation de la structure : data/sequences/FR/
    const dirPath = `./data/${dataset}/${country}`;
    try {
      await Deno.mkdir(dirPath, { recursive: true });
    } catch (e) {
      // Le dossier existe deja
    }

    for (let year = startYear; year <= endYear; year++) {
      for (const q of trimesters) {
        const trimester = `${year}${q}`;
        const extension = dataset === "sequences" ? "fasta" : "json";
        const fileName = `${dataset}_${country}_${trimester}.${extension}`;
        const filePath = `${dirPath}/${fileName}`;

        // Verification si le fichier existe deja pour gagner du temps
        try {
          await Deno.stat(filePath);
          continue; // Si le fichier existe, on passe au suivant
        } catch (e) {
          // Le fichier n'existe pas, on le telecharge
        }

        const url = `${BASE_URL}/archived/${dataset}/${country}/${trimester}`;

        try {
          const response = await fetch(url);
          
          if (response.status === 200) {
            if (dataset === "sequences") {
              const content = await response.text();
              if (content && content.trim().length > 0) {
                await Deno.writeTextFile(filePath, content);
                console.log(`Enregistre : ${filePath}`);
              }
            } else {
              const data = await response.json();
              await Deno.writeTextFile(filePath, JSON.stringify(data, null, 2));
              console.log(`Enregistre : ${filePath}`);
            }
          }
        } catch (err) {
          console.error(`Erreur sur ${url}, passage au suivant...`);
        }
      }
    }
  }
}
console.log("Extraction terminee.");