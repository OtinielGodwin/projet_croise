const BASE_URL = "http://172.22.215.130:8080";

// Testons uniquement la France et un dataset sûr
const dataset = "sequences";
const country = "FR";
const trimester = "2020Q1"; 

const url = `${BASE_URL}/archived/${dataset}/${country}/${trimester}`;

console.log(`Tentative sur : ${url}`);

try {
  const response = await fetch(url);
  console.log(`Statut HTTP : ${response.status}`);
  
  if (response.ok) {
    const data = await response.json();
    console.log("✅ DONNÉES REÇUES :", JSON.stringify(data).substring(0, 100));
  } else {
    console.log("❌ Le serveur dit que cette donnée n'existe pas (404) ou est protégée (401).");
  }
} catch (e) {
  console.log("⚠️ Erreur de connexion au serveur.");
}