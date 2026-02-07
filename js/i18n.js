let translations = {};

async function loadLanguage(lang) {
  const response = await fetch(`/portfolio/lang/${lang}.json`);
  translations = await response.json();
  applyTranslations(lang);
}

function applyTranslations(lang) {
  document.documentElement.lang = lang;

  document.querySelectorAll("[data-i18n]").forEach(el => {
    const keys = el.dataset.i18n.split(".");
    let value = translations;
    keys.forEach(k => value = value[k]);
    el.innerHTML = value;
  });

  document.querySelectorAll(".lang-btn").forEach(btn => {
    btn.classList.toggle("active", btn.dataset.lang === lang);
  });

  const cvBtn = document.getElementById("cv-btn");
  if (cvBtn) {
    cvBtn.href =
      lang === "en"
        ? "cv/EN_CV_Alejandro_Garcia.pdf"
        : "cv/ES_CV_Alejandro_Garcia.pdf";
  }

  localStorage.setItem("lang", lang);
}

document.querySelectorAll("[data-lang]").forEach(btn => {
  btn.addEventListener("click", () => loadLanguage(btn.dataset.lang));
});

const savedLang = localStorage.getItem("lang") || "es";
loadLanguage(savedLang);
