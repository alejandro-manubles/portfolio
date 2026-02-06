const translations = {
  es: {
    nav: {
      home: "Inicio",
      about: "Sobre mí",
      projects: "Proyectos",
      contact: "Contacto",
    },
    hero: {
      greeting: "Hola, soy",
      subtitle: "Graduado en Matemáticas · Programación · Análisis de Datos",
      projects: "Ver proyectos",
      contact: "Contacto",
    },
    about: {
      title: "Sobre mí",
      paragraph1: `Graduado en Matemáticas con formación en programación, análisis de datos y optimización.
      Me interesa especialmente la aplicación de métodos matemáticos a problemas reales,
      así como el desarrollo de algoritmos y modelos computacionales.`,
      paragraph2: `Durante el grado he trabajado con Python, R, Java y MATLAB en contextos académicos
      relacionados con minería de datos, análisis numérico y teoría de grafos.`,
      cv: "Descargar CV",
      skills: "Optimización y Estadística",
    },
    projects: {
      title: "Proyectos académicos",
      dropout: {
        title: "Predicción de abandono estudiantil",
        tags: "Minería de datos · R",
        desc: "Análisis exploratorio de datos y construcción de un modelo predictivo orientado a la identificación de patrones y factores de riesgo de abandono estudiantil.",
      },
      metro: {
        title: "Análisis de la red de Metro de Madrid",
        tags: "Análisis de redes · R",
        desc: "Modelización del Metro de Madrid para estudiar su conectividad, estaciones críticas, rutas óptimas y fracturación mediante teoría de grafos y análisis de redes.",
      },
    },
    contact: {
      title: "Contacto",
      text: "Si te interesa mi perfil o quieres comentar algún proyecto, puedes escribirme.",
    },
  },

  en: {
    nav: {
      home: "Home",
      about: "About me",
      projects: "Projects",
      contact: "Contact",
    },
    hero: {
      greeting: "Hi, I'm",
      subtitle: "BSc in Mathematics · Programming · Data Analysis",
      projects: "View projects",
      contact: "Contact",
    },
    about: {
      title: "About me",
      paragraph1: `Mathematics graduate with a strong background in programming, data analysis and optimisation.
      I am particularly interested in applying mathematical methods to real-world problems,
      as well as in the development of algorithms and computational models.`,
      paragraph2:`During my degree, I have worked with Python, R, Java and MATLAB in academic contexts
      related to data mining, numerical analysis and graph theory.`,
      cv: "Download CV",
      skills: "Optimisation and Statistics",
    },
    projects: {
      title: "Academic projects",
      dropout: {
        title: "Student dropout prediction",
        tags: "Data mining · R",
        desc: "Exploratory data analysis and development of a predictive model aimed at identifying patterns and risk factors associated with student dropout.",
      },
      metro: {
        title: "Madrid Metro network analysis",
        tags: "Network analysis · R",
        desc: "Modelling of the Madrid Metro system to study connectivity, critical stations, optimal routes and network fragmentation using graph theory and network analysis.",
      },
    },
    contact: {
      title: "Contact",
      text: "If you are interested in my profile or would like to discuss a project, feel free to get in touch.",
    },
  },
};

function setLanguage(lang) {
  document.documentElement.lang = lang;
  document.querySelectorAll("[data-i18n]").forEach(el => {
    const keys = el.dataset.i18n.split(".");
    let value = translations[lang];
    keys.forEach(k => value = value[k]);
    el.innerHTML = value;
  });
  localStorage.setItem("lang", lang);
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
}

document.querySelectorAll("[data-lang]").forEach(btn => {
  btn.addEventListener("click", () => setLanguage(btn.dataset.lang));
});

const savedLang = localStorage.getItem("lang") || "es";
setLanguage(savedLang);
