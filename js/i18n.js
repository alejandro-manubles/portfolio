// Translations object
const translations = {
  es: {
    nav: {
      home: "Inicio",
      about: "Sobre mí",
      projects: "Proyectos",
      contact: "Contacto"
    },
    hero: {
      greeting: "Hola, soy",
      subtitle: "Graduado en Matemáticas · Programación · Análisis de Datos",
      viewProjects: "Ver proyectos",
      contact: "Contacto"
    },
    about: {
      title: "Sobre mí",
      paragraph1: "Graduado en Matemáticas con formación en programación, análisis de datos y optimización. Me interesa especialmente la aplicación de métodos matemáticos a problemas reales, así como el desarrollo de algoritmos y modelos computacionales.",
      paragraph2: "Durante el grado he trabajado con Python, R, Java y MATLAB en contextos académicos relacionados con minería de datos, análisis numérico y teoría de grafos.",
      downloadCV: "Descargar CV",
      skills: {
        optimization: "Optimización y Estadística"
      }
    },
    projects: {
      title: "Proyectos académicos",
      dropout: {
        title: "Predicción de abandono estudiantil",
        tech: "Minería de datos · R",
        description: "Análisis exploratorio de datos y construcción de un modelo predictivo orientado a la identificación de patrones y factores de riesgo de abandono estudiantil."
      },
      metro: {
        title: "Análisis de la red de Metro de Madrid",
        tech: "Análisis de redes · R",
        description: "Modelización del Metro de Madrid para estudiar su conectividad, estaciones críticas, rutas óptimas y fracturación mediante teoría de grafos y análisis de redes."
      }
    },
    contact: {
      title: "Contacto",
      description: "Si te interesa mi perfil o quieres comentar algún proyecto, puedes escribirme.",
      email: "Email:"
    }
  },
  en: {
    nav: {
      home: "Home",
      about: "About",
      projects: "Projects",
      contact: "Contact"
    },
    hero: {
      greeting: "Hi, I'm",
      subtitle: "Mathematics Graduate · Programming · Data Analysis",
      viewProjects: "View Projects",
      contact: "Contact"
    },
    about: {
      title: "About Me",
      paragraph1: "Mathematics graduate with training in programming, data analysis, and optimization. I am particularly interested in applying mathematical methods to real-world problems, as well as developing algorithms and computational models.",
      paragraph2: "During my degree, I have worked with Python, R, Java, and MATLAB in academic contexts related to data mining, numerical analysis, and graph theory.",
      downloadCV: "Download CV",
      skills: {
        optimization: "Optimization & Statistics"
      }
    },
    projects: {
      title: "Academic Projects",
      dropout: {
        title: "Student Dropout Prediction",
        tech: "Data Mining · R",
        description: "Exploratory data analysis and construction of a predictive model aimed at identifying patterns and risk factors for student dropout."
      },
      metro: {
        title: "Madrid Metro Network Analysis",
        tech: "Network Analysis · R",
        description: "Modeling of the Madrid Metro to study its connectivity, critical stations, optimal routes, and fragmentation using graph theory and network analysis."
      }
    },
    contact: {
      title: "Contact",
      description: "If you're interested in my profile or would like to discuss any project, feel free to reach out.",
      email: "Email:"
    }
  }
};

// Language management
class LanguageManager {
  constructor() {
    this.currentLang = localStorage.getItem('preferredLanguage') || 'es';
    this.initialized = false;
    this.init();
  }

  init() {
    // Wait for DOM to be fully loaded
    const initialize = () => {
      if (this.initialized) return;
      
      try {
        // Set initial language
        this.setLanguage(this.currentLang, false);
        
        // Add event listeners to language buttons
        const langButtons = document.querySelectorAll('.lang-btn');
        if (langButtons.length > 0) {
          langButtons.forEach(btn => {
            btn.addEventListener('click', (e) => {
              e.preventDefault();
              const lang = e.currentTarget.dataset.lang;
              if (lang) {
                this.setLanguage(lang);
              }
            });
          });
        }

        // Update active button state
        this.updateButtonStates();
        this.initialized = true;
      } catch (error) {
        console.error('Error initializing language manager:', error);
      }
    };

    // Try to initialize after a short delay to ensure DOM is ready
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => {
        setTimeout(initialize, 100);
      });
    } else {
      setTimeout(initialize, 100);
    }
  }

  setLanguage(lang, save = true) {
    if (!translations[lang]) {
      console.warn(`Language '${lang}' not found, defaulting to 'es'`);
      lang = 'es';
    }

    this.currentLang = lang;
    
    // Save preference
    if (save) {
      localStorage.setItem('preferredLanguage', lang);
    }

    // Update HTML lang attribute
    document.documentElement.lang = lang;

    // Update all translatable elements
    this.updateContent();

    // Update CV link
    this.updateCVLink();

    // Update button states
    this.updateButtonStates();
  }

  updateContent() {
    const elements = document.querySelectorAll('[data-i18n]');
    
    elements.forEach(element => {
      const key = element.dataset.i18n;
      const translation = this.getNestedTranslation(key);
      
      if (translation) {
        element.textContent = translation;
      }
    });
  }

  getNestedTranslation(key) {
    const keys = key.split('.');
    let value = translations[this.currentLang];
    
    for (let k of keys) {
      if (value && value[k] !== undefined) {
        value = value[k];
      } else {
        return null;
      }
    }
    
    return value;
  }

  updateCVLink() {
    const cvLink = document.getElementById('cv-link');
    if (cvLink) {
      if (this.currentLang === 'es') {
        cvLink.href = 'cv/ES_CV_Alejandro_García.pdf';
      } else {
        cvLink.href = 'cv/EN_CV_Alejandro_García.pdf';
      }
    }
  }

  updateButtonStates() {
    const langButtons = document.querySelectorAll('.lang-btn');
    langButtons.forEach(btn => {
      if (btn.dataset.lang === this.currentLang) {
        btn.classList.add('active');
      } else {
        btn.classList.remove('active');
      }
    });
  }

  getCurrentLanguage() {
    return this.currentLang;
  }
}

// Initialize language manager only once
if (!window.languageManager) {
  window.languageManager = new LanguageManager();
}
