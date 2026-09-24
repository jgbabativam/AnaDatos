# 📊 Analítica de Datos Aplicada a Estudios sobre Desarrollo

Repositorio con materiales académicos del curso **Analítica de datos aplicada a estudios sobre desarrollo** del Centro Interdisciplinario de Estudios sobre Desarrollo (CIDER) de la Universidad de los Andes. En este repositorio se incluyen diapositivas, talleres, bases de datos, recursos en R y contenidos de apoyo para el aprendizaje de los métodos cuantitativos más utilizados en los estudios sobre desarrollo.

**Docente:** Giovany Babativa-Márquez, PhD. · j.babativamarquez@uniandes.edu.co

**URL del curso:** https://jgbabativam.github.io/AnaDatos/

---

## 📖 Descripción del Curso

El proceso de investigación puede entenderse como el ensamble de cuatro fases: formulación de la(s) pregunta(s) de investigación, recopilación de datos, **análisis de datos** y redacción. Este curso se centra en la fase de análisis de datos.

Exploraremos y comprenderemos los métodos y herramientas cuantitativas ampliamente utilizados en los estudios sobre desarrollo en las últimas dos décadas: el análisis exploratorio de datos, el análisis de regresión y el análisis multivariado. Los estudios sobre desarrollo requieren de la investigación interdisciplinaria, cuyo valor radica en ver lo que una disciplina por sí sola no logra percibir y en incorporar métodos complementarios para brindar soluciones a problemas complejos.

El curso no presupone una amplia base matemática; los conceptos centrales no son los números sino **las variables y sus relaciones**. El enfoque es práctico, a través de ejercicios desarrollados con los estudiantes en **R**, sin concentrarse en demostraciones formales.

---

## 🎯 Objetivos

Al finalizar el curso, el estudiante estará en capacidad de:

* Aplicar los principales métodos de análisis de regresión.
* Aplicar los principales métodos de análisis multivariado.
* Combinar distintas herramientas para el análisis cuantitativo.
* Usar programas estadísticos para analizar datos cuantitativos.
* Informar y discutir los resultados del análisis cuantitativo y evaluar su uso en investigaciones publicadas.

---

## 🧠 Contenidos del Curso

Los temas a mi cargo se organizan en las siguientes unidades:

1. **Modelos no lineales: regresión logística**

   * Modelos supervisados de clasificación y modelos de respuesta discreta
   * *Odds*, razón de *odds* (OR) e interpretación de coeficientes
   * Prueba Chi-cuadrado, selección de variables, matriz de confusión y curva ROC
   * Estudio de caso: factores de riesgo asociados a la ansiedad (DASS-21)

2. **Introducción a las técnicas multivariantes**

   * Análisis de componentes principales (PCA)
   * Análisis de correspondencias simple y múltiple

3. **Análisis de conglomerados (clúster)**

   * Métodos no jerárquicos: algoritmo de *K*-medias
   * Métodos jerárquicos: algoritmo de Ward
   * Caracterización de los conglomerados

4. **Análisis factorial**

   * Análisis de factores comunes
   * Aplicación e interpretación de los factores

---

## 💻 Software utilizado

El curso tiene un componente práctico en el que los conceptos y métodos se complementan mediante ejercicios y aplicaciones computacionales utilizando **R**, **RStudio** y documentos reproducibles con **R Markdown** y **Quarto**.

### Instalación de R

Para trabajar con los materiales del curso se recomienda tener instalado **R** y **RStudio** ([descargar aquí](https://posit.co/download/rstudio-desktop/)).

Video guía de instalación:

* [Instalación de R](https://youtu.be/TKplIIwwdEk)

---

## 👨‍💻 Bases de programación en R

Antes de abordar los ejercicios computacionales, se recomienda revisar los siguientes recursos para familiarizarse con las herramientas básicas de programación y análisis de datos en **R**.

### Fundamentos

* [Introducción a R](https://youtu.be/TlOKN0UzbB0)
* [Importar datos](https://youtu.be/FkpIpLgI7es)
* [Operador pipeline](https://youtu.be/8V1IzCRsSd8)
* [Seleccionar filas o columnas](https://youtu.be/FduWB2BRcBo)
* [Crear nuevas columnas](https://youtu.be/5hoBTWEpDbo)
* [Estadísticos de resumen](https://youtu.be/NTrjYX0rf9Q)
* [Documentos reproducibles](https://youtu.be/dI11RlFP99o)

---

## 📦 Contenido del repositorio

* 📘 Presentaciones del curso en formato Quarto Revealjs
* 💻 Scripts y ejemplos reproducibles en R
* 📝 Talleres y lineamientos de las evaluaciones
* 📊 Bases de datos utilizadas en ejemplos y ejercicios

---

## 📚 Material del curso

| Tema                                                      | Material de apoyo                                                                                                   | Material de clase                                                                       |
| :-------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------ | :-------------------------------------------------------------------------------------- |
| Modelos no lineales: regresión logística                  | [Taller 3](docs/2Taller3.pdf)                                                                                       | [Ver diapositivas](https://jgbabativam.github.io/AnaDatos/5LogitRegression.html)        |
| Componentes principales y análisis de correspondencias    | --                                                                                                                  | [Ver diapositivas](https://jgbabativam.github.io/AnaDatos/6PCA.html)                    |
| Análisis de conglomerados (clúster)                       | --                                                                                                                  | [Ver diapositivas](https://jgbabativam.github.io/AnaDatos/7Cluster.html)                |
| Análisis factorial                                        | --                                                                                                                  | [Ver diapositivas](https://jgbabativam.github.io/AnaDatos/8AnaFac.html)                 |
| **Exposición final**                                      | [Lineamientos](docs/1LineamientosExpoFinal.pdf) <br> [Video tutorial](https://www.youtube.com/watch?v=W0TdujuLJSo) | —                                                                                       |

> Los enlaces se actualizarán a medida que se publiquen las diapositivas y talleres correspondientes.

---

## 📝 Evaluación

La evaluación del curso está distribuida de la siguiente manera:

| Actividad                     | Porcentaje |
| :---------------------------- | :--------: |
| Talleres (3)                  |    36 %    |
| Examen 1 (individual)         |    20 %    |
| Examen 2 (individual)         |    20 %    |
| Presentación final            |    24 %    |
| **Total**                     |  **100 %** |

Los talleres y la presentación final se desarrollan en los grupos de trabajo conformados durante el semestre. Los exámenes son de carácter **individual**.

---

## 📚 Bibliografía

### Regresión logística

* Çetinkaya-Rundel, M. & Hardin, J. (2021). *Introduction to Modern Statistics*. Secciones de *Regression modeling*: 7, 8, 9 y 10. Disponible en: https://openintro-ims.netlify.app/
* Hastie, T., Tibshirani, R. & Friedman, J. (2009). *The Elements of Statistical Learning: Data Mining, Inference, and Prediction*. Springer.

### Técnicas multivariantes

* Husson, F., Lê, S. & Pagès, J. (2017). *Exploratory Multivariate Analysis by Example Using R*. CRC Press.
* Hair, J. F., Black, W. C., Babin, B. J., Anderson, R. E. & Tatham, R. L. (2006). *Multivariate Data Analysis*. 6th Edition.
* Aldás Manzano, J. & Uriel Jiménez, E. (2017). *Análisis multivariante aplicado con R*. Ediciones Paraninfo.
* [Multivariate Statistical Analysis with R](https://bookdown.org/brian_nguyen0305/Multivariate_Statistical_Analysis_with_R/)
* [Técnicas multivariadas](https://bookdown.org/jsalinas/tecnicas_multivariadas/presentacion.html)

### Material de soporte para recordar conceptos previos

* Pérez-Tejada, H. (2008). *Estadística para las ciencias sociales, del comportamiento y de la salud*. 3a edición. [Disponible aquí](https://www.uv.mx/rmipe/files/2015/09/Estadistica-para-las-ciencias-sociales-del-comportamiento-y-de-la-salud.pdf). Capítulos 2, 5 y 6.
* [Temas de estadística básica](https://bookdown.org/aquintela/EBE/)
* [Khan Academy](https://es.khanacademy.org/)
* Good, P. I. & Hardin, J. W. (2012). *Common Errors in Statistics (and How to Avoid Them)*. John Wiley & Sons.

---

## Citación y derechos de autor

Material creado por **[Giovany Babativa-Márquez](https://github.com/jgbabativam)** para el curso **Analítica de datos aplicada a estudios sobre desarrollo** del CIDER, Universidad de los Andes.

Se distribuye bajo la [licencia MIT](LICENSE). Cualquier copia parcial o total debe citar al autor.

Babativa-Márquez J.G (2026). "Materiales del curso Analítica de datos aplicada a estudios sobre desarrollo". Universidad de los Andes. https://jgbabativam.github.io/AnaDatos/
