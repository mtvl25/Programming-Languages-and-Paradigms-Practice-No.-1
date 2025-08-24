# Programming Languages and Paradigms — Practice No. 1

**Paradigma Funcional (Haskell)**

**Autores**

* Juan Esteban Torres Peña
* María Paula Gómez Garcia
* Mathias Velez Londoño

**Institución:** EAFIT
**Periodo:** 2025-2

---

## 1) Descripción general

Esta práctica implementa tres bloques, con utilidades propias cuando el enunciado restringe funciones del Prelude:

1. **Funciones utilitarias**

   * `remData :: [Int] -> Int -> Int -> [Int]` filtra elementos dentro de un **intervalo cerrado**; tolera límites invertidos (`li > hs`).
   * `orderDesc :: [Float] -> [Float]` ordena una lista en **orden descendente** por inserción.

2. **Métodos numéricos con series**

   * `expSeries x n` aproxima e^x con:   Σ\_{k=0}^{n−1} x^k / k!
   * `cosSeries x n` aproxima cos(x) con: Σ\_{k=0}^{n−1} (−1)^k · x^(2k) / (2k)!
   * `ln1pSeries x n` aproxima ln(1+x) con: Σ\_{k=1}^{n} (−1)^(k+1) · x^k / k   (válida si |x| < 1)
     Auxiliares usadas: `pow`, `factD`, `altSign`, `valorAbsoluto`, etc., para reemplazar llamadas restringidas del Prelude.

3. **Transformada Discreta del Coseno (DCT‑II)**

   * `dct :: [Double] -> [Double]` calcula X(k) = a(k) · Σ\_{n=0}^{N−1} x(n) · cos( ((n+0.5)·π·k)/N ),
     con a(0)=√(1/N) y a(k>0)=√(2/N).

> Nota: se evitaron funciones restringidas (`sqrt`, `pi`, `map`, `length`, etc.) implementando equivalentes propios: `raizCuadrada` (Newton–Raphson), `miPi`, `mapear`, `longitud`.

---

## 2) Estructura del repositorio

```
.
├─ src/
│  ├─ NumericalSeries.hs   -- expSeries, cosSeries, ln1pSeries + auxiliares
│  ├─ RemData.hs           -- remData y helpers (min2, max2, enRangoCerrado)
│  ├─ OrderDesc.hs         -- insertDesc, orderDesc
│  ├─ DCT.hs               -- dct y auxiliares (coefA, terminoDCT, raizCuadrada, miPi)
│  └─ Main.hs              -- demo mínima (imprime DCT de [1..10])
├─ README.md               -- este reporte
└─ stack.yaml / cabal.project  (opcional)
```

---

## 3) Compilación y ejecución

Con **GHC** instalado:

```bash
# Compilar binario de demo (usa Main)
ghc -O2 -Wall -i./src -main-is Main src/Main.hs -o dct_demo
./dct_demo

# En GHCi (interactivo)
ghci -i./src
:load Main
-- ejemplos rápidos:
dct [1..10]
```

---

## 4) Uso de las funciones (ejemplos)

### 4.1 Utilitarias

```haskell
-- Mantener valores en [0,5]
remData [1, 25, 5, -4] 0 5      -- => [1,5]

-- Orden descendente
orderDesc [1, 25, 5, -4]        -- => [25,5,1,-4]
```

### 4.2 Series numéricas

```haskell
expSeries 1.5 6                 -- e^1.5 con 6 términos
cosSeries 1.5 6                 -- cos(1.5) con 6 términos
ln1pSeries 0.5 6                -- ln(1+0.5) con 6 términos (|x|<1)
```

> `ln1pSeries` solo converge para |x| < 1.

### 4.3 DCT‑II

```haskell
dct [1.0,2.0..10.0]
-- Resultado esperado (redondeo aprox):
-- [17.3925,-9.0249,0,-0.9667,0,-0.3162,0,-0.1279,0,-0.0359]
```

---

## 5) Pruebas y resultados por ítem

### 5.1 `remData` y `orderDesc`

| Entrada                   | Salida esperada |
| ------------------------- | --------------- |
| `remData [1,25,5,-4] 0 5` | `[1,5]`         |
| `remData [1,25,5,-4] 5 0` | `[1,5]`         |
| `orderDesc [1,25,5,-4]`   | `[25,5,1,-4]`   |

### 5.2 Aproximaciones por series (errores vs funciones de referencia)

**e^{1.5} ≈ 4.481689**

| Términos | Aproximación | Error % |
| -------: | -----------: | ------: |
|        3 |     3.625000 | 19.1153 |
|        5 |     4.398438 |  1.8576 |
|        7 |     4.477539 |  0.0926 |
|       10 |     4.481671 |  0.0004 |

**cos(1.5) ≈ 0.0707372**

| Términos |  Aproximación |  Error % |
| -------: | ------------: | -------: |
|        2 | -0.1250000000 | 276.7104 |
|        4 |  0.0701171875 |   0.8765 |
|        6 |  0.0707369341 |   0.0004 |
|       10 |  0.0707372017 |      \~0 |

**ln(1+0.5) ≈ 0.4054651**

| Términos | Aproximación | Error % |
| -------: | -----------: | ------: |
|        2 | 0.3750000000 |  7.5136 |
|        4 | 0.4010416667 |  1.0910 |
|        6 | 0.4046875000 |  0.1918 |
|       10 | 0.4054346478 |  0.0075 |

### 5.3 DCT‑II

Entrada `x = [1..10]` produce (redondeado):
`[17.3925, -9.0249, 0, -0.9667, 0, -0.3162, 0, -0.1279, 0, -0.0359]`.

---

## 6) Dificultades y soluciones

1. **`sqrt` prohibido y normalización de DCT**
   • Dificultad: a(k) requiere raíces cuadradas.
   • Solución: `raizCuadrada` (Newton–Raphson, tol 1e−12, máx 200 iters) y `miPi`.

2. **Convergencia de `ln1pSeries`**
   • Dificultad: solo converge para |x| < 1; cerca de 1 requiere más términos.
   • Solución: documentar dominio y mostrar error vs N.

3. **Crecimiento de `n!`**
   • Dificultad: pérdida de precisión en `Double` con n grandes.
   • Solución: `factD` con acumulador; pruebas con n moderado.

4. **Potencias con exponente negativo**
   • Dificultad: implementar `pow` sin potencias nativas.
   • Solución: `pow x n | n<0 = 1 / pow x (-n)`; evitar x=0 con n<0 en pruebas.

5. **Rendimiento sin Prelude “cómodo”**
   • Dificultad: evitar `map`, `length`, etc.
   • Solución: utilidades propias (`mapear`, `longitud`) y recursión con acumulador.

6. **Intervalo invertido en `remData`**
   • Dificultad: soportar `li > hs`.
   • Solución: normalización previa (`min2`/`max2`) y filtro con `enRangoCerrado`.

7. **Complejidad de `orderDesc`**
   • Dificultad: inserción es O(n²).
   • Solución: aceptable para tamaños de lista de la práctica; prioridad a claridad.

---

## 7) Conclusiones

* Las series para `exp` y `cos` convergen rápido con pocos términos; `ln(1+x)` necesita más términos cuando |x|→1.
* Reimplementar utilidades del Prelude refuerza recursión y pensamiento funcional.
* La DCT‑II con normalización correcta entrega resultados consistentes con el ejemplo de referencia.

---

## 8) Reproducibilidad

* **Compilador:** GHC 9.x
* **SO:** Windows / macOS / Linux
* **Ejecución rápida:** `ghci -i./src` y evaluar ejemplos de las secciones 4 y 5.

---

