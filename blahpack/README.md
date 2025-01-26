# blahpack

> Semi-automated translation of BLAS/LAPACK to JavaScript

## Introduction

For various reasons, there is not a straightforward way to use BLAS/LAPACK in the browser. Cross-compiling to WebAssembly carries memory management issues along with it, and manual translation is extremely labor-intensive and often not straightforward. This project aims for "guided manual translation" in the sense that a watchful eye, careful validation, and manual performance optimization remain imperative. But perhaps, *perhaps* some of the labor can be automated away.

## Example

Consider a simplified [DGEMV](https://netlib.org/lapack/explore-html-3.6.1/d7/d15/group__double__blas__level2_gadd421a107a488d524859b4a64c1901a9.html) routine, [sample.f](./sample.f):

```fortran
      SUBROUTINE DGEMV(M,N,ALPHA,A,LDA,X,BETA,Y)
*  -- Reference BLAS level2 routine --
*     y := alpha*A*x + beta*y
      DOUBLE PRECISION ALPHA,BETA
      INTEGER LDA,M,N
      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
      DOUBLE PRECISION TEMP
      INTEGER I, J
      DO 20 I = 1,M
            Y(I) = BETA*Y(I)
  20  CONTINUE
      DO 60 J = 1,N
            TEMP = ALPHA*X(J)
            DO 50 I = 1,M
                  Y(I) = Y(I) + TEMP*A(I,J)
  50        CONTINUE
  60  CONTINUE
      RETURN
      END
```

Translating to [ESTree](https://github.com/estree/estree) and then generating JavaScript,

```bash
$ cat sample.f | ./fortran_to_estree.py | ./estree_to_js.js
```

yields:

```javascript
function dgemv(m, n, alpha, a, lda, x, beta, y) {
  //  -- Reference BLAS level2 routine --
  //     y := alpha*A*x + beta*y
  var temp;
  var i;
  var j;
  for (i = 0; i < m; i += 1) {
    y[i] = beta * y[i];
  }
  for (j = 0; j < n; j += 1) {
    temp = alpha * x[j];
    for (i = 0; i < m; i += 1) {
      y[i] = y[i] + temp * a[j + i * lda];
    }
  }
  return;
}
```

## License

&copy; 2025 Ricky Reusser. MIT License.
