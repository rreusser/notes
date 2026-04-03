# dlagv2

> Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular.

<section class="usage">

## Usage

```javascript
var dlagv2 = require( '@stdlib/lapack/base/dlagv2' );
```

#### dlagv2.ndarray( A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, alphar, strideALPHAR, offsetALPHAR, alphai, strideALPHAI, offsetALPHAI, beta, strideBETA, offsetBETA )

Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular. Computes orthogonal rotation matrices Q (CSL, SNL) and Z (CSR, SNR) such that Q^T * A * Z and Q^T * B * Z are upper triangular (real eigenvalues) or B is diagonal (complex eigenvalues).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 4.0, 2.0, 1.0, 3.0 ] );
var B = new Float64Array( [ 2.0, 0.0, 1.0, 1.0 ] );
var alphar = new Float64Array( 2 );
var alphai = new Float64Array( 2 );
var beta = new Float64Array( 2 );

var result = dlagv2.ndarray( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 );
// result => { CSL: ~0.973, SNL: ~0.230, CSR: ~0.851, SNR: ~0.526 }
```

The function has the following parameters:

-   **A**: input/output 2-by-2 matrix A.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input/output 2-by-2 upper triangular matrix B.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **alphar**: output array for real parts of eigenvalue numerators.
-   **strideALPHAR**: stride length for `alphar`.
-   **offsetALPHAR**: starting index for `alphar`.
-   **alphai**: output array for imaginary parts of eigenvalue numerators.
-   **strideALPHAI**: stride length for `alphai`.
-   **offsetALPHAI**: starting index for `alphai`.
-   **beta**: output array for eigenvalue denominators.
-   **strideBETA**: stride length for `beta`.
-   **offsetBETA**: starting index for `beta`.

The function returns an object with fields `CSL`, `SNL`, `CSR`, `SNR` (the Givens rotation parameters). The eigenvalues of the pencil (A,B) are `(ALPHAR[k] + i*ALPHAI[k]) / BETA[k]` for k=0,1.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   On entry, A is a general 2-by-2 matrix and B is upper triangular. On exit, A and B are overwritten by the generalized Schur form.
-   If the eigenvalues are real, A is reduced to upper triangular form. If the eigenvalues are complex conjugate, B is made diagonal with `B[0,0] >= B[1,1] > 0`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlagv2 = require( '@stdlib/lapack/base/dlagv2' );

var A = new Float64Array( [ 1.0, 3.0, -5.0, 1.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var alphar = new Float64Array( 2 );
var alphai = new Float64Array( 2 );
var beta = new Float64Array( 2 );

var result = dlagv2.ndarray( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 );
// returns { CSL: 1.0, SNL: 0.0, CSR: 1.0, SNR: 0.0 }
// Complex conjugate eigenvalues: (1 +/- 3.873i) / 1
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
