# dggbak

> Forms the right or left eigenvectors of a real generalized eigenvalue problem by backward transformation on the computed eigenvectors of the balanced matrix output by DGGBAL.

<section class="usage">

## Usage

```javascript
var dggbak = require( '@stdlib/lapack/base/dggbak' );
```

#### dggbak( order, job, side, N, ilo, ihi, LSCALE, strideLSCALE, RSCALE, strideRSCALE, M, V, LDV )

Forms the right or left eigenvectors of a real generalized eigenvalue problem by backward transformation on the computed eigenvectors of the balanced matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var LSCALE = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var RSCALE = new Float64Array( [ 2.0, 3.0, 0.5 ] );
var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

var info = dggbak( 'column-major', 'scale', 'right', 3, 1, 3, LSCALE, 1, RSCALE, 1, 2, V, 3 );
// V => [ 2.0, 6.0, 1.5, 8.0, 15.0, 3.0 ]
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **job**: specifies the type of backward transformation: `'none'`, `'permute'`, `'scale'`, or `'both'`.
-   **side**: `'right'` for right eigenvectors, `'left'` for left eigenvectors.
-   **N**: number of rows of `V`.
-   **ilo**: index of the first balanced row (1-based, from DGGBAL).
-   **ihi**: index of the last balanced row (1-based, from DGGBAL).
-   **LSCALE**: left scaling/permutation factors from DGGBAL.
-   **strideLSCALE**: stride for `LSCALE`.
-   **RSCALE**: right scaling/permutation factors from DGGBAL.
-   **strideRSCALE**: stride for `RSCALE`.
-   **M**: number of columns of `V`.
-   **V**: eigenvector matrix (modified in-place).
-   **LDV**: leading dimension of `V`.

#### dggbak.ndarray( job, side, N, ilo, ihi, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, M, V, strideV1, strideV2, offsetV )

Forms the right or left eigenvectors using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var LSCALE = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var RSCALE = new Float64Array( [ 2.0, 3.0, 0.5 ] );
var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

var info = dggbak.ndarray( 'scale', 'right', 3, 1, 3, LSCALE, 1, 0, RSCALE, 1, 0, 2, V, 1, 3, 0 );
// V => [ 2.0, 6.0, 1.5, 8.0, 15.0, 3.0 ]
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dggbak` is typically called after `dggbal` balances a matrix pair and eigenvalues/eigenvectors have been computed from the balanced pair. It reverses the balancing to recover eigenvectors of the original pair.
-   The `ilo` and `ihi` parameters are 1-based indices matching the Fortran convention used by DGGBAL.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dggbak = require( '@stdlib/lapack/base/dggbak' );

// Scale right eigenvectors of a 3x2 matrix:
var LSCALE = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var RSCALE = new Float64Array( [ 2.0, 3.0, 0.5 ] );
var V = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

var info = dggbak( 'column-major', 'scale', 'right', 3, 1, 3, LSCALE, 1, RSCALE, 1, 2, V, 3 );
// returns 0

console.log( V );
// => Float64Array [ 2.0, 6.0, 1.5, 8.0, 15.0, 3.0 ]
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
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
