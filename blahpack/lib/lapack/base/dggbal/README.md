# dggbal

> Balances a pair of general real matrices (A,B) for the generalized eigenvalue problem.

<section class="usage">

## Usage

```javascript
var dggbal = require( '@stdlib/lapack/base/dggbal' );
```

#### dggbal.ndarray( job, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, WORK, strideWORK, offsetWORK )

Balances a pair of general real matrices (A,B) for the generalized eigenvalue problem.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var LSCALE = new Float64Array( 2 );
var RSCALE = new Float64Array( 2 );
var WORK = new Float64Array( 12 );

var result = dggbal.ndarray( 'both', 2, A, 1, 2, 0, B, 1, 2, 0, LSCALE, 1, 0, RSCALE, 1, 0, WORK, 1, 0 );
// returns { info: 0, ilo: 1, ihi: 2 }
```

The function has the following parameters:

-   **job**: specifies the operation type (`'none'`, `'permute'`, `'scale'`, or `'both'`).
-   **N**: order of matrices A and B.
-   **A**: first real matrix (modified in-place).
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: second real matrix (modified in-place).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **LSCALE**: left scaling/permutation factors (length N).
-   **strideLSCALE**: stride length for `LSCALE`.
-   **offsetLSCALE**: starting index for `LSCALE`.
-   **RSCALE**: right scaling/permutation factors (length N).
-   **strideRSCALE**: stride length for `RSCALE`.
-   **offsetRSCALE**: starting index for `RSCALE`.
-   **WORK**: workspace array (length >= 6*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

The function returns an object with properties `info` (0 = success), `ilo` (1-based), and `ihi` (1-based).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   This routine involves two phases: (1) permuting A and B by similarity transformations to isolate eigenvalues, and (2) applying diagonal similarity transformations to rows and columns `ilo` to `ihi` to make the rows and columns as close in norm as possible.
-   The `job` parameter controls which phases are applied: `'none'` does nothing, `'permute'` applies only the permutation phase, `'scale'` applies only the scaling phase, and `'both'` applies both phases.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dggbal = require( '@stdlib/lapack/base/dggbal' );

var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var LSCALE = new Float64Array( 2 );
var RSCALE = new Float64Array( 2 );
var WORK = new Float64Array( 12 );

var result = dggbal.ndarray( 'both', 2, A, 1, 2, 0, B, 1, 2, 0, LSCALE, 1, 0, RSCALE, 1, 0, WORK, 1, 0 );
console.log( result );
// => { info: 0, ilo: 1, ihi: 2 }
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
