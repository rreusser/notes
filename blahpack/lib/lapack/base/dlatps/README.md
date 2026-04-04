# dlatps

> Solves a triangular system with scaling to prevent overflow, where the matrix is in packed storage.

<section class="usage">

## Usage

```javascript
var dlatps = require( '@stdlib/lapack/base/dlatps' );
```

#### dlatps( uplo, trans, diag, normin, N, AP, x, strideX, scale, CNORM, strideCNORM )

Solves a triangular system with scaling to prevent overflow, where the matrix is in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Upper triangular 3x3 packed: A = [[2, 1, 1], [0, 3, 2], [0, 0, 4]]
var AP = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 2.0, 4.0 ] );
var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var scale = new Float64Array( 1 );
var CNORM = new Float64Array( 3 );

dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, x, 1, scale, CNORM, 1 );
// x => <Float64Array>[ ~0.0417, ~0.1667, 0.75 ]
// scale[ 0 ] => 1.0
```

The function has the following parameters:

-   **uplo**: `'upper'` or `'lower'` - specifies whether A is upper or lower triangular.
-   **trans**: `'no-transpose'` or `'transpose'` - specifies the operation.
-   **diag**: `'unit'` or `'non-unit'` - specifies whether A is unit triangular.
-   **normin**: `'yes'` or `'no'` - specifies whether CNORM contains column norms on input.
-   **N**: order of the matrix A.
-   **AP**: packed triangular matrix of length `N*(N+1)/2`.
-   **x**: right-hand side vector of length N (overwritten with the solution).
-   **strideX**: stride length for `x`.
-   **scale**: [`Float64Array`][mdn-float64array] of length 1; `scale[0]` returns the scaling factor s.
-   **CNORM**: [`Float64Array`][mdn-float64array] of length N for column norms.
-   **strideCNORM**: stride length for `CNORM`.

#### dlatps.ndarray( uplo, trans, diag, normin, N, AP, strideAP, offsetAP, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

Solves a triangular system with scaling to prevent overflow (packed storage), using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 2.0, 4.0 ] );
var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var scale = new Float64Array( 1 );
var CNORM = new Float64Array( 3 );

dlatps.ndarray( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, CNORM, 1, 0 );
// x => <Float64Array>[ ~0.0417, ~0.1667, 0.75 ]
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine solves `A*x = s*b` or `A^T*x = s*b` where `s` is a scaling factor chosen to prevent overflow.
-   If the matrix is singular (`A(j,j) = 0`), then `s` is set to `0` and a non-trivial solution to `A*x = 0` is returned.
-   When `normin` is `'no'`, the column norms of the off-diagonal part of A are computed and stored in `CNORM`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlatps = require( '@stdlib/lapack/base/dlatps' );

// Upper triangular 3x3 packed matrix:
var AP = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 2.0, 4.0 ] );
var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var scale = new Float64Array( 1 );
var CNORM = new Float64Array( 3 );

dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, x, 1, scale, CNORM, 1 );

console.log( 'x:', x );
console.log( 'scale:', scale[ 0 ] );
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

</section>

<!-- /.links -->
