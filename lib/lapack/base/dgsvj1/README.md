<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# dgsvj1

> Pre-processor for dgesvj applying Jacobi rotations to off-diagonal block pivots

<section class="usage">

## Usage

```javascript
var dgsvj1 = require( '@stdlib/lapack/base/dgsvj1' );
```

#### dgsvj1( order, jobv, M, N, n1, A, LDA, d, strideD, sva, strideSVA, mv, V, LDV, eps, sfmin, tol, nsweep, work, strideWORK, lwork )

Pre-processor for dgesvj applying Jacobi rotations to off-diagonal block pivots

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var EPS = 2.220446049250313e-16;
var SFMIN = 2.2250738585072014e-308;
var TOL = 1.0e-10;

var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
var d = new Float64Array( [ 1, 1, 1 ] );
var sva = new Float64Array( [ Math.sqrt( 30 ), Math.sqrt( 174 ), Math.sqrt( 446 ) ] );
var V = new Float64Array( 1 );
var work = new Float64Array( 4 );

var info = dgsvj1( 'column-major', 'no-v', 4, 3, 1, A, 4, d, 1, sva, 1, 0, V, 1, EPS, SFMIN, TOL, 5, work, 1, 4 );
// info: 0 (early convergence) or 4 (all sweeps used)
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **jobv**: one of `'compute-v'` (accumulate rotations into `V`), `'apply-v'` (post-multiply an `mv`-by-`N` `V`), or `'no-v'` (discard rotations).
-   **M**: number of rows.
-   **N**: number of columns.
-   **n1**: 2x2 block partition; the first `n1` columns are rotated against the remaining `N-n1` columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **sva**: input array.
-   **strideSVA**: stride length for `sva`.
-   **mv**: mv.
-   **V**: output matrix.
-   **LDV**: leading dimension of `V`.
-   **eps**: eps.
-   **sfmin**: sfmin.
-   **tol**: tol.
-   **nsweep**: nsweep.
-   **work**: input array.
-   **strideWORK**: stride length for `work`.
-   **lwork**: lwork.

#### dgsvj1.ndarray( jobv, M, N, n1, A, strideA1, strideA2, offsetA, d, strideD, offsetD, sva, strideSVA, offsetSVA, mv, V, strideV1, strideV2, offsetV, eps, sfmin, tol, nsweep, work, strideWORK, offsetWORK, lwork )

Pre-processor for dgesvj applying Jacobi rotations to off-diagonal block pivots, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var EPS = 2.220446049250313e-16;
var SFMIN = 2.2250738585072014e-308;
var TOL = 1.0e-10;

var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
var d = new Float64Array( [ 1, 1, 1 ] );
var sva = new Float64Array( [ Math.sqrt( 30 ), Math.sqrt( 174 ), Math.sqrt( 446 ) ] );
var V = new Float64Array( 1 );
var work = new Float64Array( 4 );

var info = dgsvj1.ndarray( 'no-v', 4, 3, 1, A, 1, 4, 0, d, 1, 0, sva, 1, 0, 0, V, 1, 1, 0, EPS, SFMIN, TOL, 5, work, 1, 0, 4 );
```

The function has the following additional parameters:

-   **jobv**: one of `'compute-v'`, `'apply-v'`, or `'no-v'`.
-   **M**: number of rows.
-   **N**: number of columns.
-   **n1**: block partition; the first `n1` columns are rotated against the remaining `N-n1` columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.
-   **sva**: input array.
-   **strideSVA**: stride length for `sva`.
-   **offsetSVA**: starting index for `SVA`.
-   **mv**: mv.
-   **V**: output matrix.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
-   **eps**: eps.
-   **sfmin**: sfmin.
-   **tol**: tol.
-   **nsweep**: nsweep.
-   **work**: input array.
-   **strideWORK**: stride length for `work`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgsvj1` is the off-diagonal-block companion to [`dgsvj0`][@stdlib/lapack/base/dgsvj0]. It is invoked from `dgesvj` after `dgsvj0` has done the diagonal-block sweeps, to suppress only the (1,2) off-diagonal coupling between the first `n1` columns and the remaining `N - n1` columns. There is no de Rijk pivoting, no `lkahead`, and no convergence test inside the q-loop — the routine just performs `nsweep` block-row-cyclic Jacobi sweeps over the off-diagonal tile grid.
-   `tol` must be strictly greater than `eps` (otherwise the routine returns the negative argument-error code `-21`).
-   `lwork` must be at least `M`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dgsvj1 = require( '@stdlib/lapack/base/dgsvj1' );

var EPS = 2.220446049250313e-16;
var SFMIN = 2.2250738585072014e-308;
var TOL = 1.0e-10;

var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
var d = new Float64Array( [ 1, 1, 1 ] );
var sva = new Float64Array( [ Math.sqrt( 30 ), Math.sqrt( 174 ), Math.sqrt( 446 ) ] );
var V = new Float64Array( 1 );
var work = new Float64Array( 4 );

var info = dgsvj1( 'column-major', 'no-v', 4, 3, 1, A, 4, d, 1, sva, 1, 0, V, 1, EPS, SFMIN, TOL, 5, work, 1, 4 );
console.log( info );
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

[@stdlib/lapack/base/dgsvj0]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
