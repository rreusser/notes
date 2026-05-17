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

# zungtsqr_row

> Generate an `M`-by-`N` complex matrix `Q` with orthonormal columns from the output of `zlatsqr`, using a row-block (GETT) sweep.

<section class="usage">

## Usage

```javascript
var zungtsqr_row = require( '@stdlib/lapack/base/zungtsqr_row' );
```

#### zungtsqr_row( order, M, N, mb, nb, A, LDA, T, LDT, WORK )

Generates an `M`-by-`N` complex matrix `Q` with orthonormal columns from the output of `zlatsqr`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlatsqr = require( '@stdlib/lapack/base/zlatsqr' );

var A = new Complex128Array( [ 4.0, 0.0, 1.0, 0.0, 1.0, 0.0, 5.0, 0.0, 1.0, 0.0, 1.0, 0.0 ] );
var T = new Complex128Array( 4 );
var WORK = new Complex128Array( 2 );
zlatsqr( 'column-major', 3, 2, 4, 2, A, 3, T, 2, WORK );

WORK = new Complex128Array( 2 );
zungtsqr_row( 'column-major', 3, 2, 4, 2, A, 3, T, 2, WORK );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A` (with `M >= N`).
-   **mb**: row block size used by `zlatsqr` (`mb > N`).
-   **nb**: column block size used by `zlatsqr` (`1 <= nb <= N` when `N > 0`).
-   **A**: input/output `Complex128Array` matrix containing the lower-trapezoidal `V` blocks on entry and the orthonormal `Q` on exit.
-   **LDA**: leading dimension of `A`.
-   **T**: `Complex128Array` containing the upper-triangular block reflector factors produced by `zlatsqr`.
-   **LDT**: leading dimension of `T`.
-   **WORK**: workspace `Complex128Array` of length at least `nblocal * max(nblocal, N - nblocal)` where `nblocal = min(nb, N)`.

#### zungtsqr_row.ndarray( M, N, mb, nb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK )

Generates an `M`-by-`N` complex matrix `Q` with orthonormal columns from the output of `zlatsqr`, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlatsqr = require( '@stdlib/lapack/base/zlatsqr' ).ndarray;

var A = new Complex128Array( [ 4.0, 0.0, 1.0, 0.0, 1.0, 0.0, 5.0, 0.0, 1.0, 0.0, 1.0, 0.0 ] );
var T = new Complex128Array( 4 );
var WORK = new Complex128Array( 2 );
zlatsqr( 3, 2, 4, 2, A, 1, 3, 0, T, 1, 2, 0, WORK, 1, 0 );

WORK = new Complex128Array( 2 );
zungtsqr_row.ndarray( 3, 2, 4, 2, A, 1, 3, 0, T, 1, 2, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zungtsqr_row` reconstructs the orthonormal factor `Q` from the row-block-by-column-block compact-WY storage produced by `zlatsqr`. The algorithm sweeps row blocks bottom-up so the resulting `Q` is built in row-major order — hence the `_row` suffix.
-   `T` is treated as `nb`-by-(`N` * `Number_of_row_blocks`) where `Number_of_row_blocks = ceil((M - N) / (mb - N))` when `mb < M`, and `1` otherwise.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlatsqr = require( '@stdlib/lapack/base/zlatsqr' );
var zungtsqr_row = require( '@stdlib/lapack/base/zungtsqr_row' );

var A = new Complex128Array( 6 * 2 );
var view = A.toBuffer || null; // illustrate Complex128Array shape
var T = new Complex128Array( 2 * 2 );
var WORK = new Complex128Array( 4 );
zlatsqr( 'column-major', 6, 2, 4, 2, A, 6, T, 2, WORK );
zungtsqr_row( 'column-major', 6, 2, 4, 2, A, 6, T, 2, WORK );
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
