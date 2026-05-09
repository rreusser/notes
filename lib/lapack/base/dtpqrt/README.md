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

# dtpqrt

> Compute a blocked QR factorization of a real triangular-pentagonal matrix using the compact WY representation for `Q`.

<section class="usage">

## Usage

```javascript
var dtpqrt = require( '@stdlib/lapack/base/dtpqrt' );
```

#### dtpqrt( order, M, N, l, nb, A, LDA, B, LDB, T, LDT, WORK )

Computes a blocked QR factorization of a real triangular-pentagonal matrix `C = [ A; B ]` using the compact WY representation for `Q`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 0.0, 0.5, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.3, 0.5, 1.1 ] );
var T = new Float64Array( 4 );
var WORK = new Float64Array( 4 );
var info = dtpqrt( 'column-major', 2, 2, 0, 2, A, 2, B, 2, T, 2, WORK );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `B`.
-   **N**: number of columns of `B` and order of `A`.
-   **l**: number of rows of the upper trapezoidal part of `B` (`0 <= l <= min(M,N)`).
-   **nb**: block size (`1 <= nb`; if `N > 0`, also `nb <= N`).
-   **A**: input/output matrix; on exit contains the upper triangular factor `R`.
-   **LDA**: leading dimension of `A`.
-   **B**: input/output pentagonal matrix; on exit contains the Householder reflectors `V`.
-   **LDB**: leading dimension of `B`.
-   **T**: output matrix of upper triangular block reflector factors.
-   **LDT**: leading dimension of `T` (`LDT >= nb` for column-major).
-   **WORK**: workspace array of length at least `nb*N`.

#### dtpqrt.ndarray( M, N, l, nb, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK )

Computes a blocked QR factorization of a real triangular-pentagonal matrix `C = [ A; B ]` using the compact WY representation for `Q`, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 0.0, 0.5, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.3, 0.5, 1.1 ] );
var T = new Float64Array( 4 );
var WORK = new Float64Array( 4 );
var info = dtpqrt.ndarray( 2, 2, 0, 2, A, 1, 2, 0, B, 1, 2, 0, T, 1, 2, 0, WORK, 1, 0 );
// info => 0
```

The function has the following additional parameters:

-   **M**: number of rows of `B`.
-   **N**: number of columns of `B` and order of `A`.
-   **l**: number of rows of the upper trapezoidal part of `B`.
-   **nb**: block size.
-   **A**: input/output matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input/output pentagonal matrix.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.
-   **T**: output matrix of block reflector factors.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **WORK**: workspace array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `A` is an `N`-by-`N` upper triangular matrix; on exit it contains the upper triangular factor `R` of the QR factorization of `[ A; B ]`.
-   `B` is an `M`-by-`N` pentagonal matrix whose first `M - l` rows are rectangular and whose last `l` rows form an upper trapezoidal block; on exit it contains the Householder reflector representation `V`.
-   `T` stores the `ib`-by-`ib` upper triangular block reflector factors `T1, T2, ..., TB` for each panel, packed side-by-side as an `nb`-by-`N` matrix `T = [T1 T2 ... TB]`.
-   `nb` partitions the columns of `A` into panels of width `nb` (the last panel may be smaller). Each panel is factored by the unblocked kernel `dtpqrt2`, then the resulting block reflector is applied to the trailing columns via `dtprfb`.
-   `WORK` must be sized for at least `nb*N` doubles.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtpqrt = require( '@stdlib/lapack/base/dtpqrt' );

var A = new Float64Array( [ 2.0, 0.0, 0.0, 0.0, 0.5, 3.0, 0.0, 0.0, 0.25, 0.75, 4.0, 0.0, 0.1, 0.2, 0.3, 5.0 ] );
var B = new Float64Array( [ 1.0, 0.3, 0.7, 0.0, 0.5, 1.1, 0.4, 1.3, 0.25, 0.6, 1.2, 0.5, 0.1, 0.2, 0.9, 0.6 ] );
var T = new Float64Array( 8 );
var WORK = new Float64Array( 8 );
var info = dtpqrt( 'column-major', 4, 4, 2, 2, A, 4, B, 4, T, 2, WORK );
console.log( 'info: ' + info );
console.log( T );
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
