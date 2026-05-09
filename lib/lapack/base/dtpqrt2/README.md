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

# dtpqrt2

> Computes a QR factorization of a real triangular-pentagonal matrix using the compact WY representation for `Q`.

<section class="usage">

## Usage

```javascript
var dtpqrt2 = require( '@stdlib/lapack/base/dtpqrt2' );
```

#### dtpqrt2( order, M, N, l, A, LDA, B, LDB, T, LDT )

Computes a QR factorization of a real triangular-pentagonal matrix `C = [A; B]` using the compact WY representation for `Q`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 0.0, 0.5, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.3, 0.5, 1.1 ] );
var T = new Float64Array( 4 );
var info = dtpqrt2( 'column-major', 2, 2, 0, A, 2, B, 2, T, 2 );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `B`.
-   **N**: number of columns of `B` and order of `A`.
-   **l**: number of rows of the upper trapezoidal part of `B` (`0 <= l <= min(M,N)`).
-   **A**: `N`-by-`N` input/output upper triangular matrix; on exit contains the upper triangular factor `R`.
-   **LDA**: leading dimension of `A`.
-   **B**: `M`-by-`N` input/output pentagonal matrix; on exit contains the Householder reflectors `V`.
-   **LDB**: leading dimension of `B`.
-   **T**: `N`-by-`N` output upper triangular factor `T` of the block reflector.
-   **LDT**: leading dimension of `T`.

#### dtpqrt2.ndarray( M, N, l, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, T, strideT1, strideT2, offsetT )

Computes a QR factorization of a real triangular-pentagonal matrix `C = [A; B]` using the compact WY representation for `Q`, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 0.0, 0.5, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.3, 0.5, 1.1 ] );
var T = new Float64Array( 4 );
var info = dtpqrt2.ndarray( 2, 2, 0, A, 1, 2, 0, B, 1, 2, 0, T, 1, 2, 0 );
// info => 0
```

The function has the following additional parameters:

-   **M**: number of rows of `B`.
-   **N**: number of columns of `B` and order of `A`.
-   **l**: number of rows of the upper trapezoidal part of `B`.
-   **A**: `N`-by-`N` input/output matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: `M`-by-`N` input/output matrix.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.
-   **T**: `N`-by-`N` output matrix.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `A` is an `N`-by-`N` upper triangular matrix; on exit it contains the upper triangular factor `R`.
-   `B` is an `M`-by-`N` pentagonal matrix whose first `M - l` rows are rectangular and whose last `l` rows form an upper trapezoidal block; on exit it contains the Householder reflector representation `V`.
-   `T` is the `N`-by-`N` upper triangular factor of the compact-WY block reflector.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtpqrt2 = require( '@stdlib/lapack/base/dtpqrt2' );

var A = new Float64Array( [ 2.0, 0.0, 0.0, 0.5, 3.0, 0.0, 0.25, 0.75, 4.0 ] );
var B = new Float64Array( [ 1.1, 0.0, 0.0, 0.4, 1.5, 0.0, 0.6, 0.3, 1.7 ] );
var T = new Float64Array( 9 );
var info = dtpqrt2( 'column-major', 3, 3, 3, A, 3, B, 3, T, 3 );
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

</section>

<!-- /.links -->
