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

# zgeql2

> Compute a QL factorization of a complex M-by-N matrix

<section class="usage">

## Usage

```javascript
var zgeql2 = require( '@stdlib/lapack/base/zgeql2' );
```

#### zgeql2( order, M, N, A, LDA, TAU, strideTAU, WORK, strideWORK )

Compute a QL factorization of a complex M-by-N matrix

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgeql2 = require( '@stdlib/lapack/base/zgeql2' );

var M = 3;
var N = 2;
var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 1, 5, 1, 6, 1 ] );
var TAU = new Complex128Array( N );
var WORK = new Complex128Array( N );

zgeql2( 'column-major', M, N, A, M, TAU, 1, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.

#### zgeql2.ndarray( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK )

Compute a QL factorization of a complex M-by-N matrix, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgeql2 = require( '@stdlib/lapack/base/zgeql2' );

var M = 3;
var N = 2;
var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 1, 5, 1, 6, 1 ] );
var TAU = new Complex128Array( N );
var WORK = new Complex128Array( N );

zgeql2( 'column-major', M, N, A, M, TAU, 1, WORK, 1 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   See LAPACK reference documentation for full algorithmic details.
</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgeql2 = require( '@stdlib/lapack/base/zgeql2' );

var M = 3;
var N = 2;
var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 1, 5, 1, 6, 1 ] );
var TAU = new Complex128Array( N );
var WORK = new Complex128Array( N );

zgeql2( 'column-major', M, N, A, M, TAU, 1, WORK, 1 );
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
