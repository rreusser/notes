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

# dlatrz

> Factors an upper trapezoidal matrix by means of orthogonal transformations.

<section class="usage">

## Usage

```javascript
var dlatrz = require( '@stdlib/lapack/base/dlatrz' );
```

#### dlatrz( order, M, N, l, A, LDA, TAU, strideTAU, WORK, strideWORK )

Factors an upper trapezoidal matrix by means of orthogonal transformations.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([
    4.0, 0.0, 0.0,
    1.0, 5.0, 0.0,
    2.0, 1.0, 6.0,
    3.0, 2.0, 1.0,
    1.0, 4.0, 2.0
]);
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 3 );

dlatrz( 'column-major', 3, 5, 2, A, 3, TAU, 1, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **l**: number of columns containing the meaningful part of the Householder vectors (`N - M >= l >= 0`).
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.

#### dlatrz.ndarray( M, N, l, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK )

Factors an upper trapezoidal matrix by means of orthogonal transformations., using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([
    4.0, 0.0, 0.0,
    1.0, 5.0, 0.0,
    2.0, 1.0, 6.0,
    3.0, 2.0, 1.0,
    1.0, 4.0, 2.0
]);
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 3 );

dlatrz.ndarray( 3, 5, 2, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **l**: number of columns containing the meaningful part of the Householder vectors.
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

-   `dlatrz` factors the `M`-by-`(M+l)` real upper trapezoidal matrix `[ A1 A2 ] = [ A(0:M-1,0:M-1) A(0:M-1,N-l:N-1) ]` as `( R  0 ) * Z`, where `Z` is an `(M+l)`-by-`(M+l)` orthogonal matrix and `R` is `M`-by-`M` upper triangular.
-   On exit, the leading `M`-by-`M` upper triangular part of `A` contains `R`, and the trailing columns `N-l:N-1` of the first `M` rows, together with `TAU`, represent the orthogonal matrix `Z` as a product of `M` elementary reflectors.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlatrz = require( '@stdlib/lapack/base/dlatrz' );

var A = new Float64Array([
    4.0, 0.0, 0.0,
    1.0, 5.0, 0.0,
    2.0, 1.0, 6.0,
    3.0, 2.0, 1.0,
    1.0, 4.0, 2.0
]);
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 3 );

dlatrz( 'column-major', 3, 5, 2, A, 3, TAU, 1, WORK, 1 );
console.log( TAU );
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
